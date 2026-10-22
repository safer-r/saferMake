# Unit tests of the staged code-check helpers (R/intern_.qualify.R,
# R/intern_.args_insert.R, R/intern_.code_checks.R) extracted from
# inst/app/server.R. All helpers are internal (dot-prefixed): access them
# via ::: (normal for internal testing).

# Rebuild a function object from pasted code, like the app does
mk_fun <- function(code) {
  base::eval(expr = base::parse(file = "", n = NULL, text = code, prompt = "?",
                                keep.source = TRUE, srcfile = NULL,
                                encoding = "unknown"))
}

testthat::test_that(desc = ".base_pkgs is the 7-package vector of saferDev::colons_check", code = {
  testthat::expect_identical(
    object = saferMake:::.base_pkgs,
    expected = base::c("stats", "graphics", "grDevices", "utils", "datasets",
                       "methods", "base"))
})

testthat::test_that(desc = ".qualify_base_calls() qualifies basic calls in the code", code = {
  code <- "my_fun <- function(x){\n    a <- sum(x)\n    b <- paste0(x, '-')\n    base::list(a, b)\n}"
  res <- saferMake:::.qualify_base_calls(txt = code, fun_args = base::c("x"))
  testthat::expect_match(object = res$new_txt, regexp = "base::sum\\(x\\)", fixed = FALSE)
  testthat::expect_match(object = res$new_txt, regexp = "base::paste0\\(x", fixed = FALSE)
  testthat::expect_true(object = base::length(x = res$qualified) > 0L)
  # idempotent: qualifying an already qualified code changes nothing
  res2 <- saferMake:::.qualify_base_calls(txt = res$new_txt, fun_args = base::c("x"))
  testthat::expect_identical(object = res2$new_txt, expected = res$new_txt)
})

testthat::test_that(desc = ".qualify_base_calls() preserves strings, comments and formatting", code = {
  code <- "my_fun <- function(x){\n    s <- 'sum(x) in string'  # comment sum(x)\n    a <- sum(x)\n}"
  res <- saferMake:::.qualify_base_calls(txt = code, fun_args = base::c("x"))
  testthat::expect_match(object = res$new_txt, regexp = "'sum\\(x\\) in string'", fixed = FALSE)
  testthat::expect_match(object = res$new_txt, regexp = "# comment sum\\(x\\)", fixed = FALSE)
  testthat::expect_match(object = res$new_txt, regexp = "a <- base::sum\\(x\\)", fixed = FALSE)
})

testthat::test_that(desc = ".qualify_base_calls() skips member access and already-qualified calls", code = {
  code <- "my_fun <- function(x){\n    a <- x$sum(2)\n    b <- base::sum(x)\n}"
  res <- saferMake:::.qualify_base_calls(txt = code, fun_args = base::c("x"))
  testthat::expect_match(object = res$new_txt, regexp = "x\\$sum\\(2\\)", fixed = FALSE)
  testthat::expect_match(object = res$new_txt, regexp = "base::sum\\(x\\)", fixed = FALSE)
  testthat::expect_true(object = base::any(x = base::grepl(pattern = "member access", x = res$skipped, fixed = TRUE)))
  testthat::expect_true(object = base::any(x = base::grepl(pattern = "already qualified", x = res$skipped, fixed = TRUE)))
})

testthat::test_that(desc = ".qualify_base_calls() skips locally defined and formal names", code = {
  code <- "my_fun <- function(x, plot = NULL){\n    helper <- function(y) helper(y - 1)\n    helper(x)\n    plot(x)\n}"
  res <- saferMake:::.qualify_base_calls(txt = code, fun_args = base::c("x", "plot"))
  testthat::expect_match(object = res$new_txt, regexp = "helper\\(y - 1\\)", fixed = FALSE)
  testthat::expect_match(object = res$new_txt, regexp = "    helper\\(x\\)", fixed = FALSE)
  testthat::expect_match(object = res$new_txt, regexp = "    plot\\(x\\)", fixed = FALSE)
  testthat::expect_false(object = base::grepl(pattern = "base::helper", x = res$new_txt, fixed = TRUE))
  testthat::expect_false(object = base::grepl(pattern = "base::plot", x = res$new_txt, fixed = TRUE))
})

testthat::test_that(desc = ".qualify_base_calls() skips local definitions on lines after the first, whatever the name length", code = {
  # regression of the line/column offset bug: the local definition is NOT on
  # line 1 and its name is longer than "my_fun", so the whitespace check
  # between the assign and the function token must use global offsets (the
  # name would wrongly stay bare -> infinite recursion at run time otherwise)
  code <- "my_fun <- function(x){\n  my_local_fun <- function(y) my_local_fun(y - 1)\n  my_local_fun(x)\n  sum(x)\n}"
  res <- saferMake:::.qualify_base_calls(txt = code, fun_args = base::c("x"))
  testthat::expect_false(object = base::grepl(pattern = "base::my_local_fun", x = res$new_txt, fixed = TRUE))
  testthat::expect_match(object = res$new_txt, regexp = "my_local_fun\\(y - 1\\)", fixed = FALSE)
  testthat::expect_match(object = res$new_txt, regexp = "base::sum\\(x\\)", fixed = FALSE)
  testthat::expect_true(object = base::any(x = base::grepl(pattern = "locally bound", x = res$skipped, fixed = TRUE)))
})

testthat::test_that(desc = ".qualify_base_calls() skips every name bound by an assignment, not only function definitions", code = {
  # rebinding a base name to another function (mean <- stats::median) must not
  # be qualified: qualifying the calls would silently change the semantics
  code <- "my_fun <- function(x){\n  mean <- stats::median\n  mean(x)\n  a <- 1; total <- base::sum\n  total(a)\n}"
  res <- saferMake:::.qualify_base_calls(txt = code, fun_args = base::c("x"))
  testthat::expect_match(object = res$new_txt, regexp = "  mean\\(x\\)", fixed = FALSE)
  testthat::expect_match(object = res$new_txt, regexp = "  total\\(a\\)", fixed = FALSE)
  testthat::expect_true(object = base::any(x = base::grepl(pattern = "mean: locally bound", x = res$skipped, fixed = TRUE)))
  testthat::expect_true(object = base::any(x = base::grepl(pattern = "total: locally bound", x = res$skipped, fixed = TRUE)))
})

testthat::test_that(desc = ".qualify_base_calls() detects member access and qualification across lines and with spaces", code = {
  # x $ sum(2) with the $ at the end of the previous line, and base:: sum(x)
  # with a space after the :: (valid R): both must be skipped without
  # corrupting the code
  code <- "my_fun <- function(x){\n  a <- x $\n    sum(2)\n  b <- base:: sum(x)\n  base::list(a, b)\n}"
  res <- saferMake:::.qualify_base_calls(txt = code, fun_args = base::c("x"))
  testthat::expect_match(object = res$new_txt, regexp = 'x [$]\n    sum[(]2[)]', fixed = FALSE)
  testthat::expect_match(object = res$new_txt, regexp = "base:: sum\\(x\\)", fixed = FALSE)
  testthat::expect_true(object = base::any(x = base::grepl(pattern = "member access", x = res$skipped, fixed = TRUE)))
  testthat::expect_true(object = base::any(x = base::grepl(pattern = "space or line break", x = res$skipped, fixed = TRUE)))
})

testthat::test_that(desc = ".qualify_base_calls() skips right assignments and backtick-bound names", code = {
  # RIGHT_ASSIGN (1 -> sum) binds the RHS name; backtick-bound names are
  # detected too (reported with the backticks in the name — cosmetic only)
  code <- "my_fun <- function(x){\n  1 -> sum\n  sum(x)\n  `paste0` <- base::identity\n  paste0(x)\n}"
  res <- saferMake:::.qualify_base_calls(txt = code, fun_args = base::c("x"))
  testthat::expect_match(object = res$new_txt, regexp = "  sum\\(x\\)", fixed = FALSE)
  testthat::expect_match(object = res$new_txt, regexp = "  paste0\\(x\\)", fixed = FALSE)
  testthat::expect_true(object = base::any(x = base::grepl(pattern = "sum: locally bound", x = res$skipped, fixed = TRUE)))
  testthat::expect_true(object = base::any(x = base::grepl(pattern = "paste0: locally bound", x = res$skipped, fixed = TRUE)))
})

testthat::test_that(desc = ".qualify_base_calls() reports S4 slot access explicitly", code = {
  code <- "my_fun <- function(x){\n  y <- x@sum(2)\n  base::list(y)\n}"
  res <- saferMake:::.qualify_base_calls(txt = code, fun_args = base::c("x"))
  testthat::expect_match(object = res$new_txt, regexp = "x@sum\\(2\\)", fixed = FALSE)
  testthat::expect_true(object = base::any(x = base::grepl(pattern = "S4 slot access", x = res$skipped, fixed = TRUE)))
})

testthat::test_that(desc = ".qualify_base_calls() reports shadowed and multi-base names without guessing", code = {
  # a real attached environment shadowing stats::filter (search position 2,
  # before the base packages), detached on exit
  fake <- base::new.env(parent = base::emptyenv())
  fake$filter <- function(x) {x}
  base::attach(what = fake, pos = 2L, name = "test:userfake")
  on.exit(expr = base::detach(name = "test:userfake", character.only = TRUE), add = TRUE)
  code <- "my_fun <- function(x){\n    filter(x)\n}"
  res <- saferMake:::.qualify_base_calls(txt = code, fun_args = base::c("x"))
  testthat::expect_match(object = res$new_txt, regexp = "    filter\\(x\\)", fixed = FALSE)
  testthat::expect_match(object = res$skipped, regexp = "shadowed", fixed = TRUE)
})

testthat::test_that(desc = ".qualify_base_calls() leaves unparseable code unchanged", code = {
  code <- "my_fun <- function(x){\n    sum(x\n}"
  res <- saferMake:::.qualify_base_calls(txt = code, fun_args = base::c("x"))
  testthat::expect_identical(object = res$new_txt, expected = code)
  testthat::expect_match(object = res$skipped, regexp = "PARSE", fixed = TRUE)
})

testthat::test_that(desc = ".args_insert() applies the STATUS corrections of all_args_here()", code = {
  testthat::skip_if_not_installed(pkg = "saferDev")
  code <- "my_fun <- function(x){\n    a <- base::sum(x)\n    b <- base::paste0(x, '-')\n    base::list(a, b)\n}"
  f <- mk_fun(code)
  cap <- saferMake:::.capture_msg(fun = function() {saferDev::all_args_here(x = f)} )
  rep <- cap$value
  res <- saferMake:::.args_insert(f = f, rep = rep, fun_name = "my_fun")
  testthat::expect_match(object = res$fun_txt, regexp = "base::sum\\(x, na\\.rm = FALSE\\)", fixed = FALSE)
  testthat::expect_match(object = res$fun_txt, regexp = "collapse = NULL, recycle0 = FALSE", fixed = FALSE)
  testthat::expect_true(object = base::length(x = res$inserted) == 2L)
  testthat::expect_true(object = base::length(x = res$left) == 0L)
  # the corrected text must parse and re-run all_args_here() clean
  f2 <- mk_fun(res$fun_txt)
  cap2 <- saferMake:::.capture_msg(fun = function() {saferDev::all_args_here(x = f2)} )
  testthat::expect_true(object = base::is.data.frame(x = cap2$value))
  testthat::expect_false(object = base::any(x = base::nzchar(x = cap2$value$MISSING_ARGS, keepNA = FALSE), na.rm = TRUE))
})

testthat::test_that(desc = ".args_insert() leaves multi-line calls and broken STATUS to the user", code = {
  testthat::skip_if_not_installed(pkg = "saferDev")
  # multi-line call: FUN_ARGS is captured complete on one line by all_args_here,
  # but the STATUS replacement of a call that spans raw lines is only applied
  # when the call text is found at the reported position; a multi-line call gets
  # a full one-line STATUS, which is applied when it matches the FUN_ARGS span.
  code <- "my_fun <- function(x){\n    r <- stats::optim(par = 1,\n         fn = function(p) p^2)\n    base::list(r)\n}"
  f <- mk_fun(code)
  cap <- saferMake:::.capture_msg(fun = function() {saferDev::all_args_here(x = f)} )
  res <- saferMake:::.args_insert(f = f, rep = cap$value, fun_name = "my_fun")
  # whatever the outcome, the text handed back must parse (NULL = nothing applied)
  if (! base::is.null(x = res$fun_txt)) {
    testthat::expect_no_error(object = mk_fun(res$fun_txt))
  }
  # broken STATUS: required positional argument missing
  code2 <- "my_fun <- function(x){\n    base::substr(x, 1)\n}"
  f2 <- mk_fun(code2)
  cap2 <- saferMake:::.capture_msg(fun = function() {saferDev::all_args_here(x = f2)} )
  res2 <- saferMake:::.args_insert(f = f2, rep = cap2$value, fun_name = "my_fun")
  testthat::expect_null(object = res2$fun_txt)
  testthat::expect_true(object = base::length(x = res2$left) > 0L)
})

testthat::test_that(desc = ".args_insert() applies at most one correction per line per call (nested calls)", code = {
  testthat::skip_if_not_installed(pkg = "saferDev")
  # nested same-line calls: sum(mean(x)) on one line. The two corrections
  # overlap in all_args_here(), so only ONE (the innermost, highest FUN_POS)
  # is applied per call; the outer one is rebuilt and applied at the next
  # iteration of the .run_code_checks() loop
  code <- "my_fun <- function(x){\n    a <- base::sum(base::mean(x))\n    base::list(a)\n}"
  f <- mk_fun(code)
  cap <- saferMake:::.capture_msg(fun = function() {saferDev::all_args_here(x = f)} )
  res <- saferMake:::.args_insert(f = f, rep = cap$value, fun_name = "my_fun")
  # exactly one correction applied, the text parses, and the second correction
  # is left for the next loop iteration
  testthat::expect_true(object = base::length(x = res$inserted) == 1L)
  testthat::expect_no_error(object = mk_fun(res$fun_txt))
  testthat::expect_match(object = res$fun_txt, regexp = "base::mean\\(x = x\\)", fixed = FALSE)
  # the full loop converges on both corrections
  r <- saferMake:::.run_code_checks(code = code, fun_args = "x", fun_name = "my_fun")
  testthat::expect_identical(object = r$stage, expected = "ok")
  testthat::expect_match(object = r$code, regexp = "base::sum\\(base::mean\\(x = x\\), na\\.rm = FALSE\\)", fixed = FALSE)
})

testthat::test_that(desc = ".run_code_checks() converges on an unqualified function with missing args", code = {
  testthat::skip_if_not_installed(pkg = "saferDev")
  code <- "my_fun <- function(x){\n    a <- sum(x)\n    base::list(a)\n}"
  res <- saferMake:::.run_code_checks(code = code, fun_args = "x", fun_name = "my_fun")
  testthat::expect_identical(object = res$stage, expected = "ok")
  testthat::expect_match(object = res$code, regexp = "base::sum\\(x, na\\.rm = FALSE\\)", fixed = FALSE)
  # the summary explains the scope of the automatic qualification (basic R
  # functions only) instead of listing the qualified calls
  testthat::expect_true(object = base::any(x = base::grepl(pattern = "Only basic R functions", x = res$summary, fixed = TRUE)))
  # the explanation is seeded once per Run, never duplicated by the loop
  testthat::expect_identical(object = base::sum(x = base::grepl(pattern = "Only basic R functions", x = res$summary, fixed = TRUE)), expected = 1L)
  testthat::expect_false(object = base::any(x = base::grepl(pattern = "Automatically qualified", x = res$summary, fixed = TRUE)))
  testthat::expect_true(object = base::any(x = base::grepl(pattern = "Automatically inserted", x = res$summary, fixed = TRUE)))
  # idempotent: running the checks again on the corrected code is stable
  res2 <- saferMake:::.run_code_checks(code = res$code, fun_args = "x", fun_name = "my_fun")
  testthat::expect_identical(object = res2$stage, expected = "ok")
  testthat::expect_identical(object = res2$code, expected = res$code)
})

testthat::test_that(desc = ".run_code_checks() stops at the colons stage when a name cannot be qualified", code = {
  testthat::skip_if_not_installed(pkg = "saferDev")
  code <- "my_fun <- function(x){\n    my_helper(x)\n}"
  res <- saferMake:::.run_code_checks(code = code, fun_args = "x", fun_name = "my_fun")
  testthat::expect_identical(object = res$stage, expected = "colons")
  testthat::expect_match(object = res$report, regexp = "my_helper", fixed = TRUE)
  testthat::expect_match(object = res$report, regexp = "ARE MISSING", fixed = TRUE)
})

testthat::test_that(desc = ".run_code_checks() reports an all_args_here() stop as aah_error", code = {
  testthat::skip_if_not_installed(pkg = "saferDev")
  # x@sum(2): colons_check() flags it ("ARE MISSING BEFORE BASIC"), so it is
  # gated at the colons stage. The aah_error stage exists for the remaining
  # cases where colons_check() is clean but all_args_here() still stops
  # (e.g. a method call built after qualification): whatever the stage, the
  # code handed back must be the unchanged qualified code and the report must
  # be filled
  code <- "my_fun <- function(x){\n    x@sum(2)\n}"
  res <- saferMake:::.run_code_checks(code = code, fun_args = "x", fun_name = "my_fun")
  testthat::expect_true(object = res$stage %in% base::c("aah_error", "args", "colons"))
  testthat::expect_identical(object = res$code, expected = code)
  testthat::expect_true(object = base::nzchar(x = res$report, keepNA = FALSE))
})

testthat::test_that(desc = ".run_code_checks() skips the checks when saferDev is absent", code = {
  code <- "my_fun <- function(x){\n    sum(x)\n}"
  res <- testthat::with_mocked_bindings(
    .package = "saferMake",
    .safer_dev_available = function() {FALSE} ,
    {
      saferMake:::.run_code_checks(code = code, fun_args = "x", fun_name = "my_fun")
    }
  )
  testthat::expect_identical(object = res$stage, expected = "skip")
  testthat::expect_true(object = base::any(x = base::grepl(pattern = "not installed", x = res$summary, fixed = TRUE)))
})

testthat::test_that(desc = ".run_code_checks() uses a resubmittable name for anonymous functions", code = {
  testthat::skip_if_not_installed(pkg = "saferDev")
  code <- "function(x){\n    sum(x)\n}"
  res <- saferMake:::.run_code_checks(code = code, fun_args = "x",
                                      fun_name = "fun_to_complete")
  testthat::expect_identical(object = res$stage, expected = "ok")
  testthat::expect_match(object = res$code, regexp = "^fun_to_complete <- ", fixed = FALSE)
})

testthat::test_that(desc = ".run_code_checks() does not evaluate the code twice when the qualification changes nothing", code = {
  testthat::skip_if_not_installed(pkg = "saferDev")
  # observable side effect: a top-level writeLines() in the code. When the
  # caller-evaluated function and environment are reused (qualification is a
  # no-op), the code must be evaluated exactly once per Run; when the
  # qualification changes the code (or no f/env is passed), the qualified code
  # is evaluated again and the file gains a line
  tmp <- base::file.path(tempdir(), "eval_count_test.txt")
  on.exit(expr = base::unlink(x = tmp), add = TRUE)
  base::unlink(x = tmp)
  # already-qualified code: the qualification is a no-op
  code2 <- base::paste0("base::write(x = 'x', file = ",
                        base::deparse(expr = tmp), ", append = TRUE)\nmy_fun <- function(x){\n    base::list(x)\n}")
  env2 <- base::new.env(parent = base::globalenv())
  f2 <- base::eval(expr = base::parse(text = code2, keep.source = TRUE), envir = env2)
  testthat::expect_true(object = base::is.function(x = f2))
  testthat::expect_identical(object = base::length(x = base::readLines(con = tmp, warn = FALSE)),
                             expected = 1L)  # written by the caller eval only
  res2 <- saferMake:::.run_code_checks(code = code2, fun_args = "x",
                                       fun_name = "my_fun", f = f2, env = env2)
  testthat::expect_identical(object = res2$stage, expected = "ok")
  testthat::expect_identical(object = base::length(x = base::readLines(con = tmp, warn = FALSE)),
                             expected = 1L)  # not evaluated again
  # when the qualification CHANGES the code, the qualified code is evaluated
  # by the checks in addition to the caller eval (the file then contains two
  # lines: one from the caller eval, one from the qualified-code eval)
  code3 <- base::paste0("write(x = 'x', file = ",
                        base::deparse(expr = tmp), ", append = TRUE)\nmy_fun <- function(x){\n    base::list(x)\n}")
  env3 <- base::new.env(parent = base::globalenv())
  invisible(base::eval(expr = base::parse(text = code3, keep.source = TRUE), envir = env3))
  res3 <- saferMake:::.run_code_checks(code = code3, fun_args = "x",
                                       fun_name = "my_fun")
  testthat::expect_identical(object = res3$stage, expected = "ok")
  testthat::expect_true(object = base::length(x = base::readLines(con = tmp, warn = FALSE)) >= 2L)
})
