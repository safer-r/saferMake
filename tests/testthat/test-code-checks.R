# Unit tests of the staged code checks (R/intern_.code_checks.R): the five
# saferDev checks of the Run button of window 1. All helpers are internal
# (dot-prefixed): access them via ::: (normal for internal testing).
# The tests that run a saferDev function need the current (development) saferDev (lambda_check(),
# curly_bracket_check(), true_false_check() and the code export of
# colons_check() and all_args_here()); they are skipped otherwise.

# Rebuild a function object from pasted code, like the app does: with the
# source references (keep.source = TRUE and srcfile left missing, so that
# parse() builds it from the text), which lambda_check() and the splice of the
# modified function in the code need
mk_fun <- function(code) {
  base::eval(expr = base::parse(text = code, keep.source = TRUE))
}

# Skip the test if saferDev does not provide the five checks
skip_if_no_checks <- function() {
  testthat::skip_if_not_installed(pkg = "saferDev")
  testthat::skip_if_not(condition = saferMake:::.safer_dev_available(),
                        message = "the current (development) saferDev is needed")
}

# Run the checks like the Run button does: evaluate the code, then check it
run_checks <- function(code, fun_name = "my_fun") {
  f <- mk_fun(code = code)
  saferMake:::.run_code_checks(code = code, f = f, fun_name = fun_name)
}

# Click on Run again and again until the loop stops on something else than a
# modification. Returns the list of the stages and the last result.
run_until_stop <- function(code, fun_name = "my_fun", max_runs = 12L) {
  stages <- base::character(length = 0L)
  steps <- base::integer(length = 0L)
  for (i in base::seq_len(length.out = max_runs)) {
    res <- run_checks(code = code, fun_name = fun_name)
    stages <- base::c(stages, res$stage)
    steps <- base::c(steps, res$step)
    if (! base::identical(x = res$stage, y = "modified")) {break}
    code <- res$code
  }
  base::list(stages = stages, steps = steps, res = res)
}

# --- .capture_msg() -----------------------------------------------------------

testthat::test_that(desc = ".capture_msg() captures messages, warnings and errors", code = {
  out <- saferMake:::.capture_msg(fun = function() {
    base::message("m1")
    base::warning("w1")
    "value"
  })
  testthat::expect_identical(object = out$value, expected = "value")
  testthat::expect_identical(object = out$msgs, expected = "m1\n")
  testthat::expect_identical(object = out$warns, expected = "w1")
  testthat::expect_null(object = out$err)
  out2 <- saferMake:::.capture_msg(fun = function() {base::stop("boom")} )
  testthat::expect_null(object = out2$value)
  testthat::expect_identical(object = out2$err, expected = "boom")
})

# --- helpers ---------------------------------------------------------------------

testthat::test_that(desc = ".check_var_name() keeps syntactic names only", code = {
  testthat::expect_identical(object = saferMake:::.check_var_name(fun_name = "p.adjust"), expected = "p.adjust")
  testthat::expect_identical(object = saferMake:::.check_var_name(fun_name = "<anonymous>"), expected = "fun_to_check")
  testthat::expect_identical(object = saferMake:::.check_var_name(fun_name = "my fun"), expected = "fun_to_check")
  testthat::expect_identical(object = saferMake:::.check_var_name(fun_name = "if"), expected = "fun_to_check")
  testthat::expect_identical(object = saferMake:::.check_var_name(fun_name = ""), expected = "fun_to_check")
})

testthat::test_that(desc = ".safer_dev_required is a single logical value (temporary switch)", code = {
  testthat::expect_type(object = saferMake:::.safer_dev_required, type = "logical")
  testthat::expect_length(object = saferMake:::.safer_dev_required, n = 1L)
})

testthat::test_that(desc = ".code_check_steps lists the five checks in the required order", code = {
  funs <- base::vapply(X = saferMake:::.code_check_steps, FUN = function(st) {st$fun} ,
                       FUN.VALUE = base::character(length = 1L))
  testthat::expect_identical(
    object = funs,
    expected = base::c("lambda_check", "curly_bracket_check", "true_false_check", "colons_check", "all_args_here")
  )
})

testthat::test_that(desc = ".function_span() locates the function in the code, also with a header and a trailer", code = {
  code <- "#' header\nmy_fun <- function(x){\n    x + 1\n}\n# trailer\n"
  f <- mk_fun(code = code)
  span <- saferMake:::.function_span(code = code, f = f)
  testthat::expect_identical(object = base::substring(text = code, first = span[1], last = span[2]),
                             expected = "function(x){\n    x + 1\n}")
  # a function without source references cannot be located
  testthat::expect_null(object = saferMake:::.function_span(code = code, f = base::eval(expr = base::parse(text = "function(x) x", keep.source = FALSE))))
})

testthat::test_that(desc = ".splice_function() keeps what is written around the function", code = {
  code <- "#' header\nmy_fun <- function(x){\n    x + 1\n}\n# trailer\n"
  f <- mk_fun(code = code)
  out <- saferMake:::.splice_function(code = code, f = f, fun_txt_new = "function(x){ x + 2 }", fun_name = "my_fun")
  testthat::expect_identical(object = out, expected = "#' header\nmy_fun <- function(x){ x + 2 }\n# trailer\n")
  # function not locatable: the whole code is rebuilt from the function name
  f2 <- base::eval(expr = base::parse(text = "function(x) x", keep.source = FALSE))
  testthat::expect_identical(object = saferMake:::.splice_function(code = code, f = f2, fun_txt_new = "function(x) x", fun_name = "my_fun"),
                             expected = "my_fun <- function(x) x")
  testthat::expect_identical(object = saferMake:::.splice_function(code = code, f = f2, fun_txt_new = "function(x) x", fun_name = "<anonymous>"),
                             expected = "function(x) x")
})

# --- one check ------------------------------------------------------------------

testthat::test_that(desc = ".run_check_step() returns the modified function text, or NULL when clean", code = {
  skip_if_no_checks()
  steps <- saferMake:::.code_check_steps
  f <- mk_fun(code = "my_fun <- function(x){\n    if(x == T) base::return(x)\n}")
  out <- saferMake:::.run_check_step(step = steps[[3]], f = f, var_name = "my_fun")
  testthat::expect_null(object = out$err)
  testthat::expect_false(object = out$clean)
  testthat::expect_match(object = out$fun_txt, regexp = "^function\\(x\\)\\{")
  testthat::expect_match(object = out$fun_txt, regexp = "x == TRUE", fixed = TRUE)
  # the temporary export path is not part of the report
  testthat::expect_false(object = base::grepl(pattern = "EXPORTED", x = out$report, fixed = TRUE))
  testthat::expect_match(object = out$report, regexp = "INSIDE my_fun()", fixed = TRUE)
  # nothing to modify
  f2 <- mk_fun(code = "my_fun <- function(x){\n    if(x == TRUE){ base::return(x) }\n}")
  out2 <- saferMake:::.run_check_step(step = steps[[3]], f = f2, var_name = "my_fun")
  testthat::expect_null(object = out2$fun_txt)
  testthat::expect_true(object = out2$clean)
})

# --- the loop of the five checks ------------------------------------------------------

testthat::test_that(desc = ".run_code_checks() stops at check 1 on a lambda function and returns the modified code", code = {
  skip_if_no_checks()
  code <- "#' header\nmy_fun <- function(x){ # keep this comment\n    base::sapply(X = x, FUN = \\(i){ i * 2 }, simplify = TRUE, USE.NAMES = TRUE)\n}\n"
  res <- run_checks(code = code)
  testthat::expect_identical(object = res$stage, expected = "modified")
  testthat::expect_identical(object = res$step, expected = 1L)
  testthat::expect_match(object = res$code, regexp = "FUN = function(i)", fixed = TRUE)
  testthat::expect_false(object = base::grepl(pattern = "\\(i)", x = res$code, fixed = TRUE))
  # lambda_check() keeps the comments and the header of the code
  testthat::expect_true(object = base::startsWith(x = res$code, prefix = "#' header\nmy_fun <- function(x){ # keep this comment"))
  testthat::expect_match(object = base::paste(res$notice, collapse = "\n"), regexp = "Check 1/5", fixed = TRUE)
  testthat::expect_match(object = base::paste(res$notice, collapse = "\n"), regexp = "click on the Run button again", fixed = TRUE)
  # no warning about the comments for this check
  testthat::expect_false(object = base::any(base::grepl(pattern = "removes the comments", x = res$notice, fixed = TRUE)))
})

testthat::test_that(desc = ".run_code_checks() handles a function written as a lambda from its first character", code = {
  skip_if_no_checks()
  res <- run_checks(code = "my_fun <- \\(x){ base::sum(x, na.rm = FALSE) }", fun_name = "my_fun")
  testthat::expect_identical(object = res$stage, expected = "modified")
  testthat::expect_identical(object = res$step, expected = 1L)
  testthat::expect_identical(object = res$code, expected = "my_fun <- function(x){ base::sum(x, na.rm = FALSE) }")
})

testthat::test_that(desc = ".run_code_checks() runs the checks in order: curly brackets, then TRUE/FALSE", code = {
  skip_if_no_checks()
  code <- "my_fun <- function(x){\n    if(x == T) base::return(x)\n}"
  res <- run_checks(code = code)
  testthat::expect_identical(object = res$stage, expected = "modified")
  testthat::expect_identical(object = res$step, expected = 2L)
  testthat::expect_match(object = res$code, regexp = "if (x == T) {", fixed = TRUE)
  testthat::expect_match(object = base::paste(res$notice, collapse = "\n"), regexp = "removes the comments", fixed = TRUE)
  res2 <- run_checks(code = res$code)
  testthat::expect_identical(object = res2$stage, expected = "modified")
  testthat::expect_identical(object = res2$step, expected = 3L)
  testthat::expect_match(object = res2$code, regexp = "x == TRUE", fixed = TRUE)
})

testthat::test_that(desc = ".run_code_checks() adds the package attribution at check 4, then the arguments at check 5", code = {
  skip_if_no_checks()
  code <- "my_fun <- function(x){\n    sum(x)\n}"
  out <- run_until_stop(code = code)
  testthat::expect_identical(object = out$stages, expected = base::c("modified", "modified", "ok"))
  testthat::expect_identical(object = out$steps, expected = base::c(4L, 5L))
  testthat::expect_match(object = out$res$code, regexp = "base::sum(x, na.rm = FALSE)", fixed = TRUE)
  testthat::expect_identical(object = out$res$notice, expected = base::character(length = 0L))
  testthat::expect_identical(object = out$res$report, expected = "")
})

testthat::test_that(desc = ".run_code_checks() returns ok and the unchanged code on a clean function", code = {
  skip_if_no_checks()
  code <- "my_fun <- function(x){\n    base::sum(x, na.rm = FALSE)\n}"
  res <- run_checks(code = code)
  testthat::expect_identical(object = res$stage, expected = "ok")
  testthat::expect_identical(object = res$code, expected = code)
  testthat::expect_null(object = res$step)
})

testthat::test_that(desc = ".run_code_checks() stops on findings it cannot correct (function without package)", code = {
  skip_if_no_checks()
  code <- "my_fun <- function(x){\n    my_helper(x)\n}"
  res <- run_checks(code = code)
  testthat::expect_identical(object = res$stage, expected = "findings")
  testthat::expect_identical(object = res$step, expected = 4L)
  testthat::expect_identical(object = res$code, expected = code)
  testthat::expect_match(object = res$report, regexp = "my_helper", fixed = TRUE)
  testthat::expect_match(object = res$report, regexp = "ARE MISSING", fixed = TRUE)
  testthat::expect_match(object = base::paste(res$notice, collapse = "\n"), regexp = "Check 4/5", fixed = TRUE)
})

testthat::test_that(desc = ".run_code_checks() stops on findings of all_args_here() for calls written on several lines", code = {
  skip_if_no_checks()
  code <- "my_fun <- function(x){\n    base::paste0(x,\n        collapse = NULL)\n}"
  res <- run_checks(code = code)
  testthat::expect_identical(object = res$stage, expected = "findings")
  testthat::expect_identical(object = res$step, expected = 5L)
  testthat::expect_identical(object = res$code, expected = code)
  testthat::expect_match(object = res$report, regexp = "CORRECTIONS NOT APPLIED", fixed = TRUE)
  # the same text is not repeated in the report
  testthat::expect_identical(object = base::lengths(x = base::regmatches(x = res$report, m = base::gregexpr(pattern = "CORRECTIONS NOT APPLIED", text = res$report, fixed = TRUE))),
                             expected = 1L)
})

testthat::test_that(desc = ".run_code_checks() reports a saferDev error", code = {
  skip_if_no_checks()
  # x@sum(2): colons_check() cannot rewrite it ("modified code does not parse")
  code <- "my_fun <- function(x){\n    base::sum(x@sum(2), na.rm = FALSE)\n}"
  res <- run_checks(code = code)
  testthat::expect_true(object = res$stage %in% base::c("error", "findings"))
  testthat::expect_identical(object = res$code, expected = code)
  testthat::expect_true(object = base::nzchar(x = res$report, keepNA = FALSE))
})

testthat::test_that(desc = ".run_code_checks() works on a bare (anonymous) function and keeps it anonymous", code = {
  skip_if_no_checks()
  res <- run_checks(code = "function(x){\n    base::sum(x)\n}", fun_name = "<anonymous>")
  testthat::expect_identical(object = res$stage, expected = "modified")
  testthat::expect_identical(object = res$step, expected = 5L)
  testthat::expect_match(object = res$code, regexp = "^function\\(x\\)")
  testthat::expect_match(object = res$code, regexp = "base::sum(x, na.rm = FALSE)", fixed = TRUE)
})

testthat::test_that(desc = ".run_code_checks() stops with a message when the current saferDev is not available", code = {
  code <- "my_fun <- function(x){\n    sum(x)\n}"
  f <- mk_fun(code = code)
  res <- testthat::with_mocked_bindings(
    .package = "saferMake",
    .safer_dev_available = function() {FALSE} ,
    {
      saferMake:::.run_code_checks(code = code, f = f, fun_name = "my_fun")
    }
  )
  testthat::expect_identical(object = res$stage, expected = "skip")
  testthat::expect_identical(object = res$code, expected = code)
  testthat::expect_match(object = res$notice, regexp = "current (development) version of the saferDev package", fixed = TRUE)
})

testthat::test_that(desc = "the five checks do not leave files behind", code = {
  skip_if_no_checks()
  before <- base::list.files(path = base::tempdir(), pattern = "^saferMake_check_", full.names = FALSE)
  run_checks(code = "my_fun <- function(x){\n    sum(x)\n}")
  after <- base::list.files(path = base::tempdir(), pattern = "^saferMake_check_", full.names = FALSE)
  testthat::expect_identical(object = after, expected = before)
})
