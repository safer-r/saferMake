# Unit tests of the staged code checks (.run_code_checks() and helpers): the seven
# saferDev checks of the Run button of window 1. All helpers are internal
# (dot-prefixed): access them via ::: (normal for internal testing).
# The tests that run a saferDev function need the current (development) saferDev (super_assign_check(), lambda_check(),
# curly_bracket_check(), true_false_check(), for_loop_nb_check() and the code export of
# colons_check() and all_args_here()); they are skipped otherwise.

# Rebuild a function object from pasted code, like the app does: with the
# source references (keep.source = TRUE and srcfile left missing, so that
# parse() builds it from the text), which lambda_check() and the splice of the
# modified function in the code need
mk_fun <- function(code) {
  base::eval(expr = base::parse(text = code, keep.source = TRUE))
}

# Skip the test if saferDev does not provide the seven checks
skip_if_no_checks <- function() {
  testthat::skip_if_not_installed(pkg = "saferDev")
  testthat::skip_if_not(condition = saferMake:::.safer_dev_available(lib_path = NULL, error_text = ""),
                        message = "the current (development) saferDev is needed")
}

# Run the checks like the Run button does: evaluate the code, then check it
run_checks <- function(code, fun_name = "my_fun") {
  f <- mk_fun(code = code)
  saferMake:::.run_code_checks(code = code, f = f, fun_name = fun_name, lib_path = NULL, error_text = "")
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
  }, lib_path = NULL, error_text = "")
  testthat::expect_identical(object = out$value, expected = "value")
  testthat::expect_identical(object = out$msgs, expected = "m1\n")
  testthat::expect_identical(object = out$warns, expected = "w1")
  testthat::expect_null(object = out$err)
  out2 <- saferMake:::.capture_msg(fun = function() {base::stop("boom")} , lib_path = NULL, error_text = "")
  testthat::expect_null(object = out2$value)
  testthat::expect_identical(object = out2$err, expected = "boom")
})

# --- helpers ---------------------------------------------------------------------

testthat::test_that(desc = ".check_var_name() keeps syntactic names only", code = {
  testthat::expect_identical(object = saferMake:::.check_var_name(fun_name = "p.adjust", lib_path = NULL, error_text = ""), expected = "p.adjust")
  testthat::expect_identical(object = saferMake:::.check_var_name(fun_name = "<anonymous>", lib_path = NULL, error_text = ""), expected = "fun_to_check")
  testthat::expect_identical(object = saferMake:::.check_var_name(fun_name = "my fun", lib_path = NULL, error_text = ""), expected = "fun_to_check")
  testthat::expect_identical(object = saferMake:::.check_var_name(fun_name = "if", lib_path = NULL, error_text = ""), expected = "fun_to_check")
  testthat::expect_identical(object = saferMake:::.check_var_name(fun_name = "", lib_path = NULL, error_text = ""), expected = "fun_to_check")
})

testthat::test_that(desc = ".safer_dev_required is a single logical value (temporary switch)", code = {
  testthat::expect_type(object = saferMake:::.safer_dev_required, type = "logical")
  testthat::expect_length(object = saferMake:::.safer_dev_required, n = 1L)
})

testthat::test_that(desc = ".code_check_steps lists the seven checks in the required order", code = {
  funs <- base::vapply(X = saferMake:::.code_check_steps, FUN = function(st) {st$fun} ,
                       FUN.VALUE = base::character(length = 1L))
  testthat::expect_identical(
    object = funs,
    expected = base::c("super_assign_check", "lambda_check", "curly_bracket_check", "true_false_check", "for_loop_nb_check", "colons_check", "all_args_here")
  )
})

testthat::test_that(desc = ".function_span() locates the function in the code, also with a header and a trailer", code = {
  code <- "#' header\nmy_fun <- function(x){\n    x + 1\n}\n# trailer\n"
  f <- mk_fun(code = code)
  span <- saferMake:::.function_span(code = code, f = f, lib_path = NULL, error_text = "")
  testthat::expect_identical(object = base::substring(text = code, first = span[1], last = span[2]),
                             expected = "function(x){\n    x + 1\n}")
  # a function without source references cannot be located
  testthat::expect_null(object = saferMake:::.function_span(code = code, f = base::eval(expr = base::parse(text = "function(x) x", keep.source = FALSE)), lib_path = NULL, error_text = ""))
})

testthat::test_that(desc = ".splice_function() keeps what is written around the function", code = {
  code <- "#' header\nmy_fun <- function(x){\n    x + 1\n}\n# trailer\n"
  f <- mk_fun(code = code)
  out <- saferMake:::.splice_function(code = code, f = f, fun_txt_new = "function(x){ x + 2 }", fun_name = "my_fun", lib_path = NULL, error_text = "")
  testthat::expect_identical(object = out, expected = "#' header\nmy_fun <- function(x){ x + 2 }\n# trailer\n")
  # function not locatable: the whole code is rebuilt from the function name
  f2 <- base::eval(expr = base::parse(text = "function(x) x", keep.source = FALSE))
  testthat::expect_identical(object = saferMake:::.splice_function(code = code, f = f2, fun_txt_new = "function(x) x", fun_name = "my_fun", lib_path = NULL, error_text = ""),
                             expected = "my_fun <- function(x) x")
  testthat::expect_identical(object = saferMake:::.splice_function(code = code, f = f2, fun_txt_new = "function(x) x", fun_name = "<anonymous>", lib_path = NULL, error_text = ""),
                             expected = "function(x) x")
})

# --- one check ------------------------------------------------------------------

testthat::test_that(desc = ".run_check_step() returns the modified function text, or NULL when clean", code = {
  skip_if_no_checks()
  steps <- saferMake:::.code_check_steps
  f <- mk_fun(code = "my_fun <- function(x){\n    if(x == T) base::return(x)\n}")
  out <- saferMake:::.run_check_step(step = steps[[4]], f = f, var_name = "my_fun", lib_path = NULL, error_text = "")
  testthat::expect_null(object = out$err)
  testthat::expect_false(object = out$clean)
  testthat::expect_match(object = out$fun_txt, regexp = "^function\\(x\\)\\{")
  testthat::expect_match(object = out$fun_txt, regexp = "x == TRUE", fixed = TRUE)
  # the temporary export path is not part of the report
  testthat::expect_false(object = base::grepl(pattern = "EXPORTED", x = out$report, fixed = TRUE))
  testthat::expect_match(object = out$report, regexp = "INSIDE my_fun()", fixed = TRUE)
  # nothing to modify
  f2 <- mk_fun(code = "my_fun <- function(x){\n    if(x == TRUE){ base::return(x) }\n}")
  out2 <- saferMake:::.run_check_step(step = steps[[4]], f = f2, var_name = "my_fun", lib_path = NULL, error_text = "")
  testthat::expect_null(object = out2$fun_txt)
  testthat::expect_true(object = out2$clean)
})

# --- the loop of the seven checks ------------------------------------------------------

testthat::test_that(desc = ".run_code_checks() corrects a lambda function at check 2 and returns the modified code", code = {
  skip_if_no_checks()
  code <- "#' header\nmy_fun <- function(x){ # keep this comment\n    base::sapply(X = x, FUN = \\(i){ i * 2 }, simplify = TRUE, USE.NAMES = TRUE)\n}\n"
  res <- run_checks(code = code)
  testthat::expect_identical(object = res$stage, expected = "modified")
  testthat::expect_null(object = res$step)
  testthat::expect_true(object = 2L %in% res$modified)
  # the full report of saferMake::test() is given to the user
  testthat::expect_match(object = res$report, regexp = "CHECK 1/7 saferDev::super_assign_check(): CLEAN", fixed = TRUE)
  testthat::expect_match(object = res$report, regexp = "CHECK 2/7 saferDev::lambda_check(): MODIFIED", fixed = TRUE)
  testthat::expect_match(object = res$code, regexp = "FUN = function(i)", fixed = TRUE)
  testthat::expect_false(object = base::grepl(pattern = "\\(i)", x = res$code, fixed = TRUE))
  # lambda_check() keeps the comments and the header of the code
  testthat::expect_true(object = base::startsWith(x = res$code, prefix = "#' header\nmy_fun <- function(x){ # keep this comment"))
  testthat::expect_match(object = base::paste(res$notice, collapse = "\n"), regexp = "Check 2/7", fixed = TRUE)
  testthat::expect_match(object = base::paste(res$notice, collapse = "\n"), regexp = "click on the Run button again", fixed = TRUE)
  # the full report goes to the report file (without its first line), followed by the modified code; nothing to correct by hand in the message
  testthat::expect_identical(object = res$report_msg, expected = "")
  testthat::expect_true(object = base::startsWith(x = res$report_file, prefix = "INSIDE my_fun():\n\nCHECK 1/7 saferDev::super_assign_check(): CLEAN"))
  testthat::expect_match(object = res$report_file, regexp = "\n\n\nMODIFIED CODE:\n\nmy_fun_safer <- function(x){ # keep this comment", fixed = TRUE)
  testthat::expect_match(object = base::paste(res$notice, collapse = "\n"), regexp = "Download report", fixed = TRUE)
})

testthat::test_that(desc = ".run_code_checks() keeps only the CORRECTIONS NOT APPLIED of all_args_here() in the message when the code is modified", code = {
  skip_if_no_checks()
  # base::sum(p, na.rm): na.rm written positionally and also proposed with its name, a correction not applied by all_args_here() (stats::setNames(p, nm) is corrected by the current saferDev)
  code <- "my_fun <- function(p, na.rm){\n    base::paste0(p)\n    base::sum(p, na.rm)\n}"
  res <- run_checks(code = code)
  testthat::expect_true(object = 7L %in% res$modified)
  testthat::expect_true(object = base::startsWith(x = res$report_msg, prefix = "CORRECTIONS NOT APPLIED:"))
  testthat::expect_match(object = res$report_msg, regexp = "sum(p, na.rm, na.rm = FALSE)", fixed = TRUE)
  testthat::expect_false(object = base::grepl(pattern = "CHECK 1/7", x = res$report_msg, fixed = TRUE))
  testthat::expect_match(object = res$report_file, regexp = "REPORT OF CHECK 7 (saferDev::all_args_here()):", fixed = TRUE)
  testthat::expect_match(object = res$report_file, regexp = "MODIFIED CODE:\n\nmy_fun_safer <- function(p, na.rm){", fixed = TRUE)
})

testthat::test_that(desc = ".run_code_checks() handles a function written as a lambda from its first character", code = {
  skip_if_no_checks()
  res <- run_checks(code = "my_fun <- \\(x){ base::sum(x, na.rm = FALSE) }", fun_name = "my_fun")
  testthat::expect_identical(object = res$stage, expected = "modified")
  testthat::expect_identical(object = res$modified, expected = 2L)
  testthat::expect_identical(object = res$code, expected = "my_fun <- function(x){ base::sum(x, na.rm = FALSE) }")
})

testthat::test_that(desc = ".run_code_checks() chains the checks in one Run: curly brackets, then TRUE/FALSE", code = {
  skip_if_no_checks()
  code <- "my_fun <- function(x){\n    # keep this comment\n    if(x == T) base::return(x)   # and this one\n}"
  res <- run_checks(code = code)
  testthat::expect_identical(object = res$stage, expected = "modified")
  testthat::expect_identical(object = res$modified, expected = base::c(3L, 4L))
  # curly_bracket_check() and true_false_check() keep the comments and the indentation of the code
  testthat::expect_identical(object = res$code, expected = "my_fun <- function(x){\n    # keep this comment\n    if(x == TRUE) { base::return(x) }   # and this one\n}")
  notice <- base::paste(res$notice, collapse = "\n")
  testthat::expect_match(object = notice, regexp = "Check 3/7: ", fixed = TRUE)
  testthat::expect_match(object = notice, regexp = "Check 4/7: ", fixed = TRUE)
  testthat::expect_match(object = notice, regexp = "click on the Run button again", fixed = TRUE)
  # the next Run is clean
  testthat::expect_identical(object = run_checks(code = res$code)$stage, expected = "ok")
})

testthat::test_that(desc = ".run_code_checks() stops at check 1 on a superassignment, without modifying the code", code = {
  skip_if_no_checks()
  # the lambda is not corrected first: super_assign_check() is run before lambda_check()
  code <- "my_fun <- function(x){\n    y <<- x\n    base::sapply(X = x, FUN = \\(i){ i * 2 }, simplify = TRUE, USE.NAMES = TRUE)\n}"
  res <- run_checks(code = code)
  testthat::expect_identical(object = res$stage, expected = "error")
  testthat::expect_identical(object = res$step, expected = 1L)
  testthat::expect_identical(object = res$code, expected = code)
  testthat::expect_match(object = res$report, regexp = "THE SUPERASSIGNMENT OPERATORS <<- AND ->> ARE NOT ALLOWED", fixed = TRUE)
  testthat::expect_match(object = res$report, regexp = "y <<- x", fixed = TRUE)
  testthat::expect_match(object = base::paste(res$notice, collapse = "\n"), regexp = "Check 1/7: ", fixed = TRUE)
  # ->> is also reported
  res2 <- run_checks(code = "my_fun <- function(x){\n    x ->> y\n}")
  testthat::expect_identical(object = res2$stage, expected = "error")
  testthat::expect_identical(object = res2$step, expected = 1L)
})

testthat::test_that(desc = ".run_code_checks() returns ok on a function without any function call (NOTHING TO CHECK of colons_check() and all_args_here())", code = {
  skip_if_no_checks()
  code <- "a <- function(){y<- 1}"
  res <- run_checks(code = code, fun_name = "a")
  testthat::expect_identical(object = res$stage, expected = "ok")
  testthat::expect_identical(object = res$code, expected = code)
})

testthat::test_that(desc = ".run_code_checks() passes error_text to saferMake::test(): the error trail goes up to saferMake::make()", code = {
  skip_if_no_checks()
  code <- "my_fun <- function(x){
    y <<- x
}"
  f <- mk_fun(code = code)
  res <- saferMake:::.run_code_checks(code = code, f = f, fun_name = "my_fun", lib_path = NULL, error_text = " INSIDE saferMake::make()")
  testthat::expect_identical(object = res$stage, expected = "error")
  testthat::expect_match(object = res$report, regexp = "INSIDE saferMake::test() INSIDE saferMake::make()", fixed = TRUE)
})

testthat::test_that(desc = ".run_code_checks() adds the checking line of a for() loop at check 5", code = {
  skip_if_no_checks()
  code <- "my_fun <- function(x){\n    for(i in 1:base::length(x = x)){\n        base::print(x = i)\n    }\n}"
  res <- run_checks(code = code)
  testthat::expect_identical(object = res$stage, expected = "modified")
  testthat::expect_true(object = 5L %in% res$modified)
  testthat::expect_match(object = res$code, regexp = "tempo_for_seq1", fixed = TRUE)
  testthat::expect_match(object = base::paste(res$notice, collapse = "\n"), regexp = "Check 5/7: ", fixed = TRUE)
  # the inserted checking line passes the next checks (colons_check() and all_args_here())
  testthat::expect_identical(object = run_checks(code = res$code)$stage, expected = "ok")
})

testthat::test_that(desc = ".run_code_checks() adds the package attribution at check 6, then the arguments at check 7, in one Run", code = {
  skip_if_no_checks()
  code <- "my_fun <- function(x){\n    sum(x)\n}"
  res <- run_checks(code = code)
  testthat::expect_identical(object = res$modified, expected = base::c(6L, 7L))
  out <- run_until_stop(code = code)
  testthat::expect_identical(object = out$stages, expected = base::c("modified", "ok"))
  testthat::expect_identical(object = out$steps, expected = base::integer(length = 0L))
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
  testthat::expect_identical(object = res$step, expected = 6L)
  testthat::expect_identical(object = res$code, expected = code)
  testthat::expect_match(object = res$report, regexp = "my_helper", fixed = TRUE)
  testthat::expect_match(object = res$report, regexp = "ARE MISSING", fixed = TRUE)
  testthat::expect_match(object = base::paste(res$notice, collapse = "\n"), regexp = "Check 6/7", fixed = TRUE)
  # the report of the stopping check stays in the message; no modified code in the report file
  testthat::expect_true(object = base::startsWith(x = res$report_msg, prefix = "REPORT OF CHECK 6 (saferDev::colons_check()):"))
  testthat::expect_match(object = res$report_msg, regexp = "my_helper", fixed = TRUE)
  testthat::expect_false(object = base::grepl(pattern = "MODIFIED CODE", x = res$report_file, fixed = TRUE))
})

testthat::test_that(desc = ".run_code_checks() keeps the corrections of the checks run before the one that reports findings", code = {
  skip_if_no_checks()
  code <- "my_fun <- function(x){\n    if(x == T) my_helper(x)\n}"
  res <- run_checks(code = code)
  testthat::expect_identical(object = res$stage, expected = "findings")
  testthat::expect_identical(object = res$step, expected = 6L)
  testthat::expect_identical(object = res$modified, expected = base::c(3L, 4L))
  testthat::expect_match(object = res$code, regexp = "if(x == TRUE) { my_helper(x) }", fixed = TRUE)
  notice <- base::paste(res$notice, collapse = "\n")
  testthat::expect_match(object = notice, regexp = "already in the code of the box below", fixed = TRUE)
  testthat::expect_match(object = notice, regexp = "Check 6/7: the check reported", fixed = TRUE)
  testthat::expect_match(object = res$report, regexp = "CHECK 7/7 saferDev::all_args_here(): NOT RUN", fixed = TRUE)
})

testthat::test_that(desc = ".run_code_checks() completes the calls written on several lines at check 7", code = {
  skip_if_no_checks()
  # the current saferDev inserts the missing arguments into the original lines
  code <- "my_fun <- function(x){\n    base::paste0(x,\n        collapse = NULL)\n}"
  res <- run_checks(code = code)
  testthat::expect_identical(object = res$stage, expected = "modified")
  testthat::expect_identical(object = res$modified, expected = 7L)
  testthat::expect_identical(object = res$code, expected = "my_fun <- function(x){\n    base::paste0(x,\n        collapse = NULL, recycle0 = FALSE)\n}")
  testthat::expect_identical(object = run_checks(code = res$code)$stage, expected = "ok")
})

testthat::test_that(desc = ".run_code_checks() stops on findings of all_args_here() for a value written without its argument name", code = {
  skip_if_no_checks()
  # na.rm written positionally and also proposed with its name (stats::setNames(p, nm) is corrected by the current saferDev)
  code <- "my_fun <- function(p, na.rm){\n    base::sum(p, na.rm)\n}"
  res <- run_checks(code = code)
  testthat::expect_identical(object = res$stage, expected = "findings")
  testthat::expect_identical(object = res$step, expected = 7L)
  testthat::expect_identical(object = res$code, expected = code)
  testthat::expect_match(object = res$report, regexp = "CORRECTIONS NOT APPLIED", fixed = TRUE)
  # the same text is not repeated in the report
  testthat::expect_identical(object = base::lengths(x = base::regmatches(x = res$report, m = base::gregexpr(pattern = "CORRECTIONS NOT APPLIED", text = res$report, fixed = TRUE))),
                             expected = 1L)
  # the table of all the calls printed by all_args_here() is not kept in the
  # report (very large and slow to process), and the notice tells what to do
  testthat::expect_false(object = base::grepl(pattern = "LINE_NB", x = res$report, fixed = TRUE), info = NULL, label = NULL)
  testthat::expect_match(object = base::paste(res$notice, collapse = "\n"), regexp = "Check 7/7: the check reported", fixed = TRUE)
  testthat::expect_match(object = base::paste(res$notice, collapse = "\n"), regexp = "must be completed by hand", fixed = TRUE)
  # the CORRECTIONS NOT APPLIED section is shown once in the message (report of the stopping check)
  testthat::expect_identical(object = base::lengths(x = base::regmatches(x = res$report_msg, m = base::gregexpr(pattern = "CORRECTIONS NOT APPLIED", text = res$report_msg, fixed = TRUE))),
                             expected = 1L)
  testthat::expect_match(object = res$report_msg, regexp = "sum(p, na.rm, na.rm = FALSE)", fixed = TRUE)
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
  testthat::expect_identical(object = res$modified, expected = 7L)
  testthat::expect_match(object = res$code, regexp = "^function\\(x\\)")
  testthat::expect_match(object = res$code, regexp = "base::sum(x, na.rm = FALSE)", fixed = TRUE)
})

testthat::test_that(desc = ".run_code_checks() stops with a message when the current saferDev is not available", code = {
  code <- "my_fun <- function(x){\n    sum(x)\n}"
  f <- mk_fun(code = code)
  res <- testthat::with_mocked_bindings(
    .package = "saferMake",
    .safer_dev_available = function(lib_path, error_text) {FALSE} ,
    {
      saferMake:::.run_code_checks(code = code, f = f, fun_name = "my_fun", lib_path = NULL, error_text = "")
    }
  )
  testthat::expect_identical(object = res$stage, expected = "skip")
  testthat::expect_identical(object = res$code, expected = code)
  testthat::expect_match(object = res$notice, regexp = "current (development) version of the saferDev package", fixed = TRUE)
})

testthat::test_that(desc = "the seven checks do not leave files behind", code = {
  skip_if_no_checks()
  before <- base::list.files(path = base::tempdir(), pattern = "^saferMake_check_", full.names = FALSE)
  run_checks(code = "my_fun <- function(x){\n    sum(x)\n}")
  after <- base::list.files(path = base::tempdir(), pattern = "^saferMake_check_", full.names = FALSE)
  testthat::expect_identical(object = after, expected = before)
})
