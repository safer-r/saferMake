# Extracted from test-code-checks.R:307

# prequel ----------------------------------------------------------------------
mk_fun <- function(code) {
  base::eval(expr = base::parse(text = code, keep.source = TRUE))
}
skip_if_no_checks <- function() {
  testthat::skip_if_not_installed(pkg = "saferDev")
  testthat::skip_if_not(condition = saferMake:::.safer_dev_available(lib_path = NULL, error_text = ""),
                        message = "the current (development) saferDev is needed")
}
run_checks <- function(code, fun_name = "my_fun") {
  f <- mk_fun(code = code)
  saferMake:::.run_code_checks(code = code, f = f, fun_name = fun_name, lib_path = NULL, error_text = "")
}
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

# test -------------------------------------------------------------------------
skip_if_no_checks()
code <- "my_fun <- function(p, nm){\n    stats::setNames(p, nm)\n}"
res <- run_checks(code = code)
testthat::expect_identical(object = res$stage, expected = "findings")
testthat::expect_identical(object = res$step, expected = 7L)
testthat::expect_identical(object = res$code, expected = code)
