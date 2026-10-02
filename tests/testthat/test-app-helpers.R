# Unit tests of the internal app helpers (R/app_helpers.R, R/arg_check.R,
# R/rebuild.R) extracted from inst/app/server.R.
# All helpers are internal: access them via ::: (normal for internal testing).

# --- shared fixtures ---------------------------------------------------------

# A simple user function, as pasted in the app (single-arg, with default)
code_1 <- "my_fun <- function(x, y = 1){\n    x + y\n}"

# Same, but both arguments have no default value
code_2 <- "my_fun <- function(x, y){\n    x + y\n}"

# Body-less assignment form (single expression, no braces)
code_3 <- "my_fun <- function(x) x + 1"

# NOTE (pre-existing limitation, identical before the refactor): extract_aa_body()
# uses match_close(), which does not track string literals. A body whose code
# contains a '}' inside a string (e.g. paste0(x, ")", "}")) is truncated at
# that point. The Shiny app therefore asks users for functions whose strings
# do not contain unbalanced braces. Do NOT add a test expecting full capture
# here until match_close() is string-aware.

# arg_check() settings with a real constraint, as the app builds them
st_vec <- function() list(
  class = "vector", typeof = "NULL", mode = "NULL", length = "NULL",
  options = "NULL", prop = FALSE, double_as_integer_allowed = FALSE,
  all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE,
  inf_values = TRUE
)

# Rebuild a pasted code into its safer version, using the app's own flow
rebuild <- function(code, settings = list(), ...) {
  parts <- saferMake:::extract_aa_body(code)
  stopifnot(!is.null(parts))
  f <- eval(parse(text = code))
  fun_args <- names(formals(f))
  if (is.null(fun_args)) fun_args <- character(0)
  saferMake:::build_rebuilt(aa = parts$aa, body = parts$body, pkg = "", link = "",
                            fun_args = fun_args,
                            no_default_args = saferMake:::args_without_default(f),
                            arg_check_settings = settings, ...)
}

# --- R/app_helpers.R --------------------------------------------------------

test_that("arg_id() builds valid HTML ids and the checkbox ids are stable", {
  expect_identical(saferMake:::arg_id("x"), "arg_x")
  expect_identical(saferMake:::arg_id("a b"), "arg_a_b")
  expect_identical(saferMake:::arg_id("a-b"), "arg_a_b")
  expect_identical(saferMake:::null_cb_id("x"), "null_arg_x")
  expect_identical(saferMake:::empty_cb_id("x"), "empty_arg_x")
})

test_that("arg_check_field_ids() covers all 11 arg_check() settings", {
  ids <- saferMake:::arg_check_field_ids("arg_x")
  expect_setequal(
    names(ids),
    c("class", "typeof", "mode", "length", "prop",
      "double_as_integer_allowed", "options", "all_options_in_data",
      "na_contain", "neg_values", "inf_values")
  )
  expect_true(all(grepl("^ac_.+_arg_x$", ids)))
})

test_that("field_blank() detects NULL and whitespace-only values", {
  expect_true(saferMake:::field_blank(NULL))
  expect_true(saferMake:::field_blank(""))
  expect_true(saferMake:::field_blank("   "))
  expect_false(saferMake:::field_blank("numeric"))
})

test_that("arg_check_parse_options() splits and detects numeric sets", {
  expect_null(saferMake:::arg_check_parse_options(""))
  expect_identical(saferMake:::arg_check_parse_options("a, b 2 c"),
                   c("a", "b", "2", "c"))
  expect_identical(saferMake:::arg_check_parse_options("1, 2, 3"), c(1, 2, 3))
  expect_identical(saferMake:::arg_check_parse_options("1;2"), c(1, 2))
})

test_that("arg_check_parse_length() accepts integers only", {
  expect_identical(saferMake:::arg_check_parse_length("3"), 3L)
  expect_identical(saferMake:::arg_check_parse_length("  7  "), 7L)
  expect_null(saferMake:::arg_check_parse_length("2.5"))
  expect_null(saferMake:::arg_check_parse_length("abc"))
  expect_null(saferMake:::arg_check_parse_length(""))
})

test_that("SAFER_ARGS are exactly the three safer-r mandatory arguments", {
  expect_setequal(saferMake:::SAFER_ARGS, c("lib_path", "safer_check", "error_text"))
})

# --- R/rebuild.R ------------------------------------------------------------

test_that("extract_aa_body() captures the signature WITHOUT the closing paren", {
  # aa holds everything before the final ')': build_rebuilt() re-appends
  # the safer-r arguments and the closing brace itself
  parts <- saferMake:::extract_aa_body(code_1)
  expect_identical(parts$aa, "my_fun <- function(x, y = 1")
  expect_identical(parts$body, "\n    x + y\n")
})

test_that("extract_aa_body() captures single-expression bodies", {
  parts <- saferMake:::extract_aa_body(code_3)
  expect_identical(parts$aa, "my_fun <- function(x")
  expect_identical(parts$body, "x + 1")
})

# NOTE (pre-existing limitation, identical before the refactor): extract_aa_body()
# uses match_close(), which does not track string literals. A body whose code
# contains a '}' inside a string (e.g. paste0(x, ")", "}")) is truncated at
# that point. The Shiny app therefore asks users for functions whose strings
# do not contain unbalanced braces. Do NOT add a test expecting full capture
# here until match_close() is string-aware.

test_that("args_without_default() only flags truly default-less arguments", {
  f1 <- eval(parse(text = code_1))  # x has no default, y = 1 has
  expect_identical(saferMake:::args_without_default(f1), "x")
  f2 <- eval(parse(text = code_2))
  expect_setequal(saferMake:::args_without_default(f2), c("x", "y"))
  # explicit NULL default is NOT "no default"
  f3 <- function(x = NULL, y = 1) x
  expect_identical(saferMake:::args_without_default(f3), character(0))
})

test_that("build_rebuilt() output always parses", {
  expect_error(parse(text = rebuild(code_1)), NA)
  expect_error(parse(text = rebuild(code_3)), NA)
  expect_error(parse(text = rebuild(code_2, settings = list(
    arg_x = st_vec(), arg_y = st_vec()))), NA)
})

test_that("build_rebuilt() injects the three mandatory safer-r arguments", {
  out <- rebuild(code_1)
  expect_match(out, "lib_path = NULL,", fixed = TRUE)
  expect_match(out, "safer_check = TRUE,", fixed = TRUE)
  expect_match(out, 'error_text = ""', fixed = TRUE)
})

test_that("build_rebuilt() emits the no-default-value section only when needed", {
  out <- rebuild(code_1)  # x has no default -> section present
  expect_match(out, "######## arg with no default values", fixed = TRUE)
  out3 <- rebuild("f <- function(x = 1) x")
  expect_no_match(out3, "######## arg with no default values")
})

test_that("build_rebuilt() marks NULL-accepting args as inactive in the check", {
  out <- rebuild(code_1, null_args = "x", non_null_args = "y")
  expect_match(out, '"x", # inactivated because can be NULL', fixed = TRUE)
  expect_match(out, '"y", ', fixed = TRUE)
})

test_that("build_rebuilt() marks empty-accepting args as inactive", {
  out <- rebuild(code_1, empty_args = "x", non_empty_args = "y")
  expect_match(out, '# "x", # inactivated because can be an empty non NULL object',
               fixed = TRUE)
})

test_that("build_rebuilt() emits one arg_check() line per argument", {
  out <- rebuild(code_1)
  expect_match(out, "tempo <- saferDev::arg_check(data = x,", fixed = TRUE)
  expect_match(out, "tempo <- saferDev::arg_check(data = y,", fixed = TRUE)
  # the collection expression (ee) is emitted
  expect_match(out, "argum_check <- base::c(argum_check, tempo$problem)", fixed = TRUE)
})

test_that("build_rebuilt() pkg/link fields are handled when empty", {
  out <- rebuild(code_1)
  expect_match(out, "package_name <- NULL", fixed = TRUE)
  expect_match(out, "internal_error_report_link <- NULL", fixed = TRUE)
  parts <- saferMake:::extract_aa_body(code_1)
  out2 <- saferMake:::build_rebuilt(
    aa = parts$aa, body = parts$body,
    pkg = "mypkg", link = "https://example.com/issues",
    fun_args = c("x", "y"), no_default_args = "x")
  expect_match(out2, 'package_name <- "mypkg"', fixed = TRUE)
  expect_match(out2, '"https://example.com/issues"', fixed = TRUE)
})

# Runtime behaviour of the GENERATED function. The backbone reads formals()
# through sys.parent(n = 2), so the function MUST run in a context where two
# frames up is the global frame (a plain user script). Inside test_that()
# closures the frame chain contains testthat internals, which would trigger
# the mandatory-argument check spuriously. Therefore the generated file is
# exercised in a clean Rscript subprocess, exactly the way a user would.
test_that("generated safer function runs and blocks bad arguments (subprocess)", {
  skip_if_not_installed("saferDev")
  skip_on_cran()
  out <- rebuild(code_1, non_null_args = c("x", "y"),
                 settings = list(arg_x = st_vec(), arg_y = st_vec()))
  driver <- c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "source(args[1])",
    "tryCatch({",
    "  r <- my_fun(1)",
    "  cat('OK:', r, '\\n')",
    "}, error = function(e) cat('ERROR:', conditionMessage(e), '\\n'))",
    "tryCatch({",
    "  r <- my_fun(NULL)",
    "  cat('OK2:', r, '\\n')",
    "}, error = function(e) cat('ERROR2:', conditionMessage(e), '\\n'))",
    "tryCatch({",
    "  r <- my_fun(NA)",
    "  cat('OK3:', r, '\\n')",
    "}, error = function(e) cat('ERROR3:', conditionMessage(e), '\\n'))"
  )
  d <- file.path(tempdir(), "safermake-driver")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  fun_file <- file.path(d, "my_fun_safer.R")
  drv_file <- file.path(d, "driver.R")
  writeLines(out, fun_file)
  writeLines(driver, drv_file)
  res <- system2(file.path(R.home("bin"), "Rscript"),
                 c(shQuote(drv_file, type = "cmd"), shQuote(fun_file, type = "cmd")),
                 stdout = TRUE, stderr = TRUE)
  txt <- paste(res, collapse = "\n")
  expect_match(txt, "OK: 2", fixed = TRUE)
  expect_match(txt, "ERROR2:")
  expect_match(txt, "CANNOT BE NULL", fixed = TRUE)
  expect_match(txt, "ERROR3:")
  expect_match(txt, "CANNOT BE MADE OF NA ONLY", fixed = TRUE)
})

test_that("generated function blocks empty non-NULL arguments (subprocess)", {
  skip_if_not_installed("saferDev")
  skip_on_cran()
  out <- rebuild(code_1, non_empty_args = c("x", "y"),
                 settings = list(arg_x = st_vec(), arg_y = st_vec()))
  driver <- c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "source(args[1])",
    "tryCatch({",
    "  r <- my_fun(character(0))",
    "  cat('OK:', r, '\\n')",
    "}, error = function(e) cat('ERROR:', conditionMessage(e), '\\n'))"
  )
  d <- file.path(tempdir(), "safermake-driver2")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  fun_file <- file.path(d, "my_fun_safer.R")
  drv_file <- file.path(d, "driver.R")
  writeLines(out, fun_file)
  writeLines(driver, drv_file)
  res <- system2(file.path(R.home("bin"), "Rscript"),
                 c(shQuote(drv_file, type = "cmd"), shQuote(fun_file, type = "cmd")),
                 stdout = TRUE, stderr = TRUE)
  txt <- paste(res, collapse = "\n")
  expect_match(txt, "ERROR:")
  expect_match(txt, "CANNOT BE AN EMPTY NON NULL OBJECT", fixed = TRUE)
})

# --- R/arg_check.R -----------------------------------------------------------

test_that("arg_check_call_core() writes the full saferDev::arg_check call", {
  st <- saferMake:::ARG_CHECK_DEFAULTS
  txt <- saferMake:::arg_check_call_core("x", st)
  expect_match(txt, "^saferDev::arg_check\\(data = x,")
  expect_match(txt, "class = NULL,")          # "NULL" string -> bare NULL
  expect_match(txt, "typeof = NULL,")
  expect_match(txt, "mode = NULL,")
  expect_match(txt, "length = NULL,")
  expect_match(txt, "prop = FALSE,")
  expect_match(txt, "options = NULL,")
  expect_match(txt, "print = FALSE,")
  expect_match(txt, "safer_check = FALSE,")
  expect_match(txt, "lib_path = lib_path,")
  expect_match(txt, "error_text = embed_error_text\\)")
})

test_that("arg_check_call_core() renders numeric options as numbers", {
  st <- utils::modifyList(saferMake:::ARG_CHECK_DEFAULTS, list(length = "2", options = "1, 2"))
  txt <- saferMake:::arg_check_call_core("x", st)
  expect_match(txt, "length = 2L,")
  expect_match(txt, "options = c\\(1, 2\\),")
})

test_that("arg_check_call_core() renders character options as strings", {
  st <- utils::modifyList(saferMake:::ARG_CHECK_DEFAULTS,
                          list(class = "character", options = "a, b"))
  txt <- saferMake:::arg_check_call_core("x", st)
  expect_match(txt, 'options = c\\("a", "b"\\),')
})

test_that("ac_test_value_code() builds a value consistent with the settings", {
  st <- saferMake:::ARG_CHECK_DEFAULTS  # numeric, length 1
  expect_identical(saferMake:::ac_test_value_code(st), "1.5")
  # NOTE: the default options field is the string "NULL", which
  # arg_check_parse_options() returns as a character option set. With
  # class = "character", the test value is therefore the string "NULL"
  # (verified identical to the pre-refactor behaviour).
  st2 <- utils::modifyList(saferMake:::ARG_CHECK_DEFAULTS, list(class = "character"))
  expect_identical(saferMake:::ac_test_value_code(st2), '"NULL"')
  st3 <- utils::modifyList(saferMake:::ARG_CHECK_DEFAULTS, list(length = "2"))
  expect_identical(saferMake:::ac_test_value_code(st3), "c(1.5, 2.5)")
  # options override the kind, truncated to the length (1 by default)
  st4 <- utils::modifyList(saferMake:::ARG_CHECK_DEFAULTS, list(options = "1, 2"))
  expect_identical(saferMake:::ac_test_value_code(st4), "1")
})

# --- run_arg_check_tests() (needs saferDev; skipped otherwise) ---------------

test_that("run_arg_check_tests() returns ok=TRUE for consistent settings", {
  skip_if_not_installed("saferDev")
  res <- saferMake:::run_arg_check_tests(
    fun_args = "x",
    arg_check_settings = list(arg_x = st_vec())
  )
  expect_true(res$arg_x$ok)
  expect_null(res$arg_x$message)
})

test_that("run_arg_check_tests() reports a failure for inconsistent settings", {
  skip_if_not_installed("saferDev")
  # class = "character" but the default numeric-kind test value -> must fail
  st <- utils::modifyList(st_vec(), list(class = "character"))
  res <- saferMake:::run_arg_check_tests(
    fun_args = "x",
    arg_check_settings = list(arg_x = st)
  )
  expect_false(res$arg_x$ok)
  expect_true(nzchar(res$arg_x$message))
})
