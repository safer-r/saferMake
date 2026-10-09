# Unit tests of the internal app helpers (R/intern_.app_helpers.R,
# R/intern_.arg_check_lines.R, R/intern_.rebuild.R) extracted from
# inst/app/server.R. All helpers are internal (dot-prefixed): access them
# via ::: (normal for internal testing).

# --- shared fixtures ---------------------------------------------------------

# A simple user function, as pasted in the app (single-arg, with default)
code_1 <- "my_fun <- function(x, y = 1){\n    x + y\n}"

# Same, but both arguments have no default value
code_2 <- "my_fun <- function(x, y){\n    x + y\n}"

# Body-less assignment form (single expression, no braces)
code_3 <- "my_fun <- function(x) x + 1"

# NOTE (pre-existing limitation, identical before the refactor): .extract_aa_body()
# uses .match_close(), which does not track string literals. A body whose code
# contains a '}' inside a string (e.g. paste0(x, ")", "}")) is truncated at
# that point. The Shiny app therefore asks users for functions whose strings
# do not contain unbalanced braces. Do NOT add a test expecting full capture
# here until .match_close() is string-aware.

# arg_check() settings with a real constraint, as the app builds them
st_vec <- function() base::list(
  class = "vector", typeof = "NULL", mode = "NULL", length = "NULL",
  options = "NULL", prop = FALSE, double_as_integer_allowed = FALSE,
  all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE,
  inf_values = TRUE
)

# Rebuild a pasted code into its safer version, using the app's own flow
rebuild <- function(code, settings = base::list(), ...) {
  parts <- saferMake:::.extract_aa_body(code = code)
  base::stopifnot(! base::is.null(x = parts), exprs = , exprObject = , local = TRUE)
  # NOTE: eval() is called with its lazy default envir (parent.frame()
  # evaluated inside eval), i.e., with no envir argument written: writing
  # an explicit frame here would change the assignment target of the
  # function created from the parsed text.
  f <- base::eval(expr = base::parse(file = "", n = NULL, text = code, prompt = "?",
                                     keep.source = base::getOption(x = "keep.source", default = NULL),
                                     srcfile = NULL, encoding = "unknown"))
  fun_args <- base::names(x = base::formals(fun = f, envir = base::parent.frame(n = 1)))
  if (base::is.null(x = fun_args)) fun_args <- base::character(length = 0L)
  saferMake:::.build_rebuilt(aa = parts$aa, body = parts$body, pkg = "", link = "",
                             fun_args = fun_args,
                             no_default_args = saferMake:::.args_without_default(f = f),
                             arg_check_settings = settings, ...)
}

# --- R/intern_.app_helpers.R --------------------------------------------------

testthat::test_that(desc = ".arg_id() builds valid HTML ids and the checkbox ids are stable", code = {
  testthat::expect_identical(object = saferMake:::.arg_id(nm = "x"), expected = "arg_x")
  testthat::expect_identical(object = saferMake:::.arg_id(nm = "a b"), expected = "arg_a_b")
  testthat::expect_identical(object = saferMake:::.arg_id(nm = "a-b"), expected = "arg_a_b")
  testthat::expect_identical(object = saferMake:::.null_cb_id(nm = "x"), expected = "null_arg_x")
  testthat::expect_identical(object = saferMake:::.empty_cb_id(nm = "x"), expected = "empty_arg_x")
})

testthat::test_that(desc = ".arg_check_field_ids() covers all 12 arg_check() settings", code = {
  ids <- saferMake:::.arg_check_field_ids(id = "arg_x")
  testthat::expect_setequal(
    object = base::names(x = ids),
    expected = base::c("class", "typeof", "mode", "length", "prop",
                       "double_as_integer_allowed", "options", "all_options_in_data",
                       "na_contain", "neg_values", "inf_values", "no_empty_string")
  )
  testthat::expect_true(object = base::all(base::grepl(pattern = "^ac_.+_arg_x$", x = ids,
                                                      ignore.case = FALSE, perl = FALSE,
                                                      fixed = FALSE, useBytes = FALSE),
                                           na.rm = FALSE))
})

testthat::test_that(desc = ".field_blank() detects NULL and whitespace-only values", code = {
  testthat::expect_true(object = saferMake:::.field_blank(v = NULL))
  testthat::expect_true(object = saferMake:::.field_blank(v = ""))
  testthat::expect_true(object = saferMake:::.field_blank(v = "   "))
  testthat::expect_false(object = saferMake:::.field_blank(v = "numeric"))
})

# --- .arg_check_settings_errors() ---------------------------------------------

testthat::test_that(desc = ".arg_check_settings_errors() is silent for the default settings", code = {
  res <- saferMake:::.arg_check_settings_errors(
    fun_args = base::c("x", "y"),
    arg_check_settings = base::list(arg_x = saferMake:::.arg_check_defaults,
                                    arg_y = saferMake:::.arg_check_defaults)
  )
  testthat::expect_identical(object = res$arg_x, expected = base::character(length = 0L))
  testthat::expect_identical(object = res$arg_y, expected = base::character(length = 0L))
})

testthat::test_that(desc = ".arg_check_settings_errors() accepts character in class, typeof or mode", code = {
  st_cls <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                              val = base::list(no_empty_string = TRUE, class = "character"), keep.null = FALSE)
  st_typ <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                              val = base::list(no_empty_string = TRUE, typeof = "character"), keep.null = FALSE)
  st_mod <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                              val = base::list(no_empty_string = TRUE, mode = "character"), keep.null = FALSE)
  testthat::expect_identical(
    object = saferMake:::.arg_check_settings_errors("x", base::list(arg_x = st_cls))$arg_x,
    expected = base::character(length = 0L))
  testthat::expect_identical(
    object = saferMake:::.arg_check_settings_errors("x", base::list(arg_x = st_typ))$arg_x,
    expected = base::character(length = 0L))
  testthat::expect_identical(
    object = saferMake:::.arg_check_settings_errors("x", base::list(arg_x = st_mod))$arg_x,
    expected = base::character(length = 0L))
})

testthat::test_that(desc = ".arg_check_settings_errors() reports the missing character error", code = {
  st <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                          val = base::list(no_empty_string = TRUE), keep.null = FALSE)
  res <- saferMake:::.arg_check_settings_errors("x", base::list(arg_x = st))
  testthat::expect_length(object = res$arg_x, n = 1L)
  testthat::expect_match(object = res$arg_x, regexp = "empty strings", fixed = TRUE)
  testthat::expect_match(object = res$arg_x,
                         regexp = "character value has not been used in any of the Class, typeof or mode field",
                         fixed = TRUE)
})

testthat::test_that(desc = ".arg_check_settings_errors() accepts numeric/double in class, typeof or mode", code = {
  st_cls <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                              val = base::list(neg_values = FALSE, inf_values = FALSE, class = "numeric"), keep.null = FALSE)
  st_typ <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                              val = base::list(neg_values = FALSE, inf_values = FALSE, typeof = "double"), keep.null = FALSE)
  st_mod <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                              val = base::list(neg_values = FALSE, inf_values = FALSE, mode = "numeric"), keep.null = FALSE)
  testthat::expect_identical(
    object = saferMake:::.arg_check_settings_errors("x", base::list(arg_x = st_cls))$arg_x,
    expected = base::character(length = 0L))
  testthat::expect_identical(
    object = saferMake:::.arg_check_settings_errors("x", base::list(arg_x = st_typ))$arg_x,
    expected = base::character(length = 0L))
  testthat::expect_identical(
    object = saferMake:::.arg_check_settings_errors("x", base::list(arg_x = st_mod))$arg_x,
    expected = base::character(length = 0L))
})

testthat::test_that(desc = ".arg_check_settings_errors() reports the missing numeric errors for both boxes", code = {
  st <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                          val = base::list(neg_values = FALSE, inf_values = FALSE), keep.null = FALSE)
  res <- saferMake:::.arg_check_settings_errors("x", base::list(arg_x = st))
  testthat::expect_length(object = res$arg_x, n = 2L)
  testthat::expect_match(object = res$arg_x[1],
                         regexp = "Values can be negative if they are numeric", fixed = TRUE)
  testthat::expect_match(object = res$arg_x[2],
                         regexp = "Values can be Inf or -Inf if they are numeric", fixed = TRUE)
})

testthat::test_that(desc = ".arg_check_settings_errors() is silent when the numeric boxes are ticked (default)", code = {
  # defaults: neg_values = TRUE and inf_values = TRUE -> no error, even with
  # blank Class/typeof/mode fields
  res <- saferMake:::.arg_check_settings_errors("x", base::list(arg_x = saferMake:::.arg_check_defaults))
  testthat::expect_identical(object = res$arg_x, expected = base::character(length = 0L))
})

testthat::test_that(desc = ".arg_check_settings_errors() uses the default settings when an argument has none", code = {
  res <- saferMake:::.arg_check_settings_errors("x", arg_check_settings = base::list())
  testthat::expect_identical(object = res$arg_x, expected = base::character(length = 0L))
})

testthat::test_that(desc = ".arg_check_settings_errors() handles empty fun_args", code = {
  res <- saferMake:::.arg_check_settings_errors(base::character(length = 0L), base::list())
  testthat::expect_identical(object = res, expected = base::list())
})

testthat::test_that(desc = ".arg_check_settings_errors() reports a single neg_values error", code = {
  st <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                          val = base::list(neg_values = FALSE), keep.null = FALSE)
  res <- saferMake:::.arg_check_settings_errors("x", base::list(arg_x = st))
  testthat::expect_length(object = res$arg_x, n = 1L)
  testthat::expect_match(object = res$arg_x[1],
                         regexp = "Values can be negative if they are numeric", fixed = TRUE)
  testthat::expect_no_match(object = res$arg_x[1],
                            regexp = "Values can be Inf or -Inf if they are numeric", fixed = TRUE)
})

testthat::test_that(desc = ".arg_check_settings_errors() checks the two boxes independently", code = {
  # no_empty_string ticked AND neg_values unticked, but class = "character":
  # the character check passes while the numeric check still fires
  st <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                          val = base::list(no_empty_string = TRUE, neg_values = FALSE,
                                           class = "character"),
                          keep.null = FALSE)
  res <- saferMake:::.arg_check_settings_errors("x", base::list(arg_x = st))
  testthat::expect_length(object = res$arg_x, n = 1L)
  testthat::expect_match(object = res$arg_x[1],
                         regexp = "Values can be negative if they are numeric", fixed = TRUE)
})

testthat::test_that(desc = ".arg_check_settings_errors() drops NA kind values", code = {
  # a NA (or NULL) element in the kind fields must not count as a used value
  st <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                          val = base::list(neg_values = FALSE, class = NA_character_),
                          keep.null = FALSE)
  res <- saferMake:::.arg_check_settings_errors("x", base::list(arg_x = st))
  testthat::expect_length(object = res$arg_x, n = 1L)
})

testthat::test_that(desc = ".arg_check_parse_options() splits and detects numeric sets", code = {
  testthat::expect_null(object = saferMake:::.arg_check_parse_options(txt = ""))
  testthat::expect_identical(object = saferMake:::.arg_check_parse_options(txt = "a, b 2 c"),
                             expected = base::c("a", "b", "2", "c"))
  testthat::expect_identical(object = saferMake:::.arg_check_parse_options(txt = "1, 2, 3"),
                             expected = base::c(1, 2, 3))
  testthat::expect_identical(object = saferMake:::.arg_check_parse_options(txt = "1;2"),
                             expected = base::c(1, 2))
})

testthat::test_that(desc = ".arg_check_parse_length() accepts integers only", code = {
  testthat::expect_identical(object = saferMake:::.arg_check_parse_length(txt = "3"), expected = 3L)
  testthat::expect_identical(object = saferMake:::.arg_check_parse_length(txt = "  7  "), expected = 7L)
  testthat::expect_null(object = saferMake:::.arg_check_parse_length(txt = "2.5"))
  testthat::expect_null(object = saferMake:::.arg_check_parse_length(txt = "abc"))
  testthat::expect_null(object = saferMake:::.arg_check_parse_length(txt = ""))
})

testthat::test_that(desc = ".safer_args are exactly the three safer-r mandatory arguments", code = {
  testthat::expect_setequal(object = saferMake:::.safer_args,
                            expected = base::c("lib_path", "safer_check", "error_text"))
})

# --- R/intern_.rebuild.R -------------------------------------------------------

testthat::test_that(desc = ".extract_aa_body() captures the signature WITHOUT the closing paren", code = {
  # aa holds everything before the final ')': .build_rebuilt() re-appends
  # the safer-r arguments and the closing brace itself
  parts <- saferMake:::.extract_aa_body(code = code_1)
  testthat::expect_identical(object = parts$aa, expected = "my_fun <- function(x, y = 1")
  testthat::expect_identical(object = parts$body, expected = "\n    x + y\n")
})

testthat::test_that(desc = ".extract_aa_body() captures single-expression bodies", code = {
  parts <- saferMake:::.extract_aa_body(code = code_3)
  testthat::expect_identical(object = parts$aa, expected = "my_fun <- function(x")
  testthat::expect_identical(object = parts$body, expected = "x + 1")
})

# NOTE (pre-existing limitation, identical before the refactor): .extract_aa_body()
# uses .match_close(), which does not track string literals. A body whose code
# contains a '}' inside a string (e.g. paste0(x, ")", "}")) is truncated at
# that point. The Shiny app therefore asks users for functions whose strings
# do not contain unbalanced braces. Do NOT add a test expecting full capture
# here until .match_close() is string-aware.

testthat::test_that(desc = ".args_without_default() only flags truly default-less arguments", code = {
  f1 <- base::eval(expr = base::parse(file = "", n = NULL, text = code_1, prompt = "?",
                                      keep.source = base::getOption(x = "keep.source", default = NULL),
                                      srcfile = NULL, encoding = "unknown"))  # x has no default, y = 1 has
  testthat::expect_identical(object = saferMake:::.args_without_default(f = f1), expected = "x")
  f2 <- base::eval(expr = base::parse(file = "", n = NULL, text = code_2, prompt = "?",
                                      keep.source = base::getOption(x = "keep.source", default = NULL),
                                      srcfile = NULL, encoding = "unknown"))
  testthat::expect_setequal(object = saferMake:::.args_without_default(f = f2), expected = base::c("x", "y"))
  # explicit NULL default is NOT "no default"
  f3 <- function(x = NULL, y = 1) x
  testthat::expect_identical(object = saferMake:::.args_without_default(f = f3),
                             expected = base::character(length = 0L))
})

testthat::test_that(desc = ".build_rebuilt() output always parses", code = {
  testthat::expect_error(object = base::parse(file = "", n = NULL, text = rebuild(code = code_1),
                                              prompt = "?",
                                              keep.source = base::getOption(x = "keep.source", default = NULL),
                                              srcfile = NULL, encoding = "unknown"),
                         regexp = NA)
  testthat::expect_error(object = base::parse(file = "", n = NULL, text = rebuild(code = code_3),
                                              prompt = "?",
                                              keep.source = base::getOption(x = "keep.source", default = NULL),
                                              srcfile = NULL, encoding = "unknown"),
                         regexp = NA)
  testthat::expect_error(object = base::parse(file = "", n = NULL,
                                              text = rebuild(code = code_2,
                                                             settings = base::list(arg_x = st_vec(),
                                                                                   arg_y = st_vec())),
                                              prompt = "?",
                                              keep.source = base::getOption(x = "keep.source", default = NULL),
                                              srcfile = NULL, encoding = "unknown"),
                         regexp = NA)
})

testthat::test_that(desc = ".build_rebuilt() injects the three mandatory safer-r arguments", code = {
  out <- rebuild(code = code_1)
  testthat::expect_match(object = out, regexp = "lib_path = NULL,", fixed = TRUE)
  testthat::expect_match(object = out, regexp = "safer_check = TRUE,", fixed = TRUE)
  testthat::expect_match(object = out, regexp = 'error_text = ""', fixed = TRUE)
})

testthat::test_that(desc = ".build_rebuilt() emits the no-default-value section only when needed", code = {
  out <- rebuild(code = code_1)  # x has no default -> section present
  testthat::expect_match(object = out, regexp = "######## arg with no default values", fixed = TRUE)
  out3 <- rebuild(code = "f <- function(x = 1) x")
  testthat::expect_no_match(object = out3, regexp = "######## arg with no default values")
})

testthat::test_that(desc = ".build_rebuilt() marks NULL-accepting args as inactive in the check", code = {
  out <- rebuild(code = code_1, null_args = "x", non_null_args = "y")
  testthat::expect_match(object = out, regexp = '"x", # inactivated because can be NULL', fixed = TRUE)
  testthat::expect_match(object = out, regexp = '"y", ', fixed = TRUE)
})

testthat::test_that(desc = ".build_rebuilt() marks empty-accepting args as inactive", code = {
  out <- rebuild(code = code_1, empty_args = "x", non_empty_args = "y")
  testthat::expect_match(object = out,
                         regexp = '# "x", # inactivated because can be an empty non NULL object',
                         fixed = TRUE)
})

testthat::test_that(desc = ".build_rebuilt() emits the \"\" section only when a box is ticked", code = {
  # no ticked box -> no section
  out0 <- rebuild(code = code_1)
  testthat::expect_no_match(object = out0, regexp = '######## management of "" in arguments of mode character')
  # one ticked box -> section present, with the name ACTIVE in tempo_arg
  out <- rebuild(code = code_1, no_empty_string_args = "x")
  testthat::expect_match(object = out,
                         regexp = '######## management of "" in arguments of mode character',
                         fixed = TRUE)
  # single active name: NO trailing comma (would be a syntax error before ")")
  testthat::expect_match(object = out, regexp = '"x"\n', fixed = TRUE)
  testthat::expect_no_match(object = out, regexp = '"x", \n        # "lib_path"', fixed = TRUE)
  # the two safer-r args stay commented in the new section
  testthat::expect_match(object = out,
                         regexp = '# "lib_path" # inactivated because already checked above',
                         fixed = TRUE)
  testthat::expect_match(object = out,
                         regexp = '# "error_text" # inactivated because can be ""',
                         fixed = TRUE)
  # the section is placed after the arg_check() closing marker and before
  # "end argument secondary checking"
  pos_start <- base::regexpr(pattern = '######## end argument checking with arg_check()',
                             text = out, ignore.case = FALSE, perl = FALSE,
                             fixed = TRUE, useBytes = FALSE)
  pos_sec  <- base::regexpr(pattern = '######## management of "" in arguments of mode character',
                            text = out, ignore.case = FALSE, perl = FALSE,
                            fixed = TRUE, useBytes = FALSE)
  pos_end  <- base::regexpr(pattern = '#### end argument secondary checking',
                            text = out, ignore.case = FALSE, perl = FALSE,
                            fixed = TRUE, useBytes = FALSE)
  testthat::expect_gt(object = pos_sec, expected = pos_start)
  testthat::expect_lt(object = pos_sec, expected = pos_end)
})

testthat::test_that(desc = ".build_rebuilt() writes the improved internal error text with types", code = {
  out <- rebuild(code = code_1, no_empty_string_args = "x")
  testthat::expect_match(object = out,
                         regexp = "PLEASE RESTART WITH YOUR MAIN CODE AND SELECT PROPER CLASS, TYPE AND/OR MODE",
                         fixed = TRUE)
  testthat::expect_match(object = out, regexp = ".HERE, ARGUMENTS ARE TYPE:", fixed = TRUE)
  testthat::expect_match(object = out,
                         regexp = "base::sapply(X = tempo_arg, FUN = function(x){base::paste0(x, ': \"', base::typeof(",
                         fixed = TRUE)
})

testthat::test_that(desc = ".build_rebuilt() separates several active names by comma without a trailing one", code = {
  out <- rebuild(code = code_1, no_empty_string_args = base::c("x", "y"))
  # first name followed by a comma, last name without
  testthat::expect_match(object = out, regexp = '"x",\n        "y"\n', fixed = TRUE)
  # and the generated function still parses
  testthat::expect_error(object = base::parse(file = "", n = NULL, text = out,
                                              prompt = "?",
                                              keep.source = base::getOption(x = "keep.source", default = NULL),
                                              srcfile = NULL, encoding = "unknown"),
                         regexp = NA)
})

testthat::test_that(desc = ".build_rebuilt() emits no seed section when seed_arg is NULL", code = {
  out <- rebuild(code = code_1)  # seed_arg defaults to NULL
  testthat::expect_no_match(object = out,
                            regexp = "######## code that protects set.seed() in the global environment",
                            fixed = TRUE)
})

testthat::test_that(desc = ".build_rebuilt() treats an empty seed_arg name like NA (delimiters only)", code = {
  out <- rebuild(code = code_1, seed_arg = "")
  testthat::expect_match(object = out,
                         regexp = "######## code that protects set.seed() in the global environment",
                         fixed = TRUE)
  testthat::expect_match(object = out,
                         regexp = "######## end code that protects set.seed() in the global environment",
                         fixed = TRUE)
  # no set.seed() call inside the delimiters
  testthat::expect_no_match(object = out, regexp = "base::set.seed(", fixed = TRUE)
  # and the generated function still parses
  testthat::expect_error(object = base::parse(file = "", n = NULL, text = out,
                                              prompt = "?",
                                              keep.source = base::getOption(x = "keep.source", default = NULL),
                                              srcfile = NULL, encoding = "unknown"),
                         regexp = NA)
})

testthat::test_that(desc = ".build_rebuilt() emits only the seed section delimiters when seed_arg is NA", code = {
  out <- rebuild(code = code_1, seed_arg = NA_character_)
  testthat::expect_match(object = out, regexp = "######## code that protects set.seed() in the global environment", fixed = TRUE)
  testthat::expect_match(object = out, regexp = "######## end code that protects set.seed() in the global environment", fixed = TRUE)
  # no set.seed() call inside the delimiters
  testthat::expect_no_match(object = out, regexp = "base::set.seed(", fixed = TRUE)
  # and the generated function still parses
  testthat::expect_error(object = base::parse(file = "", n = NULL, text = out,
                                              prompt = "?",
                                              keep.source = base::getOption(x = "keep.source", default = NULL),
                                              srcfile = NULL, encoding = "unknown"),
                         regexp = NA)
})

testthat::test_that(desc = ".build_rebuilt() emits the full seed section when seed_arg is a name", code = {
  out <- rebuild(code = code_1, seed_arg = "y")
  testthat::expect_match(object = out,
                         regexp = "base::set.seed(seed = y, kind = NULL, normal.kind = NULL, sample.kind = NULL) # seed value is the seed argument of the function",
                         fixed = TRUE)
  testthat::expect_match(object = out, regexp = "tempo.random.seed <- .Random.seed", fixed = TRUE)
  testthat::expect_match(object = out, regexp = "base::on.exit(expr = base::set.seed(seed = NULL", fixed = TRUE)
  # and the generated function still parses
  testthat::expect_error(object = base::parse(file = "", n = NULL, text = out,
                                              prompt = "?",
                                              keep.source = base::getOption(x = "keep.source", default = NULL),
                                              srcfile = NULL, encoding = "unknown"),
                         regexp = NA)
})

testthat::test_that(desc = ".build_rebuilt() emits only the graphic device delimiters when graphic_dev is FALSE", code = {
  out <- rebuild(code = code_1)  # graphic_dev defaults to FALSE
  testthat::expect_match(object = out, regexp = "######## graphic device checking", fixed = TRUE)
  testthat::expect_match(object = out, regexp = "######## end graphic device checking", fixed = TRUE)
  testthat::expect_no_match(object = out, regexp = "grDevices::dev.list()", fixed = TRUE)
  testthat::expect_no_match(object = out, regexp = "graphics::par(", fixed = TRUE)
})

testthat::test_that(desc = ".build_rebuilt() emits the full graphic device section when graphic_dev is TRUE", code = {
  out <- rebuild(code = code_1, graphic_dev = TRUE)
  testthat::expect_match(object = out, regexp = "dev_list <- grDevices::dev.list()", fixed = TRUE)
  testthat::expect_match(object = out, regexp = "SOME GRAPHIC DEVICES WERE OPENED BY", fixed = TRUE)
  testthat::expect_match(object = out, regexp = "par_ini <- base::suppressWarnings(expr = graphics::par(no.readonly = TRUE), classes = \"warning\")", fixed = TRUE)
  testthat::expect_match(object = out, regexp = "base::suppressWarnings(expr = graphics::par(par_ini, no.readonly = TRUE), classes = \"warning\")", fixed = TRUE)
  # the section is placed after the secondary-checking end marker and before
  # the main code
  pos_sec <- base::regexpr(pattern = "#### end argument secondary checking",
                           text = out, ignore.case = FALSE, perl = FALSE,
                           fixed = TRUE, useBytes = FALSE)
  pos_sec2 <- base::regexpr(pattern = "#### second round of checking and data preparation",
                            text = out, ignore.case = FALSE, perl = FALSE,
                            fixed = TRUE, useBytes = FALSE)
  pos_dev <- base::regexpr(pattern = "######## graphic device checking",
                           text = out, ignore.case = FALSE, perl = FALSE,
                           fixed = TRUE, useBytes = FALSE)
  pos_end  <- base::regexpr(pattern = "#### end second round of checking and data preparation",
                            text = out, ignore.case = FALSE, perl = FALSE,
                            fixed = TRUE, useBytes = FALSE)
  testthat::expect_gt(object = pos_dev, expected = pos_sec2)
  testthat::expect_lt(object = pos_dev, expected = pos_end)
  # and the generated function still parses
  testthat::expect_error(object = base::parse(file = "", n = NULL, text = out,
                                              prompt = "?",
                                              keep.source = base::getOption(x = "keep.source", default = NULL),
                                              srcfile = NULL, encoding = "unknown"),
                         regexp = NA)
})

testthat::test_that(desc = ".build_rebuilt() places the seed section between the secondary-checking end marker and the main code", code = {
  out <- rebuild(code = code_1, seed_arg = "y", graphic_dev = TRUE)
  pos_sec  <- base::regexpr(pattern = "#### end argument secondary checking",
                            text = out, ignore.case = FALSE, perl = FALSE,
                            fixed = TRUE, useBytes = FALSE)
  pos_sec2 <- base::regexpr(pattern = "#### second round of checking and data preparation",
                            text = out, ignore.case = FALSE, perl = FALSE,
                            fixed = TRUE, useBytes = FALSE)
  pos_seed <- base::regexpr(pattern = "######## code that protects set.seed() in the global environment",
                            text = out, ignore.case = FALSE, perl = FALSE,
                            fixed = TRUE, useBytes = FALSE)
  pos_dev  <- base::regexpr(pattern = "######## graphic device checking",
                            text = out, ignore.case = FALSE, perl = FALSE,
                            fixed = TRUE, useBytes = FALSE)
  pos_end  <- base::regexpr(pattern = "#### end second round of checking and data preparation",
                            text = out, ignore.case = FALSE, perl = FALSE,
                            fixed = TRUE, useBytes = FALSE)
  testthat::expect_gt(object = pos_sec2, expected = pos_sec)
  testthat::expect_gt(object = pos_seed, expected = pos_sec2)
  testthat::expect_gt(object = pos_dev, expected = pos_seed)
  testthat::expect_lt(object = pos_dev, expected = pos_end)
})

testthat::test_that(desc = ".build_rebuilt() emits one arg_check() line per argument", code = {
  out <- rebuild(code = code_1)
  testthat::expect_match(object = out, regexp = "tempo <- saferDev::arg_check(data = x,", fixed = TRUE)
  testthat::expect_match(object = out, regexp = "tempo <- saferDev::arg_check(data = y,", fixed = TRUE)
  # the collection expression (ee) is emitted
  testthat::expect_match(object = out,
                         regexp = "argum_check <- base::c(argum_check, tempo$problem)", fixed = TRUE)
})

testthat::test_that(desc = ".build_rebuilt() pkg/link fields are handled when empty", code = {
  out <- rebuild(code = code_1)
  testthat::expect_match(object = out, regexp = "package_name <- NULL", fixed = TRUE)
  testthat::expect_match(object = out, regexp = "internal_error_report_link <- NULL", fixed = TRUE)
  parts <- saferMake:::.extract_aa_body(code = code_1)
  out2 <- saferMake:::.build_rebuilt(
    aa = parts$aa, body = parts$body,
    pkg = "mypkg", link = "https://example.com/issues",
    fun_args = base::c("x", "y"), no_default_args = "x")
  testthat::expect_match(object = out2, regexp = 'package_name <- "mypkg"', fixed = TRUE)
  testthat::expect_match(object = out2, regexp = '"https://example.com/issues"', fixed = TRUE)
})

# Runtime behaviour of the GENERATED function. The backbone reads formals()
# through sys.parent(n = 2), so the function MUST run in a context where two
# frames up is the global frame (a plain user script). Inside test_that()
# closures the frame chain contains testthat internals, which would trigger
# the mandatory-argument check spuriously. Therefore the generated file is
# exercised in a clean Rscript subprocess, exactly the way a user would.
testthat::test_that(desc = "generated safer function runs and blocks bad arguments (subprocess)", code = {
  testthat::skip_if_not_installed(pkg = "saferDev")
  testthat::skip_on_cran()
  out <- rebuild(code = code_1, non_null_args = base::c("x", "y"),
                 settings = base::list(arg_x = st_vec(), arg_y = st_vec()))
  driver <- base::c(
    "args <- base::commandArgs(trailingOnly = TRUE)",
    "base::source(file = args[1], local = FALSE)",
    "base::tryCatch(",
    "    expr = {",
    "        r <- my_fun(1)",
    "        base::cat('OK:', r, '\\n', file = \"\", sep = \" \", fill = FALSE, labels = NULL, append = FALSE)",
    "    },",
    "    error = function(e) base::cat('ERROR:', base::conditionMessage(c = e), '\\n', file = \"\", sep = \" \", fill = FALSE, labels = NULL, append = FALSE),",
    "    finally = ",
    ")",
    "base::tryCatch(",
    "    expr = {",
    "        r <- my_fun(NULL)",
    "        base::cat('OK2:', r, '\\n', file = \"\", sep = \" \", fill = FALSE, labels = NULL, append = FALSE)",
    "    },",
    "    error = function(e) base::cat('ERROR2:', base::conditionMessage(c = e), '\\n', file = \"\", sep = \" \", fill = FALSE, labels = NULL, append = FALSE),",
    "    finally = ",
    ")",
    "base::tryCatch(",
    "    expr = {",
    "        r <- my_fun(NA)",
    "        base::cat('OK3:', r, '\\n', file = \"\", sep = \" \", fill = FALSE, labels = NULL, append = FALSE)",
    "    },",
    "    error = function(e) base::cat('ERROR3:', base::conditionMessage(c = e), '\\n', file = \"\", sep = \" \", fill = FALSE, labels = NULL, append = FALSE),",
    "    finally = ",
    ")"
  )
  d <- base::file.path(tempdir(), "safermake-driver")
  base::dir.create(path = d, showWarnings = FALSE, recursive = TRUE, mode = "0777")
  fun_file <- base::file.path(d, "my_fun_safer.R")
  drv_file <- base::file.path(d, "driver.R")
  base::writeLines(text = out, con = fun_file, sep = "\n", useBytes = FALSE)
  base::writeLines(text = driver, con = drv_file, sep = "\n", useBytes = FALSE)
  res <- base::system2(command = base::file.path(base::R.home(component = "bin"), "Rscript"),
                       args = base::c(base::shQuote(string = drv_file, type = "cmd"),
                                      base::shQuote(string = fun_file, type = "cmd")),
                       stdout = TRUE, stderr = TRUE, stdin = "", input = NULL,
                       env = base::character(), wait = TRUE, minimized = FALSE,
                       invisible = TRUE, timeout = 0, receive.console.signals = TRUE)
  txt <- base::paste(res, sep = " ", collapse = "\n", recycle0 = FALSE)
  testthat::expect_match(object = txt, regexp = "OK: 2", fixed = TRUE)
  testthat::expect_match(object = txt, regexp = "ERROR2:")
  testthat::expect_match(object = txt, regexp = "CANNOT BE NULL", fixed = TRUE)
  testthat::expect_match(object = txt, regexp = "ERROR3:")
  testthat::expect_match(object = txt, regexp = "CANNOT BE MADE OF NA ONLY", fixed = TRUE)
})

testthat::test_that(desc = "generated function blocks empty non-NULL arguments (subprocess)", code = {
  testthat::skip_if_not_installed(pkg = "saferDev")
  testthat::skip_on_cran()
  out <- rebuild(code = code_1, non_empty_args = base::c("x", "y"),
                 settings = base::list(arg_x = st_vec(), arg_y = st_vec()))
  driver <- base::c(
    "args <- base::commandArgs(trailingOnly = TRUE)",
    "base::source(file = args[1], local = FALSE)",
    "base::tryCatch(",
    "    expr = {",
    "        r <- my_fun(character(0))",
    "        base::cat('OK:', r, '\\n', file = \"\", sep = \" \", fill = FALSE, labels = NULL, append = FALSE)",
    "    },",
    "    error = function(e) base::cat('ERROR:', base::conditionMessage(c = e), '\\n', file = \"\", sep = \" \", fill = FALSE, labels = NULL, append = FALSE),",
    "    finally = ",
    ")"
  )
  d <- base::file.path(tempdir(), "safermake-driver2")
  base::dir.create(path = d, showWarnings = FALSE, recursive = TRUE, mode = "0777")
  fun_file <- base::file.path(d, "my_fun_safer.R")
  drv_file <- base::file.path(d, "driver.R")
  base::writeLines(text = out, con = fun_file, sep = "\n", useBytes = FALSE)
  base::writeLines(text = driver, con = drv_file, sep = "\n", useBytes = FALSE)
  res <- base::system2(command = base::file.path(base::R.home(component = "bin"), "Rscript"),
                       args = base::c(base::shQuote(string = drv_file, type = "cmd"),
                                      base::shQuote(string = fun_file, type = "cmd")),
                       stdout = TRUE, stderr = TRUE, stdin = "", input = NULL,
                       env = base::character(), wait = TRUE, minimized = FALSE,
                       invisible = TRUE, timeout = 0, receive.console.signals = TRUE)
  txt <- base::paste(res, sep = " ", collapse = "\n", recycle0 = FALSE)
  testthat::expect_match(object = txt, regexp = "ERROR:")
  testthat::expect_match(object = txt, regexp = "CANNOT BE AN EMPTY NON NULL OBJECT", fixed = TRUE)
})

# --- R/intern_.arg_check_lines.R -----------------------------------------------

testthat::test_that(desc = ".arg_check_call_core() writes the full saferDev::arg_check call", code = {
  st <- saferMake:::.arg_check_defaults
  txt <- saferMake:::.arg_check_call_core(data_txt = "x", st = st)
  testthat::expect_match(object = txt, regexp = "^saferDev::arg_check\\(data = x,")
  testthat::expect_match(object = txt, regexp = "class = NULL,")          # "NULL" string -> bare NULL
  testthat::expect_match(object = txt, regexp = "typeof = NULL,")
  testthat::expect_match(object = txt, regexp = "mode = NULL,")
  testthat::expect_match(object = txt, regexp = "length = NULL,")
  testthat::expect_match(object = txt, regexp = "prop = FALSE,")
  testthat::expect_match(object = txt, regexp = "options = NULL,")
  testthat::expect_match(object = txt, regexp = "print = FALSE,")
  testthat::expect_match(object = txt, regexp = "safer_check = FALSE,")
  testthat::expect_match(object = txt, regexp = "lib_path = lib_path,")
  testthat::expect_match(object = txt, regexp = "error_text = embed_error_text\\)")
})

testthat::test_that(desc = ".arg_check_call_core() renders numeric options as numbers", code = {
  st <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                           val = base::list(length = "2", options = "1, 2"), keep.null = FALSE)
  txt <- saferMake:::.arg_check_call_core(data_txt = "x", st = st)
  testthat::expect_match(object = txt, regexp = "length = 2L,")
  testthat::expect_match(object = txt, regexp = "options = c\\(1, 2\\),")
})

testthat::test_that(desc = ".arg_check_call_core() renders character options as strings", code = {
  st <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                           val = base::list(class = "character", options = "a, b"), keep.null = FALSE)
  txt <- saferMake:::.arg_check_call_core(data_txt = "x", st = st)
  testthat::expect_match(object = txt, regexp = 'options = c\\("a", "b"\\),')
})

testthat::test_that(desc = ".ac_test_value_code() builds a value consistent with the settings", code = {
  st <- saferMake:::.arg_check_defaults  # numeric, length 1
  testthat::expect_identical(object = saferMake:::.ac_test_value_code(st = st), expected = "1.5")
  # NOTE: the default options field is the string "NULL", which
  # .arg_check_parse_options() returns as a character option set. With
  # class = "character", the test value is therefore the string "NULL"
  # (verified identical to the pre-refactor behaviour).
  st2 <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                           val = base::list(class = "character"), keep.null = FALSE)
  testthat::expect_identical(object = saferMake:::.ac_test_value_code(st = st2), expected = '"NULL"')
  st3 <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                           val = base::list(length = "2"), keep.null = FALSE)
  testthat::expect_identical(object = saferMake:::.ac_test_value_code(st = st3),
                             expected = "c(1.5, 2.5)")
  # options override the kind, truncated to the length (1 by default)
  st4 <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                           val = base::list(options = "1, 2"), keep.null = FALSE)
  testthat::expect_identical(object = saferMake:::.ac_test_value_code(st = st4), expected = "1")
  # regression test (reviewer finding): blank options field with a character/factor
  # kind takes the default char_code maker, which MUST produce a correctly
  # quoted string ("\"a\"" for length 1), so that the emitted
  # saferDev::arg_check(data = "a", ...) line still parses
  st5 <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                           val = base::list(class = "character", options = ""), keep.null = FALSE)
  testthat::expect_identical(object = saferMake:::.ac_test_value_code(st = st5), expected = "\"a\"")
  st6 <- utils::modifyList(x = saferMake:::.arg_check_defaults,
                           val = base::list(class = "factor", options = "  "), keep.null = FALSE)
  testthat::expect_identical(object = saferMake:::.ac_test_value_code(st = st6),
                             expected = "factor(\"a\")")
  # and the arg_check line built from that value must parse
  txt6 <- saferMake:::.arg_check_call_core(data_txt = saferMake:::.ac_test_value_code(st = st6),
                                           st = st6, lib_path_txt = "NULL", error_text_txt = "\"\"")
  testthat::expect_error(object = base::parse(file = "", n = NULL, text = base::paste0("tempo <- ",
                                                                                        txt6),
                                              prompt = "?",
                                              keep.source = base::getOption(x = "keep.source", default = NULL),
                                              srcfile = NULL, encoding = "unknown"),
                         regexp = NA)
})

# --- .run_arg_check_tests() (needs saferDev; skipped otherwise) ---------------

testthat::test_that(desc = ".run_arg_check_tests() returns ok=TRUE for consistent settings", code = {
  testthat::skip_if_not_installed(pkg = "saferDev")
  res <- saferMake:::.run_arg_check_tests(
    fun_args = "x",
    arg_check_settings = base::list(arg_x = st_vec())
  )
  testthat::expect_true(object = res$arg_x$ok)
  testthat::expect_null(object = res$arg_x$message)
})

testthat::test_that(desc = ".run_arg_check_tests() reports a failure for inconsistent settings", code = {
  testthat::skip_if_not_installed(pkg = "saferDev")
  # class = "character" but the default numeric-kind test value -> must fail
  st <- utils::modifyList(x = st_vec(), val = base::list(class = "character"), keep.null = FALSE)
  res <- saferMake:::.run_arg_check_tests(
    fun_args = "x",
    arg_check_settings = base::list(arg_x = st)
  )
  testthat::expect_false(object = res$arg_x$ok)
  testthat::expect_true(object = base::nzchar(x = res$arg_x$message, keepNA = FALSE))
})
