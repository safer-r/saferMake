# Internal helpers of the saferMake Shiny app (inst/app/).
# Pure functions with no dependency on shiny inputs/reactives, extracted
# from inst/app/server.R so they can be unit tested in tests/testthat/.
# Not exported: the app accesses them through saferMake::: .
# All internal function names start with a dot; files start with intern_.

# The three additional safer-r arguments
.safer_args <- base::c("lib_path", "safer_check", "error_text")

.arg_check_defaults <- base::list(
  class                     = "NULL",  # blank field -> "NULL" = no constraint
  typeof                    = "NULL",  # blank field -> "NULL" = no constraint
  mode                      = "NULL",  # blank field -> "NULL" = no constraint
  length                    = "NULL",  # blank field -> no constraint
  prop                      = FALSE,
  double_as_integer_allowed = FALSE,
  options                   = "NULL", # blank field -> no constraint
  all_options_in_data       = FALSE,
  na_contain                = TRUE,
  neg_values                = TRUE,
  inf_values                = TRUE,
  no_empty_string           = FALSE  # ticked -> argument cannot contain "" (empty-string section)
)

# Build a valid HTML id from an argument name
.arg_id <- function(nm) {
  base::paste0(
    "arg_",
    base::gsub(pattern = "[^[:alnum:]_]", replacement = "_", x = nm,
               ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE),
    collapse = NULL,
    recycle0 = FALSE
  )
}
# Checkbox id for a given argument name
.null_cb_id <- function(nm) {
  base::paste0("null_", .arg_id(nm = nm), collapse = NULL, recycle0 = FALSE)
}
.empty_cb_id <- function(nm) {
  base::paste0("empty_", .arg_id(nm = nm), collapse = NULL, recycle0 = FALSE)
}
# Checkbox id of the "cannot contain \"\"" setting: the checkbox is rendered
# with inputId = .arg_check_field_ids(...)$no_empty_string ("ac_noempty_<id>"),
# so read the state through that same id everywhere.
.no_empty_cb_id <- function(nm) {
  .arg_check_field_ids(id = .arg_id(nm = nm))$no_empty_string
}

.arg_check_field_ids <- function(id) {
  base::list(
    class                     = base::paste0("ac_class_",    id, collapse = NULL, recycle0 = FALSE),
    typeof                    = base::paste0("ac_typeof_",   id, collapse = NULL, recycle0 = FALSE),
    mode                      = base::paste0("ac_mode_",     id, collapse = NULL, recycle0 = FALSE),
    length                    = base::paste0("ac_length_",   id, collapse = NULL, recycle0 = FALSE),
    prop                      = base::paste0("ac_prop_",     id, collapse = NULL, recycle0 = FALSE),
    double_as_integer_allowed = base::paste0("ac_dbl_int_",  id, collapse = NULL, recycle0 = FALSE),
    options                   = base::paste0("ac_options_",   id, collapse = NULL, recycle0 = FALSE),
    all_options_in_data       = base::paste0("ac_all_opt_",  id, collapse = NULL, recycle0 = FALSE),
    na_contain                = base::paste0("ac_na_",       id, collapse = NULL, recycle0 = FALSE),
    neg_values                = base::paste0("ac_neg_",      id, collapse = NULL, recycle0 = FALSE),
    inf_values                = base::paste0("ac_inf_",      id, collapse = NULL, recycle0 = FALSE),
    no_empty_string           = base::paste0("ac_noempty_",  id, collapse = NULL, recycle0 = FALSE)
  )
}

# TRUE if the field is NULL (not yet rendered) or blank/whitespace only
.field_blank <- function(v) {
  base::is.null(x = v) ||
    ! base::nzchar(x = base::trimws(x = v, which = "both", whitespace = "[ \t\r\n]"),
                   keepNA = FALSE)
}

# Consistency errors of the arg_check() settings, detected BEFORE the
# saferDev::arg_check() lines are tested (rendered in red in the argument
# sections of the settings page, exactly like the test result messages):
# - the "If values are strings, they cannot contain empty strings \"\"" box is
#   ticked but no character value has been used in any of the Class, typeof or
#   mode fields;
# - the "Values can be negative if they are numeric" box is unticked but no
#   numeric/double value has been used in any of the Class, typeof or mode
#   fields (same for the "Values can be Inf or -Inf if they are numeric" box);
# - blank Class/typeof/mode fields are stored as "NULL" (no constraint), so
#   they never count as a used value.
# Returns a named list (key = .arg_id(nm)) of character vectors: zero-length
# when the settings of the argument are consistent, otherwise the error
# messages to display.
.arg_check_settings_errors <- function(fun_args, arg_check_settings) {
  msg_char <- base::paste0(
    "The \"If values are strings, they cannot contain empty strings \\\"\\\"\" box is ticked ",
    "but character value has not been used in any of the Class, typeof or mode field.",
    collapse = NULL,
    recycle0 = FALSE
  )
  msg_neg <- base::paste0(
    "The \"Values can be negative if they are numeric\" box is unticked ",
    "but class \"numeric\", mode \"numeric\", typeof \"double\" has not been used ",
    "in any of the Class, typeof or mode field.",
    collapse = NULL,
    recycle0 = FALSE
  )
  msg_inf <- base::paste0(
    "The \"Values can be Inf or -Inf if they are numeric\" box is unticked ",
    "but class \"numeric\", mode \"numeric\", typeof \"double\" has not been used ",
    "in any of the Class, typeof or mode field.",
    collapse = NULL,
    recycle0 = FALSE
  )
  out <- base::list()
  for (nm in fun_args) {
    aid <- .arg_id(nm = nm)
    st  <- arg_check_settings[[aid]]
    if (base::is.null(x = st)) st <- .arg_check_defaults
    kinds <- base::unlist(
      x = base::list(st$class, st$typeof, st$mode),
      recursive = TRUE,
      use.names = FALSE
    )
    kinds <- kinds[! base::is.na(x = kinds)]
    char_used <- base::any(x = kinds == "character", na.rm = TRUE)
    num_used  <- base::any(x = kinds == "numeric" | kinds == "double", na.rm = TRUE)
    errs <- base::character(length = 0L)
    if (base::isTRUE(x = st$no_empty_string) && ! char_used) {
      errs <- base::c(errs, msg_char)
    }
    if (! base::isTRUE(x = st$neg_values) && ! num_used) {
      errs <- base::c(errs, msg_neg)
    }
    if (! base::isTRUE(x = st$inf_values) && ! num_used) {
      errs <- base::c(errs, msg_inf)
    }
    out[[aid]] <- errs
  }
  out
}

# "a, b 2 c" -> c("a", "b", "2", "c"); numeric if ALL parts parse as numbers
.arg_check_parse_options <- function(txt) {
  parts <- base::trimws(
    x = base::unlist(
      x = base::strsplit(x = txt, split = "[,;[:space:]]+",
                         fixed = FALSE, perl = FALSE, useBytes = FALSE),
      recursive = TRUE,
      use.names = TRUE
    ),
    which = "both",
    whitespace = "[ \t\r\n]"
  )
  parts <- parts[base::nzchar(x = parts)]
  if (base::length(x = parts) == 0L) base::return(NULL)
  num <- base::suppressWarnings(expr = base::as.numeric(x = parts), classes = "warning")
  if (! base::anyNA(x = num, recursive = FALSE)) num else parts
}

# "3" -> 3L ; anything else (blank, "2.5", "abc") -> NULL
.arg_check_parse_length <- function(txt) {
  t <- base::trimws(x = txt, which = "both", whitespace = "[ \t\r\n]")
  if (base::grepl(pattern = "^[0-9]+$", x = t,
                  ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) {
    base::as.integer(x = t)
  } else {
    NULL
  }
}
