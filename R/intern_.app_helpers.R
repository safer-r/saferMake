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
