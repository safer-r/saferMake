# Internal helpers of the saferMake Shiny app (inst/app/).
# Pure functions with no dependency on shiny inputs/reactives, extracted
# from inst/app/server.R so they can be unit tested in tests/testthat/.
# Not exported: the app accesses them through saferMake::: .

# The three additional safer-r arguments
SAFER_ARGS <- c("lib_path", "safer_check", "error_text")

ARG_CHECK_DEFAULTS <- list(
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
  inf_values                = TRUE
)

# Build a valid HTML id from an argument name
arg_id <- function(nm) paste0("arg_", gsub("[^[:alnum:]_]", "_", nm))
# Checkbox id for a given argument name
null_cb_id <- function(nm) paste0("null_", arg_id(nm))
empty_cb_id <- function(nm) paste0("empty_", arg_id(nm))

arg_check_field_ids <- function(id) {
  list(
    class                     = paste0("ac_class_",   id),
    typeof                    = paste0("ac_typeof_",  id),
    mode                      = paste0("ac_mode_",    id),
    length                    = paste0("ac_length_",  id),
    prop                      = paste0("ac_prop_",    id),
    double_as_integer_allowed = paste0("ac_dbl_int_", id),
    options                   = paste0("ac_options_", id),
    all_options_in_data       = paste0("ac_all_opt_", id),
    na_contain                = paste0("ac_na_",      id),
    neg_values                = paste0("ac_neg_",     id),
    inf_values                = paste0("ac_inf_",     id)
  )
}

# TRUE if the field is NULL (not yet rendered) or blank/whitespace only
field_blank <- function(v) is.null(v) || !nzchar(trimws(v))

# "a, b 2 c" -> c("a", "b", "2", "c"); numeric if ALL parts parse as numbers
arg_check_parse_options <- function(txt) {
  parts <- trimws(unlist(strsplit(txt, "[,;[:space:]]+")))
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) return(NULL)
  num <- suppressWarnings(as.numeric(parts))
  if (!anyNA(num)) num else parts
}

arg_check_parse_length <- function(txt) {
  t <- trimws(txt)
  if (grepl("^[0-9]+$", t)) as.integer(t) else NULL
}
