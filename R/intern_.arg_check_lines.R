# Generation and interactive testing of the saferDev::arg_check() lines.
# Extracted from inst/app/server.R; see header of R/intern_.app_helpers.R.

# Shared builder of the saferDev::arg_check(...) call text (no leading spaces,
# no "; base::eval(...)" suffix). Used both by the code generator and by the
# interactive test runner, so the tested line is identical to the generated one.
.arg_check_call_core <- function(data_txt, st,
                                lib_path_txt = "lib_path",
                                error_text_txt = "embed_error_text") {
  class_val   <- if (identical(st$class, "NULL"))  "NULL" else deparse(st$class)
  typeof_val  <- if (identical(st$typeof, "NULL")) "NULL" else deparse(st$typeof)
  mode_val    <- if (identical(st$mode, "NULL"))   "NULL" else deparse(st$mode)
  len_val     <- .arg_check_parse_length(st$length)
  length_val  <- if (is.null(len_val)) "NULL" else paste0(len_val, "L")
  opts        <- .arg_check_parse_options(st$options)
  options_val <- if (is.null(opts)) "NULL" else
                 if (identical(opts, "NULL")) "NULL" else deparse(opts)
  paste0(
    "saferDev::arg_check(data = ", data_txt,
    ", class = ", class_val,
    ", typeof = ", typeof_val,
    ", mode = ", mode_val,
    ", length = ", length_val,
    ", prop = ", as.character(isTRUE(st$prop)),
    ", double_as_integer_allowed = ", as.character(isTRUE(st$double_as_integer_allowed)),
    ", options = ", options_val,
    ", all_options_in_data = ", as.character(isTRUE(st$all_options_in_data)),
    ", na_contain = ", as.character(isTRUE(st$na_contain)),
    ", neg_values = ", as.character(isTRUE(st$neg_values)),
    ", inf_values = ", as.character(isTRUE(st$inf_values)),
    ", print = FALSE",
    ", data_name = NULL",
    ", data_arg = TRUE",
    ", safer_check = FALSE",
    ", lib_path = ", lib_path_txt,
    ", error_text = ", error_text_txt,
    ")"
  )
}

# Build the R code (as text) of a test value consistent with the settings:
# kind from class, else typeof, else mode (default "numeric");
# length from the length field (default 1); options used when provided.
.ac_test_value_code <- function(st) {
  kind <- if (!identical(st$class, "NULL")) {
    st$class
  } else if (!identical(st$typeof, "NULL")) {
    st$typeof
  } else if (!identical(st$mode, "NULL")) {
    st$mode
  } else {
    "numeric"
  }
  n <- .arg_check_parse_length(st$length)
  if (is.null(n) || n < 1L) n <- 1L
  opts <- .arg_check_parse_options(st$options)

  vals <- if (n <= 26L) letters[seq_len(n)] else paste0("x", seq_len(n))
  char_code <- if (n == 1L) paste0("\"", vals, "\"") else
    paste0("c(", paste0("\"", vals, "\"", collapse = ", "), ")")
  num_code <- if (n == 1L) "1.5" else
    paste0("c(", paste(format(seq(1.5, by = 1, length.out = n), trim = TRUE), collapse = ", "), ")")
  int_code <- if (n == 1L) "1L" else
    paste0("c(", paste0(seq_len(n), "L", collapse = ", "), ")")
  log_code <- if (n == 1L) "TRUE" else
    paste0("c(", paste(rep(c("TRUE", "FALSE"), length.out = n), collapse = ", "), ")")

  # If options were provided, try to build the value from them (coerced to kind)
  if (!is.null(opts)) {
    num_opts <- suppressWarnings(as.numeric(opts))
    coerced <- switch(kind,
      "character" = as.character(opts),
      "numeric"   = if (!anyNA(num_opts)) num_opts,
      "double"    = if (!anyNA(num_opts)) num_opts,
      "integer"   = if (!anyNA(num_opts)) as.integer(num_opts),
      "logical"   = suppressWarnings(as.logical(opts)),
      "factor"    = factor(as.character(opts)),
      NULL
    )
    if (!is.null(coerced) && !anyNA(coerced)) {
      if (length(coerced) < n) coerced <- rep(coerced, length.out = n)
      return(deparse(coerced[seq_len(n)], width.cutoff = 500L))
    }
    # not coercible -> default maker below (arg_check() will report the mismatch)
  }

  switch(kind,
    "character"   = char_code,
    "numeric"     = num_code,
    "double"      = num_code,
    "integer"     = int_code,
    "logical"     = log_code,
    "factor"      = paste0("factor(", char_code, ")"),
    "list"        = paste0("list(", paste(rep("1.5", n), collapse = ", "), ")"),
    "matrix"      = paste0("matrix(1:", n, ", nrow = ", n, ")"),
    "data.frame"  = paste0("as.data.frame(matrix(1:", n, ", nrow = 1))"),
    "array"       = paste0("array(1:", n, ", dim = c(", n, ", 1))"),
    "table"       = paste0("table(factor(1:", n, "))"),
    "function"    = "function(x) x",
    "closure"     = "function(x) x",
    "builtin"     = "base::sum",
    "special"     = "base::`if`",
    "environment" = "new.env()",
    "expression"  = paste0("expression(", paste(rep("1", n), collapse = ", "), ")"),
    "call"        = "quote(f(1))",
    "name"        = "as.name(\"x\")",
    "symbol"      = "as.name(\"x\")",
    "pairlist"    = paste0("pairlist(", paste(rep("1", n), collapse = ", "), ")"),
    "complex"     = if (n == 1L) "1 + 0i" else paste0("(1:", n, ") + 0i"),
    "raw"         = paste0("as.raw(", paste(seq_len(n), collapse = ":"), ")"),
    "Date"        = paste0("seq(from = as.Date(\"2000-01-01\"), length.out = ", n, ")"),
    "POSIXct"     = paste0("seq(from = as.POSIXct(\"2000-01-01\"), length.out = ", n, ")"),
    "POSIXlt"     = paste0("seq(from = as.POSIXlt(\"2000-01-01\"), length.out = ", n, ")"),
    num_code      # fallback for unknown/unsupported kinds (e.g. typos, S4)
  )
}

# Evaluate each generated arg_check() line with the test value (instead of the
# real argument) and collect ok / message / tested value, keyed by .arg_id(nm).
.run_arg_check_tests <- function(fun_args, arg_check_settings) {
  out <- list()
  if (length(fun_args) == 0L) return(out)
  safer_ok <- requireNamespace("saferDev", quietly = TRUE)
  for (nm in fun_args) {
    aid <- .arg_id(nm)
    st  <- arg_check_settings[[aid]]
    if (is.null(st)) st <- .arg_check_defaults
    val_code <- .ac_test_value_code(st)
    line_txt <- paste0(
      "tempo <- ",
      .arg_check_call_core(data_txt = val_code, st = st,
                          lib_path_txt = "NULL", error_text_txt = "\"\""),
      " ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))"
    )
    res <- if (!safer_ok) {
      list(ok = FALSE,
           message = "Package saferDev is not installed: the arg_check() line could not be tested in R.",
           value_code = val_code)
    } else {
      tryCatch({
        env <- new.env(parent = globalenv())
        # same collection machinery as in the generated code header
        env$argum_check       <- NULL
        env$text_check        <- NULL
        env$checked_arg_names <- NULL
        env$ee <- base::expression(
          argum_check <- base::c(argum_check, tempo$problem),
          text_check <- base::c(text_check, tempo$text),
          checked_arg_names <- base::c(checked_arg_names, tempo$object.name)
        )
        eval(parse(text = line_txt), envir = env)
        ac    <- env$argum_check
        tempo <- env$tempo
        if (is.null(tempo) || !is.list(tempo) || is.null(tempo$problem)) {
          list(ok = FALSE,
               message = "saferDev::arg_check() did not return the expected $problem element: test inconclusive.",
               value_code = val_code)
        } else if (!is.null(ac) && any(ac, na.rm = TRUE)) {
          msg <- if (!is.null(env$text_check)) {
            paste(env$text_check[ac], collapse = "\n\n")
          } else "(no message returned by arg_check())"
          list(ok = FALSE, message = msg, value_code = val_code)
        } else {
          list(ok = TRUE, message = NULL, value_code = val_code)
        }
      }, error = function(e) {
        list(ok = FALSE, message = conditionMessage(e), value_code = val_code)
      })
    }
    out[[aid]] <- res
  }
  out
}

# Builds the whole "#### argument secondary checking" section.
# Each argument produces EXACTLY one line:
#     tempo <- saferDev::arg_check(data = <arg>, ...) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
.build_arg_check_section <- function(fun_args, arg_check_settings) {
  calls <- vapply(fun_args, function(nm) {
    st <- arg_check_settings[[.arg_id(nm)]]
    if (is.null(st)) st <- .arg_check_defaults
    paste0(
      "    tempo <- ", .arg_check_call_core(nm, st),
      " ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))\n"
    )
  }, character(1L), USE.NAMES = FALSE)
  header <- paste0(
    "    #### argument secondary checking\n",
    "\n",
    "    ######## argument checking with arg_check()\n",
    "    argum_check <- NULL\n",
    "    text_check <- NULL\n",
    "    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools\n",
    "    arg_check_error_text <- base::paste0(\"ERROR \", embed_error_text, \"\\n\\n\", collapse = NULL, recycle0 = FALSE) # must be used instead of error_text = embed_error_text when several arg_check are performed on the same argument (tempo1, tempo2, see below)\n",
    "    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))\n",
    "\n"
  )
  footer <- paste0(
    "    # lib_path already checked above\n",
    "    # safer_check already checked above\n",
    "    # error_text converted to single string above\n",
    "    if( ! base::is.null(x = argum_check)){\n",
    "        if(base::any(argum_check, na.rm = TRUE)){\n",
    "            base::stop(base::paste0(\"\\n\\n================\\n\\n\", base::paste0(text_check[argum_check], collapse = \"\\n\\n\", recycle0 = FALSE), \"\\n\\n================\\n\\n\", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)\n",
    "        }\n",
    "    }\n",
    "    # check with r_debugging_tools\n",
    "    # source(\"https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R\") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()\n",
    "    # end check with r_debugging_tools\n",
    "    ######## end argument checking with arg_check()\n",
    "\n",
    "    #### end argument secondary checking\n",
    "\n"
  )
  paste0(header, paste(calls, collapse = "\n"), footer)
}
