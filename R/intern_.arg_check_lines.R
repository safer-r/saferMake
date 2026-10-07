# Generation and interactive testing of the saferDev::arg_check() lines.
# Extracted from inst/app/server.R; see header of R/intern_.app_helpers.R.

# Shared builder of the saferDev::arg_check(...) call text (no leading spaces,
# no "; base::eval(...)" suffix). Used both by the code generator and by the
# interactive test runner, so the tested line is identical to the generated one.
.arg_check_call_core <- function(data_txt, st,
                                 lib_path_txt = "lib_path",
                                 error_text_txt = "embed_error_text") {
  class_val   <- if (base::identical(x = st$class, y = "NULL", num.eq = TRUE, single.NA = TRUE,
                                     attrib.as.set = TRUE, ignore.bytecode = TRUE,
                                     ignore.environment = FALSE, ignore.srcref = TRUE,
                                     extptr.as.ref = FALSE)) "NULL"
                 else base::deparse(expr = st$class, width.cutoff = 60L, backtick = FALSE,
                                    control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                                    nlines = -1L)
  typeof_val  <- if (base::identical(x = st$typeof, y = "NULL", num.eq = TRUE, single.NA = TRUE,
                                     attrib.as.set = TRUE, ignore.bytecode = TRUE,
                                     ignore.environment = FALSE, ignore.srcref = TRUE,
                                     extptr.as.ref = FALSE)) "NULL"
                 else base::deparse(expr = st$typeof, width.cutoff = 60L, backtick = FALSE,
                                    control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                                    nlines = -1L)
  mode_val    <- if (base::identical(x = st$mode, y = "NULL", num.eq = TRUE, single.NA = TRUE,
                                     attrib.as.set = TRUE, ignore.bytecode = TRUE,
                                     ignore.environment = FALSE, ignore.srcref = TRUE,
                                     extptr.as.ref = FALSE)) "NULL"
                 else base::deparse(expr = st$mode, width.cutoff = 60L, backtick = FALSE,
                                    control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                                    nlines = -1L)
  len_val     <- .arg_check_parse_length(txt = st$length)
  length_val  <- if (base::is.null(x = len_val)) "NULL"
                 else base::paste0(len_val, "L", collapse = NULL, recycle0 = FALSE)
  opts        <- .arg_check_parse_options(txt = st$options)
  options_val <- if (base::is.null(x = opts)) "NULL"
                 else if (base::identical(x = opts, y = "NULL", num.eq = TRUE, single.NA = TRUE,
                                          attrib.as.set = TRUE, ignore.bytecode = TRUE,
                                          ignore.environment = FALSE, ignore.srcref = TRUE,
                                          extptr.as.ref = FALSE)) "NULL"
                 else base::deparse(expr = opts, width.cutoff = 60L, backtick = FALSE,
                                    control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                                    nlines = -1L)
  base::paste0(
    "saferDev::arg_check(data = ", data_txt,
    ", class = ", class_val,
    ", typeof = ", typeof_val,
    ", mode = ", mode_val,
    ", length = ", length_val,
    ", prop = ", base::as.character(x = base::isTRUE(x = st$prop)),
    ", double_as_integer_allowed = ", base::as.character(x = base::isTRUE(x = st$double_as_integer_allowed)),
    ", options = ", options_val,
    ", all_options_in_data = ", base::as.character(x = base::isTRUE(x = st$all_options_in_data)),
    ", na_contain = ", base::as.character(x = base::isTRUE(x = st$na_contain)),
    ", neg_values = ", base::as.character(x = base::isTRUE(x = st$neg_values)),
    ", inf_values = ", base::as.character(x = base::isTRUE(x = st$inf_values)),
    ", print = FALSE",
    ", data_name = NULL",
    ", data_arg = TRUE",
    ", safer_check = FALSE",
    ", lib_path = ", lib_path_txt,
    ", error_text = ", error_text_txt,
    ")",
    collapse = NULL,
    recycle0 = FALSE
  )
}

# Build the R code (as text) of a test value consistent with the settings:
# kind from class, else typeof, else mode (default "numeric");
# length from the length field (default 1); options used when provided.
.ac_test_value_code <- function(st) {
  kind <- if (! base::identical(x = st$class, y = "NULL", num.eq = TRUE, single.NA = TRUE,
                                attrib.as.set = TRUE, ignore.bytecode = TRUE,
                                ignore.environment = FALSE, ignore.srcref = TRUE,
                                extptr.as.ref = FALSE)) {
    st$class
  } else if (! base::identical(x = st$typeof, y = "NULL", num.eq = TRUE, single.NA = TRUE,
                               attrib.as.set = TRUE, ignore.bytecode = TRUE,
                               ignore.environment = FALSE, ignore.srcref = TRUE,
                               extptr.as.ref = FALSE)) {
    st$typeof
  } else if (! base::identical(x = st$mode, y = "NULL", num.eq = TRUE, single.NA = TRUE,
                               attrib.as.set = TRUE, ignore.bytecode = TRUE,
                               ignore.environment = FALSE, ignore.srcref = TRUE,
                               extptr.as.ref = FALSE)) {
    st$mode
  } else {
    "numeric"
  }
  n <- .arg_check_parse_length(txt = st$length)
  if (base::is.null(x = n) || n < 1L) n <- 1L
  opts <- .arg_check_parse_options(txt = st$options)

  vals <- if (n <= 26L) base::letters[base::seq_len(length.out = n)] else
    base::paste0("x", base::seq_len(length.out = n), collapse = NULL, recycle0 = FALSE)
  char_code <- if (n == 1L) base::paste0("\"", vals, "\"", collapse = NULL, recycle0 = FALSE) else
    base::paste0("c(", base::paste0("\"", vals, "\"", collapse = ", ", recycle0 = FALSE), ")",
                 collapse = NULL, recycle0 = FALSE)
  num_code <- if (n == 1L) "1.5" else
    base::paste0(
      "c(",
      base::paste(base::format(x = base::seq(from = 1.5, to = , by = 1, length.out = n, along.with = ),
                               trim = TRUE),
                  sep = " ", collapse = ", ", recycle0 = FALSE),
      ")",
      collapse = NULL, recycle0 = FALSE
    )
  int_code <- if (n == 1L) "1L" else
    base::paste0("c(", base::paste0(base::seq_len(length.out = n), "L",
                                    collapse = ", ", recycle0 = FALSE), ")",
                 collapse = NULL, recycle0 = FALSE)
  log_code <- if (n == 1L) "TRUE" else
    base::paste0("c(",
                  base::paste(base::rep(x = base::c("TRUE", "FALSE"), length.out = n),
                              sep = " ", collapse = ", ", recycle0 = FALSE),
                  ")",
                  collapse = NULL, recycle0 = FALSE)

  # If options were provided, try to build the value from them (coerced to kind)
  if (! base::is.null(x = opts)) {
    num_opts <- base::suppressWarnings(expr = base::as.numeric(x = opts), classes = "warning")
    coerced <- base::switch(EXPR = kind,
      "character" = base::as.character(x = opts),
      "numeric"   = if (! base::anyNA(x = num_opts, recursive = FALSE)) num_opts,
      "double"    = if (! base::anyNA(x = num_opts, recursive = FALSE)) num_opts,
      "integer"   = if (! base::anyNA(x = num_opts, recursive = FALSE)) base::as.integer(x = num_opts),
      "logical"   = base::suppressWarnings(expr = base::as.logical(x = opts), classes = "warning"),
      "factor"    = base::factor(x = base::as.character(x = opts), levels = , labels = ,
                                 exclude = NA, ordered = FALSE, nmax = NA),
      NULL
    )
    if (! base::is.null(x = coerced) && ! base::anyNA(x = coerced, recursive = FALSE)) {
      if (base::length(x = coerced) < n) coerced <- base::rep(x = coerced, length.out = n)
      base::return(
        base::deparse(expr = coerced[base::seq_len(length.out = n)],
                      width.cutoff = 500L, backtick = FALSE,
                      control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                      nlines = -1L)
      )
    }
    # not coercible -> default maker below (arg_check() will report the mismatch)
  }

  base::switch(EXPR = kind,
    "character"   = char_code,
    "numeric"     = num_code,
    "double"      = num_code,
    "integer"     = int_code,
    "logical"     = log_code,
    "factor"      = base::paste0("factor(", char_code, ")", collapse = NULL, recycle0 = FALSE),
    "list"        = base::paste0("list(",
                                  base::paste(base::rep(x = "1.5", times = n),
                                              sep = " ", collapse = ", ", recycle0 = FALSE),
                                  ")",
                                  collapse = NULL, recycle0 = FALSE),
    "matrix"      = base::paste0("matrix(1:", n, ", nrow = ", n, ")",
                                 collapse = NULL, recycle0 = FALSE),
    "data.frame"  = base::paste0("as.data.frame(matrix(1:", n, ", nrow = 1))",
                                 collapse = NULL, recycle0 = FALSE),
    "array"       = base::paste0("array(1:", n, ", dim = c(", n, ", 1))",
                                 collapse = NULL, recycle0 = FALSE),
    "table"       = base::paste0("table(factor(1:", n, "))",
                                 collapse = NULL, recycle0 = FALSE),
    "function"    = "function(x) x",
    "closure"     = "function(x) x",
    "builtin"     = "base::sum",
    "special"     = "base::`if`",
    "environment" = "new.env()",
    "expression"  = base::paste0("expression(",
                                  base::paste(base::rep(x = "1", times = n),
                                              sep = " ", collapse = ", ", recycle0 = FALSE),
                                  ")",
                                  collapse = NULL, recycle0 = FALSE),
    "call"        = "quote(f(1))",
    "name"        = "as.name(\"x\")",
    "symbol"      = "as.name(\"x\")",
    "pairlist"    = base::paste0("pairlist(",
                                  base::paste(base::rep(x = "1", times = n),
                                              sep = " ", collapse = ", ", recycle0 = FALSE),
                                  ")",
                                  collapse = NULL, recycle0 = FALSE),
    "complex"     = if (n == 1L) "1 + 0i" else base::paste0("(1:", n, ") + 0i",
                                                            collapse = NULL, recycle0 = FALSE),
    "raw"         = base::paste0("as.raw(",
                                  base::paste(base::seq_len(length.out = n),
                                              sep = " ", collapse = ":", recycle0 = FALSE),
                                  ")",
                                  collapse = NULL, recycle0 = FALSE),
    "Date"        = base::paste0("seq(from = as.Date(\"2000-01-01\"), length.out = ", n, ")",
                                 collapse = NULL, recycle0 = FALSE),
    "POSIXct"     = base::paste0("seq(from = as.POSIXct(\"2000-01-01\"), length.out = ", n, ")",
                                 collapse = NULL, recycle0 = FALSE),
    "POSIXlt"     = base::paste0("seq(from = as.POSIXlt(\"2000-01-01\"), length.out = ", n, ")",
                                 collapse = NULL, recycle0 = FALSE),
    num_code      # fallback for unknown/unsupported kinds (e.g. typos, S4)
  )
}

# Evaluate each generated arg_check() line with the test value (instead of the
# real argument) and collect ok / message / tested value, keyed by .arg_id(nm).
.run_arg_check_tests <- function(fun_args, arg_check_settings) {
  out <- base::list()
  if (base::length(x = fun_args) == 0L) base::return(out)
  safer_ok <- base::requireNamespace(package = "saferDev", quietly = TRUE)
  for (nm in fun_args) {
    aid <- .arg_id(nm = nm)
    st  <- arg_check_settings[[aid]]
    if (base::is.null(x = st)) st <- .arg_check_defaults
    val_code <- .ac_test_value_code(st = st)
    line_txt <- base::paste0(
      "tempo <- ",
      .arg_check_call_core(data_txt = val_code, st = st,
                           lib_path_txt = "NULL", error_text_txt = "\"\""),
      " ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))",
      collapse = NULL,
      recycle0 = FALSE
    )
    res <- if (! safer_ok) {
      base::list(ok = FALSE,
                 message = "Package saferDev is not installed: the arg_check() line could not be tested in R.",
                 value_code = val_code)
    } else {
      base::tryCatch(
        expr = {
          env <- base::new.env(hash = TRUE, parent = base::globalenv(), size = 29L)
          # same collection machinery as in the generated code header
          env$argum_check       <- NULL
          env$text_check        <- NULL
          env$checked_arg_names <- NULL
          env$ee <- base::expression(
            argum_check <- base::c(argum_check, tempo$problem),
            text_check <- base::c(text_check, tempo$text),
            checked_arg_names <- base::c(checked_arg_names, tempo$object.name)
          )
          base::eval(
            expr = base::parse(file = "", n = NULL, text = line_txt, prompt = "?",
                               keep.source = base::getOption(x = "keep.source", default = NULL),
                               srcfile = NULL, encoding = "unknown"),
            envir = env,
            enclos = base::environment(fun = NULL)
          )
          ac    <- env$argum_check
          tempo <- env$tempo
          if (base::is.null(x = tempo) ||
              ! base::is.list(x = tempo) ||
              base::is.null(x = tempo$problem)) {
            base::list(ok = FALSE,
                       message = "saferDev::arg_check() did not return the expected $problem element: test inconclusive.",
                       value_code = val_code)
          } else if (! base::is.null(x = ac) && base::any(ac, na.rm = TRUE)) {
            msg <- if (! base::is.null(x = env$text_check)) {
              base::paste(env$text_check[ac], sep = " ", collapse = "\n\n", recycle0 = FALSE)
            } else {
              "(no message returned by arg_check())"
            }
            base::list(ok = FALSE, message = msg, value_code = val_code)
          } else {
            base::list(ok = TRUE, message = NULL, value_code = val_code)
          }
        },
        error = function(e) {
          base::list(ok = FALSE,
                     message = base::conditionMessage(c = e),
                     value_code = val_code)
        },
        finally = 
      )
    }
    out[[aid]] <- res
  }
  out
}

# Builds the whole "#### argument secondary checking" section.
# Each argument produces EXACTLY one line:
#     tempo <- saferDev::arg_check(data = <arg>, ...) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
# extra_section: optional text block (character string) inserted after
#     "######## end argument checking with arg_check()" and before
#     "#### end argument secondary checking" (used for the "" management
#     section, which is emitted only when at least one box is ticked).
.build_arg_check_section <- function(fun_args, arg_check_settings,
                                     extra_section = NULL) {
  calls <- base::vapply(
    X = fun_args,
    FUN = function(nm) {
      st <- arg_check_settings[[.arg_id(nm = nm)]]
      if (base::is.null(x = st)) st <- .arg_check_defaults
      base::paste0(
        "    tempo <- ",
        .arg_check_call_core(data_txt = nm, st = st,
                             lib_path_txt = "lib_path", error_text_txt = "embed_error_text"),
        " ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))\n",
        collapse = NULL,
        recycle0 = FALSE
      )
    },
    FUN.VALUE = base::character(length = 1L),
    USE.NAMES = FALSE
  )
  header <- base::paste0(
    "    #### argument secondary checking\n",
    "\n",
    "    ######## argument checking with arg_check()\n",
    "    argum_check <- NULL\n",
    "    text_check <- NULL\n",
    "    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools\n",
    "    arg_check_error_text <- base::paste0(\"ERROR \", embed_error_text, \"\\n\\n\", collapse = NULL, recycle0 = FALSE) # must be used instead of error_text = embed_error_text when several arg_check are performed on the same argument (tempo1, tempo2, see below)\n",
    "    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))\n",
    "\n",
    collapse = NULL,
    recycle0 = FALSE
  )
  footer <- base::paste0(
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
    if (base::is.null(x = extra_section)) character(0) else extra_section,
    "    #### end argument secondary checking\n",
    "\n",
    collapse = NULL,
    recycle0 = FALSE
  )
  base::paste0(
    header,
    base::paste(calls, sep = " ", collapse = "\n", recycle0 = FALSE),
    footer,
    collapse = NULL,
    recycle0 = FALSE
  )
}
