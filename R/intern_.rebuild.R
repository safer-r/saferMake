# Verbatim capture of the pasted function and rebuild of the safer-r version.
# Extracted from inst/app/server.R; see header of R/intern_.app_helpers.R.

# names of the arguments of f that have NO default value.
# In formals(), a default-less argument holds the 'missing' object,
# which is identical to quote(expr = ). An explicit default of NULL
# (e.g. function(x = NULL)) is NOT 'no default': it has a default (NULL).
.args_without_default <- function(f) {
  fm <- base::formals(fun = f, envir = base::parent.frame(n = 1))
  if (base::is.null(x = fm)) base::return(base::character(length = 0L))
  fm <- base::as.list(x = fm) # pairlist -> plain list, so vapply is safe
  if (base::length(x = fm) == 0L) base::return(base::character(length = 0L))
  no_def <- base::vapply(
    X = fm,
    FUN = function(v) base::identical(x = v, y = base::quote(expr = ),
                                      num.eq = TRUE, single.NA = TRUE,
                                      attrib.as.set = TRUE, ignore.bytecode = TRUE,
                                      ignore.environment = FALSE, ignore.srcref = TRUE,
                                      extptr.as.ref = FALSE),
    FUN.VALUE = base::logical(length = 1L),
    USE.NAMES = TRUE
  )
  base::names(x = fm)[no_def]
}

.match_close <- function(txt, open, close_ch) {
  open_ch <- base::substring(text = txt, first = open, last = open)
  chars   <- base::strsplit(x = txt, split = "", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]]
  depth   <- 0L
  for (i in open:base::nchar(x = txt, type = "chars", allowNA = FALSE, keepNA = NA)) {
    if (chars[i] == open_ch) {
      depth <- depth + 1L
    } else if (chars[i] == close_ch) {
      depth <- depth - 1L
      if (depth == 0L) base::return(i)
    }
  }
  -1L
}

.extract_aa_body <- function(code) {
  m <- base::regexpr(pattern = "function[[:space:]]*\\(", text = code,
                     ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  if (m == -1) base::return(NULL)
  p_open  <- m + base::attr(x = m, which = "match.length", exact = FALSE) - 1L
  p_close <- .match_close(txt = code, open = p_open, close_ch = ")")
  if (p_close == -1) base::return(NULL)
  aa <- base::substring(text = code, first = 1L, last = p_close - 1L)

  # NOTE: last = (empty argument, the lazy default) is required for
  # portability: substring()'s default for last is NULL only on R >= 4.6;
  # on R <= 4.5 an explicit last = NULL is an error. Do not "fix" this.
  rest <- base::substring(text = code, first = p_close + 1L, last = )
  m2   <- base::regexpr(pattern = "\\S", text = rest,
                        ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  if (m2 == -1) base::return(base::list(aa = aa, body = ""))
  p_body <- p_close + m2

  if (base::substring(text = code, first = p_body, last = p_body) == "{") {
    p_end <- .match_close(txt = code, open = p_body, close_ch = "}")
    if (p_end == -1) base::return(NULL)
    body <- base::substring(text = code, first = p_body + 1L, last = p_end - 1L)
    base::list(aa = aa, body = body)
  } else {
    base::list(aa = aa, body = base::trimws(x = rest, which = "both", whitespace = "[ \t\r\n]"))
  }
}

# null_args       : argument names that ACCEPT NULL -> commented out in tempo_arg
# non_null_args   : argument names that must NOT be NULL -> active in tempo_arg
# empty_args      : argument names that ACCEPT empty non NULL values
#                   -> commented out in the tempo_arg of the empty section
# non_empty_args  : argument names that must NOT be empty -> active there
# no_default_args : argument names with NO default value -> conditional section
# fun_args        : all argument names (to emit the arg_check() blocks)
# arg_check_settings : named list (key = .arg_id(nm)) of arg_check() settings
# no_empty_string_args : argument names checked as unable to contain "" (ticked
#                   boxes) -> the "management of \"\"" section is emitted only
#                   when this vector is not empty, and these names are ACTIVE
#                   in its tempo_arg.
.build_rebuilt <- function(aa, body, pkg, link,
                           null_args = base::character(length = 0L),
                           non_null_args = base::character(length = 0L),
                           empty_args = base::character(length = 0L),
                           non_empty_args = base::character(length = 0L),
                           no_default_args = base::character(length = 0L),
                           fun_args = base::character(length = 0L),
                           arg_check_settings = base::list(),
                           no_empty_string_args = base::character(length = 0L)) {
  aa   <- base::sub(pattern = "[[:space:]]+$", replacement = "", x = aa,
                    ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  body <- base::sub(pattern = "[[:space:]]+$", replacement = "",
                    x = base::sub(pattern = "^[[:space:]]*\n", replacement = "", x = body,
                                  ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE),
                    ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)

  comma <- if (base::grepl(pattern = "\\($", x = aa,
                           ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) ||
               base::grepl(pattern = ",$", x = aa,
                           ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) "" else ","

  pkg_line <- if (base::nzchar(x = pkg, keepNA = FALSE)) {
    base::paste0("package_name <- ",
                 base::deparse(expr = pkg, width.cutoff = 60L, backtick = FALSE,
                               control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                               nlines = -1L),
                 collapse = NULL, recycle0 = FALSE)
  } else {
    "package_name <- NULL"
  }
  link_line <- if (base::nzchar(x = link, keepNA = FALSE)) {
    base::deparse(expr = link, width.cutoff = 60L, backtick = FALSE,
                  control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                  nlines = -1L)
  } else {
    "NULL"
  }

  # ---- NULL management block ---------------------------------------------
  # FIX (bug 3): one line per argument, each comma-terminated, so commenting
  # a line out never breaks the c(...) call (a trailing comma is valid R).
  # ACTIVE line    -> argument must NOT be NULL (checked by tempo_log)
  # COMMENTED line -> argument accepts NULL (excluded from the check)
  non_null_lines <- base::vapply(
    X = non_null_args,
    FUN = function(nm) base::paste0(
      "        ",
      base::deparse(expr = nm, width.cutoff = 500L, backtick = FALSE,
                    control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                    nlines = -1L),
      ", ",
      collapse = NULL, recycle0 = FALSE
    ),
    FUN.VALUE = base::character(length = 1L),
    USE.NAMES = TRUE
  )
  null_lines <- base::vapply(
    X = null_args,
    FUN = function(nm) base::paste0(
      "        # ",
      base::deparse(expr = nm, width.cutoff = 500L, backtick = FALSE,
                    control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                    nlines = -1L),
      ", # inactivated because can be NULL",
      collapse = NULL, recycle0 = FALSE
    ),
    FUN.VALUE = base::character(length = 1L),
    USE.NAMES = TRUE
  )
  tempo_arg_block <- base::paste0(
    "    tempo_arg <- base::c(\n",
    base::paste(base::c(non_null_lines, null_lines,
                        "        \"safer_check\" ",
                        "        # \"lib_path\", # inactivated because can be NULL",
                        "        # \"error_text\" # inactivated because NULL converted to \"\" above"),
                sep = " ", collapse = "\n", recycle0 = FALSE),
    "\n    )\n",
    collapse = NULL,
    recycle0 = FALSE
  )

  # ---- "no default value" block (conditional) -----------------------------
  # Emitted ONLY if at least one argument has no default value.
  # paste0() drops zero-length arguments (including NULL), so when
  # no_default_args is empty, no_def_block is NULL and the section is absent.
  no_def_block <- if (base::length(x = no_default_args) > 0L) {
    no_def_lines <- base::vapply(
      X = no_default_args,
      FUN = function(nm) base::paste0(
        "        ",
        base::deparse(expr = nm, width.cutoff = 500L, backtick = FALSE,
                      control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                      nlines = -1L),
        collapse = NULL, recycle0 = FALSE
      ),
      FUN.VALUE = base::character(length = 1L),
      USE.NAMES = TRUE
    )
    base::paste0(
      "    ######## arg with no default values\n",
      "    # optional section: remove the code if none of your arguments has no default value\n",
      "    no_def_args <- base::c(\n",
      base::paste(no_def_lines, sep = " ", collapse = ",\n", recycle0 = FALSE), "\n    )\n",
      "    tempo <- base::eval(expr = base::parse(text = base::paste0(\"base::c(base::missing(\", base::paste0(no_def_args, collapse = \"),base::missing(\", recycle0 = FALSE), \"))\", collapse = NULL, recycle0 = FALSE), file = \"\", n = NULL, prompt = \"?\", keep.source = base::getOption(x = \"keep.source\", default = NULL), srcfile = NULL, encoding = \"unknown\"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))\n",
      "    if(base::any(tempo, na.rm = TRUE)){\n",
      "        tempo_cat <- base::paste0(\n",
      "            error_text_start, \n",
      "            \"FOLLOWING ARGUMENT\", \n",
      "            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = \"S HAVE\", no = \" HAS\"), \n",
      "            \" NO DEFAULT VALUE AND REQUIRE ONE:\\n\", \n",
      "            base::paste0(no_def_args[tempo], collapse = \"\\n\", recycle0 = FALSE), \n",
      "            collapse = NULL, \n",
      "            recycle0 = FALSE\n",
      "        )\n",
      "        base::stop(base::paste0(\"\\n\\n================\\n\\n\", tempo_cat, \"\\n\\n================\\n\\n\", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)\n",
      "    }\n",
      "    ######## end arg with no default values\n",
      "\n",
      collapse = NULL,
      recycle0 = FALSE
    )
  } else {
    NULL
  }

  # ---- tempo_arg block of the "empty non NULL" management section ----
  # Same convention as the NULL block:
  #   ACTIVE line    -> argument must NOT be an empty non NULL object (checked)
  #   COMMENTED line -> argument accepts empty non NULL objects (excluded)
  # Per your request, the empty-accepting (commented) names are placed
  # just after "tempo_arg <-base::c(", before the active names.
  non_empty_lines <- base::vapply(
    X = non_empty_args,
    FUN = function(nm) base::paste0(
      "        ",
      base::deparse(expr = nm, width.cutoff = 500L, backtick = FALSE,
                    control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                    nlines = -1L),
      ", ",
      collapse = NULL, recycle0 = FALSE
    ),
    FUN.VALUE = base::character(length = 1L),
    USE.NAMES = TRUE
  )
  can_empty_lines <- base::vapply(
    X = empty_args,
    FUN = function(nm) base::paste0(
      "        # ",
      base::deparse(expr = nm, width.cutoff = 500L, backtick = FALSE,
                    control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                    nlines = -1L),
      ", # inactivated because can be an empty non NULL object",
      collapse = NULL, recycle0 = FALSE
    ),
    FUN.VALUE = base::character(length = 1L),
    USE.NAMES = TRUE
  )
  empty_arg_block <- base::paste0(
    "    tempo_arg <-base::c(\n",
    base::paste(base::c(can_empty_lines, non_empty_lines,
                        "        \"safer_check\", ",
                        "        \"lib_path\"",
                        "        # \"error_text\" # inactivated because empty value converted to \"\" above"),
                sep = " ", collapse = "\n", recycle0 = FALSE),
    "\n    )\n",
    collapse = NULL,
    recycle0 = FALSE
  )

  # ---- tempo_arg block of the "" management section ----
  # Emitted only when at least one argument is ticked ("cannot contain \"\"").
  # Ticked names are ACTIVE (checked); the two safer-r args are pre-commented:
  #   "lib_path"   already checked above (its own section)
  #   "error_text" can legitimately be ""
  # The active names are comma-SEPARATED but the LAST one carries no trailing
  # comma (a trailing comma before ")" would be a syntax error in R).
  no_empty_lines <- base::vapply(
    X = no_empty_string_args,
    FUN = function(nm) base::paste0(
      "        ",
      base::deparse(expr = nm, width.cutoff = 500L, backtick = FALSE,
                    control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                    nlines = -1L),
      collapse = NULL, recycle0 = FALSE
    ),
    FUN.VALUE = base::character(length = 1L),
    USE.NAMES = TRUE
  )
  # section body: emitted only if at least one box ticked
  no_empty_block <- if (base::length(x = no_empty_string_args) > 0L) {
    base::paste0(
      "    ######## management of \"\" in arguments of mode character\n",
      "    # optional section: remove the code if you do not want to check if arguments of mode character of your own function cannot contain \"\"\n",
      "    tempo_arg <- base::c(\n",
      base::paste(no_empty_lines, sep = " ", collapse = ",\n", recycle0 = FALSE),
      "\n",
      "        # \"lib_path\" # inactivated because already checked above\n",
      "        # \"error_text\" # inactivated because can be \"\"\n",
      "    )\n",
      "    # nocov start\n",
      "    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.\n",
      "    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = \"any\", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == \"character\", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())  #  need to test is.null() here\n",
      "    if(base::any(tempo_log, na.rm = TRUE)){\n",
      "        # This check is here in case the developer has not correctly fill tempo_arg\n",
      "        tempo_cat <- base::paste0(\n",
      "            \"INTERNAL ERROR IN THE BACKBONE PART OF \", \n",
      "            intern_error_text_start, \n",
      "            \"IN THE SECTION \\\"management of \\\"\\\" in arguments of mode character\\\"\\n\", \n",
      "            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = \"THESE ARGUMENTS ARE\", no = \"THIS ARGUMENT IS\"), \n",
      "            \" NOT CLASS \\\"character\\\":\\n\", \n",
      "            base::paste0(tempo_arg[tempo_log], collapse = \"\\n\", recycle0 = FALSE), \n",
      "            \"\\nIf saferMake::saferMake() HAS BEEN USED TO MODIFY YOUR FUNCTION, PLEASE RESTART WITH YOUR MAIN CODE AND SELECT PROPER CLASS, TYPE AND/OR MODE FOR YOUR ARGUMENTS\\n.HERE, ARGUMENTS ARE TYPE:\\n\", \n",
      "            base::paste0(base::sapply(X = tempo_arg, FUN = function(x){base::paste0(x, ': \"', base::typeof(x = base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = \"any\", inherits = FALSE)), '\"', collapse = NULL, recycle0 = FALSE)}), collapse = \"\\n\", recycle0 = FALSE), \n",
      "            intern_error_text_end, \n",
      "            collapse = NULL, \n",
      "            recycle0 = FALSE\n",
      "        )\n",
      "        base::stop(base::paste0(\"\\n\\n================\\n\\n\", tempo_cat, \"\\n\\n================\\n\\n\", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)\n",
      "        # nocov end\n",
      "    }else{\n",
      "        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = \"any\", inherits = FALSE)}), FUN = function(x){base::any(x == \"\", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply()).  # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()\n",
      "        if(base::any(tempo_log, na.rm = TRUE)){\n",
      "            tempo_cat <- base::paste0(\n",
      "                error_text_start, \n",
      "                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = \"THESE ARGUMENTS\\n\", no = \"THIS ARGUMENT\\n\"), \n",
      "                base::paste0(tempo_arg[tempo_log], collapse = \"\\n\", recycle0 = FALSE),\n",
      "                \"\\nCANNOT CONTAIN EMPTY STRING \\\"\\\".\", \n",
      "                collapse = NULL, \n",
      "                recycle0 = FALSE\n",
      "            )\n",
      "            base::stop(base::paste0(\"\\n\\n================\\n\\n\", tempo_cat, \"\\n\\n================\\n\\n\", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) \n",
      "        }\n",
      "    }\n",
      "    ######## end management of \"\" in arguments of mode character\n",
      "\n",
      collapse = NULL,
      recycle0 = FALSE
    )
  } else {
    NULL
  }

  base::paste0(
    aa, comma,
    "\n    lib_path = NULL, \n    safer_check = TRUE, \n    error_text = \"\" \n){\n",
    "\n    #### package name\n    ", pkg_line, "\n    #### end package name\n",
    "\n    #### internal error report link\n",
    "    internal_error_report_link <- ", link_line,
    " # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message\n",
    "    #### end internal error report link\n",
    "\n",

    "    #### function name\n",
    "    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions. In addition, arguments values retrieved are not evaluated base::match.call, but this is solved with get() below\n",
    "    function_name <- base::paste0(tempo_settings[[1]], \"()\", collapse = NULL, recycle0 = FALSE) \n",
    "    # function name with \"()\" paste, which split into a vector of three: c(\"::()\", \"package ()\", \"function ()\") if \"package::function()\" is used.\n",
    "    if(function_name[1] == \"::()\" | function_name[1] == \":::()\"){\n",
    "        function_name <- function_name[3]\n",
    "    }\n",
    "    #### end function name\n",
    "\n",

    "    #### arguments settings\n",
    "    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user). Always a list, even if 1 argument. So ok for lapply() usage (management of NA section)\n",
    "    arg_user_setting_names <- base::names(x = arg_user_setting)\n",
    "    # evaluation of values if they are expression, call, etc.\n",
    "    if(base::length(x = arg_user_setting) != 0){\n",
    "        arg_user_setting_eval <- base::lapply(\n",
    "            X = arg_user_setting_names, \n",
    "            FUN = function(x){\n",
    "                base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = \"any\", inherits = TRUE) # n = 2 because of lapply(), inherit = TRUE to be sure to correctly evaluate\n",
    "            }\n",
    "        )\n",
    "        base::names(x = arg_user_setting_eval) <- arg_user_setting_names\n",
    "    }else{\n",
    "        arg_user_setting_eval <- NULL\n",
    "    }\n",
    "    # end evaluation of values if they are expression, call, etc.\n",
    "    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 2)), envir = base::parent.frame(n = 1))) # names of all the arguments\n",
    "    #### end arguments settings\n",
    "\n",

    "    #### error_text initiation\n",
    "\n",
    "    ######## basic error text start\n",
    "    error_text <- base::paste0(base::unlist(x = error_text, recursive = TRUE, use.names = TRUE), collapse = \"\", recycle0 = FALSE) # convert everything to string. if error_text is a string, changes nothing. If NULL or empty (even list) -> \"\" so no need to check for management of NULL or empty value\n",
    "    package_function_name <- base::paste0(\n",
    "        base::ifelse(test = base::is.null(x = package_name), yes = \"\", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = \"^\\\\.\", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = \":::\", no = \"::\"), collapse = NULL, recycle0 = FALSE)), \n",
    "        function_name,\n",
    "        collapse = NULL, \n",
    "        recycle0 = FALSE\n",
    "    )\n",
    "    error_text_start <- base::paste0(\n",
    "        \"ERROR IN \", # must not be changed, because this \"ERROR IN \" string is used for text replacement\n",
    "        package_function_name, \n",
    "        base::ifelse(test = error_text == \"\", yes = \".\", no = error_text), \n",
    "        \"\\n\\n\", \n",
    "        collapse = NULL, \n",
    "        recycle0 = FALSE\n",
    "    )\n",
    "    ######## end basic error text start\n",
    "\n",
    "    ######## internal error text\n",
    "    intern_error_text_start <- base::paste0(\n",
    "        package_function_name, \n",
    "        base::ifelse(test = error_text == \"\", yes = \".\", no = error_text), \n",
    "        \"\\n\\n\", \n",
    "        collapse = NULL, \n",
    "        recycle0 = FALSE\n",
    "    )\n",
    "    intern_error_text_end <- base::ifelse(test = base::is.null(x = internal_error_report_link), yes = \"\", no = base::paste0(\"\\n\\nPLEASE, REPORT THIS ERROR HERE: \", internal_error_report_link, \".\", collapse = NULL, recycle0 = FALSE))\n",
    "    ######## end internal error text\n",
    "\n",
    "    ######## error text when embedding\n",
    "    # use this in the error_text of safer functions if present in your main code \n",
    "    embed_error_text  <- base::sub(pattern = \"^ERROR IN \", replacement = \" INSIDE \", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)\n",
    "    embed_error_text  <- base::sub(pattern = \"\\n*$\", replacement = \"\", x = embed_error_text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove all the trailing \\n, because added later\n",
    "    ######## end error text when embedding\n",
    "\n",
    "    #### end error_text initiation\n",
    "\n",

    "    #### argument primary checking\n",
    "\n",

    "    ######## arg ... forbidden\n",
    "    # nocov start\n",
    "    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.\n",
    "    if(\"...\" %in% arg_names) {\n",
    "        # This check is here in case the developer has not correctly written the argument of its function\n",
    "        tempo_cat <- base::paste0(\n",
    "            error_text_start, \n",
    "            \"ARGUMENT ... IS NOT ALLOWED IN SAFER-R FUNCTIONS.\\n\\nPLEASE, REWRITE YOUR FUNCTION CORRECTLY.\", \n",
    "            collapse = NULL, \n",
    "            recycle0 = FALSE\n",
    "        )\n",
    "        base::stop(base::paste0(\"\\n\\n================\\n\\n\", tempo_cat, \"\\n\\n================\\n\\n\", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)\n",
    "    }\n",
    "    # nocov end\n",
    "    ######## end arg ... forbidden\n",
    "\n",

    "    ######## mandatory arg of safer-r functions\n",
    "    mandat_args <- base::c(\"lib_path\", \"safer_check\", \"error_text\")\n",
    "    tempo_log <- ! mandat_args %in% arg_names\n",
    "    if(base::any(x = tempo_log, na.rm = TRUE)) {\n",
    "        # This check is here in case the developer has not correctly written the argument of its function\n",
    "        tempo_cat <- base::paste0(\n",
    "            error_text_start, \n",
    "            \"FOLLOWING ARGUMENT\", \n",
    "            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = \"S ARE\", no = \" IS\"), \n",
    "            \" MANDATORY IN SAFER-R FUNCTIONS:\\n\", \n",
    "            base::paste0(mandat_args[tempo_log], collapse = \"\\n\", recycle0 = FALSE), \n",
    "            collapse = NULL, \n",
    "            recycle0 = FALSE\n",
    "        )\n",
    "        base::stop(base::paste0(\"\\n\\n================\\n\\n\", tempo_cat, \"\\n\\n================\\n\\n\", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)\n",
    "    }\n",
    "    ######## end mandatory arg of safer-r functions\n",
    "\n",

    no_def_block,
    "    ######## management of NULL arguments\n",
    "    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)\n",
    tempo_arg_block,
    "    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = \"any\", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())\n",
    "    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()\n",
    "        tempo_cat <- base::paste0(\n",
    "            error_text_start, \n",
    "            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = \"THESE ARGUMENTS\", no = \"THIS ARGUMENT\"), \n",
    "            \" CANNOT BE NULL:\\n\", \n",
    "            base::paste0(tempo_arg[tempo_log], collapse = \"\\n\", recycle0 = FALSE), \n",
    "            collapse = NULL, \n",
    "            recycle0 = FALSE\n",
    "        )\n",
    "        base::stop(base::paste0(\"\\n\\n================\\n\\n\", tempo_cat, \"\\n\\n================\\n\\n\", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)\n",
    "    }\n",
    "    ######## end management of NULL arguments\n",

    # ---- block inserted after "end management of NULL arguments" ----
    "\n",
    "    ######## management of empty non NULL arguments\n",
    "    # # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)\n",
    empty_arg_block,
    "    tempo_arg_user_setting_eval <- arg_user_setting_eval[base::names(x = arg_user_setting_eval) %in% tempo_arg]\n",
    "    if(base::length(x = tempo_arg_user_setting_eval) != 0){\n",
    "        tempo_log <- base::suppressWarnings(\n",
    "            expr = base::sapply(\n",
    "                X = tempo_arg_user_setting_eval, \n",
    "                FUN = function(x){\n",
    "                    base::length(x = x) == 0 & ! base::is.null(x = x)\n",
    "                }, \n",
    "                simplify = TRUE, \n",
    "                USE.NAMES = TRUE\n",
    "            ), \n",
    "            classes = \"warning\"\n",
    "        ) # no argument provided by the user can be empty non NULL object. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function\n",
    "        if(base::any(tempo_log, na.rm = TRUE)){\n",
    "            tempo_cat <- base::paste0(\n",
    "                error_text_start, \n",
    "                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = \"THESE ARGUMENTS\", no = \"THIS ARGUMENT\"), \n",
    "                \" CANNOT BE AN EMPTY NON NULL OBJECT:\\n\", \n",
    "                base::paste0(base::names(x = tempo_arg_user_setting_eval)[tempo_log], collapse = \"\\n\", recycle0 = FALSE), \n",
    "                collapse = NULL, \n",
    "                recycle0 = FALSE\n",
    "            )\n",
    "            base::stop(base::paste0(\"\\n\\n================\\n\\n\", tempo_cat, \"\\n\\n================\\n\\n\", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)\n",
    "        }\n",
    "    }\n",
    "    ######## end management of empty non NULL arguments\n",
    "\n",

    "    ######## management of NA arguments\n",
    "    # Mandataory section : argument of safer-r functions cannot have NA as only value, to prevent all(, na.rm = TRUE) or any(, na.rm = TRUE) to return a logical value\n",
    "    if(base::length(x = arg_user_setting_eval) != 0){\n",
    "        tempo_log <- base::suppressWarnings(\n",
    "            expr = base::sapply(\n",
    "                X = base::lapply(\n",
    "                    X = arg_user_setting_eval, \n",
    "                    FUN = function(x){\n",
    "                        base::is.na(x = x) # if x is empty, return empty, but ok with below\n",
    "                    }\n",
    "                ), \n",
    "                FUN = function(x){\n",
    "                    base::all(x = x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK\n",
    "                }, \n",
    "                simplify = TRUE, \n",
    "                USE.NAMES = TRUE\n",
    "            ), \n",
    "            classes = \"warning\"\n",
    "        ) # no argument provided by the user can be just made of NA. is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: all(x = x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function\n",
    "        if(base::any(tempo_log, na.rm = TRUE)){\n",
    "            tempo_cat <- base::paste0(\n",
    "                error_text_start, \n",
    "                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = \"THESE ARGUMENTS\", no = \"THIS ARGUMENT\"), \n",
    "                \" CANNOT BE MADE OF NA ONLY:\\n\", \n",
    "                base::paste0(base::names(x = arg_user_setting_eval)[tempo_log], collapse = \"\\n\", recycle0 = FALSE), \n",
    "                collapse = NULL, \n",
    "                recycle0 = FALSE\n",
    "            )\n",
    "            base::stop(base::paste0(\"\\n\\n================\\n\\n\", tempo_cat, \"\\n\\n================\\n\\n\", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)\n",
    "        }\n",
    "    }\n",
    "    ######## end management of NA arguments\n",
    "\n",

    "    #### end argument primary checking\n",
    "\n",

    "    #### environment checking\n",
    "\n",

    "    ######## safer_check argument checking\n",
    "    if( ! (base::all(base::typeof(x = safer_check) == \"logical\", na.rm = TRUE) & base::length(x = safer_check) == 1)){ # no need to test NA because NA only already managed above and base::length(x = safer_check) == 1)\n",
    "        if(base::all(base::mode(x = safer_check) == \"function\", na.rm = TRUE)){\n",
    "            safer_check <- base::deparse1(expr = safer_check, collapse = \"\", width.cutoff = 500L)\n",
    "        }\n",
    "        tempo_cat <- base::paste0(\n",
    "            error_text_start, \n",
    "            \"THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).\\nHERE IT IS:\\n\", \n",
    "            base::ifelse(test = base::length(x = safer_check) == 0 | base::all(base::suppressWarnings(expr = safer_check == base::quote(expr = ), classes = \"warning\"), na.rm = TRUE) | base::all(safer_check == \"\", na.rm = TRUE), yes = \"<NULL, \\\"\\\", EMPTY OBJECT OR EMPTY NAME>\", no = base::paste0(safer_check, collapse = \"\\n\", recycle0 = FALSE)),\n",
    "            collapse = NULL, \n",
    "            recycle0 = FALSE\n",
    "        )\n",
    "        base::stop(base::paste0(\"\\n\\n================\\n\\n\", tempo_cat, \"\\n\\n================\\n\\n\", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)\n",
    "    }\n",
    "    ######## end safer_check argument checking\n",
    "\n",

    "    ######## check of lib_path\n",
    "    # must be before any :: or ::: non basic package calling\n",
    "    if(safer_check == TRUE){ # this line must be inactivated if you want to use lib_path in the main code (other than in safer functions present in the main code) \n",
    "        if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.\n",
    "            if( ! base::all(base::typeof(x = lib_path) == \"character\", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == \"character\" returns FALSE)\n",
    "                if(base::all(base::mode(x = lib_path) == \"function\", na.rm = TRUE)){\n",
    "                    lib_path <- base::deparse1(expr = lib_path, collapse = \"\", width.cutoff = 500L)\n",
    "                }\n",
    "                tempo_cat <- base::paste0(\n",
    "                    error_text_start, \n",
    "                    \"THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\\nHERE IT IS:\\n\", \n",
    "                    base::ifelse(test = base::length(x = lib_path) == 0 | base::all(base::suppressWarnings(expr = lib_path == base::quote(expr = ), classes = \"warning\"), na.rm = TRUE), yes = \"<NULL, EMPTY OBJECT OR EMPTY NAME>\", no = base::paste0(lib_path, collapse = \"\\n\", recycle0 = FALSE)),\n",
    "                    collapse = NULL, \n",
    "                    recycle0 = FALSE\n",
    "                )\n",
    "                base::stop(base::paste0(\"\\n\\n================\\n\\n\", tempo_cat, \"\\n\\n================\\n\\n\", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)\n",
    "            }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA. dir.exists(paths = NA) returns an error, so ok. dir.exists(paths = \"\") returns FALSE so ok\n",
    "                tempo_log <- ! base::dir.exists(paths = lib_path)\n",
    "                tempo_cat_b <- lib_path[tempo_log] # here lib_path is character string\n",
    "                tempo_cat_b[tempo_cat_b == \"\"] <- \"\\\"\\\"\"\n",
    "                tempo_cat <- base::paste0(\n",
    "                    error_text_start, \n",
    "                    \"THE DIRECTORY PATH\",\n",
    "                    base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = \"S\", no = \"\"), \n",
    "                    \" INDICATED IN THE lib_path ARGUMENT DO\", \n",
    "                    base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = \"\", no = \"ES\"), \n",
    "                    \" NOT EXIST:\\n\", \n",
    "                    base::paste0(tempo_cat_b, collapse = \"\\n\", recycle0 = FALSE), \n",
    "                    collapse = NULL, \n",
    "                    recycle0 = FALSE\n",
    "                )\n",
    "                base::stop(base::paste0(\"\\n\\n================\\n\\n\", tempo_cat, \"\\n\\n================\\n\\n\", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)\n",
    "            }else{\n",
    "                ini_lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument\n",
    "                base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()\n",
    "                base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = \"/$|\\\\\\\\$\", replacement = \"\", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\\\ in path\n",
    "                lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument\n",
    "            }\n",
    "        }else{\n",
    "            lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base:::.libPaths(), lib_path))\n",
    "        }\n",
    "    }  # this line must be inactivated if you want to use lib_path in the main code (other than in safer functions present in the main code) \n",
    "    ######## end check of lib_path\n",
    "\n",

    "    ######## check of the required functions from the required packages\n",
    "    if(safer_check == TRUE){\n",
    "        .pack_and_function_check <- utils::getFromNamespace(x = \".pack_and_function_check\", ns = \"saferDev\", pos = , envir = )\n",
    "        .pack_and_function_check(\n",
    "            fun = base::c(\n",
    "                # functions required in this code\n",
    "                \"saferDev::arg_check\", # write each function preceeded by their package name\n",
    "                # end functions required in this code\n",
    "                # internal functions required in this code\n",
    "                \"saferDev:::.base_op_check\"\n",
    "                # end internal functions required in this code\n",
    "            ),\n",
    "            lib_path = lib_path, # write NULL if your function does not have any lib_path argument\n",
    "            error_text = embed_error_text\n",
    "        )\n",
    "    }\n",
    "    ######## end check of the required functions from the required packages\n",
    "\n",

    "    ######## escaping CRAN submission NOTE for internal functions\n",
    "\n",
    "    .base_op_check <- utils::getFromNamespace(x = \".base_op_check\", ns = \"saferDev\", pos = , envir = )\n",
    "    # add here in the internal functions that are used in your main code (copy-paste the line above and replace .base_op_check by the name of the internal function\n",
    "    # not mandatory if your function is not designed for submission to the CRAN\n",
    "\n",
    "    ######## end escaping CRAN submission NOTE for internal functions\n",
    "\n",

    "    ######## critical operator checking\n",
    "    if(safer_check == TRUE){\n",
    "        .base_op_check(\n",
    "            error_text = embed_error_text\n",
    "        )\n",
    "    }\n",
    "    ######## end critical operator checking\n",
    "\n",
    "    #### end environment checking\n",

    # ---- argument secondary checking (arg_check() blocks) ------------
    "\n",
    .build_arg_check_section(fun_args = fun_args, arg_check_settings = arg_check_settings,
                             extra_section = no_empty_block),

    "\n    #### main code\n",
    body,
    "\n    #### end main code\n",
    "}\n",
    collapse = NULL,
    recycle0 = FALSE
  )
}
