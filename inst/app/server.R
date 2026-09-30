library(shiny)

server <- function(input, output, session) {
  
  # ---- Constants ------------------------------------------------------------
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
  
  # Channel 1: ONLY for errors raised when running the pasted function
  ERROR_TEXT <- "The code provided returned an error. Please, click on the back button and provide a function that runs well."
  
  # Channel 3: internal errors of the interface
  INTERNAL_ERROR_TEXT <- "An internal error occurred in the interface. This is not related to your function. Please report here https://github.com/safer-r/saferMake/issues/new."
  
  # ---- App state ----------------------------------------------------------
  rv <- reactiveValues(
    screen          = "input",   # "input" | "error" | "result"
    code            = "",        # last pasted code (so "Back" restores the box)
    pkg_name        = "",        # "Package name" box value (kept on Back)
    link_name       = "",        # "Error report link" box value (kept on Back)
    null_args       = character(0),  # argument names checked as NULL-accepting
    non_null_args   = character(0),  # argument names NOT checked
    empty_args      = character(0),  # argument names checked as empty-accepting
    non_empty_args  = character(0),  # argument names NOT checked (empty checkbox)
    no_default_args = character(0),  # argument names with NO default value
    error_msg       = NULL,      # real error: logged to console only, NEVER displayed
    error_display   = NULL,      # text actually displayed on the error screen
    fun_name        = NULL,
    fun_args        = NULL,      # character vector of argument names
    fun_body        = NULL,      # verbatim body text (everything between '{' and '}')
    aa              = NULL,      # verbatim: beginning of pasted code up to the last argument
    rebuilt         = NULL,      # not strictly needed; kept for future preview use
    arg_check_settings = list()  # per-argument arg_check() settings (key = arg_id(nm))
  )
  
  # Build a valid HTML id from an argument name
  arg_id <- function(nm) paste0("arg_", gsub("[^[:alnum:]_]", "_", nm))
  # Checkbox id for a given argument name
  null_cb_id <- function(nm) paste0("null_", arg_id(nm))
  empty_cb_id <- function(nm) paste0("empty_", arg_id(nm))
  
  # ---- arg_check() helpers ---------------------------------------------
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
  
  get_arg_check_settings <- function() {
    if (length(rv$fun_args) == 0L) return(list())
    out <- lapply(rv$fun_args, function(nm) {
      ids <- arg_check_field_ids(arg_id(nm))
      list(
        # Blank text field = default value (class/typeof: "NULL" = no constraint,
        # mode: "numeric", length/options: "" = no constraint)
        class                     = if (field_blank(input[[ids$class]])) "NULL" else trimws(input[[ids$class]]),
        typeof                    = if (field_blank(input[[ids$typeof]])) "NULL" else trimws(input[[ids$typeof]]),
        mode                      = if (field_blank(input[[ids$mode]])) "NULL" else trimws(input[[ids$mode]]),
        length                    = if (field_blank(input[[ids$length]])) "NULL" else trimws(input[[ids$length]]),
        prop                      = isTRUE(input[[ids$prop]]),
        double_as_integer_allowed = isTRUE(input[[ids$double_as_integer_allowed]]),
        options                   = if (field_blank(input[[ids$options]])) "NULL" else trimws(input[[ids$options]]),
        all_options_in_data       = isTRUE(input[[ids$all_options_in_data]]),
        na_contain                = isTRUE(input[[ids$na_contain]]),
        neg_values                = isTRUE(input[[ids$neg_values]]),
        inf_values                = isTRUE(input[[ids$inf_values]])
      )
    })
    names(out) <- vapply(rv$fun_args, arg_id, character(1L), USE.NAMES = FALSE)
    out
  }
  
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
  
  # Builds the whole "#### argument secondary checking" section.
  # Each argument produces EXACTLY one line:
  #     tempo <- saferDev::arg_check(data = <arg>, class = NULL, typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
  build_arg_check_section <- function(fun_args, arg_check_settings) {
    calls <- vapply(fun_args, function(nm) {
      st <- arg_check_settings[[arg_id(nm)]]
      if (is.null(st)) st <- ARG_CHECK_DEFAULTS
      class_val   <- if (identical(st$class, "NULL"))  "NULL" else deparse(st$class)
      typeof_val  <- if (identical(st$typeof, "NULL")) "NULL" else deparse(st$typeof)
      mode_val    <- if (identical(st$mode, "NULL"))   "NULL" else deparse(st$mode)
      len_val     <- arg_check_parse_length(st$length)
      length_val  <- if (is.null(len_val)) "NULL" else paste0(len_val, "L")
      opts        <- arg_check_parse_options(st$options)
      options_val <- if (is.null(opts)) "NULL" else deparse(opts)
      paste0(
        "    tempo <- saferDev::arg_check(data = ", nm,
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
        ", lib_path = lib_path",
        ", error_text = embed_error_text",
        ") ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))\n"
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
  
  # ---- Error channels -------------------------------------------------------
  set_error <- function(detail, display) {
    message("App error (not shown to the user): ", detail)
    rv$error_msg     <- detail
    rv$error_display <- display
    rv$screen        <- "error"
  }
  user_code_error <- function(detail) set_error(detail, ERROR_TEXT)
  internal_error  <- function(detail) set_error(detail, INTERNAL_ERROR_TEXT)
  
  # ---- Argument helper ------------------------------------------------------
  # names of the arguments of f that have NO default value.
  # In formals(), a default-less argument holds the 'missing' object,
  # which is identical to quote(expr = ). An explicit default of NULL
  # (e.g. function(x = NULL)) is NOT 'no default': it has a default (NULL).
  args_without_default <- function(f) {
    fm <- formals(f)
    if (is.null(fm)) return(character(0))
    fm <- as.list(fm) # pairlist -> plain list, so vapply is safe
    if (length(fm) == 0L) return(character(0))
    no_def <- vapply(fm, function(v) identical(v, quote(expr = )), logical(1L))
    names(fm)[no_def]
  }
  
  # ---- Verbatim capture helpers --------------------------------------------
  match_close <- function(txt, open, close_ch) {
    open_ch  <- substring(txt, open, open)
    chars    <- strsplit(txt, "")[[1]]
    depth    <- 0L
    for (i in open:nchar(txt)) {
      if (chars[i] == open_ch) {
        depth <- depth + 1L
      } else if (chars[i] == close_ch) {
        depth <- depth - 1L
        if (depth == 0L) return(i)
      }
    }
    -1L
  }
  
  extract_aa_body <- function(code) {
    m <- regexpr("function[[:space:]]*\\(", code)
    if (m == -1) return(NULL)
    p_open  <- m + attr(m, "match.length") - 1L
    p_close <- match_close(code, p_open, ")")
    if (p_close == -1) return(NULL)
    aa <- substring(code, 1, p_close - 1L)
    
    rest    <- substring(code, p_close + 1L)
    m2      <- regexpr("\\S", rest)
    if (m2 == -1) return(list(aa = aa, body = ""))
    p_body <- p_close + m2
    
    if (substring(code, p_body, p_body) == "{") {
      p_end <- match_close(code, p_body, "}")
      if (p_end == -1) return(NULL)
      body <- substring(code, p_body + 1L, p_end - 1L)
      list(aa = aa, body = body)
    } else {
      list(aa = aa, body = trimws(rest))
    }
  }
  
  # ---- Rebuild the safer function -------------------------------------------
  # null_args       : argument names that ACCEPT NULL -> commented out in tempo_arg
  # non_null_args   : argument names that must NOT be NULL -> active in tempo_arg
  # empty_args      : argument names that ACCEPT empty non NULL values
  #                   -> commented out in the tempo_arg of the empty section
  # non_empty_args  : argument names that must NOT be empty -> active there
  # no_default_args : argument names with NO default value -> conditional section
  # fun_args        : all argument names (to emit the arg_check() blocks)
  # arg_check_settings : named list (key = arg_id(nm)) of arg_check() settings
  build_rebuilt <- function(aa, body, pkg, link,
                            null_args = character(0),
                            non_null_args = character(0),
                            empty_args = character(0),
                            non_empty_args = character(0),
                            no_default_args = character(0),
                            fun_args = character(0),
                            arg_check_settings = list()) {
    aa   <- sub("[[:space:]]+$", "", aa)
    body <- sub("[[:space:]]+$", "", sub("^[[:space:]]*\n", "", body))
    
    comma <- if (grepl("\\($", aa) || grepl(",$", aa)) "" else ","
    
    pkg_line  <- if (nzchar(pkg)) paste0("package_name <- ", deparse(pkg)) else "package_name <- NULL"
    link_line <- if (nzchar(link)) deparse(link) else "NULL"
    
    # ---- NULL management block ---------------------------------------------
    # FIX (bug 3): one line per argument, each comma-terminated, so commenting
    # a line out never breaks the c(...) call (a trailing comma is valid R).
    # ACTIVE line    -> argument must NOT be NULL (checked by tempo_log)
    # COMMENTED line -> argument accepts NULL (excluded from the check)
    non_null_lines <- vapply(
      non_null_args,
      function(nm) paste0("        ", deparse(nm, width.cutoff = 500L), ", "),
      character(1L)
    )
    null_lines <- vapply(
      null_args,
      function(nm) paste0("        # ", deparse(nm, width.cutoff = 500L),
                          ", # inactivated because can be NULL"),
      character(1L)
    )
    tempo_arg_block <- paste0(
      "    tempo_arg <- base::c(\n",
      paste(c(non_null_lines, null_lines,
              "        \"safer_check\" ",
              "        # \"lib_path\", # inactivated because can be NULL",
              "        # \"error_text\" # inactivated because NULL converted to \"\" above"),
            collapse = "\n"),
      "\n    )\n"
    )
    
    # ---- "no default value" block (conditional) -----------------------------
    # Emitted ONLY if at least one argument has no default value.
    # paste0() drops zero-length arguments (including NULL), so when
    # no_default_args is empty, no_def_block is NULL and the section is absent.
    no_def_block <- if (length(no_default_args) > 0L) {
      no_def_lines <- vapply(
        no_default_args,
        function(nm) paste0("        ", deparse(nm, width.cutoff = 500L)),
        character(1L)
      )
      paste0(
        "    ######## arg with no default values\n",
        "    # optional section: remove the code if none of your arguments has no default value\n",
        "    no_def_args <- base::c(\n",
        paste(no_def_lines, collapse = ",\n"), "\n    )\n",
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
        "\n"
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
    non_empty_lines <- vapply(
      non_empty_args,
      function(nm) paste0("        ", deparse(nm, width.cutoff = 500L), ", "),
      character(1L)
    )
    can_empty_lines <- vapply(
      empty_args,
      function(nm) paste0("        # ", deparse(nm, width.cutoff = 500L),
                          ", # inactivated because can be an empty non NULL object"),
      character(1L)
    )
    empty_arg_block <- paste0(
      "    tempo_arg <-base::c(\n",
      paste(c(can_empty_lines, non_empty_lines,
              "        \"safer_check\", ",
              "        \"lib_path\"",
              "        # \"error_text\" # inactivated because empty value converted to \"\" above"),
            collapse = "\n"),
      "\n    )\n"
    )
    
    paste0(
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
      
      "    ######## error text when embedding\n",
      "    # use this in the error_text of safer functions if present in your main code \n",
      "    embed_error_text  <- base::sub(pattern = \"^ERROR IN \", replacement = \" INSIDE \", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)\n",
      "    embed_error_text  <- base::sub(pattern = \"\\n*$\", replacement = \"\", x = embed_error_text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove all the trailing \\n, because added later\n",
      "    ######## end error text when embedding\n",
      "    #### end error_text initiation\n",
      
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
      "                lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base:::.libPaths(), lib_path))\n",
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
      build_arg_check_section(fun_args = fun_args, arg_check_settings = arg_check_settings),
      
      "\n    #### main code\n",
      body,
      "\n    #### end main code\n",
      "}\n"
    )
  }
  
  # ---- Shared UI fragments (used on input and error screens) --------------
  intro <- function() {
    list(
      h4(id = "intro", "Introduction"),
      tags$div(
        tags$ol(class = "input-instruction-list",
                tags$li(
                  "This interface helps to convert a R function of class S3 into a function of same class including the ",
                  tags$a(href = "https://github.com/safer-r", "safer-r rules",
                         target = "_blank",
                         style = "color: #2980B9; text-decoration: none; font-style: italic;"),
                  " to make the function safer."
                ),
                tags$li(HTML("The returned function includes a backbone at the beginning of the internal code, as well as three additional <i>safer-r</i> arguments."))
        )
      ),
      hr()
    )
  }
  
  code_instructions <- function() {
    tags$div(
      tags$ol(class = "input-instruction-list",
              tags$li("Fill the field."),
              tags$li("Click on the run button."),
              tags$li("Go to the newly created tab and complete the fields.")
      )
    )
  }
  
  wrap_panel <- function(...) {
    fluidRow(column(width = 12,
                    wellPanel(style = "background-color: #fcfcfc; border: none; padding: 0; margin: 0;", ...)))
  }
  
  back_btn <- function(id) {
    div(style = "text-align: right;",
        actionButton(inputId = id, label = "Back", icon = icon("arrow-left")))
  }
  
  # ---- RUN logic -------------------------------------------------------------
  run_clicked <- function() {
    code <- isolate(input$user_ini_fun)
    code <- if (is.null(code)) "" else code
    rv$code          <- code
    rv$error_msg     <- NULL
    rv$error_display <- NULL
    rv$fun_name      <- NULL
    rv$fun_args      <- NULL
    rv$fun_body      <- NULL
    rv$aa            <- NULL
    rv$null_args     <- character(0)
    rv$non_null_args <- character(0)
    rv$empty_args    <- character(0)
    rv$non_empty_args <- character(0)
    rv$no_default_args <- character(0)
    rv$arg_check_settings <- list()
    
    if (!nzchar(trimws(code))) {
      set_error("The field is empty.", "The field is empty: there is no code to run.")
      return(invisible())
    }
    
    env <- new.env(parent = globalenv())
    err <- NULL
    res <- tryCatch(
      eval(parse(text = code), envir = env),
      error = function(e) { err <<- conditionMessage(e); NULL }
    )
    if (!is.null(err)) {
      user_code_error(err)
      return(invisible())
    }
    
    fnames <- Filter(function(n) is.function(get(n, envir = env)), ls(envir = env))
    f <- NULL
    if (length(fnames) == 1L) {
      rv$fun_name <- fnames
      f <- get(fnames, envir = env)
    } else if (length(fnames) == 0L && is.function(res)) {
      rv$fun_name <- "<anonymous>"
      f <- res
    } else if (length(fnames) > 1L) {
      set_error(
        paste0("Several functions defined: ", paste(fnames, collapse = ", "), "."),
        paste0("The pasted code defines several functions (",
               paste(fnames, collapse = ", "),
               "). Please paste the code of exactly one function.")
      )
      return(invisible())
    } else {
      set_error("No function defined.",
                "The pasted code does not define any function. Please paste the code of a function.")
      return(invisible())
    }
    
    args <- names(formals(f))
    rv$fun_args <- if (is.null(args)) character(0) else args
    rv$no_default_args <- args_without_default(f)    
    parts <- extract_aa_body(code)
    if (is.null(parts)) {
      internal_error("extract_aa_body() could not locate the signature/body.")
      return(invisible())
    }
    rv$aa        <- parts$aa
    rv$fun_body  <- parts$body
    
    collide <- intersect(rv$fun_args, SAFER_ARGS)
    if (length(collide) > 0L) {
      set_error(
        paste0("Argument name collision with safer-r arguments: ",
               paste(collide, collapse = ", "), "."),
        display = paste0(
          "The following argument ", ifelse(test = length(collide) > 1, "names", no = "name"), " of your function ", ifelse(test = length(collide) > 1, "are", no = "is"), " reserved by the ",
          "safer-r rules and cannot be used:\n",
          paste(collide, collapse = "\n"),
          ".\nPlease rename ", ifelse(test = length(collide) > 1, "them", no = "it"), " and run again."
        )
      )
      return(invisible())
    }
    
    # FIX (bug 1): pass ALL arguments (named) and log the real parse error
    ok <- tryCatch({
      parse(text = build_rebuilt(aa = rv$aa, body = rv$fun_body, pkg = "", link = "",
                                 null_args = character(0),
                                 non_null_args = character(0),
                                 empty_args = character(0),
                                 non_empty_args = character(0),
                                 no_default_args = rv$no_default_args,
                                 fun_args = rv$fun_args,
                                 arg_check_settings = list()))      # defaults on first parse check
      TRUE
    }, error = function(e) {
      message("Parse check of the rebuilt function failed: ", conditionMessage(e))
      FALSE
    })
    if (!ok) {
      internal_error("The rebuilt function does not parse - check build_rebuilt()/server.R.")
      return(invisible())
    }
    
    rv$screen <- "result"
  }
  
  observeEvent(input$run_button, {
    tryCatch(
      run_clicked(),
      error = function(e) {
        internal_error(paste0("Unexpected error in the server code: ",
                              conditionMessage(e)))
      }
    )
  })
  
  # ---- BACK buttons (checkbox states are preserved like pkg/link) ----------
  observeEvent(input$back_from_result, {
    pkg  <- input$pkg_name
    rv$pkg_name <- if (is.null(pkg)) "" else pkg
    link <- input$link_name
    rv$link_name <- if (is.null(link)) "" else link
    if (length(rv$fun_args) > 0L) {
      checked <- vapply(rv$fun_args, function(nm) {
        isTRUE(input[[null_cb_id(nm)]])
      }, logical(1L))
      rv$null_args     <- rv$fun_args[checked]
      rv$non_null_args <- rv$fun_args[!checked]
      checked_empty <- vapply(rv$fun_args, function(nm) {
        isTRUE(input[[empty_cb_id(nm)]])
      }, logical(1L))
      rv$empty_args     <- rv$fun_args[checked_empty]
      rv$non_empty_args <- rv$fun_args[!checked_empty]
    }
    rv$arg_check_settings <- get_arg_check_settings()
    rv$screen <- "input"
  })
  observeEvent(input$back_from_error, { rv$screen <- "input" })
  
  # ---- DOWNLOAD: the modified (safer) function -------------------------------
  output$download_safer <- downloadHandler(
    filename = function() {
      paste0(rv$fun_name, "_safer.R")
    },
    content = function(file) {
      pkg  <- if (is.null(input$pkg_name)) "" else trimws(input$pkg_name)
      link <- if (is.null(input$link_name)) "" else trimws(input$link_name)
      
      if (length(rv$fun_args) > 0L) {
        checked <- vapply(rv$fun_args, function(nm) {
          isTRUE(input[[null_cb_id(nm)]])
        }, logical(1L))
        null_argument     <- rv$fun_args[checked]
        non_null_argument <- rv$fun_args[!checked]
        checked_empty <- vapply(rv$fun_args, function(nm) {
          isTRUE(input[[empty_cb_id(nm)]])
        }, logical(1L))
        empty_argument     <- rv$fun_args[checked_empty]
        non_empty_argument <- rv$fun_args[!checked_empty]
      } else {
        null_argument      <- character(0)
        non_null_argument  <- character(0)
        empty_argument     <- character(0)
        non_empty_argument <- character(0)
      }
      rv$null_args      <- null_argument      # kept in sync for "Back"
      rv$non_null_args  <- non_null_argument  # available for your part 4
      rv$empty_args     <- empty_argument
      rv$non_empty_args <- non_empty_argument
      
      arg_check_settings <- get_arg_check_settings()
      rv$arg_check_settings <- arg_check_settings       # kept in sync for "Back"
      
      # FIX (bug 2): pass the LOCAL vectors
      code <- build_rebuilt(aa = rv$aa, body = rv$fun_body, pkg = pkg, link = link,
                            null_args = null_argument,
                            non_null_args = non_null_argument,
                            empty_args = empty_argument,
                            non_empty_args = non_empty_argument,
                            no_default_args = rv$no_default_args,
                            fun_args = rv$fun_args,
                            arg_check_settings = arg_check_settings)
      rv$rebuilt <- code
      
      # Guard: never write a file that does not parse
      if (!tryCatch({ parse(text = code); TRUE },
                    error = function(e) {
                      message("Download blocked (rebuilt code does not parse): ",
                              conditionMessage(e))
                      FALSE
                    })) {
        return(invisible())
      }
      writeLines(code, file)
    }
  )
  
  # ---- Sidebar: Table of Contents (depends on the current screen) -----------
  output$toc <- renderUI({
    entries <- if (identical(rv$screen, "result")) {
      c(
        list(tags$li(tags$a(href = "#pkg_section", "Package name"))),
        list(tags$li(tags$a(href = "#link_section", "Error report link"))),
        if (length(rv$fun_args) == 0L) {
          list(tags$li("No arguments"))
        } else {
          lapply(rv$fun_args, function(nm) {
            tags$li(tags$a(href = paste0("#", arg_id(nm)), paste0("Argument: ", nm)))
          })
        }
      )
    } else {
      list(
        tags$li(tags$a(href = "#intro", "Introduction")),
        tags$li(tags$a(href = "#sec_code", "Code of your function"))
      )
    }
    div(class = "wy-menu wy-menu-vertical",
        p(class = "caption", "Table of Contents"),
        tags$ul(entries))
  })
  
  # ---- The three screens -----------------------------------------------------
  output$screen <- renderUI({
    wrap_panel(
      switch(rv$screen,
             
             input = tagList(
               intro(),
               h4(id = "sec_code", "Code of your function"),
               code_instructions(),
               textAreaInput(inputId = "user_ini_fun", label = NULL,
                             value = rv$code,
                             placeholder = "my_fun <- function(x){\n    x + 1\n}",
                             rows = 15, width = "100%"),
               hr(),
               div(style = "text-align: right;",
                   actionButton(inputId = "run_button", label = "Run", class = "btn-primary"))
             ),
             
             error = tagList(
               intro(),
               h4(id = "sec_code", "Code of your function"),
               code_instructions(),
               div(class = "alert alert-danger", role = "alert",
                   style = "margin-top: 10px; white-space: pre-line;",
                   if (is.null(rv$error_display)) ERROR_TEXT else rv$error_display),
               hr(),
               back_btn("back_from_error")
             ),
             
             result = {
               args_txt <- if (length(rv$fun_args) == 0L) {
                 "none"
               } else {
                 paste(rv$fun_args, collapse = ", ")
               }
               no_def_txt <- if (length(rv$no_default_args) == 0L) {
                 "none"
               } else {
                 paste(rv$no_default_args, collapse = ", ")
               }
               
               detected_block <- tags$div(
                 tags$ol(class = "input-instruction-list",
                         tags$li(paste0("Function detected: ", rv$fun_name)),
                         tags$li(paste0("Arguments detected: ", args_txt)),
                         tags$li(paste0("Arguments with no default value: ", no_def_txt))
                 )
               )
               
               pkg_section <- tagList(
                 h4(id = "pkg_section", "Package name"),
                 tags$div(
                   tags$ol(class = "input-instruction-list",
                           tags$li("Does this function belong to a package? If yes, indicate its name. Otherwise, leave blanck.")
                   )
                 ),
                 textInput(inputId = "pkg_name",
                           label = NULL,
                           value = rv$pkg_name,
                           placeholder = "Package name",
                           width = "100%")
               )
               
               link_section <- tagList(
                 h4(id = "link_section", "Error report link"),
                 tags$div(
                   tags$ol(class = "input-instruction-list",
                           tags$li("Do you have a link for users of your function to report any internal errors? If yes, indicate the full link. Otherwise, leave blanck.")
                   )
                 ),
                 textInput(inputId = "link_name",
                           label = NULL,
                           value = rv$link_name,
                           placeholder = "Error report link",
                           width = "100%")
               )
               
               result_footer <- div(
                 style = "text-align: right;",
                 downloadButton(outputId = "download_safer",
                                label = "Run", class = "btn-primary"),
                 actionButton(inputId = "back_from_result",
                              label = "Back", icon = icon("arrow-left"))
               )
               
               if (length(rv$fun_args) == 0L) {
                 tagList(
                   detected_block,
                   hr(),
                   pkg_section,
                   hr(),
                   link_section,
                   hr(),
                   result_footer
                 )
               } else {
                 n <- length(rv$fun_args)
                 tagList(
                   detected_block,
                   hr(),
                   pkg_section,
                   hr(),
                   link_section,
                   hr(),
                   # One section per argument: NULL/empty checkboxes, then the five
                   # arg_check() text fields stacked vertically (free text, no
                   # dropdown: blank = default value), then the six tick boxes
                   # stacked vertically.
                   lapply(seq_len(n), function(i) {
                     nm <- rv$fun_args[i]
                     aid <- arg_id(nm)
                     st  <- rv$arg_check_settings[[aid]]
                     if (is.null(st)) st <- ARG_CHECK_DEFAULTS
                     ids <- arg_check_field_ids(aid)
                     # Show blank in the field when the stored value is the default
                     class_disp  <- if (identical(st$class, "NULL")) "" else st$class
                     typeof_disp <- if (identical(st$typeof, "NULL")) "" else st$typeof
                     mode_disp   <- if (identical(st$mode, "NULL")) "" else st$mode
                     sec <- tagList(
                       h4(id = aid, paste0("Argument: ", nm)),
                       checkboxInput(inputId = null_cb_id(nm),
                                     label = "Accepts the NULL value",
                                     value = nm %in% rv$null_args,
                                     width = "100%"),
                       checkboxInput(inputId = empty_cb_id(nm),
                                     label = HTML("Can be an empty argument (e.g., <code>character()</code>)"),
                                     value = nm %in% rv$empty_args,
                                     width = "100%"),
                       hr(),
                       textInput(inputId = ids$class,
                                 label = "Class (left blank = default NULL, i.e., no class constraint)",
                                 value = class_disp,
                                 placeholder = "NULL",
                                 width = "100%"),
                       textInput(inputId = ids$typeof,
                                 label = "Typeof (left blank = default NULL, i.e., no typeof constraint)",
                                 value = typeof_disp,
                                 placeholder = "NULL",
                                 width = "100%"),
                       textInput(inputId = ids$mode,
                                 label = "Mode (left blank = default NULL, i.e., no mode constraint)",
                                 value = mode_disp,
                                 placeholder = "NULL",
                                 width = "100%"),
                       textInput(inputId = ids$length,
                                 label = "Length (left blank = default NULL, i.e., no length constraint)",
                                 value = st$length,
                                 placeholder = "e.g., 3",
                                 width = "100%"),
                       textInput(inputId = ids$options,
                                 label = "Options, separated by commas or spaces (left blank = default NULL, i.e., no options constraint)",
                                 value = st$options,
                                 placeholder = "e.g., option1, option2",
                                 width = "100%"),
                       hr(),
                       checkboxInput(inputId = ids$prop,
                                     label = "prop (tick means TRUE)",
                                     value = isTRUE(st$prop),
                                     width = "100%"),
                       checkboxInput(inputId = ids$double_as_integer_allowed,
                                     label = "double_as_integer_allowed (tick means TRUE)",
                                     value = isTRUE(st$double_as_integer_allowed),
                                     width = "100%"),
                       checkboxInput(inputId = ids$all_options_in_data,
                                     label = "all_options_in_data (tick means TRUE)",
                                     value = isTRUE(st$all_options_in_data),
                                     width = "100%"),
                       checkboxInput(inputId = ids$na_contain,
                                     label = "na_contain (tick means TRUE)",
                                     value = isTRUE(st$na_contain),
                                     width = "100%"),
                       checkboxInput(inputId = ids$neg_values,
                                     label = "neg_values (tick means TRUE)",
                                     value = isTRUE(st$neg_values),
                                     width = "100%"),
                       checkboxInput(inputId = ids$inf_values,
                                     label = "inf_values (tick means TRUE)",
                                     value = isTRUE(st$inf_values),
                                     width = "100%")
                     )
                     if (i < n) sec <- tagList(sec, hr())
                     sec
                   }),
                   hr(),
                   result_footer
                 )
               }
             }
      )
    )
  })
}