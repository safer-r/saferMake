# Full staged check of the pasted code (the seven checks of the Run button).
# code     : the pasted code (character), already evaluated without error
# f        : the function evaluated from code with keep.source = TRUE
# fun_name : the function name ("<anonymous>" when the code is a bare function)
# Returns list(stage, code, step, modified, notice, report, report_msg,
# report_file) with:
#   stage    : "ok" (the seven checks are clean), "modified" (at least one
#              check modified the code, none reports findings), "findings"
#              (the check number step reports something that it cannot
#              correct itself: the user must complete the code), "error" (the
#              saferDev function number step stopped), or "skip" (the saferDev
#              of the R session does not provide the seven checks: no check can
#              be done, the user is told to load the current saferDev)
#   code     : the code to display in window 1: the code corrected by the
#              chained checks (also at stages "findings" and "error" when the
#              checks before the stopping one modified it), otherwise the
#              unchanged pasted code
#   step     : number (1 to 7) of the check that stopped the run (findings or
#              error), NULL at stages "ok", "modified" and "skip"
#   modified : numbers of the checks that modified the code (integer(0) if none)
#   notice   : lines of the orange message of window 1 (character(0) at stage
#              "ok")
#   report   : full report printed by saferMake::test() (status of the seven
#              checks and report of each check that modified the code,
#              reported findings or stopped; "" at stage "ok")
#   report_msg : part of report shown in the message of window 1: the report
#              of the check that stopped the run (stages "findings" and
#              "error"), and the CORRECTIONS NOT APPLIED section of
#              all_args_here() ("" if none)
#   report_file : text of the report file downloaded from the message of
#              window 1: report without its first line ("AFTER RUNNING
#              saferMake::test()."), followed by the code of the modified
#              function written as exported by saferMake::test(export = TRUE)
#              (<name>_safer <- function...) when the code has been modified
#              ("" at stages "ok" and "skip")
# error_text : passed to saferMake::test(), so that the error messages give the
#              full trail (" INSIDE saferMake::make()" when run by the app)
#' @title Staged Code Checks
#' @description
#' Full staged check of the pasted code (the seven checks of the Run button), performed by \code{saferMake::test()}.
#' @param code Single character string of the pasted code, already evaluated without error.
#' @param f Function evaluated from the pasted code with \code{base::parse(keep.source = TRUE)}.
#' @param fun_name Single character string: the function name (\code{"<anonymous>"} when the code is a bare function).
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default): only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns A list (stage, code, step, modified, notice, report, report_msg, report_file): see the comments above the function.
#' @details
#' \code{saferMake::test()} is called with \code{safer_check = FALSE}, \code{lib_path = lib_path} and \code{error_text = error_text} (the \code{error_text} received, not \code{embed_error_text}, so that the error messages give the trail \code{INSIDE saferMake::test() INSIDE saferMake::make()}).
#' 
#' Warnings:
#' \itemize{
#'   \item requires \code{saferDev::arg_check}, \code{saferMake::test}, \code{saferMake:::.safer_dev_available}, \code{saferMake:::.check_var_name}, \code{saferMake:::.capture_msg}, \code{saferMake:::.splice_function}.
#'   \item In the safer Backbone section \code{"######## check of the required functions from the required packages"}, add also these 6 functions when checking for the presence of \code{.run_code_checks}.
#' }
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @keywords internal
.run_code_checks <- function(
    code, 
    f, 
    fun_name, 
    lib_path, # required because of saferDev::arg_check()
    error_text # warning: in internal functions, error_text without default value returns a R classical non traced error message (specific of internal functions since classical functions are error_text = "")
){
    # EXAMPLE (because the CRAN does not accept examples for unexported functions)
    # .run_code_checks(code = "my_fun <- function(x){x + 1}", f = base::eval(expr = base::parse(text = "my_fun <- function(x){x + 1}", keep.source = TRUE)), fun_name = "my_fun", lib_path = NULL, error_text = " INSIDE P1::F1")
    # DEBUGGING
    # code = "my_fun <- function(x){x + 1}" ; f = base::eval(expr = base::parse(text = code, keep.source = TRUE)) ; fun_name = "my_fun" ; lib_path = NULL ; error_text = " INSIDE P1::F1"
    #### package name
    package_name <- "saferMake" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions. In addition, arguments values retrieved are not evaluated base::match.call, but this is solved with get() below
    function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE) 
    # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function_name[1] == "::()" | function_name[1] == ":::()"){
        function_name <- function_name[3]
    }
    #### end function name

    #### arguments settings
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user). Always a list, even if 1 argument. So ok for lapply() usage (management of NA section)
    arg_user_setting_names <- base::names(x = arg_user_setting)
    # evaluation of values if they are expression, call, etc.
    if(base::length(x = arg_user_setting) != 0){
        arg_user_setting_eval <- base::lapply(
            X = arg_user_setting_names, 
            FUN = function(x){
                base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = TRUE) # n = 2 because of lapply(), inherit = TRUE to be sure to correctly evaluate
            }
        )
        base::names(x = arg_user_setting_eval) <- arg_user_setting_names
    }else{
        arg_user_setting_eval <- NULL
    }
    # end evaluation of values if they are expression, call, etc.
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 0)), envir = base::parent.frame(n = 1))) # names of all the arguments
    #### end arguments settings

    #### error_text initiation

    ######## basic error text start
    error_text <- base::paste0(base::unlist(x = error_text, recursive = TRUE, use.names = TRUE), collapse = "", recycle0 = FALSE) # convert everything to string. if error_text is a string, changes nothing. If NULL or empty (even list) -> "" so no need to check for management of NULL or empty value
    package_function_name <- base::paste0(
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name,
        collapse = NULL, 
        recycle0 = FALSE
    )
    error_text_start <- base::paste0(
        "ERROR IN ", # must not be changed, because this "ERROR IN " string is used for text replacement
        package_function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    ######## end basic error text start

    ######## internal error text
    intern_error_text_start <- base::paste0(
        package_function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    intern_error_text_end <- base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, ".", collapse = NULL, recycle0 = FALSE))
    ######## end internal error text

    ######## error text when embedding
    # use this in the error_text of safer functions if present below 
    embed_error_text  <- base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    embed_error_text  <- base::sub(pattern = "\n*$", replacement = "", x = embed_error_text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove all the trailing \n, because added later
    ######## end error text when embedding

    #### end error_text initiation

    #### argument primary checking

    ######## arg ... forbidden
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    if("..." %in% arg_names) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "ARGUMENT ... IS NOT ALLOWED IN SAFER-R FUNCTIONS.\n\nPLEASE, REWRITE YOUR FUNCTION CORRECTLY.", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # nocov end
    ######## end arg ... forbidden

    ######## mandatory arg of safer-r functions
    # personalized for internal functions: lib_path and error_text are mandatory; safer_check does not exist in internal functions
    mandat_args <- base::c("lib_path", "error_text")
    tempo_log <- ! mandat_args %in% arg_names
    if(base::any(tempo_log, na.rm = TRUE)) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S ARE", no = " IS"), 
            " MANDATORY IN SAFER-R FUNCTIONS:\n", 
            base::paste0(mandat_args[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end mandatory arg of safer-r functions

    ######## arg with no default values
    # optional section: remove the code if none of your arguments has no default value
    no_def_args <- base::c(
        "code", 
        "f", 
        "fun_name", 
        "lib_path"
        # "error_text" # inactivated because error_text already used above. Specific of my internal functions that error_text has no default value
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(no_def_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            base::paste0(no_def_args[tempo], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "code", 
        "f", 
        "fun_name"
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame.
    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0(
            error_text_start, 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
            " CANNOT BE NULL:\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "code", 
        "f", 
        "fun_name", 
        "lib_path"
        # "error_text" # inactivated because empty value converted to "" above
    )
    tempo_arg_user_setting_eval <- arg_user_setting_eval[base::names(x = arg_user_setting_eval) %in% tempo_arg]
    if(base::length(x = tempo_arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = tempo_arg_user_setting_eval, 
                FUN = function(x){
                    base::length(x = x) == 0 & ! base::is.null(x = x)
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be empty non NULL object. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE AN EMPTY NON NULL OBJECT:\n", 
                base::paste0(base::names(x = tempo_arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    if(base::length(x = arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = base::lapply(
                    X = arg_user_setting_eval, 
                    FUN = function(x){
                        base::is.na(x = x) # if x is empty, return empty, but ok with below
                    }
                ), 
                FUN = function(x){
                    base::all(x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be just made of NA. is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: all(x = x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE MADE OF NA ONLY:\n", 
                base::paste0(base::names(x = arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    # not used here
    ######## end safer_check argument checking

    ######## check of lib_path
    # check already done in the main safer function
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    # saferDev::arg_check is required here
    # saferMake::test is required here
    # saferMake:::.safer_dev_available is required here
    # saferMake:::.check_var_name is required here
    # saferMake:::.capture_msg is required here
    # saferMake:::.splice_function is required here
    # saferMake:::.code_check_steps is required here
    # but check already done in the main safer function
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .safer_dev_available <- utils::getFromNamespace(x = ".safer_dev_available", ns = "saferMake", pos = -1, envir = )
    .check_var_name <- utils::getFromNamespace(x = ".check_var_name", ns = "saferMake", pos = -1, envir = )
    .capture_msg <- utils::getFromNamespace(x = ".capture_msg", ns = "saferMake", pos = -1, envir = )
    .splice_function <- utils::getFromNamespace(x = ".splice_function", ns = "saferMake", pos = -1, envir = )
    .code_check_steps <- utils::getFromNamespace(x = ".code_check_steps", ns = "saferMake", pos = -1, envir = )

    ######## end escaping CRAN submission NOTE for internal functions

    ######## critical operator checking
    # check already done in the main safer function
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    arg_check_error_text <- base::paste0("ERROR ", embed_error_text, "\n\n", collapse = NULL, recycle0 = FALSE) # must be used instead of error_text = embed_error_text when several arg_check are performed on the same argument (tempo1, tempo2, see below)
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object_name))
    tempo <- saferDev::arg_check(data = code, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = f, class = "function", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = fun_name, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    # lib_path already checked above
    # error_text converted to single string above
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste0(text_check[argum_check], collapse = "\n\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    # not required
        # "code", # inactivated because can be ""
        # "f", # inactivated because not character
        # "fun_name", # inactivated because can be ""
        # "lib_path", # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## reserved words
    ######## end reserved words

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    res <- base::list(stage = "ok", code = code, step = NULL, modified = base::integer(length = 0L),
                      notice = base::character(length = 0L), report = "", report_msg = "", report_file = "")
    if (! .safer_dev_available(lib_path = lib_path, error_text = embed_error_text)) {
      # the seven checks cannot be run: never go on silently to window 2
      res$stage <- "skip"
      res$notice <- base::paste0("The safer-r code verifications need the current (development) version of the saferDev package (https://github.com/safer-r/saferDev), which provides super_assign_check(), lambda_check(), curly_bracket_check(), true_false_check() and for_loop_nb_check(). The saferDev of the R session running this app does not (version: ",
                                 if (base::requireNamespace(package = "saferDev", quietly = TRUE)) {base::as.character(x = utils::packageVersion(pkg = "saferDev", lib.loc = NULL))} else {"none"},
                                 "). Please, load the current saferDev (for instance with devtools::load_all() on its local folder) before saferMake, then run saferMake::make() again.",
                                 collapse = NULL, recycle0 = FALSE)
      base::return(res)
    }
    var_name <- .check_var_name(fun_name = fun_name, lib_path = lib_path, error_text = embed_error_text)
    # saferMake::test() designates the function by the name written in the
    # call (saferDev messages): the call is built with that name and evaluated
    # in env
    env <- base::new.env(hash = TRUE, parent = base::globalenv(), size = 29L)
    base::assign(x = var_name, value = f, pos = -1, envir = env, inherits = FALSE, immediate = TRUE)
    cl <- base::call(name = "test", x = base::as.name(x = var_name), stop_at_first = FALSE,
                     export = FALSE, out_path = ".", safer_check = FALSE, lib_path = lib_path, error_text = error_text) # error_text as received (not embed_error_text), so that the error trail reads "INSIDE saferMake::test() INSIDE saferMake::make()"
    cl[[1]] <- base::call(name = "::", base::as.name(x = "saferMake"), base::as.name(x = "test"))
    # the summary message of test() is captured: it is the report shown to the user
    cap <- .capture_msg(fun = function() {base::eval(expr = cl, envir = env, enclos = base::baseenv())} , lib_path = lib_path, error_text = embed_error_text)
    n_steps <- base::length(x = .code_check_steps)
    if (! base::is.null(x = cap$err)) {
      # saferMake::test() itself stopped (should not happen: errors of the
      # saferDev functions are returned in its output)
      res$stage <- "error"
      res$report <- base::trimws(x = cap$err, which = "both", whitespace = "[ \t\r\n=]")
      res$report_msg <- res$report
      res$notice <- "saferMake::test() stopped with the message below. Please, correct your code in the box below, then click on the Run button again."
      base::return(res)
    }
    out <- cap$value
    if (base::identical(x = out$stage, y = "ok", num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)) {
      base::return(res)
    }
    # full report of test(): status of the seven checks, then the report of each
    # check that modified the code, reported findings or stopped
    res$report <- base::trimws(x = base::paste(cap$msgs, sep = " ", collapse = "\n", recycle0 = FALSE), which = "both", whitespace = "[ \t\r\n]")
    # corrections of the chained checks: put back in the code (also when a later
    # check stops on findings or on an error, so that no correction is lost and
    # the report of the stopping check matches the code of the box)
    res$modified <- base::which(x = out$checks$status == "modified", arr.ind = FALSE, useNames = FALSE)
    if (! base::is.null(x = out$code)) {
      new_code <- .splice_function(code = code, f = f, fun_txt_new = out$code, fun_name = fun_name, lib_path = lib_path, error_text = embed_error_text)
      if (base::identical(x = new_code, y = code, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)) {
        # handing back the same code as "modified" would loop without end
        res$modified <- base::integer(length = 0L)
      } else {
        res$code <- new_code
      }
    }
    # report file: the report of test() without its first line, followed by the
    # modified code (as exported by test(export = TRUE))
    res$report_file <- base::sub(pattern = "^AFTER RUNNING [^\n]*\n", replacement = "", x = res$report, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)
    if (base::length(x = res$modified) > 0L) {
      res$report_file <- base::paste0(res$report_file, "\n\n\nMODIFIED CODE:\n\n", var_name, "_safer <- ", out$code, "\n", collapse = NULL, recycle0 = FALSE)
    }
    # message of window 1: only what the user must correct by hand (the full
    # report is in the report file), i.e., the report of the check that stopped
    # the run and the calls that all_args_here() could not correct
    stop_report <- base::character(length = 0L)
    na_reports <- out$checks$report
    if (out$stage %in% base::c("error", "findings")) {
      stop_report <- base::paste0("REPORT OF CHECK ", out$step, " (saferDev::", .code_check_steps[[out$step]]$fun, "()):\n", out$checks$report[out$step], collapse = NULL, recycle0 = FALSE)
      na_reports <- out$checks$report[- out$step] # already in stop_report
    }
    na_txt <- base::regmatches(x = na_reports, m = base::regexpr(pattern = "CORRECTIONS NOT APPLIED:[\\s\\S]*$", text = na_reports, ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE), invert = FALSE)
    res$report_msg <- base::trimws(x = base::paste(base::c(stop_report, na_txt), sep = " ", collapse = "\n\n", recycle0 = FALSE), which = "both", whitespace = "[ \t\r\n]")
    dl_txt <- "The full report of the checks (followed by the modified code, if any) can be downloaded with the Download report button below."
    # one line per check, in the order of execution: "clear", the modification
    # made, the findings or the error that stopped the run, or "not run"
    step_txt <- base::vapply(X = base::seq_len(length.out = n_steps), FUN = function(i) {
      tempo_status <- out$checks$status[i]
      tempo_txt <- if (tempo_status == "modified" & i %in% res$modified) {
        .code_check_steps[[i]]$done
      } else if (tempo_status %in% base::c("clean", "modified")) {
        "clear."
      } else if (tempo_status == "error") {
        "saferDev stopped with the message given in the report below."
      } else if (tempo_status == "findings") {
        "the check reported the findings given in the report below, that could not be corrected automatically."
      } else {
        base::paste0("not run (the run stopped at check ", out$step, "/", n_steps, ").", collapse = NULL, recycle0 = FALSE)
      }
      base::paste0("Check ", i, "/", n_steps, " (", .code_check_steps[[i]]$label, "): ", tempo_txt, collapse = NULL, recycle0 = FALSE)
    } , FUN.VALUE = base::character(length = 1L), USE.NAMES = FALSE)
    if (out$stage %in% base::c("error", "findings")) {
      i <- out$step
      res$stage <- out$stage
      res$step <- i
      stop_txt <- if (out$stage == "error") {
        "Please, correct your code in the box below, then click on the Run button again."
      } else {
        base::paste0("Please, complete your code in the box below, then click on the Run button again.",
                     if (.code_check_steps[[i]]$key == "args") {" The calls listed under CORRECTIONS NOT APPLIED must be completed by hand, as explained by the REASON given for each of them (e.g., a value written without its argument name); otherwise this check stops again at each Run."} else {""},
                     collapse = NULL, recycle0 = FALSE)
      }
      res$notice <- base::c(
        step_txt,
        if (base::length(x = res$modified) > 0L) {"The corrections above are already in the code of the box below."} else {base::character(length = 0L)},
        stop_txt,
        dl_txt
      )
      base::return(res)
    }
    # stage "modified"
    if (base::length(x = res$modified) == 0L) {
      # saferDev reported a modification that leaves the code unchanged
      res$stage <- "findings"
      res$notice <- base::c("The checks reported the findings given in the report below, that could not be corrected automatically. Please, complete your code in the box below, then click on the Run button again.", dl_txt)
      res$report_msg <- res$report
      base::return(res)
    }
    res$stage <- "modified"
    res$notice <- base::c(
      step_txt,
      "The modified code is now in the box below. Please, check it, then click on the Run button again.",
      dl_txt
    )
    output <- res
    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(output)
    #### end output

}
