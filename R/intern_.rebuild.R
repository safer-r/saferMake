# Verbatim capture of the pasted function and rebuild of the safer-r version.
# Extracted from inst/app/server.R; see header of R/intern_.app_helpers.R.

#' @title Arguments Without Default Value
#' @description
#' Return the names of the arguments of a function that have NO default value.
#' @param f Function.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns A vector of characters of the names of the arguments of \code{f} without default value (\code{character(0)} if none).
#' @details
#' In \code{formals()}, a default-less argument holds the 'missing' object, which is identical to \code{quote(expr = )}. An explicit default of \code{NULL} (e.g. \code{function(x = NULL)}) is NOT 'no default': it has a default (\code{NULL}).
#'
#' Warnings:
#' \itemize{
#'   \item requires \code{saferDev::arg_check}.
#'   \item In the safer Backbone section \code{"######## check of the required functions from the required packages"}, add also \code{saferDev::arg_check} when checking for the presence of the function calling this one.
#' }
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @keywords internal
.args_without_default <- function(
    f,
    lib_path, # required because of saferDev::arg_check()
    error_text # warning: in internal functions, error_text without default value returns a R classical non traced error message (specific of internal functions since classical functions are error_text = "")
){
    # EXAMPLE (because the CRAN does not accept examples for unexported functions)
    # .args_without_default(f = function(x, y = 1, z = NULL){x}, lib_path = NULL, error_text = " INSIDE P1::F1")
    # DEBUGGING
    # f = function(x, y = 1, z = NULL){x} ; lib_path = NULL ; error_text = " INSIDE P1::F1"
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
        "f", 
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
        "f"
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
        "f", 
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
    # but check already done in the main safer function
    ######## end check of the required functions from the required packages

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
    tempo <- saferDev::arg_check(data = f, class = "function", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
        # # "f" # inactivated because not a character argument
        # "lib_path" # inactivated because already checked above
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
    fm <- base::formals(fun = f, envir = base::parent.frame(n = 1))
    if (base::is.null(x = fm)) {base::return(base::character(length = 0L))}
    fm <- base::as.list(x = fm) # pairlist -> plain list, so vapply is safe
    if (base::length(x = fm) == 0L) {base::return(base::character(length = 0L))}
    no_def <- base::vapply(
      X = fm,
      FUN = function(v) {base::identical(x = v, y = base::quote(expr = ),
                                          num.eq = TRUE, single.NA = TRUE,
                                          attrib.as.set = TRUE, ignore.bytecode = TRUE,
                                          ignore.environment = FALSE, ignore.srcref = TRUE,
                                          extptr.as.ref = FALSE)} ,
      FUN.VALUE = base::logical(length = 1L),
      USE.NAMES = TRUE
    )
    output <- base::names(x = fm)[no_def]
    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(output)
    #### end output

}

#' @title Matching Closing Character
#' @description
#' Return the position of the character closing the opening character located at position \code{open} of a string.
#' @param txt Single string.
#' @param open Single positive integer value: position, in \code{txt}, of the opening character (e.g. \code{"("} or \code{"\{"}).
#' @param close_ch Single character: the closing character (e.g. \code{")"} or \code{"\}"}).
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns The position of the matching closing character in \code{txt}, or \code{-1L} if not found.
#' @details
#' Known limitation: string literals are not tracked, so that unbalanced opening or closing characters inside strings give a wrong position.
#'
#' Warnings:
#' \itemize{
#'   \item requires \code{saferDev::arg_check}.
#'   \item In the safer Backbone section \code{"######## check of the required functions from the required packages"}, add also \code{saferDev::arg_check} when checking for the presence of the function calling this one.
#' }
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @keywords internal
.match_close <- function(
    txt,
    open,
    close_ch,
    lib_path, # required because of saferDev::arg_check()
    error_text # warning: in internal functions, error_text without default value returns a R classical non traced error message (specific of internal functions since classical functions are error_text = "")
){
    # EXAMPLE (because the CRAN does not accept examples for unexported functions)
    # .match_close(txt = "f <- function(x){x}", open = 14L, close_ch = ")", lib_path = NULL, error_text = " INSIDE P1::F1")
    # DEBUGGING
    # txt = "f <- function(x){x}" ; open = 14L ; close_ch = ")" ; lib_path = NULL ; error_text = " INSIDE P1::F1"
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
        "txt", 
        "open", 
        "close_ch", 
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
        "txt", 
        "open", 
        "close_ch"
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
        "txt", 
        "open", 
        "close_ch", 
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
    # but check already done in the main safer function
    ######## end check of the required functions from the required packages

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
    tempo <- saferDev::arg_check(data = txt, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = open, class = NULL, typeof = "integer", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = TRUE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = FALSE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = close_ch, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
        # # "txt" # inactivated because can be "" (returns -1L)
        # # "open" # inactivated because not a character argument
        # # "close_ch" # inactivated because can be "" (returns -1L)
        # "lib_path" # inactivated because already checked above
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
    open_ch <- base::substring(text = txt, first = open, last = open)
    chars   <- base::strsplit(x = txt, split = "", fixed = FALSE, perl = FALSE, useBytes = FALSE)[[1]]
    depth   <- 0L
    for (i in open:base::nchar(x = txt, type = "chars", allowNA = FALSE, keepNA = NA)) {
        if (chars[i] == open_ch) {
            depth <- depth + 1L
        } else if (chars[i] == close_ch) {
            depth <- depth - 1L
            if (depth == 0L) {base::return(i)}
        }
    }
    output <- -1L
    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(output)
    #### end output

}

#' @title Extract Signature and Body
#' @description
#' Split the verbatim code of a function into its signature (everything before the closing parenthesis of the arguments) and its body.
#' @param code Single string: the verbatim code of the function (e.g. \code{"f <- function(x){x}"}).
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns A list containing:
#' \itemize{
#'   \item \code{aa}: the signature, without the closing parenthesis of the arguments.
#'   \item \code{body}: the body, without the enclosing curly brackets.
#' }
#' \code{NULL} if the signature or the body cannot be located.
#' @details
#' Known limitation: uses \code{.match_close()}, which does not track string literals.
#'
#' Warnings:
#' \itemize{
#'   \item requires \code{saferDev::arg_check}, \code{.match_close}.
#'   \item In the safer Backbone section \code{"######## check of the required functions from the required packages"}, add also these functions when checking for the presence of the function calling this one.
#' }
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @keywords internal
.extract_aa_body <- function(
    code,
    lib_path, # required because of saferDev::arg_check()
    error_text # warning: in internal functions, error_text without default value returns a R classical non traced error message (specific of internal functions since classical functions are error_text = "")
){
    # EXAMPLE (because the CRAN does not accept examples for unexported functions)
    # .extract_aa_body(code = "f <- function(x){x}", lib_path = NULL, error_text = " INSIDE P1::F1")
    # DEBUGGING
    # code = "f <- function(x){x}" ; lib_path = NULL ; error_text = " INSIDE P1::F1"
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
        "code"
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
    # saferMake:::.match_close is required here
    # but check already done in the main safer function
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .match_close <- utils::getFromNamespace(x = ".match_close", ns = "saferMake", pos = , envir = )

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
        # # "code" # inactivated because can be "" (returns NULL)
        # "lib_path" # inactivated because already checked above
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
    m <- base::regexpr(pattern = "function[[:space:]]*\\(", text = code,
                       ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    if (m == -1) {base::return(NULL)}
    p_open  <- m + base::attr(x = m, which = "match.length", exact = FALSE) - 1L
    p_close <- .match_close(txt = code, open = p_open, close_ch = ")", lib_path = lib_path, error_text = embed_error_text)
    if (p_close == -1) {base::return(NULL)}
    aa <- base::substring(text = code, first = 1L, last = p_close - 1L)

    # NOTE: last = (empty argument, the lazy default) is required for
    # portability: substring()'s default for last is NULL only on R >= 4.6;
    # on R <= 4.5 an explicit last = NULL is an error. Do not "fix" this.
    rest <- base::substring(text = code, first = p_close + 1L, last = )
    m2   <- base::regexpr(pattern = "\\S", text = rest,
                          ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    if (m2 == -1) {base::return(base::list(aa = aa, body = ""))}
    p_body <- p_close + m2

    if (base::substring(text = code, first = p_body, last = p_body) == "{") {
        p_end <- .match_close(txt = code, open = p_body, close_ch = "}", lib_path = lib_path, error_text = embed_error_text)
        if (p_end == -1) {base::return(NULL)}
        body <- base::substring(text = code, first = p_body + 1L, last = p_end - 1L)
        output <- base::list(aa = aa, body = body)
    } else {
        output <- base::list(aa = aa, body = base::trimws(x = rest, which = "both", whitespace = "[ \t\r\n]"))
    }
    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(output)
    #### end output

}

#' @title Rename an Argument into lib_path
#' @description
#' Rename an argument of the submitted function to lib_path, in the verbatim signature (\code{aa}) and in the verbatim body. Used when the user declares, in the "Library path argument" section of the app, an argument whose value is an R library path: the backbone expects this argument to be named lib_path.
#' @param aa Single string: verbatim signature, as returned by \code{.extract_aa_body()}.
#' @param body Single string: verbatim body, as returned by \code{.extract_aa_body()}.
#' @param name Single string: original argument name (e.g. \code{"lib_paths"}). Never \code{"lib_path"} itself (a literal lib_path argument is rejected by the app).
#' @param has_no_default Single logical value. When \code{TRUE}, \code{" = NULL"} is inserted right after the formal declaration in the signature, because the backbone expects lib_path to default to NULL. Only the first formal-looking occurrence is modified.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns A list containing the modified \code{aa} and \code{body}.
#' @details
#' Replacement uses word boundaries: no alphanumeric, "_" or "." character is accepted just before or just after the name, so that renaming "lib_paths" never touches "my_lib_paths" or "lib_paths2".
#'
#' Known limitations (same class as \code{.extract_aa_body()}, which does not track string literals either): occurrences of the name inside string literals, and list member accesses like df$name, are also renamed; and if another argument default value uses the renamed argument inside a call (e.g. \code{function(x = paste(lib_paths), lib_paths)}), the "= NULL" insertion can hit that occurrence instead of the formal declaration. If the function name itself equals the renamed argument name (e.g. \code{lib_paths <- function(x, lib_paths)}), the function name is also renamed in the signature.
#'
#' Warnings:
#' \itemize{
#'   \item requires \code{saferDev::arg_check}.
#'   \item In the safer Backbone section \code{"######## check of the required functions from the required packages"}, add also \code{saferDev::arg_check} when checking for the presence of the function calling this one.
#' }
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @keywords internal
.rename_lib_path_arg <- function(
    aa,
    body,
    name,
    has_no_default = FALSE,
    lib_path, # required because of saferDev::arg_check()
    error_text # warning: in internal functions, error_text without default value returns a R classical non traced error message (specific of internal functions since classical functions are error_text = "")
){
    # EXAMPLE (because the CRAN does not accept examples for unexported functions)
    # .rename_lib_path_arg(aa = "f <- function(x, lib_paths", body = "x", name = "lib_paths", has_no_default = TRUE, lib_path = NULL, error_text = " INSIDE P1::F1")
    # DEBUGGING
    # aa = "f <- function(x, lib_paths" ; body = "x" ; name = "lib_paths" ; has_no_default = TRUE ; lib_path = NULL ; error_text = " INSIDE P1::F1"
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
        "aa", 
        "body", 
        "name", 
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
        "aa", 
        "body", 
        "name", 
        "has_no_default"
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
        "aa", 
        "body", 
        "name", 
        "has_no_default", 
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
    # but check already done in the main safer function
    ######## end check of the required functions from the required packages

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
    tempo <- saferDev::arg_check(data = aa, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = body, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = name, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = has_no_default, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
        # # "aa" # inactivated because the code is left unchanged when ""
        # # "body" # inactivated because can be "" (empty body)
        # # "name" # inactivated because the code is left unchanged when ""
        # # "has_no_default" # inactivated because not a character argument
        # "lib_path" # inactivated because already checked above
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
    # \Q...\E quoting of the verbatim name: a literal \E inside the name would
    # terminate the quoting early, so escape it ("\\E\\E" resumes the quoting)
    name_q <- base::gsub(pattern = "\\\\E", replacement = "\\\\E\\\\E", x = name,
                         ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)
    # word-boundary pattern around the (verbatim) argument name
    bound <- base::paste0("(?<![A-Za-z0-9_.])\\Q", name_q, "\\E(?![A-Za-z0-9_.])",
                          collapse = NULL, recycle0 = FALSE)
    if (base::isTRUE(x = has_no_default)) {
        # a formal declaration is always preceded by "(", "," or a newline (plus
        # optional spacing) inside the signature: \A cannot occur because aa
        # always starts with "<fun_name> <- function("
        form_pat <- base::paste0(
          "((?:[(,]|\\n)[ \\t\\n]*)(?<![A-Za-z0-9_.])\\Q", name_q,
          "\\E(?![A-Za-z0-9_.])(?![ \\t]*=)",
          collapse = NULL,
          recycle0 = FALSE
        )
        aa <- base::sub(pattern = form_pat,
                        replacement = base::paste0("\\1", name, " = NULL",
                                                   collapse = NULL, recycle0 = FALSE),
                        x = aa, ignore.case = FALSE, perl = TRUE, fixed = FALSE,
                        useBytes = FALSE)
    }
    aa   <- base::gsub(pattern = bound, replacement = "lib_path", x = aa,
                       ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)
    body <- base::gsub(pattern = bound, replacement = "lib_path", x = body,
                       ignore.case = FALSE, perl = TRUE, fixed = FALSE, useBytes = FALSE)
    output <- base::list(aa = aa, body = body)
    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(output)
    #### end output

}

#' @title Build the Safer-R Function
#' @description
#' Build the code of the safer-r version of the submitted function: the verbatim signature and body wrapped in the safer-r backbone (v21.1).
#' @param aa Single string: verbatim signature, as returned by \code{.extract_aa_body()}.
#' @param body Single string: verbatim body, as returned by \code{.extract_aa_body()}.
#' @param pkg Single string: package name of the generated function (\code{""} for no package, i.e., \code{package_name <- NULL}).
#' @param link Single string: internal error report link of the generated function (\code{""} for no link, i.e., \code{NULL}).
#' @param null_args Vector of characters: argument names that ACCEPT NULL, commented out in the tempo_arg of the NULL section.
#' @param non_null_args Vector of characters: argument names that must NOT be NULL, active in the tempo_arg of the NULL section.
#' @param empty_args Vector of characters: argument names that ACCEPT empty non NULL values, commented out in the tempo_arg of the empty section.
#' @param non_empty_args Vector of characters: argument names that must NOT be empty, active there.
#' @param no_default_args Vector of characters: argument names with NO default value (conditional section).
#' @param fun_args Vector of characters: all argument names (to emit the arg_check() blocks).
#' @param arg_check_settings Named list (key = \code{.arg_id(nm)}) of arg_check() settings.
#' @param no_empty_string_args Vector of characters: argument names checked as unable to contain \code{""} (ticked boxes). The "management of \code{""}" section is emitted only when this vector is not empty, and these names are ACTIVE in its tempo_arg.
#' @param seed_arg \code{NULL} (section not inserted), \code{NA} or \code{""} (blank "seed argument" field: only the section delimiters are emitted) or a single string (the user's argument name used as the seed value of set.seed(): full section emitted).
#' @param graphic_dev Single logical value: \code{TRUE} (full graphic device checking section emitted), \code{FALSE} (only the section delimiters emitted).
#' @param user_lib_path Single logical value: \code{TRUE} when the user function provides its own lib_path argument (declared in the "Library path argument" section of the app and renamed to lib_path by the caller): the signature does NOT append a second lib_path = NULL (the user formal is kept, with its own default), and "lib_path" is excluded from all the per-argument checking lists (NULL, empty, \code{""}, arg_check()): it is covered by the standard commented safer-r lines of the tempo_arg blocks and by the backbone "check of lib_path" section.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns A single string: the code of the safer-r function.
#' @details
#' The \code{lib_path} and \code{error_text} arguments concern \code{.build_rebuilt()} itself, not the generated function (whose lib_path is managed by \code{user_lib_path}).
#'
#' Warnings:
#' \itemize{
#'   \item requires \code{saferDev::arg_check}, \code{.build_arg_check_section}.
#'   \item In the safer Backbone section \code{"######## check of the required functions from the required packages"}, add also these functions when checking for the presence of the function calling this one.
#' }
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @keywords internal
.build_rebuilt <- function(
    aa,
    body,
    pkg,
    link,
    null_args = base::character(length = 0L),
    non_null_args = base::character(length = 0L),
    empty_args = base::character(length = 0L),
    non_empty_args = base::character(length = 0L),
    no_default_args = base::character(length = 0L),
    fun_args = base::character(length = 0L),
    arg_check_settings = base::list(),
    no_empty_string_args = base::character(length = 0L),
    seed_arg = NULL,
    graphic_dev = FALSE,
    user_lib_path = FALSE,
    lib_path, # required because of saferDev::arg_check()
    error_text # warning: in internal functions, error_text without default value returns a R classical non traced error message (specific of internal functions since classical functions are error_text = "")
){
    # EXAMPLE (because the CRAN does not accept examples for unexported functions)
    # .build_rebuilt(aa = "f <- function(x", body = "x", pkg = "", link = "", null_args = base::character(length = 0L), non_null_args = "x", empty_args = base::character(length = 0L), non_empty_args = "x", no_default_args = "x", fun_args = "x", arg_check_settings = base::list(), no_empty_string_args = base::character(length = 0L), seed_arg = NULL, graphic_dev = FALSE, user_lib_path = FALSE, lib_path = NULL, error_text = " INSIDE P1::F1")
    # DEBUGGING
    # aa = "f <- function(x" ; body = "x" ; pkg = "" ; link = "" ; null_args = base::character(length = 0L) ; non_null_args = "x" ; empty_args = base::character(length = 0L) ; non_empty_args = "x" ; no_default_args = "x" ; fun_args = "x" ; arg_check_settings = base::list() ; no_empty_string_args = base::character(length = 0L) ; seed_arg = NULL ; graphic_dev = FALSE ; user_lib_path = FALSE ; lib_path = NULL ; error_text = " INSIDE P1::F1"
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
        "aa", 
        "body", 
        "pkg", 
        "link", 
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
        "aa", 
        "body", 
        "pkg", 
        "link", 
        "null_args", 
        "non_null_args", 
        "empty_args", 
        "non_empty_args", 
        "no_default_args", 
        "fun_args", 
        "arg_check_settings", 
        "no_empty_string_args", 
        "graphic_dev", 
        "user_lib_path"
        # "seed_arg", # inactivated because can be NULL
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
        "aa", 
        "body", 
        "pkg", 
        "link", 
        "seed_arg", 
        "graphic_dev", 
        "user_lib_path", 
        "lib_path"
        # "null_args", # inactivated because can be an empty non NULL object
        # "non_null_args", # inactivated because can be an empty non NULL object
        # "empty_args", # inactivated because can be an empty non NULL object
        # "non_empty_args", # inactivated because can be an empty non NULL object
        # "no_default_args", # inactivated because can be an empty non NULL object
        # "fun_args", # inactivated because can be an empty non NULL object
        # "arg_check_settings", # inactivated because can be an empty non NULL object
        # "no_empty_string_args", # inactivated because can be an empty non NULL object
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
    # saferMake:::.build_arg_check_section is required here
    # but check already done in the main safer function
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .build_arg_check_section <- utils::getFromNamespace(x = ".build_arg_check_section", ns = "saferMake", pos = , envir = )

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
    tempo <- saferDev::arg_check(data = aa, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = body, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = pkg, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = link, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = null_args, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = non_null_args, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = empty_args, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = non_empty_args, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = no_default_args, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = fun_args, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = arg_check_settings, class = "list", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = no_empty_string_args, class = NULL, typeof = "character", mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if( ! base::is.null(x = seed_arg)){
        tempo <- saferDev::arg_check(data = seed_arg, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    }
    tempo <- saferDev::arg_check(data = graphic_dev, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = user_lib_path, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
        # # "aa" # inactivated because written verbatim in the output
        # # "body" # inactivated because can be "" (empty body)
        # # "pkg" # inactivated because can be "" (package_name <- NULL)
        # # "link" # inactivated because can be "" (no internal error report link)
        # # "null_args", "non_null_args", "empty_args", "non_empty_args", "no_default_args", "fun_args", "no_empty_string_args" # inactivated because argument names, never ""
        # # "seed_arg" # inactivated because can be "" (normalized to NA: blank seed field)
        # # "arg_check_settings", "graphic_dev", "user_lib_path" # inactivated because not character arguments
        # "lib_path" # inactivated because already checked above
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
    aa   <- base::sub(pattern = "[[:space:]]+$", replacement = "", x = aa,
                      ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    body <- base::sub(pattern = "[[:space:]]+$", replacement = "",
                      x = base::sub(pattern = "^[[:space:]]*\n", replacement = "", x = body,
                                    ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE),
                      ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)

    # ---- user-provided lib_path argument ------------------------------------
    # When the user function provides its own lib_path argument (renamed from
    # its original name by the caller, and signalled with user_lib_path = TRUE),
    # the name appears among fun_args but must not be treated as a "regular"
    # user argument in the per-argument checking lists: it is covered by the
    # backbone "check of lib_path" section, the standard commented safer-r lines
    # of the tempo_arg blocks, and no arg_check() line is generated for it.
    # Note: a literal lib_path argument in the pasted code is rejected upstream
    # by the app (reserved name), so this can only come from the rename flow.
    if (base::isTRUE(x = user_lib_path)) {
        null_args            <- base::setdiff(x = null_args,            y = "lib_path")
        non_null_args        <- base::setdiff(x = non_null_args,        y = "lib_path")
        empty_args           <- base::setdiff(x = empty_args,           y = "lib_path")
        non_empty_args       <- base::setdiff(x = non_empty_args,       y = "lib_path")
        no_default_args      <- base::setdiff(x = no_default_args,      y = "lib_path")
        fun_args             <- base::setdiff(x = fun_args,             y = "lib_path")
        no_empty_string_args <- base::setdiff(x = no_empty_string_args, y = "lib_path")
    }

    comma <- if (base::grepl(pattern = "\\($", x = aa,
                             ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) ||
                 base::grepl(pattern = ",$", x = aa,
                             ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)) {""} else {","}

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
      FUN = function(nm) {base::paste0(
          "        ",
          base::deparse(expr = nm, width.cutoff = 500L, backtick = FALSE,
                        control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                        nlines = -1L),
          ", ",
          collapse = NULL, recycle0 = FALSE
        )} ,
      FUN.VALUE = base::character(length = 1L),
      USE.NAMES = TRUE
    )
    null_lines <- base::vapply(
      X = null_args,
      FUN = function(nm) {base::paste0(
          "        # ",
          base::deparse(expr = nm, width.cutoff = 500L, backtick = FALSE,
                        control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                        nlines = -1L),
          ", # inactivated because can be NULL",
          collapse = NULL, recycle0 = FALSE
        )} ,
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
          FUN = function(nm) {base::paste0(
              "        ",
              base::deparse(expr = nm, width.cutoff = 500L, backtick = FALSE,
                            control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                            nlines = -1L),
              collapse = NULL, recycle0 = FALSE
            )} ,
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
      FUN = function(nm) {base::paste0(
          "        ",
          base::deparse(expr = nm, width.cutoff = 500L, backtick = FALSE,
                        control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                        nlines = -1L),
          ", ",
          collapse = NULL, recycle0 = FALSE
        )} ,
      FUN.VALUE = base::character(length = 1L),
      USE.NAMES = TRUE
    )
    can_empty_lines <- base::vapply(
      X = empty_args,
      FUN = function(nm) {base::paste0(
          "        # ",
          base::deparse(expr = nm, width.cutoff = 500L, backtick = FALSE,
                        control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                        nlines = -1L),
          ", # inactivated because can be an empty non NULL object",
          collapse = NULL, recycle0 = FALSE
        )} ,
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
      FUN = function(nm) {base::paste0(
          "        ",
          base::deparse(expr = nm, width.cutoff = 500L, backtick = FALSE,
                        control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                        nlines = -1L),
          collapse = NULL, recycle0 = FALSE
        )} ,
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

    # ---- "code that protects set.seed()" section -----------------------------
    # Emitted according to the "seed argument" web page field:
    #   NULL       -> not emitted at all (field not applicable)
    #   NA         -> blank field: only the section delimiters are emitted
    #   "name"     -> full section, with `seed` replaced by the argument name
    # The argument name is emitted using deparse() with backticks, so names like
    # `a b` or `if` remain valid R after rebuilding.
    # "" is normalized to NA first (same behaviour: blank field).
    if (! base::is.null(x = seed_arg) &&
        base::length(x = seed_arg) == 1L &&
        ! base::nzchar(x = seed_arg, keepNA = FALSE)) {
        seed_arg <- NA_character_
    }
    seed_block <- if (base::is.null(x = seed_arg)) {
        NULL
    } else if (base::is.na(x = seed_arg)) {
        base::paste0(
          "    ######## code that protects set.seed() in the global environment\n",
          "    ######## end code that protects set.seed() in the global environment\n",
          "\n",
          collapse = NULL,
          recycle0 = FALSE
        )
    } else {
        seed_nm <- base::deparse(expr = base::as.name(x = seed_arg),
                                 width.cutoff = 500L, backtick = TRUE,
                                 control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                                 nlines = -1L)
        base::paste0(
          "    ######## code that protects set.seed() in the global environment\n",
          "    # optional section: remove the code if your own function has no seed argument\n",
          "    # Warning: seeding is always at the .GlobalEnv level, whenever the seeding is applied inside a function, another envir, etc.\n",
          "    if (base::exists(x = \".Random.seed\", where = -1, envir = .GlobalEnv, frame = , mode = \"any\", inherits = TRUE)) {\n",
          "        # if .Random.seed does not exists, it means that no random operation has been performed yet in any R environment\n",
          "        tempo_random_seed <- .Random.seed\n",
          "        base::on.exit(expr = base::assign(x = \".Random.seed\", value = tempo_random_seed, pos = -1, envir = .GlobalEnv, inherits = FALSE, immediate = TRUE), add = TRUE, after = TRUE)\n",
          "    }else{\n",
          "        base::on.exit(expr = base::set.seed(seed = NULL, kind = NULL, normal.kind = NULL, sample.kind = NULL), add = TRUE, after = TRUE) # inactivate seeding -> return to complete randomness\n",
          "    }\n",
          "    base::set.seed(seed = ", seed_nm, ", kind = NULL, normal.kind = NULL, sample.kind = NULL) # seed value is the seed argument of the function\n",
          "    ######## end code that protects set.seed() in the global environment\n",
          "\n",
          collapse = NULL,
          recycle0 = FALSE
        )
    }

    # ---- "graphic device checking" section ------------------------------------
    # Emitted according to the "Graphic device management" tick box:
    #   TRUE  -> full section (device count check on exit + par() restoration)
    #   FALSE -> only the section delimiters are emitted
    graphic_block <- if (base::isTRUE(x = graphic_dev)) {
        base::paste0(
          "    ######## graphic device checking\n",
          "    # optional section: remove the code if no graphics used in your functions\n",
          "    # check the number of graphic devices on exit\n",
          "    dev_list <- grDevices::dev.list() \n",
          "    # This check is here in case the developer has not correctly fill tempo_arg\n",
          "    # nocov start\n",
          "    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.\n",
          "    base::on.exit(\n",
          "        expr = if(base::length(x = dev_list) != base::length(x = grDevices::dev.list())){\n",
          "            tempo_cat <- base::paste0(\n",
          "                \"INTERNAL ERROR IN THE BACKBONE PART OF \", \n",
          "                intern_error_text_start, \n",
          "                \"SOME GRAPHIC DEVICES WERE OPENED BY \", \n",
          "                function_name, \n",
          "                \" BUT NOT CLOSED BEFORE END OF EXECUTION.\\n\\nIF IT IS EXPECTED, JUST REMOVE THE CODE DISPLAYING THIS MESSAGE INSIDE \", \n",
          "                function_name, \n",
          "                \".\\n\\nOTHERWISE, THE PROBLEM COMES FROM OPENED GRAPHIC DEVICES BEFORE RUNNING \", \n",
          "                function_name, \n",
          "                \" (n = \", \n",
          "                base::length(x = dev_list), \n",
          "                \") AND AFTER (n = \", \n",
          "                base::length(x = grDevices::dev.list()), \n",
          "                \").\", \n",
          "                intern_error_text_end, \n",
          "                collapse = NULL, \n",
          "                recycle0 = FALSE\n",
          "            )\n",
          "            base::stop(base::paste0(\"\\n\\n================\\n\\n\", tempo_cat, \"\\n\\n================\\n\\n\", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)\n",
          "\n",
          "        }, \n",
          "        add = TRUE, \n",
          "        after = TRUE\n",
          "    )\n",
          "    # nocov end\n",
          "    # end check the number of graphic devices on exit\n",
          "    # restore the graphic parameters on exit\n",
          "    if(base::length(x = grDevices::dev.list()) > 0){\n",
          "        par_ini <- base::suppressWarnings(expr = graphics::par(no.readonly = TRUE), classes = \"warning\") # to recover the present graphical parameters\n",
          "        base::on.exit(expr = base::suppressWarnings(expr = graphics::par(par_ini, no.readonly = TRUE), classes = \"warning\"), add = TRUE, after = TRUE)\n",
          "    }\n",
          "    # end restore the graphic parameters on exit\n",
          "    ######## end graphic device checking\n",
          "\n",
          collapse = NULL,
          recycle0 = FALSE
        )
    } else {
        base::paste0(
          "    ######## graphic device checking\n",
          "    ######## end graphic device checking\n",
          "\n",
          collapse = NULL,
          recycle0 = FALSE
        )
    }

    output <- base::paste0(
      aa, comma,
      if (base::isTRUE(x = user_lib_path)) {
          "\n    safer_check = TRUE, \n    error_text = \"\" \n){\n"
      } else {
          "\n    lib_path = NULL, \n    safer_check = TRUE, \n    error_text = \"\" \n){\n"
      },
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
      "    if(base::any(tempo_log, na.rm = TRUE)) {\n",
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
      "                    base::all(x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK\n",
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
      "    # always executed: lib_path is always checked, whatever the value of safer_check\n",
      "    if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.\n",
      "        if( ! base::all(base::typeof(x = lib_path) == \"character\", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == \"character\" returns FALSE)\n",
      "            if(base::all(base::mode(x = lib_path) == \"function\", na.rm = TRUE)){\n",
      "                lib_path <- base::deparse1(expr = lib_path, collapse = \"\", width.cutoff = 500L)\n",
      "            }\n",
      "            tempo_cat <- base::paste0(\n",
      "                error_text_start, \n",
      "                \"THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\\nHERE IT IS:\\n\", \n",
      "                base::ifelse(test = base::length(x = lib_path) == 0 | base::all(base::suppressWarnings(expr = lib_path == base::quote(expr = ), classes = \"warning\"), na.rm = TRUE), yes = \"<NULL, EMPTY OBJECT OR EMPTY NAME>\", no = base::paste0(lib_path, collapse = \"\\n\", recycle0 = FALSE)),\n",
      "                collapse = NULL, \n",
      "                recycle0 = FALSE\n",
      "            )\n",
      "            base::stop(base::paste0(\"\\n\\n================\\n\\n\", tempo_cat, \"\\n\\n================\\n\\n\", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)\n",
      "        }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA. dir.exists(paths = NA) returns an error, so ok. dir.exists(paths = \"\") returns FALSE so ok\n",
      "            tempo_log <- ! base::dir.exists(paths = lib_path)\n",
      "            tempo_cat_b <- lib_path[tempo_log] # here lib_path is character string\n",
      "            tempo_cat_b[tempo_cat_b == \"\"] <- \"\\\"\\\"\"\n",
      "            tempo_cat <- base::paste0(\n",
      "                error_text_start, \n",
      "                \"THE DIRECTORY PATH\",\n",
      "                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = \"S\", no = \"\"), \n",
      "                \" INDICATED IN THE lib_path ARGUMENT DO\", \n",
      "                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = \"\", no = \"ES\"), \n",
      "                \" NOT EXIST:\\n\", \n",
      "                base::paste0(tempo_cat_b, collapse = \"\\n\", recycle0 = FALSE), \n",
      "                collapse = NULL, \n",
      "                recycle0 = FALSE\n",
      "            )\n",
      "            base::stop(base::paste0(\"\\n\\n================\\n\\n\", tempo_cat, \"\\n\\n================\\n\\n\", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)\n",
      "        }else{\n",
      "            ini_lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument\n",
      "            base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()\n",
      "            base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = \"/$|\\\\\\\\$\", replacement = \"\", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\\\ in path\n",
      "            lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument\n",
      "        }\n",
      "    }else{\n",
      "        lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base:::.libPaths(), lib_path))\n",
      "    }\n",
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
      # arguments commented out (or absent) in the tempo_arg of the NULL
      # management section accept NULL: their arg_check() line is wrapped in
      # if( ! base::is.null(x = <arg>)){ ... } (skipped when NULL).
      "\n",
      .build_arg_check_section(fun_args = fun_args, arg_check_settings = arg_check_settings,
                               extra_section = no_empty_block,
                               wrap_null_args = base::setdiff(x = fun_args, y = non_null_args), lib_path = lib_path, error_text = embed_error_text),

      # ---- "second round of checking and data preparation" section -------
      # (seed protection + graphic device checking, both user-controlled)
      "    #### second round of checking and data preparation\n",
      "\n",
      seed_block,
      graphic_block,
      "    #### end second round of checking and data preparation\n",
      "\n",

      "\n    #### main code\n",
      body,
      "\n    #### end main code\n",
      "}\n",
      collapse = NULL,
      recycle0 = FALSE
    )
    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(output)
    #### end output

}

