# Shared builder of the saferDev::arg_check(...) call text (no leading spaces,
# no "; base::eval(...)" suffix). Used both by the code generator and by the
# interactive test runner, so the tested line is identical to the generated one.
#' @title Arg Check Call Core
#' @description
#' Build the text of a \code{saferDev::arg_check(...)} call (no leading spaces, no \code{"; base::eval(...)"} suffix) from the settings of one argument. Used both by the code generator (\code{.build_arg_check_section()}) and by the interactive test runner (\code{.run_arg_check_tests()}), so that the tested line is identical to the generated one.
#' @param data_txt Single string of the R code written after \code{data = } (name of the argument, or code of a test value).
#' @param st List of the arg_check() settings of the argument (same structure as \code{.arg_check_defaults}).
#' @param lib_path_txt Single string of the R code written after \code{lib_path = } (e.g., \code{"lib_path"} or \code{"NULL"}).
#' @param error_text_txt Single string of the R code written after \code{error_text = } (e.g., \code{"embed_error_text"}, or the R code of an empty string).
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns A single string of the \code{saferDev::arg_check(...)} call.
#' @details
#' Warnings:
#' \itemize{
#'   \item requires \code{saferDev::arg_check}, \code{saferMake:::.arg_check_parse_length} and \code{saferMake:::.arg_check_parse_options}.
#'   \item In the safer Backbone section \code{"######## check of the required functions from the required packages"}, add also \code{saferDev::arg_check}, \code{saferMake:::.arg_check_parse_length} and \code{saferMake:::.arg_check_parse_options} when checking for the presence of \code{.arg_check_call_core}.
#' }
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @keywords internal
.arg_check_call_core <- function(
    data_txt,
    st,
    lib_path_txt,
    error_text_txt,
    lib_path, # required because of saferDev::arg_check()
    error_text # warning: in internal functions, error_text without default value returns a R classical non traced error message (specific of internal functions since classical functions are error_text = "")
){
    # EXAMPLE (because the CRAN does not accept examples for unexported functions)
    # .arg_check_call_core(data_txt = "x", st = .arg_check_defaults, lib_path_txt = "lib_path", error_text_txt = "embed_error_text", lib_path = NULL, error_text = " INSIDE P1::F1")
    # DEBUGGING
    # data_txt = "x" ; st = saferMake:::.arg_check_defaults ; lib_path_txt = "lib_path" ; error_text_txt = "embed_error_text" ; lib_path = NULL ; error_text = " INSIDE P1::F1"
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
        "data_txt",
        "st",
        "lib_path_txt",
        "error_text_txt",
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
        "data_txt",
        "st",
        "lib_path_txt",
        "error_text_txt"
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
        "data_txt",
        "st",
        "lib_path_txt",
        "error_text_txt",
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
    # .arg_check_parse_length is required here
    # .arg_check_parse_options is required here
    # but check already done in the main safer function
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .arg_check_parse_length <- utils::getFromNamespace(x = ".arg_check_parse_length", ns = "saferMake", pos = , envir = )
    .arg_check_parse_options <- utils::getFromNamespace(x = ".arg_check_parse_options", ns = "saferMake", pos = , envir = )

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
    tempo <- saferDev::arg_check(data = data_txt, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = st, class = "list", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = lib_path_txt, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = error_text_txt, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
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
    tempo_arg <- base::c(
        "data_txt",
        "lib_path_txt",
        "error_text_txt"
        # "lib_path" # inactivated because already checked above
        # "error_text" # inactivated because can be ""
    )
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    tempo_log <- ! base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){if(base::is.null(x = x)){base::return(TRUE)}else{base::all(base::mode(x = x) == "character", na.rm = TRUE)}}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame.  #  need to test is.null() here
    if(base::any(tempo_log, na.rm = TRUE)){
        # This check is here in case the developer has not correctly fill tempo_arg
        tempo_cat <- base::paste0(
            "INTERNAL ERROR IN THE BACKBONE PART OF ", 
            intern_error_text_start, 
            "IN THE SECTION \"management of \"\" in arguments of mode character\"\n", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS ARE", no = "THIS ARGUMENT IS"), 
            " NOT CLASS \"character\":\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            "\nIf saferMake::saferMake() HAS BEEN USED TO MODIFY YOUR FUNCTION, PLEASE RESTART WITH YOUR MAIN CODE AND SELECT PROPER CLASS, TYPE AND/OR MODE FOR YOUR ARGUMENTS\n.HERE, ARGUMENTS ARE TYPE:\n", 
            base::paste0(base::sapply(X = tempo_arg, FUN = function(x){base::paste0(x, ": \"", base::typeof(x = base::get(x = x, pos = -1L, envir = base::parent.frame(n = 3), mode = "any", inherits = FALSE)), "\"", collapse = NULL, recycle0 = FALSE)}, simplify = TRUE, USE.NAMES = TRUE), collapse = "\n", recycle0 = FALSE), # parent.frame(n = 3) because the get() is evaluated inside the FUN closure of sapply. But sapply(FUN) calls lapply(FUN) internally !! So parent.frame(n = 1) is the internal lapply() frame called by sapply, parent.frame(n = 2) is sapply's frame, parent.frame(n = 3) is BACKBONE's frame. NB: paste0() and typeof() are primitives and add no frame.
            intern_error_text_end, 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }else{
        tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::any(x == "", na.rm = TRUE)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame. # for character argument that can also be NULL, if NULL -> returns FALSE. Thus no need to test is.null()
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS\n", no = "THIS ARGUMENT\n"), 
                base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE),
                "\nCANNOT CONTAIN EMPTY STRING \"\".", 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL) # == in stop() to be able to add several messages between ==
        }
    }
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
    class_val  <- if (base::identical(x = st$class, y = "NULL", num.eq = TRUE, single.NA = TRUE,
                                       attrib.as.set = TRUE, ignore.bytecode = TRUE,
                                       ignore.environment = FALSE, ignore.srcref = TRUE,
                                       extptr.as.ref = FALSE)) {"NULL"}
                   else {base::deparse(expr = st$class, width.cutoff = 60L, backtick = FALSE,
                                      control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                                      nlines = -1L)}
    typeof_val  <- if (base::identical(x = st$typeof, y = "NULL", num.eq = TRUE, single.NA = TRUE,
                                       attrib.as.set = TRUE, ignore.bytecode = TRUE,
                                       ignore.environment = FALSE, ignore.srcref = TRUE,
                                       extptr.as.ref = FALSE)) {"NULL"}
                   else {base::deparse(expr = st$typeof, width.cutoff = 60L, backtick = FALSE,
                                      control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                                      nlines = -1L)}
    mode_val    <- if (base::identical(x = st$mode, y = "NULL", num.eq = TRUE, single.NA = TRUE,
                                       attrib.as.set = TRUE, ignore.bytecode = TRUE,
                                       ignore.environment = FALSE, ignore.srcref = TRUE,
                                       extptr.as.ref = FALSE)) {"NULL"}
                   else {base::deparse(expr = st$mode, width.cutoff = 60L, backtick = FALSE,
                                      control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                                      nlines = -1L)}
    len_val     <- .arg_check_parse_length(txt = st$length, lib_path = lib_path, error_text = embed_error_text)
    length_val  <- if (base::is.null(x = len_val)) {"NULL"}
                   else {base::paste0(len_val, "L", collapse = NULL, recycle0 = FALSE)}
    opts        <- .arg_check_parse_options(txt = st$options, lib_path = lib_path, error_text = embed_error_text)
    options_val <- if (base::is.null(x = opts)) {"NULL"}
                   else if (base::identical(x = opts, y = "NULL", num.eq = TRUE, single.NA = TRUE,
                                            attrib.as.set = TRUE, ignore.bytecode = TRUE,
                                            ignore.environment = FALSE, ignore.srcref = TRUE,
                                            extptr.as.ref = FALSE)) {"NULL"}
                   else {base::deparse(expr = opts, width.cutoff = 60L, backtick = FALSE,
                                      control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"),
                                      nlines = -1L)}
    output <- base::paste0(
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
    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(output)
    #### end output

}
