#' @title Safer-r code checks of a function
#' @description
#' Run, in this order, the seven code checks of the safer-r project on a function, each with the dedicated function of the saferDev package:
#'
#' 1. \code{saferDev::super_assign_check()}: no superassignment \code{<<-} or \code{->>}. This check never modifies the code: it returns an error (status \code{"error"}) when a superassignment is detected, because what these operators modify cannot be guessed from the code.
#'
#' 2. \code{saferDev::lambda_check()}: no lambda function written \code{\\(x)}.
#'
#' 3. \code{saferDev::curly_bracket_check()}: curly brackets \code{\{ \}} written after \code{if}, \code{else}, \code{for}, \code{while}, \code{repeat} and \code{function}.
#'
#' 4. \code{saferDev::true_false_check()}: \code{TRUE} and \code{FALSE} written in full (no \code{T} or \code{F}).
#'
#' 5. \code{saferDev::for_loop_nb_check()}: a checking line of the sequence added before the \code{for()} loops whose number of iterations is not fixed by the writing itself (like \code{1:length(x)}).
#'
#' 6. \code{saferDev::colons_check()}: package attribution (\code{<package>::}) of the called functions.
#'
#' 7. \code{saferDev::all_args_here()}: all the arguments of the called functions written.
#'
#' These are the checks run by the Run button of the first window of the \code{saferMake::make()} app, that calls this function.
#' @param x A function name, written without quotes and brackets.
#' @param stop_at_first Single logical value. Stop at the first check that modifies the code? If \code{FALSE} (default), the checks are chained: each check is run on the function corrected by the previous checks, so that the returned function is corrected by the seven checks. Whatever this argument, the run stops at the first check that reports findings that it cannot correct itself, or that stops with an error, because the next checks can depend on these findings (e.g., \code{saferDev::all_args_here()} needs the package attributions checked by \code{saferDev::colons_check()}). If \code{TRUE}, the code returned is the one corrected by the check that stopped the run only, so that each correction can be checked before running the checks again. The \code{saferMake::make()} app uses \code{FALSE} and displays the message of this function (see the returned value) to the user.
#' @param export Single logical value. Write the corrected function into a file? If \code{TRUE}, the file \code{<function name>_safer.R} is written in \code{out_path} (e.g., \code{fun_safer.R} for \code{x = fun}), containing an assignment of the corrected function (\code{fun_safer <- function(...) ...}), so that \code{base::source()}ing it recreates the corrected function. Nothing is exported if nothing has been corrected. An error is returned if the file already exists, and if \code{x} is not designated by a function name (like \code{fun} or \code{package::fun}).
#' @param out_path Single character string indicating the path of the folder where to write the file when \code{export = TRUE}. \code{"."} (default) means the R working directory. Ignored if \code{export = FALSE}.
#' @param safer_check Single logical value. Perform some "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) correct \code{lib_path} argument value 2) required functions and related packages effectively present in local R libraries and 3) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default): only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns
#' A message is always printed, with the result of each check and the reports of the saferDev functions.
#'
#' A list, returned invisibly when nothing has been corrected, made of:
#'
#' - \code{$stage}: single character string. \code{"ok"} (the seven checks report nothing), \code{"modified"} (at least one check has corrected the code, and no check reports findings), \code{"findings"} (a check reports something that it cannot correct itself, like a non basic function written without its package, or a value written without its argument name for \code{all_args_here()}: the code must be completed by the user), or \code{"error"} (a saferDev function stopped with an error, which is the expected result of \code{saferDev::super_assign_check()} when a superassignment is detected). The next checks are not run after \code{"findings"} or \code{"error"}.
#'
#' - \code{$step}: single integer. Number (1 to 7) of the check that stopped the run (findings, error, or modification with \code{stop_at_first = TRUE}), \code{NULL} otherwise.
#'
#' - \code{$fun}: the corrected function (with the environment of \code{x}), or \code{NULL} if nothing has been corrected.
#'
#' - \code{$code}: single character string. Code of the corrected function (starting with \code{function}), or \code{NULL} if nothing has been corrected.
#'
#' - \code{$checks}: data frame with one row per check (columns \code{check}: name of the saferDev function; \code{status}: \code{"clean"}, \code{"modified"}, \code{"findings"}, \code{"error"} or \code{"not run"}; \code{report}: text printed by the saferDev function, or its error message).
#' @details
#' Requires a version of the saferDev package that provides the seven functions above (current version of saferDev, see \url{https://github.com/safer-r/saferDev}).
#'
#' Warning: \code{saferDev::lambda_check()} only sees the \code{\\(x)} writing in a function carrying its source references (attribute \code{srcref}, e.g., after a \code{base::parse(keep.source = TRUE)}, see the examples). The function corrected by a check is rebuilt with its source references before the next check.
#'
#' The comments and the indentation of the code are kept by the seven checks. Warning: comments are only available when \code{x} carries its source references (attribute \code{srcref}). Otherwise, the code is the deparsed one: no comments and standard indentation.
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot}
#' @examples
#' # Warning: these examples may not work well when using the "Run examples" link
#' # because of a particular environment. Please, copy-paste in a local environment.
#' code1 <- "function(x){ if(x == T) sum(x) else base::sapply(X = 1:3, FUN = \\(y){ y * 2 }) }"
#' fun1 <- eval(parse(text = code1, keep.source = TRUE))
#' res <- saferMake::test(x = fun1)
#' res$stage
#' res$checks
#' res$fun # function corrected by the seven checks
#' res2 <- saferMake::test(x = fun1, stop_at_first = TRUE) # stops at check 2 (lambda)
#' res2$step
#' # Write the corrected function in the fun1_safer.R file of a new temporary folder:
#' tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
#' base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
#' saferMake::test(x = fun1, export = TRUE, out_path = tempo_dir)
#' @export
test <- function(
    x,
    stop_at_first = FALSE,
    export = FALSE,
    out_path = ".",
    safer_check = TRUE,
    lib_path = NULL,
    error_text = ""
){
    # DEBUGGING
    # x = fun1 ; stop_at_first = FALSE ; export = FALSE ; out_path = "." ; safer_check = TRUE ; lib_path = NULL ; error_text = " INSIDE A"
    # function_name <- "test()" ; arg_user_setting = base::list(x = as.name(x = "fun1"), stop_at_first = FALSE, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = " INSIDE A")
    # arg_names <- base::c("x", "stop_at_first", "export", "out_path", "safer_check", "lib_path", "error_text")

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
    mandat_args <- base::c("lib_path", "safer_check", "error_text")
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
        "x"
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
    tempo_arg <- base::c(
        "x",
        "stop_at_first",
        "export",
        "out_path",
        "safer_check"
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
    tempo_arg <- base::c(
        "x",
        "stop_at_first",
        "export",
        "out_path",
        "safer_check",
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
    # Mandatory section : argument of safer-r functions cannot have NA as only value, to prevent all(, na.rm = TRUE) or any(, na.rm = TRUE) to return a logical value
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
    if( ! (base::all(base::typeof(x = safer_check) == "logical", na.rm = TRUE) & base::length(x = safer_check) == 1)){ # no need to test NA because NA only already managed above and base::length(x = safer_check) == 1)
        if(base::all(base::mode(x = safer_check) == "function", na.rm = TRUE)){
            safer_check <- base::deparse1(expr = safer_check, collapse = "", width.cutoff = 500L)
        }
        tempo_cat <- base::paste0(
            error_text_start,
            "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).\nHERE IT IS:\n",
            base::ifelse(test = base::length(x = safer_check) == 0 | base::all(base::suppressWarnings(expr = safer_check == base::quote(expr = ), classes = "warning"), na.rm = TRUE) | base::all(safer_check == "", na.rm = TRUE), yes = "<NULL, \"\", EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(safer_check, collapse = "\n", recycle0 = FALSE)),
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end safer_check argument checking

    ######## check of lib_path
    # must be before any :: or ::: non basic package calling
    # always executed: lib_path is always checked, whatever the value of safer_check
    if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.
        if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == "character" returns FALSE)
            if(base::all(base::mode(x = lib_path) == "function", na.rm = TRUE)){
                lib_path <- base::deparse1(expr = lib_path, collapse = "", width.cutoff = 500L)
            }
            tempo_cat <- base::paste0(
                error_text_start,
                "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n",
                base::ifelse(test = base::length(x = lib_path) == 0 | base::all(base::suppressWarnings(expr = lib_path == base::quote(expr = ), classes = "warning"), na.rm = TRUE), yes = "<NULL, EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(lib_path, collapse = "\n", recycle0 = FALSE)),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA. dir.exists(paths = NA) returns an error, so ok. dir.exists(paths = "") returns FALSE so ok
            tempo_log <- ! base::dir.exists(paths = lib_path)
            tempo_cat_b <- lib_path[tempo_log] # here lib_path is character string
            tempo_cat_b[tempo_cat_b == ""] <- "\"\""
            tempo_cat <- base::paste0(
                error_text_start,
                "THE DIRECTORY PATH",
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S", no = ""),
                " INDICATED IN THE lib_path ARGUMENT DO",
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "", no = "ES"),
                " NOT EXIST:\n",
                base::paste0(tempo_cat_b, collapse = "\n", recycle0 = FALSE),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }else{
            ini_lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
            base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
            lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
        }
    }else{
        lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base:::.libPaths(), lib_path))
    }
    ######## end check of lib_path

    ######## check of the saferDev package
    # saferDev is in the Suggests field of the DESCRIPTION file of saferMake (only make() and test() need it): its presence is checked before any saferDev:: calling
    if( ! base::requireNamespace(package = "saferDev", lib.loc = lib_path, quietly = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start,
            "THE saferDev PACKAGE IS REQUIRED (https://github.com/safer-r/saferDev).\nPLEASE, INSTALL IT, OR INDICATE ITS LIBRARY PATH IN THE lib_path ARGUMENT.",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end check of the saferDev package

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev", pos = , envir = )
        .pack_and_function_check(
            fun = base::c(
                # functions required in this code
                "saferDev::arg_check",
                "saferDev::super_assign_check",
                "saferDev::lambda_check",
                "saferDev::curly_bracket_check",
                "saferDev::true_false_check",
                "saferDev::for_loop_nb_check",
                "saferDev::colons_check",
                "saferDev::all_args_here",
                # end functions required in this code
                # internal functions required in this code
                "saferDev:::.base_op_check" # also in internal functions
                # end internal functions required in this code
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .code_check_steps <- utils::getFromNamespace(x = ".code_check_steps", ns = "saferMake", pos = , envir = )
    .safer_dev_available <- utils::getFromNamespace(x = ".safer_dev_available", ns = "saferMake", pos = , envir = )
    .check_var_name <- utils::getFromNamespace(x = ".check_var_name", ns = "saferMake", pos = , envir = )
    .fun_text <- utils::getFromNamespace(x = ".fun_text", ns = "saferMake", pos = , envir = )
    .run_check_step <- utils::getFromNamespace(x = ".run_check_step", ns = "saferMake", pos = , envir = )

    ######## end escaping CRAN submission NOTE for internal functions

    ######## critical operator checking
    if(safer_check == TRUE){
        .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = , envir = )
        .base_op_check(
            error_text = embed_error_text
        )
    }
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    arg_check_error_text <- base::paste0("ERROR ", embed_error_text, "\n\n", collapse = NULL, recycle0 = FALSE) # must be used instead of error_text = embed_error_text when several arg_check are performed on the same argument (tempo1, tempo2, see below)
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object_name))
    # add as many lines as below, for each of your arguments of your function in development
    tempo <- saferDev::arg_check(data = x, class = "function", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = stop_at_first, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = export, class = NULL, typeof = "logical", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    tempo <- saferDev::arg_check(data = out_path, class = NULL, typeof = "character", mode = NULL, length = 1, prop = FALSE, options = NULL, double_as_integer_allowed = FALSE, all_options_in_data = FALSE, na_contain = FALSE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text) ; base::eval(expr = ee, envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    # lib_path already checked above
    # safer_check already checked above
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
        "out_path"
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
    # x must be a closure (a primitive function has no code to check, and no environment to rebuild the corrected function)
    if(base::is.primitive(x = x)){
        tempo_cat <- base::paste0(
            error_text_start,
            "THE x ARGUMENT CANNOT BE A PRIMITIVE FUNCTION (NO R CODE TO CHECK).",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # the saferDev of the R session must provide the seven checks (current version of saferDev). Warning: base::requireNamespace() is used instead of saferDev::is_package_here() because the released saferDev version does not provide the seven checks: the presence of the functions themselves is tested by .safer_dev_available()
    if( ! .safer_dev_available()){
        tempo_cat <- base::paste0(
            error_text_start,
            "THE saferDev PACKAGE OF THE R SESSION DOES NOT PROVIDE THE SEVEN FUNCTIONS REQUIRED:\n",
            base::paste0("saferDev::", base::vapply(X = .code_check_steps, FUN = function(st){st$fun}, FUN.VALUE = base::character(length = 1L), USE.NAMES = FALSE), "()", collapse = "\n", recycle0 = FALSE),
            "\n\nPLEASE, INSTALL OR LOAD THE CURRENT VERSION OF saferDev (https://github.com/safer-r/saferDev).",
            collapse = NULL,
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # name under which x is handed to the saferDev functions (messages and name of the exported file): the name written by the user, or "fun_to_check" for an inline function definition or a component writing (like obj$fun)
    if(base::is.symbol(x = arg_user_setting$x)){
        tempo_fun_name <- base::as.character(x = arg_user_setting$x)[1]
        tempo_name_ok <- TRUE
    }else if(base::is.call(x = arg_user_setting$x) && base::as.character(x = arg_user_setting$x[[1]])[1] %in% base::c("::", ":::")){
        tempo_fun_name <- base::as.character(x = arg_user_setting$x)[3] # form "package::function" or "package:::function"
        tempo_name_ok <- TRUE
    }else{
        tempo_fun_name <- "fun_to_check"
        tempo_name_ok <- FALSE
    }
    var_name <- .check_var_name(fun_name = tempo_fun_name) # "fun_to_check" if not a syntactic name
    if(export == TRUE){
        # removal of trailing / or \\ in the path
        if(base::grepl(x = out_path, pattern = "/$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
            out_path <- base::sub(pattern = "/$", replacement = "", x = out_path, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        }else if(base::grepl(x = out_path, pattern = "\\\\$", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)){
            out_path <- base::sub(pattern = "\\\\$", replacement = "", x = out_path, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
        }
        # end removal of trailing / or \\ in the path
        if( ! base::dir.exists(paths = out_path)){
            tempo_cat <- base::paste0(
                error_text_start,
                "DIRECTORY PATH INDICATED IN THE out_path ARGUMENT DOES NOT EXISTS:\n",
                out_path,
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        if(tempo_name_ok == FALSE){
            tempo_cat <- base::paste0(
                error_text_start,
                "THE x ARGUMENT MUST BE DESIGNATED BY A FUNCTION NAME (NOT BY AN INLINE FUNCTION DEFINITION, NOR BY A COMPONENT WRITING LIKE obj$fun) WHEN THE export ARGUMENT IS TRUE.\nHERE IT IS:\n",
                base::paste0(base::deparse(expr = arg_user_setting$x, width.cutoff = 60L, backtick = FALSE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), collapse = "\n", recycle0 = FALSE),
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
        out_name <- base::paste0(var_name, "_safer.R", collapse = NULL, recycle0 = FALSE)
        if(base::file.exists(base::paste0(out_path, "/", out_name, collapse = NULL, recycle0 = FALSE))){
            tempo_cat <- base::paste0(
                error_text_start,
                "FILE NAME ALREADY EXISTS AT THE INDICATED PATH:\n",
                base::paste0(out_path, "/", out_name, collapse = NULL, recycle0 = FALSE),
                "\n\nPLEASE, RENAME OR REMOVE THIS FILE, OR CHANGE THE out_path ARGUMENT, AND RERUN ",
                package_function_name,
                ".",
                collapse = NULL,
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    n_steps <- base::length(x = .code_check_steps)
    check_names <- base::vapply(X = .code_check_steps, FUN = function(st){st$fun}, FUN.VALUE = base::character(length = 1L), USE.NAMES = FALSE)
    check_status <- base::rep(x = "not run", times = n_steps)
    check_report <- base::rep(x = "", times = n_steps)
    current_fun <- x # function checked at each step: the function corrected by the previous checks
    current_txt <- NULL # code of current_fun (NULL as long as nothing has been corrected)
    stop_step <- NULL
    for(i1 in base::seq_len(length.out = n_steps)){
        step <- .code_check_steps[[i1]]
        out <- .run_check_step(step = step, f = current_fun, var_name = var_name, safer_check = FALSE, lib_path = lib_path, error_text = embed_error_text)
        if( ! base::is.null(x = out$err)){
            # the saferDev function stopped: the next checks cannot be run on a code that the current check cannot analyse
            check_status[i1] <- "error"
            check_report[i1] <- out$err
            stop_step <- i1
            break
        }
        check_report[i1] <- out$report
        if( ! base::is.null(x = out$fun_txt)){
            if(base::identical(x = out$fun_txt, y = .fun_text(f = current_fun), num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)){
                # saferDev reported something but the code is unchanged: considered as findings (otherwise make() would loop without end)
                check_status[i1] <- "findings"
            }else{
                check_status[i1] <- "modified"
                current_txt <- out$fun_txt
                # the corrected function is rebuilt with its source references (required by the next checks, see lambda_check()), in the environment of x. Warning: srcfile = NULL (the default value proposed by all_args_here()) removes the source references: an explicit srcfile object is written instead, as in saferDev::colons_check()
                current_fun <- base::eval(expr = base::parse(file = "", n = NULL, text = current_txt, prompt = "?", keep.source = TRUE, srcfile = base::srcfilecopy(filename = "<text>", lines = current_txt, timestamp = base::Sys.time(), isFile = FALSE), encoding = "unknown")[[1]], envir = base::environment(fun = x), enclos = base::baseenv())
            }
        }else if(out$clean == TRUE){
            check_status[i1] <- "clean"
        }else{
            check_status[i1] <- "findings" # findings that the saferDev function cannot correct itself
        }
        # the run stops on findings (the next checks can depend on them, like all_args_here() that needs the package attributions of colons_check()), and also on a modification when stop_at_first is TRUE
        if(check_status[i1] == "findings" | (stop_at_first == TRUE & check_status[i1] == "modified")){
            stop_step <- i1
            break
        }
    }
    # end loop over the seven checks
    # overall stage
    if(base::any(check_status == "error", na.rm = TRUE)){
        stage <- "error"
    }else if(base::any(check_status == "findings", na.rm = TRUE)){
        stage <- "findings"
    }else if( ! base::is.null(x = current_txt)){
        stage <- "modified"
    }else{
        stage <- "ok"
    }
    # end overall stage
    # export
    if(export == TRUE & ! base::is.null(x = current_txt)){
        base::writeLines(text = base::paste0(var_name, "_safer <- ", current_txt, collapse = NULL, recycle0 = FALSE), con = base::paste0(out_path, "/", out_name, collapse = NULL, recycle0 = FALSE), sep = "\n", useBytes = FALSE)
        # the export message is printed with on.exit(), after the message of the main code: it only claims an export that actually happened
        base::on.exit(expr = base::message(base::paste0("MODIFIED CODE EXPORTED IN:\n", base::normalizePath(path = base::paste0(out_path, "/", out_name, collapse = NULL, recycle0 = FALSE), winslash = "/", mustWork = FALSE), "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE), add = TRUE, after = TRUE)
    }
    # end export
    # message
    tempo_status_txt <- base::toupper(x = check_status)
    tempo_status_txt[check_status == "modified"] <- base::paste0("MODIFIED (", base::vapply(X = .code_check_steps, FUN = function(st){st$what}, FUN.VALUE = base::character(length = 1L), USE.NAMES = FALSE)[check_status == "modified"], ")", collapse = NULL, recycle0 = FALSE)
    tempo_report_log <- check_status %in% base::c("modified", "findings", "error") & base::nzchar(x = check_report, keepNA = FALSE)
    tempo_cat <- base::paste0(
        "AFTER RUNNING ",
        package_function_name,
        ".\nINSIDE ",
        var_name,
        "():\n\n",
        base::paste0("CHECK ", base::seq_len(length.out = n_steps), "/", n_steps, " saferDev::", check_names, "(): ", tempo_status_txt, collapse = "\n", recycle0 = FALSE),
        base::ifelse(
            test = base::any(tempo_report_log, na.rm = TRUE),
            yes = base::paste0(
                "\n\n",
                base::paste0("REPORT OF CHECK ", base::which(x = tempo_report_log, arr.ind = FALSE, useNames = TRUE), " (saferDev::", check_names[tempo_report_log], "()):\n", check_report[tempo_report_log], collapse = "\n\n", recycle0 = FALSE),
                collapse = NULL,
                recycle0 = FALSE
            ),
            no = ""
        ),
        base::ifelse(test = stage == "ok", yes = "\n\nEVERYTHING SEEMS CLEAN.", no = ""),
        collapse = NULL,
        recycle0 = FALSE
    )
    base::message(base::paste0("\n\n", tempo_cat, "\n\n", collapse = NULL, recycle0 = FALSE), domain = NULL, appendLF = TRUE)
    # end message
    #### end main code

    #### warning output
    #### end warning output

    #### output
    output <- base::list(
        stage = stage,
        step = stop_step,
        fun = if(base::is.null(x = current_txt)){NULL}else{current_fun},
        code = current_txt,
        checks = base::data.frame(check = check_names, status = check_status, report = check_report, row.names = NULL, check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE, stringsAsFactors = FALSE)
    )
    if(stage == "ok"){
        base::return(base::invisible(x = output))
    }else{
        base::return(output)
    }
    #### end output

    #### end of function
}
