# Staged code checks of the pasted function (the seven checks run by the Run
# button of window 1, after the pasted code has been evaluated without error).
# Extracted from inst/app/server.R; see the header of R/intern_.app_helpers.R.
#
# The seven checks, always run in this order, each with the dedicated function
# of the current (development) version of saferDev:
#   1. saferDev::super_assign_check()   no superassignment <<- or ->> (error only:
#                                       nothing can be corrected automatically)
#   2. saferDev::lambda_check()         no lambda function written \(x)
#   3. saferDev::curly_bracket_check()  curly brackets after if, else, for, ...
#   4. saferDev::true_false_check()     TRUE and FALSE written in full
#   5. saferDev::for_loop_nb_check()    checking of the for() loop sequences
#   6. saferDev::colons_check()         package attribution (pkg::) of the functions
#   7. saferDev::all_args_here()        all the arguments of the functions written
# super_assign_check() is first because it cannot correct the code: the user
# rewrites the superassignments before any automatic modification.
# for_loop_nb_check() is before colons_check() and all_args_here() because the
# checking lines that it inserts must also be checked by these two functions.
# The seven checks are run by the exported saferMake::test() (R/test.R), that
# can also be used on its own. In the app, .run_code_checks() calls test() with
# stop_at_first = TRUE: when a check modifies the code, the loop stops, the
# modified code is handed back to window 1 (orange message), and the user
# clicks on Run again, which restarts the seven checks on the modified code.

# Run a zero-argument function, capture every message and warning (muffled) and
# any error.
# Returns list(value, msgs, warns, err): value is the returned value (NULL on
# error), msgs and warns the captured messages and warnings as character
# vectors, err the error message (NULL when no error).
# The captured texts are stored in the explicit environment store (no
# superassignment <<-, see saferDev::super_assign_check()).
.capture_msg <- function(fun) {
  store <- base::new.env(hash = TRUE, parent = base::emptyenv(), size = 29L)
  store$msgs <- base::character(length = 0L)
  store$warns <- base::character(length = 0L)
  store$err <- NULL
  value <- base::tryCatch(
    expr = base::withCallingHandlers(
      expr = base::do.call(what = fun, args = base::list(), quote = FALSE, envir = ), # do.call() instead of fun(): fun cannot be preceded by a package name. Empty envir: default value base::parent.frame() of base::do.call()
      message = function(m) {
        store$msgs <- base::c(store$msgs, base::conditionMessage(c = m))
        base::invokeRestart(r = "muffleMessage")
      },
      warning = function(w) {
        store$warns <- base::c(store$warns, base::conditionMessage(c = w))
        base::invokeRestart(r = "muffleWarning")
      }
    ),
    error = function(e) {
      store$err <- base::conditionMessage(c = e)
      NULL
    },
    finally =
  )
  base::list(value = value, msgs = store$msgs, warns = store$warns, err = store$err)
}

# The seven checks, in the order of execution.
# key        : short identifier
# fun        : name of the saferDev function
# export_arg : name of the argument of the saferDev function that exports the
#              modified function in a file (all_args_here() has two export
#              arguments: table_export for the table, fun_export for the code).
#              NULL for super_assign_check(), that never modifies the code (it
#              returns an error when a superassignment is detected)
# what       : sentence explaining the modification, displayed in window 1
# The saferDev functions keep the comments and the indentation of the
# submitted code (the function evaluated with keep.source = TRUE carries its
# source references).
.code_check_steps <- base::list(
  base::list(key = "super_assign", fun = "super_assign_check", export_arg = NULL,
             what = "no modification (the superassignments <<- and ->> must be rewritten by the user)."),
  base::list(key = "lambda", fun = "lambda_check", export_arg = "export",
             what = "the lambda functions written \\(x) have been rewritten function(x)."),
  base::list(key = "curly", fun = "curly_bracket_check", export_arg = "export",
             what = "curly brackets { } have been added to the bodies of the if, else, for, while, repeat and function that had none."),
  base::list(key = "true_false", fun = "true_false_check", export_arg = "export",
             what = "the T and F written as values have been rewritten TRUE and FALSE."),
  base::list(key = "for_loop", fun = "for_loop_nb_check", export_arg = "export",
             what = "a checking line of the sequence has been added before the for() loops whose number of iterations is not fixed by the writing itself (like 1:length(x))."),
  base::list(key = "colons", fun = "colons_check", export_arg = "export",
             what = "the package attribution (<package>::) of the basic R functions has been added."),
  base::list(key = "args", fun = "all_args_here", export_arg = "fun_export",
             what = "the missing arguments of the called functions have been added.")
)

# TEMPORARY SWITCH (set to FALSE on 2026-10-22 so that the rest of the app can be
# tested with a saferDev version that does not provide the seven checks).
# TRUE  : when the saferDev of the R session does not provide the seven checks,
#         window 1 shows a red message asking to load the current saferDev,
#         and window 2 cannot be reached.
# FALSE : the seven checks are skipped (message in the R console only) and the
#         app goes on to window 2.
# Set it back to TRUE once the current saferDev is released (CRAN).
.safer_dev_required <- FALSE

# Is saferDev available, in a version that provides the seven checks (current,
# development version; not the released CRAN version)? An
# internal helper (instead of a direct base::requireNamespace() call) so that
# the "saferDev not available" branch can be unit tested.
.safer_dev_available <- function() {
  if (! base::requireNamespace(package = "saferDev", quietly = TRUE)) {
    base::return(FALSE)
  }
  funs <- base::vapply(X = .code_check_steps, FUN = function(st) {st$fun} ,
                       FUN.VALUE = base::character(length = 1L), USE.NAMES = FALSE)
  base::all(base::vapply(X = funs, FUN = function(fn) {
    base::exists(x = fn, where = -1, envir = base::asNamespace(ns = "saferDev", base.OK = TRUE), frame = , mode = "function", inherits = FALSE)
  } , FUN.VALUE = base::logical(length = 1L), USE.NAMES = FALSE), na.rm = FALSE)
}

# Name of the variable under which the function is handed to saferDev: the
# saferDev functions designate the function by its name (messages, name of the
# exported file). The user function name is used when it is a syntactic name,
# otherwise (anonymous function, non syntactic name) "fun_to_check".
.check_var_name <- function(fun_name) {
  if (base::is.character(x = fun_name) && base::length(x = fun_name) == 1L &&
      ! base::is.na(x = fun_name) && base::nzchar(x = fun_name, keepNA = FALSE) &&
      base::identical(x = base::make.names(names = fun_name, unique = FALSE, allow_ = TRUE),
                      y = fun_name, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)) {
    fun_name
  } else {
    "fun_to_check"
  }
}

# Text of the code of function f: the text of its source references when it
# carries them (parse(keep.source = TRUE)), otherwise its deparsed code.
.fun_text <- function(f) {
  src <- base::attr(x = f, which = "srcref", exact = TRUE)
  if (base::is.null(x = src)) {
    base::paste(base::deparse(expr = f, width.cutoff = 60L, backtick = TRUE, control = base::c("keepNA", "keepInteger", "niceNames", "showAttributes"), nlines = -1L), sep = " ", collapse = "\n", recycle0 = FALSE)
  } else {
    base::paste(base::as.character(x = src, useSource = TRUE), sep = " ", collapse = "\n", recycle0 = FALSE)
  }
}

# Position of the text of function f inside the code it was evaluated from.
# The function carries its source references (parse(keep.source = TRUE)): their
# text is searched in the code. Returns c(start, end) (character positions) or
# NULL when the text cannot be located.
.function_span <- function(code, f) {
  src <- base::attr(x = f, which = "srcref", exact = TRUE)
  if (base::is.null(x = src)) {base::return(NULL)}
  txt <- base::paste(base::as.character(x = src, useSource = TRUE), sep = " ", collapse = "\n", recycle0 = FALSE)
  if (! base::nzchar(x = txt, keepNA = FALSE)) {base::return(NULL)}
  start <- base::regexpr(pattern = txt, text = code, ignore.case = FALSE, perl = FALSE,
                         fixed = TRUE, useBytes = FALSE)
  if (start == -1L) {base::return(NULL)}
  base::c(base::as.integer(x = start),
          base::as.integer(x = start) + base::nchar(x = txt, type = "chars", allowNA = FALSE, keepNA = NA) - 1L)
}

# Put the modified function text back in the code: what is written before and
# after the function in the pasted code (name of the function and assignment,
# header comments, ...) is kept. When the function cannot be located, the
# modified function replaces the whole code.
# fun_txt_new : text of the modified function, starting with "function"
.splice_function <- function(code, f, fun_txt_new, fun_name) {
  .function_span <- utils::getFromNamespace(x = ".function_span", ns = "saferMake", pos = , envir = )
  span <- .function_span(code = code, f = f)
  if (base::is.null(x = span)) {
    # the pasted code could not be located: rebuild a resubmittable code
    base::return(if (base::identical(x = fun_name, y = "<anonymous>", num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)) {
      fun_txt_new
    } else {
      base::paste0(fun_name, " <- ", fun_txt_new, collapse = NULL, recycle0 = FALSE)
    })
  }
  base::paste0(
    base::substring(text = code, first = 1L, last = span[1] - 1L),
    fun_txt_new,
    base::substring(text = code, first = span[2] + 1L, last = ),
    collapse = NULL, recycle0 = FALSE
  )
}

# Run ONE check on the function f.
# step     : one element of .code_check_steps
# f        : the function, evaluated from the pasted code with keep.source = TRUE
# var_name : name under which f is handed to saferDev (see .check_var_name())
# safer_check, lib_path, error_text : passed to the saferDev function
#            (safer_check = FALSE when called by saferMake::test(), that has
#            already performed these checks)
# The modified code is obtained by exporting it in a temporary file
# (<var_name>_safer.R, written by saferDev) that is read back and deleted.
# A step without export argument (export_arg NULL) is called without export
# and out_path: it never writes a file.
# Returns list(fun_txt, report, clean, err):
#   fun_txt : text of the modified function (starting with "function"), or NULL
#             if the check did not modify the code
#   report  : text printed by saferDev (messages and warnings, without the
#             message giving the path of the temporary export file)
#   clean   : TRUE when the check reports nothing to modify or to complete
#   err     : error message of saferDev (NULL if no error)
.run_check_step <- function(step, f, var_name, safer_check = TRUE, lib_path = NULL, error_text = "") {
  .capture_msg <- utils::getFromNamespace(x = ".capture_msg", ns = "saferMake", pos = , envir = )
  out_dir <- base::tempfile(pattern = "saferMake_check_", tmpdir = base::tempdir(check = FALSE), fileext = "")
  base::dir.create(path = out_dir, showWarnings = FALSE, recursive = FALSE, mode = "0777")
  base::on.exit(expr = base::unlink(x = out_dir, recursive = TRUE, force = TRUE, expand = TRUE), add = TRUE, after = TRUE)
  env <- base::new.env(hash = TRUE, parent = base::globalenv(), size = 29L)
  base::assign(x = var_name, value = f, pos = -1, envir = env, inherits = FALSE, immediate = TRUE)
  # saferDev designates the function by the name written in the call: the call
  # is built with that name and evaluated in env
  if (base::is.null(x = step$export_arg)) {
    # check without export argument (super_assign_check(): never modifies the code)
    args <- base::list(base::as.name(x = var_name), safer_check, lib_path, error_text)
    base::names(x = args) <- base::c("x", "safer_check", "lib_path", "error_text")
  } else {
    args <- base::list(base::as.name(x = var_name), TRUE, out_dir, safer_check, lib_path, error_text)
    base::names(x = args) <- base::c("x", step$export_arg, "out_path", "safer_check", "lib_path", "error_text")
  }
  cl <- base::as.call(x = base::c(
    base::list(base::call(name = "::", base::as.name(x = "saferDev"), base::as.name(x = step$fun))),
    args
  ))
  cap <- .capture_msg(fun = function() {base::eval(expr = cl, envir = env, enclos = base::baseenv())} )
  # the message giving the path of the temporary file is of no use to the user
  msgs <- cap$msgs[ ! base::grepl(pattern = "MODIFIED CODE EXPORTED IN", x = cap$msgs,
                                  ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)]
  # saferDev can print the same text several times (message printed twice, or
  # message repeated in a warning): each distinct text is kept once
  all_txt <- base::trimws(x = base::c(msgs, cap$warns), which = "both", whitespace = "[ \t\r\n]")
  all_txt <- all_txt[base::nzchar(x = all_txt, keepNA = FALSE) & ! base::duplicated(x = all_txt, incomparables = FALSE)]
  report <- base::paste(all_txt, sep = " ", collapse = "\n\n", recycle0 = FALSE)
  res <- base::list(fun_txt = NULL, report = report, clean = FALSE, err = NULL)
  if (! base::is.null(x = cap$err)) {
    res$err <- base::trimws(x = cap$err, which = "both", whitespace = "[ \t\r\n=]")
    base::return(res)
  }
  files <- base::list.files(path = out_dir, pattern = NULL, all.files = FALSE, full.names = TRUE,
                            recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE, fixed = FALSE)
  if (base::length(x = files) > 0L) {
    lines <- base::enc2utf8(x = base::readLines(con = files[1], n = -1L, ok = TRUE, warn = FALSE,
                                                encoding = "unknown", skipNul = FALSE))
    prefix <- base::paste0(var_name, "_safer <- ", collapse = NULL, recycle0 = FALSE)
    if (base::length(x = lines) == 0L || ! base::startsWith(x = lines[1], prefix = prefix)) {
      res$err <- "The modified code returned by saferDev has an unexpected format."
      base::return(res)
    }
    lines[1] <- base::substring(text = lines[1], first = base::nchar(x = prefix, type = "chars", allowNA = FALSE, keepNA = NA) + 1L, last = )
    res$fun_txt <- base::paste(lines, sep = " ", collapse = "\n", recycle0 = FALSE)
    base::return(res)
  }
  # nothing modified: clean unless saferDev reports findings that it cannot
  # correct itself (other functions without package, calls on several lines, ...).
  # "NOTHING TO CHECK" (warning of colons_check() and all_args_here() when the
  # function contains no function call, e.g. function(){y <- 1}) is also clean
  res$clean <- (base::grepl(pattern = "EVERYTHING SEEMS CLEAN", x = report, ignore.case = FALSE,
                            perl = FALSE, fixed = TRUE, useBytes = FALSE) ||
                base::grepl(pattern = "NOTHING TO CHECK", x = report, ignore.case = FALSE,
                            perl = FALSE, fixed = TRUE, useBytes = FALSE)) &&
               ! base::grepl(pattern = "ARE MISSING", x = report, ignore.case = FALSE,
                             perl = FALSE, fixed = TRUE, useBytes = FALSE)
  res
}

# Full staged check of the pasted code (the seven checks of the Run button).
# code     : the pasted code (character), already evaluated without error
# f        : the function evaluated from code with keep.source = TRUE
# fun_name : the function name ("<anonymous>" when the code is a bare function)
# Returns list(stage, code, step, notice, report) with:
#   stage  : "ok" (the seven checks are clean), "modified" (the check number
#            step modified the code), "findings" (the check number step
#            reports something that it cannot correct itself: the user must
#            complete the code), "error" (the saferDev function stopped), or
#            "skip" (the saferDev of the R session does not provide the seven
#            checks: no check can be done, the user is told to load the
#            current saferDev)
#   code   : the code to display in window 1 (modified code at stage
#            "modified", otherwise the unchanged pasted code)
#   step   : number (1 to 7) of the check that stopped the loop, NULL at stage
#            "ok" and "skip"
#   notice : lines of the orange message of window 1 (character(0) at stage
#            "ok")
#   report : text printed by saferDev for that check ("" at stage "ok")
# error_text : passed to saferMake::test(), so that the error messages give the
#              full trail (" INSIDE saferMake::make()" when run by the app)
.run_code_checks <- function(code, f, fun_name = "<anonymous>", error_text = "") {
  .safer_dev_available <- utils::getFromNamespace(x = ".safer_dev_available", ns = "saferMake", pos = , envir = )
  .check_var_name <- utils::getFromNamespace(x = ".check_var_name", ns = "saferMake", pos = , envir = )
  .capture_msg <- utils::getFromNamespace(x = ".capture_msg", ns = "saferMake", pos = , envir = )
  .splice_function <- utils::getFromNamespace(x = ".splice_function", ns = "saferMake", pos = , envir = )
  res <- base::list(stage = "ok", code = code, step = NULL,
                    notice = base::character(length = 0L), report = "")
  if (! .safer_dev_available()) {
    # the seven checks cannot be run: never go on silently to window 2
    res$stage <- "skip"
    res$notice <- base::paste0("The safer-r code verifications need the current (development) version of the saferDev package (https://github.com/safer-r/saferDev), which provides super_assign_check(), lambda_check(), curly_bracket_check(), true_false_check() and for_loop_nb_check(). The saferDev of the R session running this app does not (version: ",
                               if (base::requireNamespace(package = "saferDev", quietly = TRUE)) {base::as.character(x = utils::packageVersion(pkg = "saferDev", lib.loc = NULL))} else {"none"},
                               "). Please, load the current saferDev (for instance with devtools::load_all() on its local folder) before saferMake, then run saferMake::make() again.",
                               collapse = NULL, recycle0 = FALSE)
    base::return(res)
  }
  var_name <- .check_var_name(fun_name = fun_name)
  # saferMake::test() designates the function by the name written in the
  # call (saferDev messages): the call is built with that name and evaluated
  # in env
  env <- base::new.env(hash = TRUE, parent = base::globalenv(), size = 29L)
  base::assign(x = var_name, value = f, pos = -1, envir = env, inherits = FALSE, immediate = TRUE)
  cl <- base::call(name = "test", x = base::as.name(x = var_name), stop_at_first = TRUE,
                   export = FALSE, out_path = ".", safer_check = FALSE, lib_path = NULL, error_text = error_text)
  cl[[1]] <- base::call(name = "::", base::as.name(x = "saferMake"), base::as.name(x = "test"))
  # the summary message of test() is not displayed (the app builds its own)
  cap <- .capture_msg(fun = function() {base::eval(expr = cl, envir = env, enclos = base::baseenv())} )
  n_steps <- base::length(x = .code_check_steps)
  if (! base::is.null(x = cap$err)) {
    # saferMake::test() itself stopped (should not happen: errors of the
    # saferDev functions are returned in its output)
    res$stage <- "error"
    res$report <- base::trimws(x = cap$err, which = "both", whitespace = "[ \t\r\n=]")
    res$notice <- "saferMake::test() stopped with the message below. Please, correct your code in the box below, then click on the Run button again."
    base::return(res)
  }
  out <- cap$value
  if (base::identical(x = out$stage, y = "ok", num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)) {
    base::return(res)
  }
  i <- out$step
  step <- .code_check_steps[[i]]
  head_txt <- base::paste0("Check ", i, "/", n_steps, " (saferDev::", step$fun, "()): ",
                           collapse = NULL, recycle0 = FALSE)
  res$step <- i
  res$report <- out$checks$report[i]
  findings_txt <- base::paste0(head_txt, "the check reported the findings below, that could not be corrected automatically. Please, complete your code in the box below, then click on the Run button again.",
                               collapse = NULL, recycle0 = FALSE)
  if (out$stage == "error") {
    res$stage <- "error"
    res$notice <- base::paste0(head_txt, "saferDev stopped with the message below. Please, correct your code in the box below, then click on the Run button again.",
                               collapse = NULL, recycle0 = FALSE)
    base::return(res)
  }
  if (out$stage == "findings") {
    res$stage <- "findings"
    res$notice <- findings_txt
    base::return(res)
  }
  # stage "modified"
  new_code <- .splice_function(code = code, f = f, fun_txt_new = out$code, fun_name = fun_name)
  if (base::identical(x = new_code, y = code, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)) {
    # handing back the same code as "modified" would loop without end
    res$stage <- "findings"
    res$notice <- findings_txt
    base::return(res)
  }
  res$stage <- "modified"
  res$code <- new_code
  res$notice <- base::c(
    base::paste0(head_txt, step$what, collapse = NULL, recycle0 = FALSE),
    "The modified code is now in the box below. Please, check it, then click on the Run button again."
  )
  res
}
