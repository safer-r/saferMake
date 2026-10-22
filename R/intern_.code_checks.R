# Staged code checks of the pasted function (the five checks run by the Run
# button of window 1, after the pasted code has been evaluated without error).
# Extracted from inst/app/server.R; see the header of R/intern_.app_helpers.R.
#
# The five checks, always run in this order, each with the dedicated function
# of saferDev (>= 3.0.0):
#   1. saferDev::lambda_check()         no lambda function written \(x)
#   2. saferDev::curly_bracket_check()  curly brackets after if, else, for, ...
#   3. saferDev::true_false_check()     TRUE and FALSE written in full
#   4. saferDev::colons_check()         package attribution (pkg::) of the functions
#   5. saferDev::all_args_here()        all the arguments of the functions written
# When a check modifies the code, the loop stops: the modified code is handed
# back to window 1 (orange message), and the user clicks on Run again, which
# restarts the five checks on the modified code.

# Run a zero-argument function, capture every message and warning (muffled) and
# any error.
# Returns list(value, msgs, warns, err): value is the returned value (NULL on
# error), msgs and warns the captured messages and warnings as character
# vectors, err the error message (NULL when no error).
.capture_msg <- function(fun) {
  msgs <- base::character(length = 0L)
  warns <- base::character(length = 0L)
  err <- NULL
  value <- base::tryCatch(
    expr = base::withCallingHandlers(
      expr = fun(),
      message = function(m) {
        msgs <<- base::c(msgs, base::conditionMessage(c = m))
        base::invokeRestart(r = "muffleMessage")
      },
      warning = function(w) {
        warns <<- base::c(warns, base::conditionMessage(c = w))
        base::invokeRestart(r = "muffleWarning")
      }
    ),
    error = function(e) {
      err <<- base::conditionMessage(c = e)
      NULL
    },
    finally = 
  )
  base::list(value = value, msgs = msgs, warns = warns, err = err)
}

# The five checks, in the order of execution.
# key        : short identifier
# fun        : name of the saferDev function
# export_arg : name of the argument of the saferDev function that exports the
#              modified function in a file (all_args_here() has two export
#              arguments: table_export for the table, fun_export for the code)
# what       : sentence explaining the modification, displayed in window 1
# loses_format : TRUE when the saferDev function rebuilds the code from its
#              analysis, so that the comments (and the indentation) of the
#              submitted code are lost (see the Details of each function)
.code_check_steps <- base::list(
  base::list(key = "lambda", fun = "lambda_check", export_arg = "export",
             what = "the lambda functions written \\(x) have been rewritten function(x).",
             loses_format = FALSE),
  base::list(key = "curly", fun = "curly_bracket_check", export_arg = "export",
             what = "curly brackets { } have been added to the bodies of the if, else, for, while, repeat and function that had none.",
             loses_format = TRUE),
  base::list(key = "true_false", fun = "true_false_check", export_arg = "export",
             what = "the T and F written as values have been rewritten TRUE and FALSE.",
             loses_format = FALSE),
  base::list(key = "colons", fun = "colons_check", export_arg = "export",
             what = "the package attribution (<package>::) of the basic R functions has been added.",
             loses_format = TRUE),
  base::list(key = "args", fun = "all_args_here", export_arg = "fun_export",
             what = "the missing arguments of the called functions have been added.",
             loses_format = TRUE)
)

# TEMPORARY SWITCH (set to FALSE on 2026-10-22 so that the rest of the app can be
# tested without saferDev >= 3.0.0 installed).
# TRUE  : when saferDev (>= 3.0.0) is not available, window 1 shows a red
#         message asking to install it, and window 2 cannot be reached.
# FALSE : the five checks are skipped (message in the R console only) and the
#         app goes on to window 2.
# Set it back to TRUE as soon as saferDev >= 3.0.0 is installed.
.safer_dev_required <- FALSE

# Is saferDev available, in a version that provides the five checks? An
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
# The modified code is obtained by exporting it in a temporary file
# (<var_name>_safer.R, written by saferDev) that is read back and deleted.
# Returns list(fun_txt, report, clean, err):
#   fun_txt : text of the modified function (starting with "function"), or NULL
#             if the check did not modify the code
#   report  : text printed by saferDev (messages and warnings, without the
#             message giving the path of the temporary export file)
#   clean   : TRUE when the check reports nothing to modify or to complete
#   err     : error message of saferDev (NULL if no error)
.run_check_step <- function(step, f, var_name) {
  out_dir <- base::tempfile(pattern = "saferMake_check_", tmpdir = base::tempdir(check = FALSE), fileext = "")
  base::dir.create(path = out_dir, showWarnings = FALSE, recursive = FALSE, mode = "0777")
  base::on.exit(expr = base::unlink(x = out_dir, recursive = TRUE, force = TRUE, expand = TRUE), add = TRUE, after = TRUE)
  env <- base::new.env(hash = TRUE, parent = base::globalenv(), size = 29L)
  base::assign(x = var_name, value = f, pos = -1, envir = env, inherits = FALSE, immediate = TRUE)
  # saferDev designates the function by the name written in the call: the call
  # is built with that name and evaluated in env
  args <- base::list(base::as.name(x = var_name), TRUE, out_dir)
  base::names(x = args) <- base::c("x", step$export_arg, "out_path")
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
                            recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
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
  # correct itself (other functions without package, calls on several lines, ...)
  res$clean <- base::grepl(pattern = "EVERYTHING SEEMS CLEAN", x = report, ignore.case = FALSE,
                           perl = FALSE, fixed = TRUE, useBytes = FALSE) &&
               ! base::grepl(pattern = "ARE MISSING", x = report, ignore.case = FALSE,
                             perl = FALSE, fixed = TRUE, useBytes = FALSE)
  res
}

# Full staged check of the pasted code (the five checks of the Run button).
# code     : the pasted code (character), already evaluated without error
# f        : the function evaluated from code with keep.source = TRUE
# fun_name : the function name ("<anonymous>" when the code is a bare function)
# Returns list(stage, code, step, notice, report) with:
#   stage  : "ok" (the five checks are clean), "modified" (the check number
#            step modified the code), "findings" (the check number step
#            reports something that it cannot correct itself: the user must
#            complete the code), "error" (the saferDev function stopped), or
#            "skip" (saferDev >= 3.0.0 is not available: no check can be
#            done, the user is told to install it)
#   code   : the code to display in window 1 (modified code at stage
#            "modified", otherwise the unchanged pasted code)
#   step   : number (1 to 5) of the check that stopped the loop, NULL at stage
#            "ok" and "skip"
#   notice : lines of the orange message of window 1 (character(0) at stage
#            "ok")
#   report : text printed by saferDev for that check ("" at stage "ok")
.run_code_checks <- function(code, f, fun_name = "<anonymous>") {
  res <- base::list(stage = "ok", code = code, step = NULL,
                    notice = base::character(length = 0L), report = "")
  if (! .safer_dev_available()) {
    # the five checks cannot be run: never go on silently to window 2
    res$stage <- "skip"
    res$notice <- base::paste0("The safer-r code verifications need the saferDev package, version 3.0.0 or later, which is not available in the R session running this app (installed version: ",
                               if (base::requireNamespace(package = "saferDev", quietly = TRUE)) {base::as.character(x = utils::packageVersion(pkg = "saferDev", lib.loc = NULL))} else {"none"},
                               "). Please, install saferDev >= 3.0.0, restart R, and run saferMake::make() again.",
                               collapse = NULL, recycle0 = FALSE)
    base::return(res)
  }
  var_name <- .check_var_name(fun_name = fun_name)
  n_steps <- base::length(x = .code_check_steps)
  for (i in base::seq_len(length.out = n_steps)) {
    step <- .code_check_steps[[i]]
    out <- .run_check_step(step = step, f = f, var_name = var_name)
    head_txt <- base::paste0("Check ", i, "/", n_steps, " (saferDev::", step$fun, "()): ",
                             collapse = NULL, recycle0 = FALSE)
    if (! base::is.null(x = out$err)) {
      res$stage <- "error"
      res$step <- i
      res$report <- out$err
      res$notice <- base::paste0(head_txt, "saferDev stopped with the message below. Please, correct your code in the box below, then click on the Run button again.",
                                 collapse = NULL, recycle0 = FALSE)
      base::return(res)
    }
    if (! base::is.null(x = out$fun_txt)) {
      new_code <- .splice_function(code = code, f = f, fun_txt_new = out$fun_txt, fun_name = fun_name)
      res$step <- i
      res$report <- out$report
      if (base::identical(x = new_code, y = code, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE, extptr.as.ref = FALSE)) {
        # saferDev reported something but the text is unchanged: handing back
        # the same code as "modified" would loop without end
        res$stage <- "findings"
        res$notice <- base::paste0(head_txt, "the check reported the findings below, that could not be corrected automatically. Please, complete your code in the box below, then click on the Run button again.",
                                   collapse = NULL, recycle0 = FALSE)
        base::return(res)
      }
      res$stage <- "modified"
      res$code <- new_code
      res$notice <- base::c(
        base::paste0(head_txt, step$what, collapse = NULL, recycle0 = FALSE),
        "The modified code is now in the box below. Please, check it, then click on the Run button again.",
        if (base::isTRUE(x = step$loses_format)) {
          base::paste0("Warning: saferDev::", step$fun, "() removes the comments of the code and can change its indentation.",
                       collapse = NULL, recycle0 = FALSE)
        }
      )
      base::return(res)
    }
    if (! out$clean) {
      res$stage <- "findings"
      res$step <- i
      res$report <- out$report
      res$notice <- base::paste0(head_txt, "the check reported the findings below, that could not be corrected automatically. Please, complete your code in the box below, then click on the Run button again.",
                                 collapse = NULL, recycle0 = FALSE)
      base::return(res)
    }
  }
  res
}
