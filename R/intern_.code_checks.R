# Staged code checks (qualification + colons_check + all_args_here) and the
# correction loop of the code_check screen. Extracted from inst/app/server.R;
# see the header of R/intern_.app_helpers.R.

# Run a zero-argument function, capture every message (muffled) and any error.
# Returns list(value, msgs, err): value is the returned value (NULL on error),
# msgs the captured messages as a character vector, err the error message (NULL
# when no error).
.capture_msg <- function(fun) {
  msgs <- base::character(length = 0L)
  err <- NULL
  value <- base::tryCatch(
    expr = withCallingHandlers(
      expr = fun(),
      message = function(m) {
        msgs <<- base::c(msgs, base::conditionMessage(c = m))
        base::invokeRestart("muffleMessage")
      }
    ),
    error = function(e) {
      err <<- base::conditionMessage(c = e)
      NULL
    }
  )
  base::list(value = value, msgs = msgs, err = err)
}

# Run saferDev::colons_check() on a function, with the messages captured.
# Returns list(clean, report): clean is TRUE when no finding was reported
# (the "ARE MISSING" strings of the BASIC and OTHER channels); a stop of
# colons_check() (e.g. the "ARGUMENT ... IS NOT ALLOWED IN SAFER-R FUNCTIONS"
# check) is also a finding: its text is displayed to the user.
.run_colons_check <- function(f) {
  out <- .capture_msg(fun = function() saferDev::colons_check(x = f))
  report <- base::paste(out$msgs, collapse = "", recycle0 = FALSE)
  clean <- ! base::grepl(pattern = "ARE MISSING", x = report, ignore.case = FALSE,
                         perl = FALSE, fixed = TRUE, useBytes = FALSE)
  if (! base::is.null(x = out$err)) {
    clean <- FALSE
    report <- base::paste0(report, out$err, collapse = NULL, recycle0 = FALSE)
  }
  base::list(clean = clean, report = report)
}

# Run saferDev::all_args_here() on a function, with the messages captured.
# Returns list(clean, report, df): clean TRUE when the function needs no
# argument completion ("EVERYTHING SEEMS CLEAN" message, i.e. no row with a
# non-empty MISSING_ARGS), report the raw captured text, df the returned
# data.frame (NULL when all_args_here() stopped with an error, whose text is
# then in report).
.run_all_args <- function(f) {
  out <- .capture_msg(fun = function() saferDev::all_args_here(x = f))
  if (! base::is.null(x = out$err)) {
    base::return(base::list(clean = FALSE, report = out$err, df = NULL))
  }
  df <- out$value
  if (base::is.null(x = df)) {
    base::return(base::list(clean = FALSE, report = out$err, df = NULL))
  }
  clean <- TRUE
  if (base::is.data.frame(x = df) && base::nrow(x = df) > 0L) {
    clean <- ! base::any(x = base::nzchar(x = df$MISSING_ARGS, keepNA = FALSE),
                         na.rm = TRUE)
  }
  report <- base::paste(out$msgs, collapse = "", recycle0 = FALSE)
  base::list(clean = clean, report = report, df = df)
}

# Is saferDev available for the staged checks? An internal helper (instead of a
# direct base::requireNamespace() call) so that the "saferDev not installed"
# branch can be unit tested.
.safer_dev_available <- function() {
  base::requireNamespace(package = "saferDev", quietly = TRUE)
}

# Full staged check of the pasted code (the conversion loop of the app):
# 1. qualify the basic-R calls of the code (auto, always on);
# 2. saferDev::colons_check() on the qualified function: when findings remain
#    (shadowed names, non-resolvable names), stop at stage "colons" and hand
#    the qualified code back to the user;
# 3. saferDev::all_args_here() on the qualified function: apply the corrections
#    of its STATUS column to the code (auto), then re-run it on the corrected
#    function: when findings remain (multi-line calls, broken STATUS, calls
#    that cannot be completed), stop at stage "args" and hand the corrected
#    code back to the user.
# code     : the pasted code (character)
# fun_args : the argument names of the function (for the qualification)
# fun_name : the function name (for the rebuilt text of the args stage; the
#            "<anonymous>" marker of the app is replaced by "fun_to_complete",
#            because the corrected code must be valid R the user can resubmit)
# f, env   : optional function object and environment already evaluated from
#            code by the caller (the app): when the qualification does not
#            change the code, they are reused instead of evaluating the code a
#            second time, so that the side effects of the user code (print,
#            message, file writes, ...) run only once per Run. When the
#            qualification changes the code, the qualified code is evaluated
#            here (its side effects necessarily run again) and f is ignored.
# Returns list(stage, code, summary, report, df, error_detail) with:
#   stage        : "ok" (all checks clean), "colons", "args", "aah_error",
#                  "skip" (saferDev not installed) or "user_error" (the code
#                  does not eval; error_detail holds the message)
#   code         : the code to hand back (qualified verbatim, or the corrected
#                  function text; unchanged input text when stage = "ok")
#   summary      : human-readable lines describing the automatic modifications
#   report       : raw report text to display ("" at stage "ok")
#   df           : the all_args_here data.frame at stage "args", else NULL
#   error_detail : only at stage "user_error"
.run_code_checks <- function(code, fun_args = base::character(length = 0L),
                             fun_name = "fun_to_complete", f = NULL,
                             env = NULL) {
  .run_code_checks_impl(code = code, fun_args = fun_args, fun_name = fun_name,
                        summary = base::character(length = 0L), depth = 0L,
                        f = f, env = env)
}

# Implementation of .run_code_checks(): the summary accumulates over the
# iterations of the correction loop (one automatic pass per stage per Run).
# depth guards against a pathological non-converging loop (hard limit 20).
# f/env are only honored at depth 0 (see the header of .run_code_checks()).
.run_code_checks_impl <- function(code, fun_args, fun_name, summary, depth,
                                  f = NULL, env = NULL) {
  res <- base::list(stage = "ok", code = code, summary = summary,
                    report = "", df = NULL, error_detail = NULL)
  if (depth > 20L) {
    res$stage <- "args"
    res$report <- "The automatic correction loop did not converge. Please, complete the code manually."
    res$summary <- summary
    base::return(res)
  }
  if (! .safer_dev_available()) {
    res$stage <- "skip"
    res$summary <- base::c(res$summary,
      "The saferDev package is not installed: the colons_check() and all_args_here() checks were skipped.")
    base::return(res)
  }
  # 1. qualification of the basic-R calls
  qual <- .qualify_base_calls(txt = code, fun_args = fun_args)
  qualified_code <- qual$new_txt
  if (base::length(x = qual$qualified) > 0L) {
    summary <- base::c(summary, base::paste0(
      "Automatically qualified with <pkg>:: (basic R functions): ",
      base::paste(base::unique(x = qual$qualified), collapse = ", "),
      ".", collapse = NULL, recycle0 = FALSE))
  }
  if (base::length(x = qual$skipped) > 0L) {
    summary <- base::c(summary, base::paste0(
      "Not qualified automatically (complete manually if needed): ",
      base::paste(base::unique(x = qual$skipped), collapse = "; "),
      ".", collapse = NULL, recycle0 = FALSE))
  }
  # rebuild the function from the qualified code; keep.source = TRUE so that
  # the function keeps its srcref and .args_insert() sees the original text.
  # When the qualification changed nothing and the caller already evaluated
  # the same code (f and env passed at depth 0), reuse the function and the
  # environment: the side effects of the user code (print, message, file
  # writes, ...) then run only once per Run.
  if (depth == 0L && identical(x = qualified_code, y = code) &&
      base::is.function(x = f) && base::is.environment(x = env)) {
    base::return(.checks_on_function(f = f, code = code,
                                     qualified_code = qualified_code,
                                     fun_name = fun_name, summary = summary,
                                     depth = depth))
  }
  env2 <- base::new.env(parent = base::globalenv())
  ev <- .capture_msg(fun = function() base::eval(expr = base::parse(text = qualified_code, keep.source = TRUE), envir = env2))
  if (! base::is.null(x = ev$err)) {
    res$stage <- "user_error"
    res$error_detail <- ev$err
    res$code <- code
    res$summary <- base::character(length = 0L)
    base::return(res)
  }
  f2 <- ev$value
  if (! base::is.function(x = f2)) {
    # the code defines the function by assignment: eval() returns the value of
    # the assignment (the function), but fetch it from the env to be safe
    fnames <- base::Filter(f = function(n) base::is.function(x = base::get(x = n, envir = env2, mode = "any", inherits = FALSE)),
                           x = base::ls(name = env2, all.names = FALSE, sorted = FALSE))
    f2 <- if (base::length(x = fnames) > 0L) base::get(x = fnames[1], envir = env2) else ev$value
    if (! base::is.function(x = f2)) {
      res$stage <- "user_error"
      res$error_detail <- "no function found after qualification"
      res$code <- code
      res$summary <- base::character(length = 0L)
      base::return(res)
    }
  }
  .checks_on_function(f = f2, code = code, qualified_code = qualified_code,
                      fun_name = fun_name, summary = summary, depth = depth)
}

# Stages 2 (colons_check) and 3 (all_args_here + insertion) on the evaluated
# function. Shared by the reuse path and the fresh-eval path.
.checks_on_function <- function(f, code, qualified_code, fun_name, summary,
                                depth) {
  res <- base::list(stage = "ok", code = code, summary = summary,
                    report = "", df = NULL, error_detail = NULL)
  # 2. colons_check on the qualified function
  cc <- .run_colons_check(f = f)
  if (! cc$clean) {
    res$stage <- "colons"
    res$code <- qualified_code
    res$report <- cc$report
    res$summary <- base::unique(x = summary)
    base::return(res)
  }
  # 3. all_args_here on the qualified function
  aa <- .run_all_args(f = f)
  if (! aa$clean) {
    if (base::is.null(x = aa$df)) {
      # all_args_here() stopped (e.g. method call x@sum(2) that colons_check
      # passes but all_args_here cannot resolve): display its error text
      res$stage <- "aah_error"
      res$code <- qualified_code
      res$report <- aa$report
      res$summary <- base::unique(x = summary)
      base::return(res)
    }
    ins <- .args_insert(f = f, rep = aa$df, fun_name = fun_name)
    if (! base::is.null(x = ins$fun_txt)) {
      summary <- base::c(summary, base::paste0(
        "Automatically inserted the argument names reported by all_args_here(): ",
        base::paste(x = ins$inserted, collapse = " | ", recycle0 = FALSE),
        ".", collapse = NULL, recycle0 = FALSE))
      # re-run the whole check on the corrected code (one auto pass per Run:
      # the remaining findings, if any, are handed back to the user)
      return(.run_code_checks_impl(code = ins$fun_txt, fun_args = fun_args,
                                   fun_name = fun_name, summary = summary,
                                   depth = depth + 1L))
    }
    res$stage <- "args"
    res$code <- qualified_code
    res$report <- aa$report
    res$df <- aa$df
    res$summary <- base::unique(x = summary)
    base::return(res)
  }
  # everything clean: hand back the (possibly qualified) code
  res$code <- qualified_code
  res$summary <- base::unique(x = summary)
  res
}
