# Insertion of the missing argument names reported by saferDev::all_args_here().
# Extracted from inst/app/server.R; see the header of R/intern_.app_helpers.R.

# Apply the corrections suggested by saferDev::all_args_here() (its STATUS
# column) to a function: each row of the data.frame contains the whole corrected
# call text (STATUS), the call text as read (FUN_ARGS, WITHOUT any pkg:: prefix)
# and the position of the call name (FUN_POS) in the NORMALIZED code produced by
# .functions_detect inside saferDev: utils::capture.output() of the function
# (original text, comments included when the function was parsed with
# keep.source = TRUE), then all " +" collapsed to " " and leading spaces of each
# line removed. LINE_NB is the 1-based line number of that capture.output text
# (the "function(x){" header is line 1). Verified against saferDev source.
# This helper reproduces the normalization, maps the normalized positions back
# to the raw lines, applies the corrections back-to-front (later lines first,
# so that earlier lines stay valid) and never applies a correction that does
# not pass its guards: FUN_ARGS must be balanced on one line (multi-line calls
# are left to the user), the STATUS must parse, and the final corrected code
# must parse (all_args_here can produce a broken STATUS when a required
# positional argument is missing, e.g. substr(x, 1) -> "substr(x = x,
# start =  1, stop = )").
# At most ONE correction is applied per line per call: the STATUS of
# all_args_here() is rebuilt from the code it read, so the corrections of
# nested calls on the same line (sum(mean(x))) overlap, and applying the
# outer correction with a span computed from the original line would corrupt
# the already-corrected inner one. The caller loop re-runs all_args_here() on
# the corrected code, so the outer correction is rebuilt and applied at the
# next iteration of the loop.
# f        : the function object that was given to all_args_here() (same object,
#            so that the capture.output text and the FUN_POS positions match)
# rep      : the data.frame returned by all_args_here()
# fun_name : name of the function (for the rebuilt "fun_name <- function..." text)
# Returns list(fun_txt, inserted, left) where fun_txt is the corrected function
# text (NULL when nothing was applied), inserted the applied corrections and
# left the rows that could not be applied (with their line and STATUS).
.args_insert <- function(f, rep, fun_name) {
  res <- base::list(fun_txt = NULL,
                    inserted = base::character(length = 0L),
                    left = base::character(length = 0L))
  if (base::is.null(x = f) || base::is.null(x = rep) ||
      ! base::is.data.frame(x = rep) || base::nrow(x = rep) == 0L) {
    base::return(res)
  }
  keep <- base::nzchar(x = rep$MISSING_ARGS, keepNA = FALSE) &
    ! base::is.na(x = rep$MISSING_ARGS)
  if (! base::any(x = keep, na.rm = TRUE)) {
    base::return(res)
  }
  rows <- rep[keep, , drop = FALSE]
  # raw function text, exactly what all_args_here() read
  code_lines <- utils::capture.output(x = f, file = NULL, append = FALSE,
                                      type = NULL, split = FALSE)
  code_lines <- code_lines[! base::grepl(pattern = "^<environment:", x = code_lines,
                                         ignore.case = FALSE, perl = FALSE,
                                         fixed = FALSE, useBytes = FALSE)]
  # same normalization as .functions_detect, per line
  norm <- code_lines
  norm <- base::gsub(pattern = " +", replacement = " ", x = norm, perl = FALSE,
                     fixed = FALSE, useBytes = FALSE)
  norm <- base::sub(pattern = "^ +", replacement = "", x = norm, perl = FALSE,
                    fixed = FALSE, useBytes = FALSE)
  # apply back-to-front: highest LINE_NB first, then highest FUN_POS, so that
  # replacing a span never invalidates the position of a not-yet-applied row;
  # at most one row per line (see the header: nested calls on the same line
  # have overlapping spans, the outer one is rebuilt and applied by the next
  # iteration of the caller loop)
  ord <- base::order(x = rows$LINE_NB, rows$FUN_POS, decreasing = TRUE)
  ord <- ord[! base::duplicated(x = rows$LINE_NB[ord])]
  inserted <- base::character(length = 0L)
  left <- base::character(length = 0L)
  for (i in ord) {
    st <- rows$STATUS[i]
    pos <- rows$FUN_POS[i]
    ln <- rows$LINE_NB[i]
    # validation 1: the STATUS must parse as R code AND must not contain an
    # empty argument value (" = )" or " =,"): R parses those as a "missing"
    # argument, which would insert a no-op correction with odd formatting when
    # all_args_here reports a required positional argument without a default
    ok_parse <- FALSE
    empty_val <- base::grepl(pattern = "=\\s*[,)]", x = st, ignore.case = FALSE,
                             perl = TRUE, fixed = FALSE, useBytes = FALSE)
    if (! empty_val) {
      ok_parse <- base::tryCatch(
        expr = {
          base::parse(file = "", n = NULL, text = st, prompt = "?",
                      keep.source = base::getOption(x = "keep.source", default = NULL),
                      srcfile = NULL, encoding = "unknown")
          TRUE
        },
        error = function(e) FALSE
      )
    }
    if (! ok_parse || ln < 1L || ln > base::length(x = norm) ||
        base::is.na(x = pos) || pos < 1L) {
      left <- base::c(left, base::paste0("line ", ln, ": ", st,
                                         collapse = NULL, recycle0 = FALSE))
      next
    }
    # validation 2: FUN_ARGS must be a complete one-line call: parse it (a
    # multi-line call is truncated at the line end and does not parse, and a
    # call containing a "(" string still parses)
    fa <- rows$FUN_ARGS[i]
    fa_len <- base::nchar(x = fa, type = "chars", allowNA = FALSE, keepNA = NA)
    if (fa_len == 0L || ! base::tryCatch(
      expr = {
        base::parse(file = "", n = NULL, text = fa, prompt = "?",
                    keep.source = base::getOption(x = "keep.source", default = NULL),
                    srcfile = NULL, encoding = "unknown")
        TRUE
      },
      error = function(e) FALSE
    )) {
      left <- base::c(left, base::paste0("line ", ln, ": ", st,
                                         collapse = NULL, recycle0 = FALSE))
      next
    }
    # validation 3: FUN_ARGS must sit exactly at FUN_POS in the normalized line
    at <- base::substr(x = norm[ln], start = pos,
                       stop = pos + fa_len - 1L)
    if (base::is.na(x = at) || at != fa) {
      left <- base::c(left, base::paste0("line ", ln, ": ", st,
                                         collapse = NULL, recycle0 = FALSE))
      next
    }
    # map the normalized positions back to the raw line: the normalization only
    # removes leading spaces and collapses runs of spaces, so walk the raw line
    raw_line <- code_lines[ln]
    nch_raw <- base::nchar(x = raw_line, type = "chars", allowNA = FALSE, keepNA = NA)
    nch_norm <- base::nchar(x = norm[ln], type = "chars", allowNA = FALSE, keepNA = NA)
    if (pos > nch_norm || pos + fa_len - 1L > nch_norm) {
      left <- base::c(left, base::paste0("line ", ln, ": ", st,
                                         collapse = NULL, recycle0 = FALSE))
      next
    }
    map_raw <- base::integer(length = nch_norm)
    j <- 1L          # norm index
    k <- 1L          # raw index
    lead_done <- FALSE
    while (k <= nch_raw && j <= nch_norm) {
      ch <- base::substr(x = raw_line, start = k, stop = k)
      if (ch == " " && ! lead_done) {
        k <- k + 1L
        next
      }
      lead_done <- TRUE
      if (ch == " " && base::substr(x = norm[ln], start = j, stop = j) == " ") {
        map_raw[j] <- k
        j <- j + 1L
      } else if (ch != " ") {
        map_raw[j] <- k
        j <- j + 1L
      }
      k <- k + 1L
    }
    pos_raw <- map_raw[pos]
    end_raw <- map_raw[pos + fa_len - 1L]
    if (base::length(x = pos_raw) == 0L || base::length(x = end_raw) == 0L ||
        pos_raw < 1L || end_raw > nch_raw) {
      left <- base::c(left, base::paste0("line ", ln, ": ", st,
                                         collapse = NULL, recycle0 = FALSE))
      next
    }
    # apply on the RAW line: the prefix of the line before the corrected span is
    # untouched, so the norm positions of the remaining rows on this same line
    # (all with smaller FUN_POS, since rows are applied in decreasing order)
    # stay valid against the original norm
    code_lines[ln] <- base::paste0(
      base::substr(x = raw_line, start = 1L, stop = pos_raw - 1L),
      st,
      base::substr(x = raw_line, start = end_raw + 1L, stop = nch_raw),
      collapse = NULL,
      recycle0 = FALSE
    )
    inserted <- base::c(inserted, base::paste0("line ", ln, ": ", st,
                                               collapse = NULL, recycle0 = FALSE))
  }
  if (base::length(x = inserted) > 0L) {
    fun_txt <- base::paste0(fun_name, " <- ",
                            base::paste(code_lines, collapse = "\n", recycle0 = FALSE))
    # final guard: never hand back code that does not parse
    ok_full <- base::tryCatch(
      expr = {
        base::parse(file = "", n = NULL, text = fun_txt, prompt = "?",
                    keep.source = base::getOption(x = "keep.source", default = NULL),
                    srcfile = NULL, encoding = "unknown")
        TRUE
      },
      error = function(e) FALSE
    )
    if (! ok_full) {
      res$left <- base::c(res$left, inserted,
                          "corrected code does not parse: all corrections discarded")
      inserted <- base::character(length = 0L)
      base::return(res)
    }
    res$fun_txt <- fun_txt
  }
  res$inserted <- inserted
  res$left <- left
  res
}
