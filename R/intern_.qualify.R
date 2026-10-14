# Base-R function qualification: add <pkg>:: before basic R function calls in
# the submitted code. Part of the staged check loop of the app (see
# R/intern_.code_checks.R).

# The 7 default search-path packages, i.e. exactly the set that
# saferDev::colons_check() considers "basic" (verified against its source:
# stats, graphics, grDevices, utils, datasets, methods, base).
.base_pkgs <- base::c("stats", "graphics", "grDevices", "utils", "datasets",
                      "methods", "base")

# Resolve a call name to its package when the first binding of the name on the
# current search path is one of the base packages (so that a user-attached
# package like dplyr shadowing stats::filter is NOT silently qualified: the
# name is skipped and reported instead). This mirrors what R actually calls
# for a bare call: the first binding found on the search path.
# Returns:
#   pkg        : single package name when the first binding is a base package
#                (even when the name also exists in other base packages: the
#                first search-path binding is the one R uses),
#                NA_character_ (not found anywhere on the search path), or
#                NULL (the name is shadowed: first binding is not a base
#                package, e.g. a user-attached package or the global env)
.base_pkg_of <- function(name) {
  where <- utils::find(name)   # e.g. c("package:stats", "package:base")
  if (base::length(x = where) == 0L) {
    base::return(NA_character_)
  }
  first <- where[1]
  if (base::grepl(pattern = "^(package:)?(stats|graphics|grDevices|utils|datasets|methods|base)$",
                  x = first, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                  useBytes = FALSE)) {
    base::return(base::sub(pattern = "^package:", replacement = "", x = first,
                           ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                           useBytes = FALSE))
  }
  NULL
}

# Terminal SYMBOL and SYMBOL_FUNCTION_CALL tokens of the subtree of the expr
# with id eid in the parse data pd (recursion into the nested exprs): the
# names bound by the LHS of an assignment, e.g. mean for
# `mean <- function(y) ...`, names and x for `names(x) <- v`, or a and b for
# `a$b <- 1`. SYMBOL_FORMALS (named arguments like FUN = sum) are excluded.
.syms_in_expr <- function(pd, eid) {
  kids <- pd[pd$parent == eid, , drop = FALSE]
  out <- base::character(length = 0L)
  for (j in base::seq_len(length.out = base::nrow(x = kids))) {
    if (kids$token[j] %in% base::c("SYMBOL", "SYMBOL_FUNCTION_CALL") &&
        kids$terminal[j]) {
      # backtick-quoted names keep their backticks in the parse data: strip
      # them so that `paste0` <- ... binds the bare call name paste0
      txt <- base::sub(pattern = "^`(.*)`$", replacement = "\\1",
                       x = kids$text[j], ignore.case = FALSE, perl = TRUE,
                       fixed = FALSE, useBytes = FALSE)
      out <- base::c(out, txt)
    } else if (kids$token[j] == "expr") {
      out <- base::c(out, .syms_in_expr(pd = pd, eid = kids$id[j]))
    }
  }
  out
}

# Document span (start and end keys, line * 1e7 + column) of every expr of the
# parse data pd, computed from its terminal descendants. getParseData can
# produce synthetic expr rows whose own line1/col1/col2 columns are
# inconsistent (e.g. col2 < col1 for the expr wrapping a function definition),
# so the real span must be derived from the terminal tokens: children always
# have smaller ids than their parent, hence the single reverse-id pass.
.expr_spans <- function(pd) {
  is_expr <- pd$token == "expr"
  start_key <- pd$line1 * 1e7 + pd$col1
  end_key <- pd$line1 * 1e7 + pd$col2
  ord <- base::order(x = pd$id, decreasing = TRUE, na.last = NA, method = "auto")
  for (r in ord) {
    if (is_expr[r]) {
      kids <- base::which(x = pd$parent == pd$id[r])
      if (base::length(x = kids) > 0L) {
        start_key[r] <- base::min(x = start_key[kids], na.rm = TRUE)
        end_key[r] <- base::max(x = end_key[kids], na.rm = TRUE)
      }
    }
  }
  base::list(start = start_key, end = end_key)
}

# Qualify basic-R calls in the submitted code with <pkg>:: (text-preserving
# splice: comments, strings and user formatting are untouched outside the
# inserted text). fun_args: names of the function's own formals — a call whose
# name is a formal (e.g. function(plot) plot(x)) must stay bare, it is a
# function VALUE.
# Returns list(new_txt, qualified, skipped) with reason strings for the report.
.qualify_base_calls <- function(txt, fun_args = base::character(length = 0L)) {
  res <- base::list(new_txt = txt,
                    qualified = base::character(length = 0L),
                    skipped = base::character(length = 0L))
  if (base::length(x = base::trimws(x = txt, which = "both", whitespace = "[ \t\r\n]")) == 0L ||
      ! base::nzchar(x = base::trimws(x = txt, which = "both", whitespace = "[ \t\r\n]"),
                     keepNA = FALSE)) {
    base::return(res)
  }
  p <- base::tryCatch(
    expr = base::parse(text = txt, keep.source = TRUE),
    error = function(e) NULL
  )
  if (base::is.null(x = p)) {
    res$skipped <- base::c(res$skipped, "PARSE: the code does not parse")
    base::return(res)
  }
  pd <- utils::getParseData(x = p)
  # getParseData rows are NOT strictly document-ordered: always use the (line1,
  # col1, col2) positions, never the row order or the id column, to reason about
  # what precedes a token.
  lines <- base::strsplit(x = txt, split = "\n", fixed = TRUE)[[1]]
  if (base::length(x = lines) == 0L) {
    base::return(res)
  }
  # names bound by the assignments of the code: for EVERY assignment (not only
  # `name <- function(...)`), the bound names must not be qualified. A bare
  # call to a name that the code binds anywhere (a local function called
  # recursively, or a base name rebound to another function like
  # mean <- stats::median) must stay bare: qualifying it would silently change
  # the semantics of the code (and can create infinite recursion). The LHS is
  # the expr sibling immediately before the assign token within the same
  # statement expr (the expr after it for RIGHT_ASSIGN, e.g. 1 -> x). Chained
  # assignments on one line (a <- 1; mean <- function(v) mean(v)) are handled
  # because each assign belongs to its own statement expr, and nested
  # assignments (a <- b <- function(v) ...) because the inner assign belongs
  # to the RHS expr of the outer one. All the SYMBOL (and SYMBOL_FUNCTION_CALL,
  # e.g. names(x) <- v) tokens of the LHS subtree are collected, so that
  # a$b <- 1 also binds a and b (conservative: a skipped call is reported, the
  # user can qualify it manually).
  bound <- base::character(length = 0L)
  assigns <- pd[pd$token %in% base::c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN"), , drop = FALSE]
  if (base::nrow(assigns) > 0L) {
    spans <- .expr_spans(pd = pd)
    for (i in base::seq_len(length.out = base::nrow(x = assigns))) {
      sib <- pd[pd$parent == assigns$parent[i] & pd$token == "expr", , drop = FALSE]
      if (base::nrow(x = sib) == 0L) {
        next
      }
      akey <- assigns$line1[i] * 1e7 + assigns$col1[i]
      aend <- assigns$line1[i] * 1e7 + assigns$col2[i]
      if (assigns$token[i] == "RIGHT_ASSIGN") {
        cand <- sib[spans$start[match(sib$id, pd$id)] >= aend, , drop = FALSE]
        if (base::nrow(x = cand) == 0L) {
          next
        }
        lhs <- cand[base::which.min(x = spans$start[match(cand$id, pd$id)]), , drop = FALSE]
      } else {
        cand <- sib[spans$end[match(sib$id, pd$id)] < akey, , drop = FALSE]
        if (base::nrow(x = cand) == 0L) {
          next
        }
        lhs <- cand[base::which.max(x = spans$end[match(cand$id, pd$id)]), , drop = FALSE]
      }
      bound <- base::c(bound, .syms_in_expr(pd = pd, eid = lhs$id[1]))
    }
    bound <- base::unique(x = bound)
  }
  calls <- pd[pd$token %in% base::c("SYMBOL_FUNCTION_CALL", "SLOT"), , drop = FALSE]
  if (base::nrow(calls) == 0L) {
    base::return(res)
  }
  # build the splice list: (pos, pkg) where pos is the char offset of the token
  # the start of line k in the body is 1 + sum(nchar(lines[1..k-1]) + 1) (one \n
  # after each line)
  nch <- base::nchar(x = lines, type = "chars", allowNA = FALSE, keepNA = NA)
  line_starts <- base::c(1L, base::cumsum(x = nch + 1L)[base::seq_len(length.out = base::length(x = lines) - 1L)] + 1L)
  splices <- base::list()
  for (k in base::seq(along.with = base::seq_len(length.out = base::nrow(calls)))) {
    nm <- calls$text[k]
    ln <- calls$line1[k]
    c1 <- calls$col1[k]
    # 0. skip S4 slot access (x@sum(2)): the slot name is a SLOT token, never
    #    a function call, but colons_check() reports it, so tell the user
    #    explicitly instead of leaving it unexplained
    if (calls$token[k] == "SLOT") {
      res$skipped <- base::c(res$skipped,
        base::paste0(nm, ": S4 slot access (x@...), complete manually", collapse = NULL, recycle0 = FALSE))
      next
    }
    # 1. skip if member access (x$sum(2), obj@f(), x $ sum(2) possibly across
    #    lines): the parent expr of the call token contains a '$' or '@' token
    #    before the call name (checked in the parsed structure, so that it is
    #    safe whatever the spacing and the line breaks are)
    sib_mem <- pd[pd$parent == calls$parent[k] &
                    pd$token %in% base::c("'$'", "'@'"), , drop = FALSE]
    if (base::nrow(x = sib_mem) > 0L &&
        base::any(x = sib_mem$line1 < ln |
                    (sib_mem$line1 == ln & sib_mem$col1 < c1), na.rm = TRUE)) {
      res$skipped <- base::c(res$skipped,
        base::paste0(nm, ": member access ($ or @), left as is", collapse = NULL, recycle0 = FALSE))
      next
    }
    # 2. skip if already qualified (pkg::fun, pkg:::fun, with or without spaces
    #    around :: / :::): the parent expr of the call token contains an
    #    NS_GET / NS_GET_INT token before the call name (pkg:: fun is a single
    #    expr, so the :: is a sibling of the call name). When whitespace (or a
    #    line break) sits between the :: and the name, the code is valid R but
    #    saferDev::colons_check() does not recognize the qualification and
    #    reports the call as missing, so the skip reason says the space must
    #    be removed (the space is not removed automatically: user formatting
    #    is preserved).
    sib_ns <- pd[pd$parent == calls$parent[k] &
                   pd$token %in% base::c("NS_GET", "NS_GET_INT"), , drop = FALSE]
    if (base::nrow(x = sib_ns) > 0L) {
      before <- sib_ns[sib_ns$line1 < ln |
                         (sib_ns$line1 == ln & sib_ns$col1 < c1), , drop = FALSE]
      if (base::nrow(x = before) > 0L) {
        ns_tok <- before[base::nrow(x = before), , drop = FALSE]
        gap <- base::substr(x = txt,
                            start = line_starts[ns_tok$line1[1]] + ns_tok$col2[1],
                            stop = line_starts[ln] + c1 - 2L)
        if (base::nzchar(x = gap, keepNA = FALSE)) {
          res$skipped <- base::c(res$skipped,
            base::paste0(nm, ": qualified with a space or line break after :: or ::: (remove it), left as is", collapse = NULL, recycle0 = FALSE))
        } else {
          res$skipped <- base::c(res$skipped,
            base::paste0(nm, ": already qualified, left as is", collapse = NULL, recycle0 = FALSE))
        }
        next
      }
    }
    # 3. skip if the name is bound in the code (local function, rebound base
    #    name, recursion) or is a formal name of the function. NOTE: the formals
    #    of inline anonymous functions in the body (e.g. lapply(x, function(sum)
    #    sum(1))) are NOT in fun_args, so an inner base-name call shadowed by
    #    such a formal IS qualified (base::sum) — a silent semantic change, but
    #    exactly what saferDev::colons_check() demands, and the safer-r rule
    #    takes precedence: base names must always be qualified
    if (nm %in% bound || nm %in% fun_args) {
      res$skipped <- base::c(res$skipped,
        base::paste0(nm, ": locally bound name or function argument, left as is", collapse = NULL, recycle0 = FALSE))
      next
    }
    # 4. resolve: first search-path binding is a base package
    pkg <- .base_pkg_of(name = nm)
    if (base::is.null(x = pkg)) {
      res$skipped <- base::c(res$skipped,
        base::paste0(nm, ": name resolves first to a non-base package (shadowed); complete manually", collapse = NULL, recycle0 = FALSE))
      next
    }
    if (base::is.na(x = pkg)) {
      res$skipped <- base::c(res$skipped,
        base::paste0(nm, ": not found on the search path (user-defined or other package); complete manually", collapse = NULL, recycle0 = FALSE))
      next
    }
    # splice
    pos <- line_starts[ln] + c1 - 1L
    splices[[base::length(x = splices) + 1L]] <- base::list(pos = pos, txt = base::paste0(pkg, "::", collapse = NULL, recycle0 = FALSE))
    res$qualified <- base::c(res$qualified, base::paste0(pkg, "::", nm, collapse = NULL, recycle0 = FALSE))
  }
  # apply splices back-to-front so that positions stay valid
  if (base::length(x = splices) > 0L) {
    sp <- base::do.call(base::rbind, args = base::lapply(X = splices, FUN = function(s) {
      base::data.frame(pos = s$pos, txt = s$txt, stringsAsFactors = FALSE)
    }))
    sp <- sp[base::order(x = sp$pos, decreasing = TRUE), , drop = FALSE]
    new_txt <- txt
    for (j in base::seq(along.with = base::seq_len(length.out = base::nrow(sp)))) {
      pos <- sp$pos[j]
      new_txt <- base::paste0(
        base::substr(x = new_txt, start = 1L, stop = pos - 1L),
        sp$txt[j],
        base::substr(x = new_txt, start = pos, stop = base::nchar(x = new_txt, type = "chars", allowNA = FALSE, keepNA = NA)),
        collapse = NULL,
        recycle0 = FALSE
      )
    }
    res$new_txt <- new_txt
  }
  res
}
