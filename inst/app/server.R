library(shiny)

server <- function(input, output, session) {

  # ---- Internal logic (package namespace, aliased for local use) ----------
  # The pure code-generation logic lives in R/ (intern_.app_helpers.R,
  # intern_.arg_check_lines.R, intern_.rebuild.R) so that it can be unit tested
  # in tests/testthat/. The aliases below keep the call sites in this file
  # unchanged. Consequently, running the app from source requires
  # devtools::load_all() (or an installed package) so that the saferMake
  # namespace is available.

  .safer_args           <- saferMake:::.safer_args
  .arg_check_defaults   <- saferMake:::.arg_check_defaults
  .arg_id               <- saferMake:::.arg_id
  .null_cb_id           <- saferMake:::.null_cb_id
  .empty_cb_id          <- saferMake:::.empty_cb_id
  .arg_check_field_ids  <- saferMake:::.arg_check_field_ids
  .no_empty_cb_id       <- saferMake:::.no_empty_cb_id
  .field_blank          <- saferMake:::.field_blank
  .run_arg_check_tests  <- saferMake:::.run_arg_check_tests
  .args_without_default <- saferMake:::.args_without_default
  .extract_aa_body      <- saferMake:::.extract_aa_body
  .build_rebuilt        <- saferMake:::.build_rebuilt

  # ---- Constants ------------------------------------------------------------
  # ERROR_TEXT / INTERNAL_ERROR_TEXT: user-facing texts of the two error channels
  # (Channel 1 = user pasted code error, Channel 3 = internal interface error).
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
    arg_check_settings = list(), # per-argument arg_check() settings (key = .arg_id(nm))
    arg_check_test   = list(),   # per-argument test result (ok, message, value_code)
    # NEW: state needed for the requested behaviours
    prev_fun_name    = NULL,     # function name of the previous successful run
    prev_fun_args    = NULL,     # argument names of the previous successful run
    prev_screen      = NULL,     # screen to return to from the error screen
    check_failed     = character(0), # arguments whose arg_check() test failed
    no_empty_string_args = character(0), # arguments ticked as unable to contain ""
    scroll_to        = NULL      # html id of the block to scroll to (or NULL)
  )

  # ---- JS message handler (register the programmatic download trigger) ------
  # Registered once per rendered screen; re-registering is harmless (overwrite).
  js_download_handler <- function() {
    tags$script(HTML("
      if (typeof Shiny !== 'undefined') {
        Shiny.addCustomMessageHandler('safer_trigger_download', function(id) {
          var el = document.getElementById(id);
          if (el) { el.click(); }
        });
      }
    "))
  }

  # NEW: build the inline script that scrolls the window to a target element
  scroll_to_js <- function(target) {
    if (is.null(target) || !nzchar(target)) return(NULL)
    tags$script(HTML(sprintf(
      "setTimeout(function(){var el = document.getElementById('%s');
       if(el){el.scrollIntoView({behavior:'smooth', block:'start'});}}, 50);",
      target
    )))
  }

  get_arg_check_settings <- function() {
    if (length(rv$fun_args) == 0L) return(list())
    out <- lapply(rv$fun_args, function(nm) {
      ids <- .arg_check_field_ids(.arg_id(nm))
      list(
        # Blank text field = default value (class/typeof: "NULL" = no constraint,
        # mode: "numeric", length/options: "" = no constraint)
        class                     = if (.field_blank(input[[ids$class]])) "NULL" else trimws(input[[ids$class]]),
        typeof                    = if (.field_blank(input[[ids$typeof]])) "NULL" else trimws(input[[ids$typeof]]),
        mode                      = if (.field_blank(input[[ids$mode]])) "NULL" else trimws(input[[ids$mode]]),
        length                    = if (.field_blank(input[[ids$length]])) "NULL" else trimws(input[[ids$length]]),
        prop                      = isTRUE(input[[ids$prop]]),
        double_as_integer_allowed = isTRUE(input[[ids$double_as_integer_allowed]]),
        options                   = if (.field_blank(input[[ids$options]])) "NULL" else trimws(input[[ids$options]]),
        all_options_in_data       = isTRUE(input[[ids$all_options_in_data]]),
        na_contain                = isTRUE(input[[ids$na_contain]]),
        neg_values                = isTRUE(input[[ids$neg_values]]),
        inf_values                = isTRUE(input[[ids$inf_values]]),
        no_empty_string           = isTRUE(input[[ids$no_empty_string]])
      )
    })
    names(out) <- vapply(rv$fun_args, .arg_id, character(1L), USE.NAMES = FALSE)
    out
  }

  # ---- Error channels -------------------------------------------------------
  # NEW: snapshot_result_inputs() keeps the current result-screen inputs so that
  # the Back button can restore the settings page with all the user values.
  snapshot_result_inputs <- function() {
    pkg <- input$pkg_name
    rv$pkg_name <- if (is.null(pkg)) rv$pkg_name else trimws(pkg)
    lnk <- input$link_name
    rv$link_name <- if (is.null(lnk)) rv$link_name else trimws(lnk)
    if (length(rv$fun_args) > 0L) {
      checked <- vapply(rv$fun_args, function(nm) {
        isTRUE(input[[.null_cb_id(nm)]])
      }, logical(1L))
      rv$null_args     <- rv$fun_args[checked]
      rv$non_null_args <- rv$fun_args[!checked]
      checked_empty <- vapply(rv$fun_args, function(nm) {
        isTRUE(input[[.empty_cb_id(nm)]])
      }, logical(1L))
      rv$empty_args     <- rv$fun_args[checked_empty]
      rv$non_empty_args <- rv$fun_args[!checked_empty]
      s <- get_arg_check_settings()
      if (length(s) > 0L) rv$arg_check_settings <- s
      rv$no_empty_string_args <- rv$fun_args[vapply(rv$fun_args, function(nm) {
        isTRUE(input[[.no_empty_cb_id(nm)]])
      }, logical(1L))]
    }
  }

  set_error <- function(detail, display) {
    # NEW: preserve the user values if the error occurs on the result screen
    if (identical(rv$screen, "result")) snapshot_result_inputs()
    # NEW: remember where to go back when the Back button of the error screen is used
    rv$prev_screen   <- if (identical(rv$screen, "result")) "result" else "input"
    message("App error (not shown to the user): ", detail)
    rv$error_msg     <- detail
    rv$error_display <- display
    rv$scroll_to     <- "error_alert_top"   # NEW: scroll target for the error screen
    rv$screen        <- "error"
  }
  user_code_error <- function(detail) set_error(detail, ERROR_TEXT)
  internal_error  <- function(detail) set_error(detail, INTERNAL_ERROR_TEXT)

  # ---- Shared UI fragments (used on input and error screens) --------------
  # NEW: render a character string containing markdown-style `code` spans as a
  # tag list, where each backtick-quoted span becomes a <code> element. Used
  # for labels/headings so that `class()` etc. display with the code style.
  md_code <- function(txt) {
    nbt <- nchar(txt) - nchar(gsub(pattern = "`", replacement = "", x = txt,
                                   fixed = TRUE, useBytes = FALSE))
    parts <- unlist(strsplit(x = txt, split = "`", fixed = TRUE),
                    recursive = TRUE, use.names = FALSE)
    # strsplit() drops a trailing empty field: "a`b`" -> c("a", "b").
    # Re-append the dropped empty part when the backticks are paired so that
    # the last real segment keeps an even (code) index.
    if (nbt %% 2L == 0L) parts <- c(parts, "")
    do.call(tagList,
            lapply(seq_along(parts), function(i) {
              # odd indices are plain text, even indices are inside `...`;
              # with an odd number of backticks, the last segment is literal
              if (i %% 2L == 1L) {
                parts[i]
              } else if (i == length(parts) && nbt %% 2L == 1L) {
                paste0("`", parts[i])
              } else if (!nzchar(parts[i])) {
                character(0)   # dropped empty trailing part: render nothing
              } else {
                tags$code(parts[i])
              }
            }))
  }

  intro <- function() {
    list(
      js_download_handler(),   # NEW: register the programmatic download trigger
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
    rv$fun_body      <- NULL
    rv$aa            <- NULL
    # FIX (new behaviour): the checkbox states and arg_check() settings are NOT
    # wiped anymore here. They are kept when the same function is re-run, so the
    # Back button can restore the settings page with all the user values. They
    # are reset only if the pasted function (name or arguments) changed.

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
    rv$no_default_args <- .args_without_default(f)
    parts <- .extract_aa_body(code)
    if (is.null(parts)) {
      internal_error(".extract_aa_body() could not locate the signature/body.")
      return(invisible())
    }
    rv$aa        <- parts$aa
    rv$fun_body  <- parts$body

    # NEW: keep or reset the user settings depending on whether the same
    # function (same name and same arguments) is re-run.
    same_fun <- identical(rv$fun_name, rv$prev_fun_name) &&
                identical(as.character(rv$fun_args), as.character(rv$prev_fun_args))
    if (!same_fun) {
      rv$null_args         <- character(0)
      rv$non_null_args     <- character(0)
      rv$empty_args        <- character(0)
      rv$non_empty_args    <- character(0)
      rv$no_empty_string_args <- character(0)
      rv$arg_check_settings <- list()
      rv$arg_check_test    <- list()
    } else {
      # keep only the settings/tests matching the current arguments
      keep <- intersect(names(rv$arg_check_settings),
                        vapply(rv$fun_args, .arg_id, character(1L)))
      rv$arg_check_settings <- rv$arg_check_settings[keep]
      keep_test <- intersect(names(rv$arg_check_test),
                             vapply(rv$fun_args, .arg_id, character(1L)))
      rv$arg_check_test <- rv$arg_check_test[keep_test]
      # sanitize the checkbox vectors against the current argument list
      rv$null_args      <- intersect(rv$null_args,      rv$fun_args)
      rv$non_null_args  <- setdiff(rv$fun_args, rv$null_args)
      rv$empty_args     <- intersect(rv$empty_args,     rv$fun_args)
      rv$non_empty_args <- setdiff(rv$fun_args, rv$empty_args)
      rv$no_empty_string_args <- intersect(rv$no_empty_string_args, rv$fun_args)
    }
    rv$prev_fun_name <- rv$fun_name
    rv$prev_fun_args <- rv$fun_args
    rv$check_failed  <- character(0)   # NEW: reset the failing list on a new run
    rv$scroll_to     <- NULL           # NEW

    collide <- intersect(rv$fun_args, .safer_args)
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
      parse(text = .build_rebuilt(aa = rv$aa, body = rv$fun_body, pkg = "", link = "",
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
      internal_error("The rebuilt function does not parse - check .build_rebuilt() (R/intern_.rebuild.R).")
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

  # ---- NEW: CHECK + SAVE logic -----------------------------------------------
  # The "Run" button of the result screen runs the arg_check() tests. The
  # download (pop-up to save the modified function) is triggered ONLY if no
  # arg_check() line failed. Otherwise, a summary error is displayed at the top
  # of the result screen and the window scrolls to it.
  observeEvent(input$check_and_save, {
    tryCatch({
      snapshot_result_inputs()
      rv$arg_check_test <- .run_arg_check_tests(rv$fun_args, rv$arg_check_settings)
      failed <- rv$fun_args[vapply(rv$fun_args, function(nm) {
        !isTRUE(rv$arg_check_test[[.arg_id(nm)]]$ok)
      }, logical(1L))]
      if (length(failed) == 0L) {
        rv$check_failed <- character(0)
        rv$scroll_to    <- NULL
        # trigger the (hidden) downloadButton programmatically -> save pop-up
        session$sendCustomMessage("safer_trigger_download", "download_safer")
      } else {
        rv$check_failed <- failed
        rv$scroll_to    <- "arg_check_error_summary"
      }
    }, error = function(e) {
      internal_error(paste0("Unexpected error in the server code: ",
                            conditionMessage(e)))
    })
  })

  # ---- BACK buttons (checkbox states are preserved like pkg/link) ----------
  observeEvent(input$back_from_result, {
    # NEW: centralized snapshot (pkg, link, checkboxes, arg_check() settings)
    snapshot_result_inputs()
    rv$screen <- "input"
  })
  # NEW: the error-screen Back button returns to the screen the error came from
  # (input OR result), with all the user values restored from rv.
  observeEvent(input$back_from_error, {
    rv$screen <- if (!is.null(rv$prev_screen)) rv$prev_screen else "input"
  })

  # ---- DOWNLOAD: the modified (safer) function -------------------------------
  output$download_safer <- downloadHandler(
    filename = function() {
      paste0(rv$fun_name, "_safer.R")
    },
    content = function(file) {
      # NEW: fallback to rv values if the inputs are not currently rendered
      pkg  <- if (is.null(input$pkg_name)) rv$pkg_name else trimws(input$pkg_name)
      link <- if (is.null(input$link_name)) rv$link_name else trimws(input$link_name)

      if (length(rv$fun_args) > 0L) {
        checked <- vapply(rv$fun_args, function(nm) {
          isTRUE(input[[.null_cb_id(nm)]])
        }, logical(1L))
        null_argument     <- rv$fun_args[checked]
        non_null_argument <- rv$fun_args[!checked]
        checked_empty <- vapply(rv$fun_args, function(nm) {
          isTRUE(input[[.empty_cb_id(nm)]])
        }, logical(1L))
        empty_argument     <- rv$fun_args[checked_empty]
        non_empty_argument <- rv$fun_args[!checked_empty]
        checked_noempty <- vapply(rv$fun_args, function(nm) {
          isTRUE(input[[.no_empty_cb_id(nm)]])
        }, logical(1L))
        no_empty_argument <- rv$fun_args[checked_noempty]
      } else {
        null_argument      <- character(0)
        non_null_argument  <- character(0)
        empty_argument     <- character(0)
        non_empty_argument <- character(0)
        no_empty_argument  <- character(0)
      }
      rv$null_args      <- null_argument      # kept in sync for "Back"
      rv$non_null_args  <- non_null_argument
      rv$empty_args     <- empty_argument
      rv$non_empty_args <- non_empty_argument
      rv$no_empty_string_args <- no_empty_argument

      arg_check_settings <- get_arg_check_settings()
      rv$arg_check_settings <- arg_check_settings       # kept in sync for "Back"

      # NEW: the arg_check() tests are NOT run here anymore. They are run by the
      # check_and_save observer BEFORE the download is triggered, so that the
      # save pop-up appears only when no arg_check() line failed.

      # FIX (bug 2): pass the LOCAL vectors
      code <- .build_rebuilt(aa = rv$aa, body = rv$fun_body, pkg = pkg, link = link,
                            null_args = null_argument,
                            non_null_args = non_null_argument,
                            empty_args = empty_argument,
                            non_empty_args = non_empty_argument,
                            no_default_args = rv$no_default_args,
                            fun_args = rv$fun_args,
                            arg_check_settings = arg_check_settings,
                            no_empty_string_args = no_empty_argument)
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
        if (length(rv$check_failed) > 0L) {
          list(tags$li(tags$a(href = "#arg_check_error_summary",
                              "arg_check() check errors")))
        } else list(),
        list(tags$li(tags$a(href = "#detection_section", "Detection"))),
        list(tags$li(tags$a(href = "#pkg_section", "Package name"))),
        list(tags$li(tags$a(href = "#link_section", "Error report link"))),
        if (length(rv$fun_args) == 0L) {
          list(tags$li("No arguments"))
        } else {
          lapply(rv$fun_args, function(nm) {
            tags$li(tags$a(href = paste0("#", .arg_id(nm)), md_code(paste0("Values of argument `", nm, "`"))))
          })
        }
      )
    } else if (identical(rv$screen, "error")) {
      list(
        tags$li(tags$a(href = "#intro", "Introduction")),
        tags$li(tags$a(href = "#error_alert_top", "Error message"))
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
               div(id = "error_alert_top",              # NEW: scroll target
                   class = "alert alert-danger", role = "alert",
                   style = "margin-top: 10px; white-space: pre-line;",
                   if (is.null(rv$error_display)) ERROR_TEXT else rv$error_display),
               hr(),
               back_btn("back_from_error"),
               scroll_to_js(rv$scroll_to)               # NEW: slide to the top error message
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

               detection_section <- tagList(
                 h4(id = "detection_section", "Detection"),
                 tags$div(
                   tags$ol(class = "input-instruction-list",
                           tags$li(paste0("Function name: ", rv$fun_name)),
                           tags$li(paste0("Arguments: ", args_txt)),
                           tags$li(paste0("Arguments with no default value: ", no_def_txt))
                   )
                 )
               )

               pkg_section <- tagList(
                 h4(id = "pkg_section", "Package name"),
                 textInput(inputId = "pkg_name",
                           label = "Does this function belong to a package? If yes, indicate its name. Otherwise, leave blanck.",
                           value = rv$pkg_name,
                           placeholder = "Package name",
                           width = "100%")
               )

               link_section <- tagList(
                 h4(id = "link_section", "Error report link"),
                 textInput(inputId = "link_name",
                           label = "Do you have a link for users of your function to report any internal errors? If yes, indicate the full link. Otherwise, leave blanck.",
                           value = rv$link_name,
                           placeholder = "Error report link",
                           width = "100%")
               )

               # NEW: summary of the failed arg_check() tests, displayed at the
               # top of the settings page (scroll target when a check fails).
               check_summary <- if (length(rv$check_failed) > 0L) {
                 div(id = "arg_check_error_summary",
                     class = "alert alert-danger", role = "alert",
                     style = "margin-top: 10px; white-space: pre-line;",
                     paste0(
                       "The saferDev::arg_check() check failed for the following argument",
                       ifelse(test = length(rv$check_failed) > 1, yes = "s", no = ""),
                       " (see the red messages in the argument sections below):\n",
                       paste(rv$check_failed, collapse = "\n"),
                       "\n\nPlease fix the settings below and run again."
                     ))
               } else {
                 NULL
               }

               # NEW: the visible "Run" button runs the arg_check() tests; the
               # real downloadButton is hidden and clicked programmatically by
               # the server ONLY when no arg_check() line failed (save pop-up).
               result_footer <- div(
                 style = "text-align: right;",
                 actionButton(inputId = "check_and_save",
                              label = "Run", class = "btn-primary"),
                 downloadButton(outputId = "download_safer",
                                label = "",
                                style = "position: absolute; visibility: hidden;"),
                 actionButton(inputId = "back_from_result",
                              label = "Back", icon = icon("arrow-left"))
               )

               if (length(rv$fun_args) == 0L) {
                 tagList(
                   js_download_handler(),     # NEW: register the download trigger
                   detection_section,
                   hr(),
                   check_summary,             # NEW (NULL when no failure)
                   pkg_section,
                   hr(),
                   link_section,
                   hr(),
                   result_footer,
                   scroll_to_js(rv$scroll_to) # NEW: slide to the top error message
                 )
               } else {
                 n <- length(rv$fun_args)
                 tagList(
                   js_download_handler(),     # NEW: register the download trigger
                   detection_section,
                   hr(),
                   check_summary,             # NEW (NULL when no failure)
                   pkg_section,
                   hr(),
                   link_section,
                   hr(),
                   # One section per argument: NULL/empty checkboxes, then the five
                   # arg_check() text fields stacked vertically (free text, no
                   # dropdown: blank = default value), then the six tick boxes
                   # stacked vertically, then the test result.
                   lapply(seq_len(n), function(i) {
                     nm <- rv$fun_args[i]
                     aid <- .arg_id(nm)
                     st  <- rv$arg_check_settings[[aid]]
                     if (is.null(st)) st <- .arg_check_defaults
                     ids <- .arg_check_field_ids(aid)
                     test_res <- rv$arg_check_test[[aid]]
                     # Show blank in the field when the stored value is the default
                     class_disp  <- if (identical(st$class, "NULL")) "" else st$class
                     typeof_disp <- if (identical(st$typeof, "NULL")) "" else st$typeof
                     mode_disp   <- if (identical(st$mode, "numeric")) "" else st$mode
                     sec <- tagList(
                       h4(id = aid, md_code(paste0("Value of argument `", nm, "`"))),
                       checkboxInput(inputId = .null_cb_id(nm),
                                     label = "Accepts the NULL value",
                                     value = nm %in% rv$null_args,
                                     width = "100%"),
                       checkboxInput(inputId = .empty_cb_id(nm),
                                     label = HTML("Can be an empty argument (e.g., <code>character()</code>)"),
                                     value = nm %in% rv$empty_args,
                                     width = "100%"),
                       hr(),
                       textInput(inputId = ids$class,
                                 label = md_code("Class of the argument values. Left blank means not evaluated by `class()`"),
                                 value = class_disp,
                                 placeholder = "NULL",
                                 width = "100%"),
                       textInput(inputId = ids$typeof,
                                 label = md_code("Type of the argument values. Left blank means not evaluated by `typeof()`."),
                                 value = typeof_disp,
                                 placeholder = "NULL",
                                 width = "100%"),
                       textInput(inputId = ids$mode,
                                 label = md_code("Mode of the argument values. Left blank means not evaluated by `mode()`."),
                                 value = mode_disp,
                                 placeholder = "NULL",
                                 width = "100%"),
                       textInput(inputId = ids$length,
                                 label = md_code("Length of the argument values. Left blank means not evaluated by `length()`."),
                                 value = st$length,
                                 placeholder = "e.g., 3",
                                 width = "100%"),
                       textInput(inputId = ids$options,
                                 label = "Argument values can only be these restricted values (separated by commas or spaces). Left blank means not evaluated.",
                                 value = st$options,
                                 placeholder = "e.g., option1, option2",
                                 width = "100%"),
                       hr(),
                       checkboxInput(inputId = ids$prop,
                                     label = "Argument values are proportions.",
                                     value = isTRUE(st$prop),
                                     width = "100%"),
                       checkboxInput(inputId = ids$double_as_integer_allowed,
                                     label = md_code("Authorize the type `double` for integers if the numeric values have no digits. Example `c(1,2)` authorized even if `typeof(c(1,2))` returns `double`, not `integer`."),
                                     value = isTRUE(st$double_as_integer_allowed),
                                     width = "100%"),
                       checkboxInput(inputId = ids$all_options_in_data,
                                     label = md_code("If the `options` field above is not blank, the argument values must always be all the options, not some of them."),
                                     value = isTRUE(st$all_options_in_data),
                                     width = "100%"),
                       checkboxInput(inputId = ids$na_contain,
                                     label = "Values can contain NA.",
                                     value = isTRUE(st$na_contain),
                                     width = "100%"),
                       checkboxInput(inputId = ids$neg_values,
                                     label = "Values can be negative if they are numeric.",
                                     value = isTRUE(st$neg_values),
                                     width = "100%"),
                       checkboxInput(inputId = ids$inf_values,
                                     label = "Values can be Inf or -Inf if they are numeric.",
                                     value = isTRUE(st$inf_values),
                                     width = "100%"),
                       checkboxInput(inputId = ids$no_empty_string,
                                     label = md_code("If values are strings, they cannot contain empty strings `\"\"`."),
                                     value = isTRUE(st$no_empty_string),
                                     width = "100%"),
                       # result of the arg_check() line test evaluation
                       if (!is.null(test_res)) {
                         list(
                           tags$p(style = "margin-top: 8px;",
                                  "Tested with: ",
                                  tags$code(test_res$value_code)),
                           if (isTRUE(test_res$ok)) {
                             div(class = "alert alert-success", role = "alert",
                                 style = "margin-top: 5px; margin-bottom: 5px;",
                                 "The arg_check() line ran without error for this argument.")
                           } else {
                             div(class = "alert alert-danger", role = "alert",
                                 style = "margin-top: 5px; margin-bottom: 5px; white-space: pre-line;",
                                 test_res$message)
                           }
                         )
                       }
                     )
                     if (i < n) sec <- tagList(sec, hr())
                     sec
                   }),
                   hr(),
                   result_footer,
                   scroll_to_js(rv$scroll_to) # NEW: slide to the top error message
                 )
               }
             }
      )
    )
  })
}
