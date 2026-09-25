library(shiny)

server <- function(input, output, session) {
  
  # ---- App state ----------------------------------------------------------
  rv <- reactiveValues(
    screen    = "input",   # "input" | "error" | "result"
    code      = "",        # last pasted code (so "Back" restores the box)
    pkg_name  = "",        # value typed in the "Package name" box (kept on Back)
    error_msg = NULL,      # real error: logged to console only, NEVER displayed
    fun_name  = NULL,
    fun_args  = NULL,      # character vector of argument names
    fun_body  = NULL       # single string: everything after '{'
  )
  
  # Fixed error message requested by the user (what the user sees)
  ERROR_TEXT <- "The code provided returned an error. Please, click on the back button and provide a function that runs well."
  
  # Build a valid HTML id from an argument name
  arg_id <- function(nm) paste0("arg_", gsub("[^[:alnum:]_]", "_", nm))
  
  # Go to the error screen; detail is logged in the R console for the developer
  go_error <- function(detail) {
    message("App error (not shown to the user): ", detail)
    rv$error_msg <- detail
    rv$screen    <- "error"
  }
  
  # ---- Shared UI fragments (used on input and error screens) --------------
  intro <- function() {
    list(
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
  
  # ---- RUN: catch the pasted code, execute it, extract the pieces ----------
  observeEvent(input$run_button, {
    code <- isolate(input$user_ini_fun)
    code <- if (is.null(code)) "" else code
    rv$code      <- code
    rv$error_msg <- NULL
    rv$fun_name  <- NULL
    rv$fun_args  <- NULL
    rv$fun_body  <- NULL
    
    # Empty field -> error screen
    if (!nzchar(trimws(code))) {
      go_error("The field is empty.")
      return(invisible())
    }
    
    # 1) Parse + execute in a scratch environment
    env <- new.env(parent = globalenv())
    err <- NULL
    res <- tryCatch(
      eval(parse(text = code), envir = env),
      error = function(e) { err <<- conditionMessage(e); NULL }
    )
    
    if (!is.null(err)) {
      go_error(err)
      return(invisible())
    }
    
    # 2) Identify the function defined by the pasted code
    fnames <- Filter(function(n) is.function(get(n, envir = env)), ls(envir = env))
    f <- NULL
    if (length(fnames) == 1L) {
      rv$fun_name <- fnames
      f <- get(fnames, envir = env)
    } else if (length(fnames) == 0L && is.function(res)) {
      rv$fun_name <- "<anonymous>"     # e.g. just `function(x) ...` was pasted
      f <- res
    } else if (length(fnames) > 1L) {
      go_error(paste0("Execution succeeded but the code defines several functions (",
                      paste(fnames, collapse = ", "), ")."))
      return(invisible())
    } else {
      go_error("Execution succeeded but no function was defined by the code.")
      return(invisible())
    }
    
    # 3) Extract: argument names + full body (everything after '{')
    args <- names(formals(f))
    rv$fun_args <- if (is.null(args)) character(0) else args
    
    bl <- deparse(body(f), width.cutoff = 500L)
    if (length(bl) && identical(trimws(bl[1]), "{"))          bl <- bl[-1]
    if (length(bl) && identical(trimws(bl[length(bl)]), "}")) bl <- bl[-length(bl)]
    rv$fun_body <- paste(bl, collapse = "\n")
    
    rv$screen <- "result"
  })
  
  # ---- BACK buttons ---------------------------------------------------------
  # Keep the package name typed by the user before leaving the result screen
  observeEvent(input$back_from_result, {
    pkg <- input$pkg_name
    rv$pkg_name <- if (is.null(pkg)) "" else pkg
    rv$screen <- "input"
  })
  observeEvent(input$back_from_error, { rv$screen <- "input" })
  
  # ---- Sidebar: Table of Contents (depends on the current screen) -----------
  output$toc <- renderUI({
    entries <- if (identical(rv$screen, "result")) {
      c(
        list(tags$li(tags$a(href = "#pkg_section", "Package name"))),
        if (length(rv$fun_args) == 0L) {
          list(tags$li("No arguments"))
        } else {
          lapply(rv$fun_args, function(nm) {
            tags$li(tags$a(href = paste0("#", arg_id(nm)), nm))
          })
        }
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
                        value = rv$code,   # restores the pasted code after "Back"
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
          # the pasted-code box is REPLACED by the fixed error message
          div(class = "alert alert-danger", role = "alert", style = "margin-top: 10px;",
              ERROR_TEXT),
          hr(),
          back_btn("back_from_error")
        ),
        
        # Result screen:
        #   1) untitled detection text (intro style, NOT in the TOC)
        #   2) "Package name" section (in the TOC)
        #   3) one titled section per argument name (in the TOC)
        result = {
          args_txt <- if (length(rv$fun_args) == 0L) {
            "none"
          } else {
            paste(rv$fun_args, collapse = ", ")
          }
          
          # 1) Detection text, styled exactly like the intro list items
          detected_block <- tags$div(
            tags$ol(class = "input-instruction-list",
              tags$li(paste0("Function detected: ", rv$fun_name)),
              tags$li(paste0("Arguments detected: ", args_txt))
            )
          )
          
          # 2) "Package name" section
          pkg_section <- tagList(
            h4(id = "pkg_section", "Package name"),
            tags$div(
              tags$ol(class = "input-instruction-list",
                tags$li("Does this function belong to a package? If yes, indicate its name. Otherwise, leave blanck.")
              )
            ),
            textInput(inputId = "pkg_name",
                      label = NULL,
                      value = rv$pkg_name,
                      placeholder = "Package name",
                      width = "100%")
          )
          
          if (length(rv$fun_args) == 0L) {
            tagList(
              detected_block,
              hr(),
              pkg_section,
              hr(),
              back_btn("back_from_result")
            )
          } else {
            n <- length(rv$fun_args)
            tagList(
              detected_block,
              hr(),
              pkg_section,
              hr(),
              lapply(seq_len(n), function(i) {
                sec <- tagList(h4(id = arg_id(rv$fun_args[i]), rv$fun_args[i]))
                if (i < n) sec <- tagList(sec, hr())
                sec
              }),
              hr(),
              back_btn("back_from_result")
            )
          }
        }
      )
    )
  })
}