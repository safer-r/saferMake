library(shiny)

server <- function(input, output, session) {
  
  # ---- App state ----------------------------------------------------------
  rv <- reactiveValues(
    screen    = "input",   # "input" | "error" | "result"
    code      = "",        # last pasted code (so "Back" restores the box)
    error_msg = NULL,
    fun_name  = NULL,
    fun_args  = NULL,      # character vector of argument names
    fun_body  = NULL       # single string: everything after '{'
  )
  
  # ---- Shared UI fragments (identical on every screen) ---------------------
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
      rv$error_msg <- "The field is empty: there is no code to run."
      rv$screen <- "error"
      return(invisible())
    }
    
    # 1) Parse + execute in a scratch environment (errors are caught here,
    #    whether they come from parse() itself or from evaluation)
    env <- new.env(parent = globalenv())
    err <- NULL
    res <- tryCatch(
      eval(parse(text = code), envir = env),
      error = function(e) { err <<- conditionMessage(e); NULL }
    )
    
    if (!is.null(err)) {
      rv$error_msg <- err
      rv$screen <- "error"
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
      rv$error_msg <- paste0("Execution succeeded but the code defines several functions (",
                             paste(fnames, collapse = ", "),
                             "). Please define exactly one function.")
      rv$screen <- "error"
      return(invisible())
    } else {
      rv$error_msg <- "Execution succeeded but no function was defined by the code."
      rv$screen <- "error"
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
  observeEvent(input$back_from_error,  { rv$screen <- "input" })
  observeEvent(input$back_from_result, { rv$screen <- "input" })
  
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
          # the pasted-code box is REPLACED by the error message
          div(class = "alert alert-danger", role = "alert", style = "margin-top: 10px;",
              tags$strong("Error: "), rv$error_msg),
          hr(),
          back_btn("back_from_error")
        ),
        
        result = tagList(
          intro(),
          h4(id = "sec_code", "Code of your function"),
          tags$pre(rv$code),   # your pasted code, shown read-only
          hr(),
          # =================================================================
          # NEW SECTIONS - PLACEHOLDERS. Tell me the sections you want and
          # I replace this block. Everything you need is already available
          # server-side in: rv$fun_name, rv$fun_args, rv$fun_body, rv$code.
          # =================================================================
          h4(id = "sec_new1", "Section title 1"),
          p(class = "input-instruction-label", "Function name"),
          tags$pre(rv$fun_name),
          
          h4(id = "sec_new2", "Section title 2"),
          p(class = "input-instruction-label", "Argument names"),
          tags$ul(lapply(rv$fun_args, tags$li)),
          
          h4(id = "sec_new3", "Section title 3"),
          p(class = "input-instruction-label", "Function body (everything after '{')"),
          tags$pre(rv$fun_body),
          
          hr(),
          back_btn("back_from_result")
        )
      )
    )
  })
}