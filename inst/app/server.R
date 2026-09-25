library(shiny)

server <- function(input, output, session) {
  
  # ---- Constants ------------------------------------------------------------
  # The three additional safer-r arguments
  SAFER_ARGS <- c("lib_path", "safer_check", "error_text")
  
  # Fixed error message requested by the user (default on the error screen)
  ERROR_TEXT <- "The code provided returned an error. Please, click on the back button and provide a function that runs well."
  
  # ---- App state ----------------------------------------------------------
  rv <- reactiveValues(
    screen        = "input",   # "input" | "error" | "result"
    code          = "",        # last pasted code (so "Back" restores the box)
    pkg_name      = "",        # value typed in the "Package name" box (kept on Back)
    error_msg     = NULL,      # real error: logged to console only, NEVER displayed
    error_display = NULL,      # text actually displayed on the error screen
    fun_name  = NULL,
    fun_args  = NULL,          # character vector of argument names
    fun_body  = NULL,          # verbatim body text (everything between '{' and '}')
    aa        = NULL,          # verbatim: beginning of pasted code up to the last argument
    rebuilt   = NULL           # not strictly needed; kept for future preview use
  )
  
  # Build a valid HTML id from an argument name
  arg_id <- function(nm) paste0("arg_", gsub("[^[:alnum:]_]", "_", nm))
  
  # Go to the error screen; 'detail' is logged in the R console for the developer,
  # 'display' is what the user sees (defaults to the fixed message)
  go_error <- function(detail, display = NULL) {
    message("App error (not shown to the user): ", detail)
    rv$error_msg     <- detail
    rv$error_display <- if (is.null(display)) ERROR_TEXT else display
    rv$screen        <- "error"
  }
  
  # ---- Verbatim capture helpers --------------------------------------------
  # Position of the closing bracket matching the one at position 'open'
  match_close <- function(txt, open, close_ch) {
    open_ch  <- substring(txt, open, open)
    chars    <- strsplit(txt, "")[[1]]
    depth    <- 0L
    for (i in open:nchar(txt)) {
      if (chars[i] == open_ch) {
        depth <- depth + 1L
      } else if (chars[i] == close_ch) {
        depth <- depth - 1L
        if (depth == 0L) return(i)
      }
    }
    -1L
  }
  
  # Extracts:
  #   $aa   : verbatim text from position 1 up to the last argument
  #           (i.e., everything BEFORE the closing ')' of the signature)
  #   $body : verbatim text between the '{' and '}' of the function body
  extract_aa_body <- function(code) {
    m <- regexpr("function[[:space:]]*\\(", code)
    if (m == -1) return(NULL)
    p_open  <- m + attr(m, "match.length") - 1L        # position of '('
    p_close <- match_close(code, p_open, ")")
    if (p_close == -1) return(NULL)
    aa <- substring(code, 1, p_close - 1L)
    
    # Locate the body after the signature's ')'
    rest    <- substring(code, p_close + 1L)
    m2      <- regexpr("\\S", rest)
    if (m2 == -1) return(list(aa = aa, body = ""))
    p_body <- p_close + m2                              # first non-space char
    
    if (substring(code, p_body, p_body) == "{") {
      p_end <- match_close(code, p_body, "}")
      if (p_end == -1) return(NULL)
      body <- substring(code, p_body + 1L, p_end - 1L)
      list(aa = aa, body = body)
    } else {
      # Single-expression body, e.g. function(x) x + 1
      list(aa = aa, body = trimws(rest))
    }
  }
  
  # ---- Rebuild the safer function -------------------------------------------
  build_rebuilt <- function(aa, body, pkg) {
    aa   <- sub("[[:space:]]+$", "", aa)
    body <- sub("[[:space:]]+$", "", sub("^[[:space:]]*\n", "", body))
    
    # Comma after the last initial argument (not if signature is empty
    # or already ends with a comma)
    comma <- if (grepl("\\($", aa) || grepl(",$", aa)) "" else ","
    
    pkg_line <- if (nzchar(pkg)) {
      paste0("package_name <- ", deparse(pkg))
    } else {
      "package_name <- NULL"
    }
    
    paste0(
      aa, comma,
      "\n    lib_path = NULL, \n    safer_check = TRUE, \n    error_text = \"\" \n){\n",
      "\n    #### package name\n    ", pkg_line, "\n    #### end package name\n",
      "\n    #### main code\n",
      body,
      "\n    #### end main code\n}\n"
    )
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
    rv$aa        <- NULL
    
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
    
    # 3) Extract: argument names + verbatim AA + verbatim body
    args <- names(formals(f))
    rv$fun_args <- if (is.null(args)) character(0) else args
    
    parts <- extract_aa_body(code)
    if (is.null(parts)) {
      go_error("Could not locate the function signature/body in the pasted code.")
      return(invisible())
    }
    rv$aa       <- parts$aa
    rv$fun_body <- parts$body
    
    # 3b) SPECIFIC ERROR: any of the three safer-r argument names already
    #     present among the user's arguments?
    collide <- intersect(rv$fun_args, SAFER_ARGS)
    if (length(collide) > 0L) {
      go_error(
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
    
    # 4) Validate that the rebuilt function is valid R (safety net)
    ok <- tryCatch({
      parse(text = build_rebuilt(rv$aa, rv$fun_body, ""))
      TRUE
    }, error = function(e) FALSE)
    if (!ok) {
      go_error("The rebuilt function does not parse.")
      return(invisible())
    }
    
    rv$screen <- "result"
  })
  
  # ---- BACK buttons ---------------------------------------------------------
  observeEvent(input$back_from_result, {
    pkg <- input$pkg_name
    rv$pkg_name <- if (is.null(pkg)) "" else pkg
    rv$screen <- "input"
  })
  observeEvent(input$back_from_error, { rv$screen <- "input" })
  
  # ---- DOWNLOAD: the modified (safer) function -------------------------------
  output$download_safer <- downloadHandler(
    filename = function() {
      paste0(rv$fun_name, "_safer.R")
    },
    content = function(file) {
      pkg  <- if (is.null(input$pkg_name)) "" else trimws(input$pkg_name)
      code <- build_rebuilt(rv$aa, rv$fun_body, pkg)
      rv$rebuilt <- code
      writeLines(code, file)
    }
  )
  
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
          # Specific message when set; fixed message otherwise.
          # white-space: pre-line  ->  every \n becomes a real line break
          div(class = "alert alert-danger", role = "alert",
              style = "margin-top: 10px; white-space: pre-line;",
              if (is.null(rv$error_display)) ERROR_TEXT else rv$error_display),
          hr(),
          back_btn("back_from_error")
        ),
        
        # Result screen: detection text, Package name section,
        # one section per argument, then [Run] [Back]
        result = {
          args_txt <- if (length(rv$fun_args) == 0L) {
            "none"
          } else {
            paste(rv$fun_args, collapse = ", ")
          }
          
          detected_block <- tags$div(
            tags$ol(class = "input-instruction-list",
              tags$li(paste0("Function detected: ", rv$fun_name)),
              tags$li(paste0("Arguments detected: ", args_txt))
            )
          )
          
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
          
          # Bottom row: the download ("Run") button BESIDE the Back button
          result_footer <- div(
            style = "text-align: right;",
            downloadButton(outputId = "download_safer",
                           label = "Run", class = "btn-primary"),
            actionButton(inputId = "back_from_result",
                         label = "Back", icon = icon("arrow-left"))
          )
          
          if (length(rv$fun_args) == 0L) {
            tagList(
              detected_block,
              hr(),
              pkg_section,
              hr(),
              result_footer
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
              result_footer
            )
          }
        }
      )
    )
  })
}