library(shiny)

# Finds the character offset (in code_text) of the opening "{" that starts
# the body of the first function() found, using R's own parser.
find_body_brace_offset <- function(code_text) {

  expr <- tryCatch(
    parse(text = code_text, keep.source = TRUE),
    error = function(e) NULL
  )
  if (is.null(expr)) {
    stop("The pasted code could not be parsed as valid R code. Please check for syntax errors.")
  }

  pd <- getParseData(expr)
  if (is.null(pd) || nrow(pd) == 0) {
    stop("Could not analyze the code structure (no parse data returned).")
  }

  fun_rows <- pd[pd$token == "FUNCTION", ]
  if (nrow(fun_rows) == 0) {
    stop("No `function(...)` definition was found in the pasted code.")
  }
  first_fun <- fun_rows[order(fun_rows$line1, fun_rows$col1), ][1, ]

  brace_rows <- pd[pd$token == "'{'", ]
  after_fun <- brace_rows[
    (brace_rows$line1 > first_fun$line1) |
      (brace_rows$line1 == first_fun$line1 & brace_rows$col1 > first_fun$col1),
  ]
  if (nrow(after_fun) == 0) {
    stop("No opening brace `{` was found after `function(...)`. Brace-less one-line functions are not supported yet.")
  }
  brace <- after_fun[order(after_fun$line1, after_fun$col1), ][1, ]

  lines <- strsplit(code_text, "\n", fixed = TRUE)[[1]]
  n_prev_lines <- brace$line1 - 1
  offset <- if (n_prev_lines > 0) {
    sum(nchar(lines[seq_len(n_prev_lines)])) + n_prev_lines  # + newline chars
  } else {
    0
  }
  offset + brace$col2  # position of the "{" character itself
}

# Inserts the package_name block right after the function body's "{"
build_output_code <- function(user_code, package_name) {

  offset <- find_body_brace_offset(user_code)

  before <- substr(user_code, 1, offset)
  after  <- substr(user_code, offset + 1, nchar(user_code))

  pkg_value <- if (is.null(package_name) || trimws(package_name) == "") {
    "NULL"
  } else {
    paste0('"', package_name, '"')
  }

  insertion <- paste0(
    "\n    #### package name\n",
    "    package_name <- ", pkg_value, "\n",
    "    #### end package name\n"
  )

  paste0(before, insertion, after)
}

server <- function(input, output, session) {

  output$run_button <- downloadHandler(
    filename = function() {
      "generated_function.R"
    },
    content = function(file) {
      validate(
        need(nzchar(trimws(input$user_code)), "Please paste your function code in field 1 before clicking Run.")
      )

      result <- tryCatch(
        build_output_code(input$user_code, input$package_name),
        error = function(e) e
      )

      if (inherits(result, "error")) {
        showNotification(conditionMessage(result), type = "error", duration = NULL)
        stop(conditionMessage(result))
      }

      writeLines(result, con = file)
    },
    contentType = "text/plain"
  )
}