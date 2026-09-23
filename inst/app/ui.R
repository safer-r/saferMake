library(shiny)

ui <- fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    
    titlePanel("Function > safer-r function converter"),
    
    fluidRow(
        column(
            width = 8, offset = 2,
            wellPanel(
                h4("Code of your function"),
                textAreaInput(
                    inputId = "user_ini_fun",
                    label   = "Paste your R function code here.",
                    placeholder = "my_fun <- function(x){\n    x + 1\n}",
                    rows = 15,
                    width = "100%"
                ),
                
                hr(),
                
                h4("Package name"),
                textInput(
                    inputId = "package_name",
                    label   = "Leave empty for NULL, or type a package name",
                    placeholder = "NULL"
                ),
                
                hr(),

                h4("Report"),
                textInput(
                    inputId = "package_name",
                    label   = "Leave empty for NULL, or type a package name",
                    placeholder = "NULL"
                ),
                
                hr(),



                div(
                    style = "text-align: right;",
                    downloadButton(
                        outputId = "run_button",
                        label    = "Run",
                        class    = "btn-primary"
                    )
                )
            )
        )
    )
)