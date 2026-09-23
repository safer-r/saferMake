#' @title Shiny app launcher
#' @description
#' Launch the shiny app that convert the user function to a safer-r function.
#' @returns 
#' A .R file which correspond to the converted function.
#' @author \href{mailto:gael.millot@pasteur.fr}{Gael Millot} 
#' @export
#' @importFrom shiny runApp

make <- function(){
    # Locate the shiny app files inside the installed package
    app_dir <- system.file("app", package = "saferMake")
    
    # Fallback error if the directory isn't found
    if (app_dir == "") {
        stop("Could not find the application directory. Try re-installing `saferMake.", call. = FALSE)
    }
    # Run the application
    shiny::runApp(app_dir, display.mode = "normal")
}
