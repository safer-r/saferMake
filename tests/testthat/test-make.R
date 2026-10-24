testthat::test_that("make()", {

    ## data argument values
    str1 <- "INSIDE A"
    ## end data argument values

    # the app itself (shiny::runApp(), that blocks the R session) is not run here: only the argument checks of the backbone, that stop before the main code

    ## tests (ordered by arg appearance and conditions in the code)

    ######## management of NULL arguments
    testthat::expect_error(object = saferMake::make(safer_check = NULL, lib_path = NULL, error_text = ""), regexp = "THIS ARGUMENT CANNOT BE NULL", fixed = TRUE)
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(object = saferMake::make(safer_check = TRUE, lib_path = base::character(length = 0L), error_text = ""), regexp = "CANNOT BE AN EMPTY NON NULL OBJECT", fixed = TRUE)
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(object = saferMake::make(safer_check = NA, lib_path = NULL, error_text = ""), regexp = "CANNOT BE MADE OF NA ONLY", fixed = TRUE)
    ######## end management of NA arguments

    ######## safer_check argument checking
    testthat::expect_error(object = saferMake::make(safer_check = "a", lib_path = NULL, error_text = ""), regexp = "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE", fixed = TRUE)
    testthat::expect_error(object = saferMake::make(safer_check = base::c(TRUE, FALSE), lib_path = NULL, error_text = ""), regexp = "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE", fixed = TRUE)
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_error(object = saferMake::make(safer_check = TRUE, lib_path = 1, error_text = ""), regexp = "MUST BE A VECTOR OF CHARACTERS", fixed = TRUE)
    testthat::expect_error(object = saferMake::make(safer_check = TRUE, lib_path = "a_folder_that_does_not_exist", error_text = ""), regexp = "NOT EXIST", fixed = TRUE)
    ######## end check of lib_path

    ######## error_text
    testthat::expect_error(object = saferMake::make(safer_check = "a", lib_path = NULL, error_text = str1), regexp = str1, fixed = TRUE)
    ######## end error_text

    ## end tests

    base::rm(list = base::ls(all.names = FALSE, envir = base::environment(fun = NULL), sorted = TRUE, pattern = ))
})
