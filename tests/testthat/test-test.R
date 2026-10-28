testthat::test_that("test()", {

    ## data argument values
    # functions rebuilt with their source references, as the app does (lambda_check() needs them)
    code1 <- "function(x){ if(x == T) sum(x) else base::sapply(X = 1:3, FUN = \\(y){ y * 2 }, simplify = TRUE, USE.NAMES = TRUE) }"
    fun1 <- base::eval(expr = base::parse(text = code1, keep.source = TRUE))
    fun_clean <- base::eval(expr = base::parse(text = "function(x){\n    base::sum(x, na.rm = FALSE)\n}", keep.source = TRUE))
    fun_helper <- base::eval(expr = base::parse(text = "function(x){\n    my_helper(x)\n}", keep.source = TRUE))
    fun_err <- base::eval(expr = base::parse(text = "function(x){\n    base::sum(x@sum(2), na.rm = FALSE)\n}", keep.source = TRUE))
    str1 <- "a"
    ## end data argument values

    ## initialization of tests
    testthat::skip_if_not_installed(pkg = "saferDev")
    testthat::skip_if_not(condition = saferMake:::.safer_dev_available(lib_path = NULL, error_text = ""), message = "the current (development) saferDev is needed")
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    ######## arg with no default values
    testthat::expect_error(object = saferMake::test(stop_at_first = FALSE, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "NO DEFAULT VALUE")
    ######## end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(object = saferMake::test(x = fun1, stop_at_first = NULL, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "CANNOT BE NULL")
    ######## end management of NULL arguments

    ######## argument checking with arg_check()
    testthat::expect_error(object = saferMake::test(x = str1, stop_at_first = FALSE, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(object = saferMake::test(x = fun1, stop_at_first = str1, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(object = saferMake::test(x = fun1, stop_at_first = FALSE, export = str1, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(object = saferMake::test(x = fun1, stop_at_first = FALSE, export = FALSE, out_path = "", safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "CANNOT CONTAIN EMPTY STRING")
    ######## end management of "" in arguments of mode character

    ######## other checkings
    testthat::expect_error(object = saferMake::test(x = base::sum, stop_at_first = FALSE, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "PRIMITIVE FUNCTION")
    testthat::expect_error(
        object = testthat::with_mocked_bindings(
            .package = "saferMake",
            .safer_dev_available = function(lib_path, error_text){FALSE},
            code = {saferMake::test(x = fun1, stop_at_first = FALSE, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = NULL, error_text = "")}
        ),
        regexp = "DOES NOT PROVIDE THE SEVEN FUNCTIONS"
    )
    tempo_dir <- base::tempfile(pattern = "dir", tmpdir = base::tempdir(), fileext = "")
    base::dir.create(path = tempo_dir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    testthat::expect_error(object = saferMake::test(x = fun1, stop_at_first = FALSE, export = TRUE, out_path = base::file.path(tempo_dir, "not_here", fsep = "/"), safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "DOES NOT EXISTS")
    testthat::expect_error(object = saferMake::test(x = function(x){x}, stop_at_first = FALSE, export = TRUE, out_path = tempo_dir, safer_check = TRUE, lib_path = NULL, error_text = ""), regexp = "MUST BE DESIGNATED BY A FUNCTION NAME")
    ######## end other checkings

    ######## main code
    # chained checks: the seven checks correct the function (super_assign_check() and for_loop_nb_check() have nothing to report)
    res <- base::suppressMessages(expr = saferMake::test(x = fun1, stop_at_first = FALSE, export = FALSE, out_path = ".", safer_check = TRUE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = res$stage, expected = "modified")
    testthat::expect_null(object = res$step)
    testthat::expect_identical(object = res$checks$check, expected = base::c("super_assign_check", "lambda_check", "curly_bracket_check", "true_false_check", "for_loop_nb_check", "colons_check", "all_args_here"))
    testthat::expect_identical(object = res$checks$status, expected = base::c("clean", "modified", "modified", "modified", "clean", "modified", "modified"))
    testthat::expect_true(object = base::is.function(x = res$fun))
    testthat::expect_identical(object = base::environment(fun = res$fun), expected = base::environment(fun = fun1))
    testthat::expect_match(object = res$code, regexp = "x == TRUE", fixed = TRUE)
    testthat::expect_match(object = res$code, regexp = "base::sum(x, na.rm = FALSE)", fixed = TRUE)
    testthat::expect_match(object = res$code, regexp = "FUN = function(y)", fixed = TRUE)
    testthat::expect_identical(object = res$fun(1), expected = 1)
    # the message
    testthat::expect_message(object = saferMake::test(x = fun1, stop_at_first = FALSE, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "CHECK 7/7 saferDev::all_args_here\\(\\): MODIFIED")
    # stop at the first check that modifies the code
    res2 <- base::suppressMessages(expr = saferMake::test(x = fun1, stop_at_first = TRUE, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = res2$stage, expected = "modified")
    testthat::expect_identical(object = res2$step, expected = 2L)
    testthat::expect_identical(object = res2$checks$status, expected = base::c("clean", "modified", base::rep(x = "not run", times = 5)))
    testthat::expect_identical(object = res2$code, expected = base::sub(pattern = "\\(y)", replacement = "function(y)", x = code1, fixed = TRUE))
    # clean function: invisible output, nothing corrected
    testthat::expect_invisible(call = base::suppressMessages(expr = saferMake::test(x = fun_clean, stop_at_first = FALSE, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message"))
    res3 <- base::suppressMessages(expr = saferMake::test(x = fun_clean, stop_at_first = FALSE, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = res3$stage, expected = "ok")
    testthat::expect_null(object = res3$fun)
    testthat::expect_null(object = res3$code)
    testthat::expect_identical(object = res3$checks$status, expected = base::rep(x = "clean", times = 7))
    testthat::expect_message(object = saferMake::test(x = fun_clean, stop_at_first = FALSE, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "EVERYTHING SEEMS CLEAN")
    # findings that cannot be corrected (function without package): the run stops, also when the checks are chained
    res4 <- base::suppressMessages(expr = saferMake::test(x = fun_helper, stop_at_first = FALSE, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = res4$stage, expected = "findings")
    testthat::expect_identical(object = res4$step, expected = 6L)
    testthat::expect_identical(object = res4$checks$status[6], expected = "findings")
    testthat::expect_identical(object = res4$checks$status[7], expected = "not run")
    testthat::expect_null(object = res4$fun)
    testthat::expect_match(object = res4$checks$report[6], regexp = "my_helper", fixed = TRUE)
    res5 <- base::suppressMessages(expr = saferMake::test(x = fun_helper, stop_at_first = TRUE, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = res5$stage, expected = "findings")
    testthat::expect_identical(object = res5$step, expected = 6L)
    testthat::expect_identical(object = res5$checks$status[7], expected = "not run")
    # saferDev error or findings on a code that cannot be rewritten
    res6 <- base::suppressMessages(expr = saferMake::test(x = fun_err, stop_at_first = FALSE, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = res6$stage %in% base::c("error", "findings"))
    # superassignment: error at check 1, the next checks are not run, nothing corrected
    fun_super <- base::eval(expr = base::parse(text = "function(x){\n    y <<- x\n}", keep.source = TRUE))
    res7 <- base::suppressMessages(expr = saferMake::test(x = fun_super, stop_at_first = FALSE, export = FALSE, out_path = ".", safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_identical(object = res7$stage, expected = "error")
    testthat::expect_identical(object = res7$step, expected = 1L)
    testthat::expect_identical(object = res7$checks$status, expected = base::c("error", base::rep(x = "not run", times = 6)))
    testthat::expect_null(object = res7$fun)
    testthat::expect_match(object = res7$checks$report[1], regexp = "y <<- x", fixed = TRUE)
    # export
    base::suppressMessages(expr = saferMake::test(x = fun1, stop_at_first = FALSE, export = TRUE, out_path = tempo_dir, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_true(object = base::file.exists(base::file.path(tempo_dir, "fun1_safer.R", fsep = "/")))
    tempo_env <- base::new.env(hash = TRUE, parent = base::globalenv(), size = 29L)
    base::sys.source(file = base::file.path(tempo_dir, "fun1_safer.R", fsep = "/"), envir = tempo_env, chdir = FALSE, keep.source = FALSE, keep.parse.data = FALSE, toplevel.env = tempo_env)
    testthat::expect_true(object = base::is.function(x = tempo_env$fun1_safer))
    testthat::expect_error(object = saferMake::test(x = fun1, stop_at_first = FALSE, export = TRUE, out_path = tempo_dir, safer_check = FALSE, lib_path = NULL, error_text = ""), regexp = "FILE NAME ALREADY EXISTS")
    # nothing exported when nothing is corrected
    base::suppressMessages(expr = saferMake::test(x = fun_clean, stop_at_first = FALSE, export = TRUE, out_path = tempo_dir, safer_check = FALSE, lib_path = NULL, error_text = ""), classes = "message")
    testthat::expect_false(object = base::file.exists(base::file.path(tempo_dir, "fun_clean_safer.R", fsep = "/")))
    base::unlink(x = tempo_dir, recursive = TRUE, force = FALSE, expand = TRUE)
    ######## end main code

    ## end tests

    ## other tests
    ## end other tests

    base::rm(list = base::ls())
})
