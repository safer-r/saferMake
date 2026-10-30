# Internal objects (not functions) of saferMake.
# Each internal function has its own file R/intern_.<name>.R; these objects
# are shared by several of them and by the Shiny app (inst/app/).
# Not exported: the app accesses them through saferMake::: .
# All internal function names start with a dot; files start with intern_.
# Inside the package, an internal function is called through a local copy
# fetched with utils::getFromNamespace() (safer-r convention: no bare call,
# and no saferMake::: call, that would raise a R CMD check NOTE).

# The three additional safer-r arguments
.safer_args <- base::c("lib_path", "safer_check", "error_text")

.arg_check_defaults <- base::list(
  class                     = "NULL",  # blank field -> "NULL" = no constraint
  typeof                    = "NULL",  # blank field -> "NULL" = no constraint
  mode                      = "NULL",  # blank field -> "NULL" = no constraint
  length                    = "NULL",  # blank field -> no constraint
  prop                      = FALSE,
  double_as_integer_allowed = FALSE,
  options                   = "NULL", # blank field -> no constraint
  all_options_in_data       = FALSE,
  na_contain                = TRUE,
  neg_values                = TRUE,
  inf_values                = TRUE,
  no_empty_string           = FALSE  # ticked -> argument cannot contain "" (empty-string section)
)

# Staged code checks of the pasted function (the seven checks run by the Run
# button of window 1, after the pasted code has been evaluated without error).
#
# The seven checks, always run in this order, each with the dedicated function
# of the current (development) version of saferDev:
#   1. saferDev::super_assign_check()   no superassignment <<- or ->> (error only:
#                                       nothing can be corrected automatically)
#   2. saferDev::lambda_check()         no lambda function written \(x)
#   3. saferDev::curly_bracket_check()  curly brackets after if, else, for, ...
#   4. saferDev::true_false_check()     TRUE and FALSE written in full
#   5. saferDev::for_loop_nb_check()    checking of the for() loop sequences
#   6. saferDev::colons_check()         package attribution (pkg::) of the functions
#   7. saferDev::all_args_here()        all the arguments of the functions written
# super_assign_check() is first because it cannot correct the code: the user
# rewrites the superassignments before any automatic modification.
# for_loop_nb_check() is before colons_check() and all_args_here() because the
# checking lines that it inserts must also be checked by these two functions.
# The seven checks are run by the exported saferMake::test() (R/test.R), that
# can also be used on its own. In the app, .run_code_checks() calls test() with
# stop_at_first = FALSE: the checks are chained (each one on the code corrected
# by the previous ones) and the run only stops on findings or on an error. All
# the corrections are handed back at once to window 1 (orange message, with the
# full report of test()), and the user clicks on Run again, which restarts the
# seven checks on the modified code (window 2 is reached only after a Run
# without any modification, finding or error).

# The seven checks, in the order of execution.
# key        : short identifier
# fun        : name of the saferDev function
# export_arg : name of the argument of the saferDev function that exports the
#              modified function in a file (all_args_here() has two export
#              arguments: table_export for the table, fun_export for the code).
#              NULL for super_assign_check(), that never modifies the code (it
#              returns an error when a superassignment is detected)
# what       : sentence explaining the modification, written in the report of
#              saferMake::test()
# label      : topic of the check, displayed in brackets after "Check i/7" in
#              the message of window 1 (text between backticks shown as code)
# done       : short text of the modification, displayed after the label in
#              the message of window 1 (NULL for super_assign_check(), that
#              never modifies the code)
# The saferDev functions keep the comments and the indentation of the
# submitted code (the function evaluated with keep.source = TRUE carries its
# source references).
.code_check_steps <- base::list(
  base::list(key = "super_assign", fun = "super_assign_check", export_arg = NULL,
             what = "no modification (the superassignments <<- and ->> must be rewritten by the user).",
             label = "superassignments `<<-` `->>`", done = NULL),
  base::list(key = "lambda", fun = "lambda_check", export_arg = "export",
             what = "the lambda functions written \\(x) have been rewritten function(x).",
             label = "lambda functions `\\(x)`", done = "rewritten `function(x)`."),
  base::list(key = "curly", fun = "curly_bracket_check", export_arg = "export",
             what = "curly brackets { } have been added to the bodies of the if, else, for, while, repeat and function that had none.",
             label = "curly brackets `{ }`", done = "added."),
  base::list(key = "true_false", fun = "true_false_check", export_arg = "export",
             what = "the T and F written as values have been rewritten TRUE and FALSE.",
             label = "`T` and `F`", done = "rewritten `TRUE` and `FALSE`."),
  base::list(key = "for_loop", fun = "for_loop_nb_check", export_arg = "export",
             what = "a checking line of the sequence has been added before the for() loops whose number of iterations is not fixed by the writing itself (like 1:length(x)).",
             label = "`for()` loops", done = "checking line(s) added."),
  base::list(key = "colons", fun = "colons_check", export_arg = "export",
             what = "the package attribution (<package>::) of the basic R functions has been added.",
             label = "package attribution `<package>::<function>`", done = "added for basic R functions."),
  base::list(key = "args", fun = "all_args_here", export_arg = "fun_export",
             what = "the missing arguments of the called functions have been added.",
             label = "all arguments written", done = "added.")
)

# TEMPORARY SWITCH (set to FALSE on 2026-10-22 so that the rest of the app can be
# tested with a saferDev version that does not provide the seven checks).
# TRUE  : when the saferDev of the R session does not provide the seven checks,
#         window 1 shows a red message asking to load the current saferDev,
#         and window 2 cannot be reached.
# FALSE : the seven checks are skipped (message in the R console only) and the
#         app goes on to window 2.
# Set it back to TRUE once the current saferDev is released (CRAN).
.safer_dev_required <- FALSE
