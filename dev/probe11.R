library(saferDev)
# a call split across lines
code <- "my_fun <- function(x){\n    r <- stats::optim(par = 1,\n         fn = function(p) p^2)\n    base::list(r)\n}"
f <- eval(parse(text = code, keep.source = TRUE))
r <- suppressMessages(all_args_here(x = f))
print(r)
cat("----\n")
# and a call whose args live on the NEXT line
code2 <- "my_fun <- function(x){\n    r <- stats::optim(\n    par = 1, fn = base::sum)\n    base::list(r)\n}"
f2 <- eval(parse(text = code2, keep.source = TRUE))
r2 <- suppressMessages(all_args_here(x = f2))
print(r2)