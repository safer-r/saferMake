library(saferDev)
code <- "my_fun <- function(x){\n    a <- base::sum(x)\n    b <- base::paste0(x, '-')\n    base::list(a, b)\n}"
f <- eval(parse(text = code, keep.source = TRUE))
r <- all_args_here(x = f)
print(r)
cat("---- all_args_here source:\n")
print(getSrcref(all_args_here))