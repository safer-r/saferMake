library(saferDev)
code <- "my_fun <- function(x){\n    a <- base::paste0(base::sum(x), '-')\n}"
f <- eval(parse(text = code, keep.source = TRUE))
r <- all_args_here(x = f)
print(r)