# sanity: what does the normalized FUN_POS look like when a line has leading spaces collapsed?
library(saferDev)
code <- "my_fun <- function(x){\n    a <- base::substr(x, 1)\n}"
f <- eval(parse(text = code, keep.source = TRUE))
r <- suppressMessages(all_args_here(x = f))
print(r)