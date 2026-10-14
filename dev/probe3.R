txt <- 'caf\u00e9 <- function(y){ return(base::nchar(y)) }'
p <- base::parse(text = txt, keep.source = TRUE)
pd <- utils::getParseData(x = p)
print(pd[pd$token %in% c('SYMBOL_FUNCTION_CALL','SYMBOL','FUNCTION'), c('token','text','line1','col1')])
cat('nchar chars:', nchar(txt, type='chars'), ' bytes:', nchar(txt, type='bytes'), '\n')