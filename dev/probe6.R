cat(nzchar(find("colons_check")), "|", find("colons_check"), "|", is.null(find("colons_check")), "\n")
saferDev_pkg <- grep("saferDev", find("colons_check"), value = TRUE)
cat("grep:", saferDev_pkg, "\n")