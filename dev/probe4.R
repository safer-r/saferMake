ip <- .libPaths()
for (d in ip) { p <- file.path(d, "saferDev"); if (dir.exists(p)) cat(p, "\n") }