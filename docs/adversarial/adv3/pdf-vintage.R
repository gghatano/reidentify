setwd(normalizePath(commandArgs(trailingOnly = TRUE)[1]))
b <- readBin("reidentify.pdf", "raw", file.size("reidentify.pdf"))
s <- rawToChar(b[b > as.raw(31) & b < as.raw(127)])
pat <- "CreationDate[^)]{0,60}|Producer[^)]{0,60}|Title[^)]{0,60}"
print(unlist(regmatches(s, gregexpr(pat, s))))
