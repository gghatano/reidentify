## The harness compares only `#>` lines. Every number that appears in README
## PROSE (outside code fences) is unchecked. List them with their line numbers
## so each can be traced by hand to the verified output above/below it.
setwd(normalizePath(commandArgs(trailingOnly = TRUE)[1]))
x <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
fence <- grep("^```", x)
inblock <- rep(FALSE, length(x))
for (i in seq(1, length(fence), by = 2))
  inblock[fence[i]:fence[i + 1L]] <- TRUE
prose <- which(!inblock)
hit <- prose[grepl("[0-9]", x[prose])]
## skip pure table-of-contents / link lines and headings without claims
hit <- hit[!grepl("^\\s*(#|\\||-{3,})", x[hit]) | grepl("\\*\\*", x[hit])]
for (i in hit) cat(sprintf("%5d | %s\n", i, x[i]))
cat("\n", length(hit), "prose lines containing a number\n")
