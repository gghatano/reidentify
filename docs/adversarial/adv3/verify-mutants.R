## sanity: every generated mutant must (a) sit at the right byte offset and
## (b) still parse.
setwd(normalizePath(commandArgs(trailingOnly = TRUE)[1]))
mut <- readRDS("docs/adversarial/mutants.rds")
bad_pos <- 0L; bad_parse <- 0L
for (m in mut) {
  src <- readLines(m$file, warn = FALSE)
  if (!identical(src[m$line], m$orig_line)) { bad_pos <- bad_pos + 1L; next }
  src[m$line] <- m$new_line
  ok <- tryCatch({ parse(text = paste(src, collapse = "\n")); TRUE },
                 error = function(e) FALSE)
  if (!ok) bad_parse <- bad_parse + 1L
}
cat("mutants:", length(mut), " bad position:", bad_pos,
    " non-parsing:", bad_parse, "\n")
## non-ASCII in R/?
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) {
  l <- readLines(f, warn = FALSE)
  n <- sum(grepl("[^\x01-\x7f]", l, useBytes = TRUE))
  if (n) cat("non-ASCII lines in", f, ":", n, "\n")
}
