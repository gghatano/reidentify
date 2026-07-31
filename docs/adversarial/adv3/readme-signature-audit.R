## The README's "関数一覧" tables show each function with an argument list,
## e.g. `score_multi(dat, targets, weights, method, screen)`.
## The verification harness compares only `#>` output and bare function names,
## so those argument lists are unchecked prose. Check them here.
setwd(normalizePath(commandArgs(trailingOnly = TRUE)[1]))
suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = FALSE))
ns <- readLines("NAMESPACE", warn = FALSE)
exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))

txt <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
## only table rows: | `fn(args)` | description |
rows <- grep("^\\|\\s*`[A-Za-z._][A-Za-z0-9._]*\\(", txt)
bad <- 0L
for (i in rows) {
  m <- regmatches(txt[i], regexpr("`[A-Za-z._][A-Za-z0-9._]*\\([^`]*\\)`", txt[i]))
  if (!length(m)) next
  m <- gsub("`", "", m)
  fn <- sub("\\(.*$", "", m)
  if (!(fn %in% exports)) next
  argtxt <- sub("^[^(]*\\(", "", sub("\\)$", "", m))
  args <- trimws(strsplit(argtxt, ",")[[1]])
  args <- sub("\\s*=.*$", "", args)
  args <- args[nzchar(args) & args != "..."]
  real <- names(formals(get(fn, envir = asNamespace("reidentify"))))
  unknown <- setdiff(args, real)
  if (length(unknown)) {
    bad <- bad + 1L
    cat(sprintf("README.md:%d  %s\n   README shows : %s\n   real formals : %s\n   NOT A FORMAL : %s\n\n",
                i, fn, paste(args, collapse = ", "),
                paste(real, collapse = ", "), paste(unknown, collapse = ", ")))
  }
}
cat("table rows scanned:", length(rows), " rows with a non-existent argument:", bad, "\n")
