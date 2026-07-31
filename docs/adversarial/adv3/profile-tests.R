setwd(normalizePath(commandArgs(trailingOnly = TRUE)[1]))
t0 <- Sys.time()
suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = FALSE))
cat("load_all elapsed:", as.numeric(difftime(Sys.time(), t0, units = "secs")), "s\n")
library(testthat)
fs <- list.files("tests/testthat", pattern = "^test-", full.names = TRUE)
res <- sapply(fs, function(f) {
  t <- system.time(invisible(capture.output(suppressMessages(
    testthat::test_file(f, package = "reidentify", reporter = "silent")))))
  t[["elapsed"]]
})
o <- order(res, decreasing = TRUE)
for (i in o) cat(sprintf("%7.2f  %s\n", res[i], basename(fs[i])))
cat("TOTAL:", sum(res), "\n")
