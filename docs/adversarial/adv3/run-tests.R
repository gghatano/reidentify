# Baseline / mutation test runner: load_all + test_dir (no installed copy)
pkg <- normalizePath(".")
suppressMessages(pkgload::load_all(pkg, quiet = TRUE, export_all = FALSE))
library(testthat)
filt <- Sys.getenv("ADV_FILTER", "")
res <- test_dir("tests/testthat",
                package = "reidentify",
                filter = if (nzchar(filt)) filt else NULL,
                reporter = "summary", stop_on_failure = FALSE)
df <- as.data.frame(res)
cat("\n==SUMMARY== files:", nrow(df),
    " pass:", sum(df$passed),
    " fail:", sum(df$failed),
    " warn:", sum(df$warning),
    " skip:", sum(df$skipped), "\n")
