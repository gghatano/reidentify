## generalization_units() is the one exported function with no direct test
## reference. Is its content asserted anywhere, even indirectly?
## Delete two units from the dictionary and run the whole suite.
root <- normalizePath(commandArgs(trailingOnly = TRUE)[1], winslash = "/")
work <- file.path(tempdir(), "units-demo")
unlink(work, recursive = TRUE); dir.create(work, recursive = TRUE)
for (d in c("R", "tests", "man", "inst")) file.copy(file.path(root, d), work, recursive = TRUE)
for (f in c("DESCRIPTION", "NAMESPACE")) file.copy(file.path(root, f), work, overwrite = TRUE)
setwd(work); suppressMessages(library(testthat))

p <- file.path(work, "R/generalize.R"); orig <- readLines(p, warn = FALSE)
i <- grep('"%", "yr", "yrs", "y", "years", "kg", "km", "cm", "m", "g"', orig, fixed = TRUE)
stopifnot(length(i) == 1)
x <- orig; x[i] <- sub('"%", "yr", "yrs", "y", "years", "kg", "km", "cm", "m", "g"',
                       '"zz1", "zz2"', x[i], fixed = TRUE)
writeLines(x, p)
suppressMessages(pkgload::load_all(work, quiet = TRUE, export_all = FALSE))
cat("mutated dictionary:", paste(generalization_units(), collapse = " "), "\n")
r <- test_dir("tests/testthat", package = "reidentify", reporter = "silent",
              stop_on_failure = FALSE)
d <- as.data.frame(r)
cat("RESULT pass=", sum(d$passed), " fail=", sum(d$failed), " err=", sum(d$error),
    "\n", sep = "")
bad <- d[d$failed > 0 | d$error > 0, c("file", "test")]
if (nrow(bad)) print(bad, row.names = FALSE)
writeLines(orig, p)
