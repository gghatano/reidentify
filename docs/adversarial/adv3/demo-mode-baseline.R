## test-evaluate.R:83 is the only test of the mode baseline and it asserts
##   expect_true(mode_rate %in% c(0, 1/6))
## Measure which of the two the implementation actually produces, and what a
## broken implementation produces.
root <- normalizePath(commandArgs(trailingOnly = TRUE)[1], winslash = "/")
work <- file.path(tempdir(), "modebase")
unlink(work, recursive = TRUE); dir.create(work, recursive = TRUE)
for (d in c("R", "man", "inst")) file.copy(file.path(root, d), work, recursive = TRUE)
for (f in c("DESCRIPTION", "NAMESPACE")) file.copy(file.path(root, f), work, overwrite = TRUE)

## same fixture as the test (tests/testthat/test-evaluate.R, make_uniq3_tied3)
make_uniq3_tied3 <- function() {
  raw <- data.frame(ROW_NUMBER = 1:6, V = c(10, 20, 30, 40, 40, 40))
  join_raw_anon_data(raw, raw)
}
get_mode <- function() {
  suppressMessages(pkgload::load_all(work, quiet = TRUE, export_all = FALSE))
  e <- reid_evaluate(score_num(make_uniq3_tied3(), "V"), seeds = 1:5)
  e$baseline$rate[e$baseline$method == "mode"]
}
orig <- get_mode()
p <- file.path(work, "R/evaluate.R"); x <- readLines(p, warn = FALSE)
i <- grep("baseline_mode <- sum\\(as\\.character\\(per_anon", x)
x[i] <- sub("sum(", "prod(", x[i], fixed = TRUE); writeLines(x, p)
mut <- get_mode()
cat(sprintf("original baseline_mode = %.6f   (1/6 = %.6f)\n", orig, 1/6))
cat(sprintf("mutant   baseline_mode = %.6f\n", mut))
cat(sprintf("assertion is  mode_rate %%in%% c(0, 1/6):  original %s, mutant %s\n",
            orig %in% c(0, 1/6), mut %in% c(0, 1/6)))
