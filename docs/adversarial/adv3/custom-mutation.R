## Hand-written mutations aimed at specific documented guarantees.
## Runs the WHOLE suite for each mutation on a private copy of the package.
##   usage: Rscript docs/adversarial/custom-mutation.R <repo_root> [name ...]
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(args[1], winslash = "/")
only <- args[-1]

work <- file.path(tempdir(), "custom-mut")
unlink(work, recursive = TRUE); dir.create(work, recursive = TRUE)
for (d in c("R", "tests", "man")) file.copy(file.path(root, d), work, recursive = TRUE)
for (f in c("DESCRIPTION", "NAMESPACE"))
  file.copy(file.path(root, f), work, overwrite = TRUE)
setwd(work)
suppressMessages(library(testthat))

## each mutation: file, and a function(lines) -> lines
muts <- list(
  list(name = "drop-screen-warn-warning",
       what = "R/multiattr.R: delete the screen='warn' warning() entirely",
       file = "R/multiattr.R",
       fn = function(x) {
         i <- grep('^  warning\\(fn_name, "\\(\\): axis/axes that show no signal',
                   x)
         stopifnot(length(i) == 1)
         j <- i; while (!grepl("call\\. = FALSE\\)", x[j])) j <- j + 1L
         x[i:j] <- ""
         x
       }),
  list(name = "screen-never-fires",
       what = "R/multiattr.R: apply_axis_screen() treats every axis as informative",
       file = "R/multiattr.R",
       fn = function(x) {
         i <- grep("^  dead <- !is\\.na\\(report\\$informative\\)", x)
         stopifnot(length(i) == 1)
         x[i] <- "  dead <- rep(FALSE, nrow(report))"
         x
       }),
  list(name = "blocking-recall-never-warns",
       what = "R/blocking.R: warn_blocking_loss() never warns",
       file = "R/blocking.R",
       fn = function(x) {
         i <- grep("^  if \\(!is\\.na\\(info\\$recall\\) && info\\$recall < 1\\) \\{", x)
         stopifnot(length(i) == 1)
         x[i] <- "  if (FALSE) {"
         x
       }),
  list(name = "recall-1-instead-of-NA",
       what = "R/blocking.R: recall = 1 when there is no ground truth (the exact thing the comment forbids)",
       file = "R/blocking.R",
       fn = function(x) {
         i <- grep("recall = if \\(n_true_pairs > 0\\)", x)
         stopifnot(length(i) == 1)
         x[i] <- sub("else NA_real_", "else 1", x[i])
         x
       }),
  list(name = "kept-fraction-1-instead-of-NA",
       what = "R/blocking.R: kept_fraction/reduction get numbers instead of NA on an empty pair set",
       file = "R/blocking.R",
       fn = function(x) {
         i <- grep("kept_fraction = if \\(n_pairs_full > 0\\)", x)
         j <- grep("reduction = if \\(n_pairs_full > 0\\)", x)
         x[i] <- sub("else NA_real_", "else 1", x[i])
         x[j] <- sub("else NA_real_", "else 0", x[j])
         x
       }),
  list(name = "split-not-literal",
       what = "R/activity.R: strsplit(fixed = TRUE) -> fixed = FALSE (regex split)",
       file = "R/activity.R",
       fn = function(x) {
         i <- grep("strsplit\\(as\\.character\\(values\\), split = split, fixed = TRUE\\)", x)
         stopifnot(length(i) >= 1)
         x[i] <- sub("fixed = TRUE", "fixed = FALSE", x[i], fixed = TRUE)
         x
       })
)

timing_order <- c("test-calc-kl.R","test-contract.R","test-distribution-distance.R",
  "test-boundary-cases.R","test-known-values.R","test-distance-metrics.R",
  "test-master-column-names.R","test-tie-and-na.R","test-column-selection.R",
  "test-unicity.R","test-args-and-determinism.R","test-split-literal.R",
  "test-assignment.R","test-setsim.R","test-blocking.R","test-confidence.R",
  "test-scoreboard.R","test-generalize.R","test-statistical-properties.R",
  "test-layers.R","test-activity.R","test-tiebreak-seed.R",
  "test-spatiotemporal.R","test-generalized-column-guard.R","test-knowledge.R",
  "test-self-contained.R","test-axis-screening.R","test-idf.R",
  "test-evaluate.R","test-multiattr.R")

for (m in muts) {
  if (length(only) && !(m$name %in% only)) next
  orig <- readLines(file.path(work, m$file), warn = FALSE)
  writeLines(m$fn(orig), file.path(work, m$file))
  suppressMessages(pkgload::load_all(work, quiet = TRUE, export_all = FALSE))
  killers <- character(0); tot_f <- 0L
  for (tf in timing_order) {
    r <- tryCatch({
      invisible(capture.output(suppressMessages(suppressWarnings(
        res <- testthat::test_file(file.path("tests/testthat", tf),
                                   package = "reidentify", reporter = "silent")))))
      d <- as.data.frame(res); sum(d$failed) + sum(d$error)
    }, error = function(e) 1L)
    if (r > 0) { killers <- c(killers, paste0(tf, "(", r, ")")); tot_f <- tot_f + r }
  }
  cat(sprintf("\n%-32s %s\n  %s\n  -> %s  failing assertions: %d  %s\n",
              m$name, if (length(killers)) "KILLED" else "*** SURVIVED ***",
              m$what, if (length(killers)) "killed by" else "no test failed",
              tot_f, paste(killers, collapse = " ")))
  writeLines(orig, file.path(work, m$file))
}
