## Second batch of hand-written mutations, checked against BOTH gates the
## project relies on: the 2104-assertion test suite (R CMD check job) and the
## README harness (README examples job).
##   usage: Rscript docs/adversarial/custom-mutation2.R <repo_root>
root <- normalizePath(commandArgs(trailingOnly = TRUE)[1], winslash = "/")
work <- file.path(tempdir(), "custom-mut2")
unlink(work, recursive = TRUE); dir.create(work, recursive = TRUE)
for (d in c("R", "tests", "man", "inst", "docs"))
  file.copy(file.path(root, d), work, recursive = TRUE)
for (f in c("DESCRIPTION", "NAMESPACE", "README.md"))
  file.copy(file.path(root, f), work, overwrite = TRUE)
setwd(work)
suppressMessages(library(testthat))

muts <- list(
  list(name = "warn-even-when-lossless", file = "R/blocking.R",
       what = "warn_blocking_loss(): recall < 1 -> recall <= 1, so a LOSSLESS block warns too",
       fn = function(x) {
         i <- grep("if \\(!is\\.na\\(info\\$recall\\) && info\\$recall < 1\\)", x)
         stopifnot(length(i) == 1); x[i] <- sub("< 1", "<= 1", x[i]); x }),
  list(name = "topk-group-offsets", file = "R/blocking.R",
       what = "top_k_candidates(): head(runs, -1L) -> tail(runs, -1L) (group start offsets)",
       fn = function(x) {
         i <- grep("starts <- cumsum\\(c\\(0L, utils::head\\(runs, -1L\\)\\)\\)", x)
         stopifnot(length(i) == 1)
         x[i] <- sub("utils::head", "utils::tail", x[i]); x }),
  list(name = "no-candidate-count-wrong", file = "R/blocking.R",
       what = "n_anon_without_candidate: drop the unique() so duplicates are counted",
       fn = function(x) {
         i <- grep("n_anon_without_candidate = n_anon - length\\(unique\\(ai\\)\\)", x)
         stopifnot(length(i) >= 1)
         x[i] <- sub("length(unique(ai))", "length(ai)", x[i], fixed = TRUE); x }),
  list(name = "mode-baseline-broken", file = "R/evaluate.R",
       what = "reid_evaluate(): baseline_mode computed with prod() instead of sum()",
       fn = function(x) {
         i <- grep("baseline_mode <- sum\\(as\\.character\\(per_anon", x)
         stopifnot(length(i) == 1)
         x[i] <- sub("sum(", "prod(", x[i], fixed = TRUE); x }),
  list(name = "partial-nonnumeric-silent", file = "R/activity.R",
       what = "split_collapsed(): any(bad) -> all(bad); a partly-unparseable column returns NA instead of stopping",
       fn = function(x) {
         i <- grep("^  if \\(any\\(bad\\)\\) \\{", x)
         stopifnot(length(i) == 1); x[i] <- "  if (all(bad)) {"; x })
)

order_t <- basename(list.files("tests/testthat", pattern = "^test-"))
order_t <- c("test-blocking.R", "test-evaluate.R", "test-activity.R",
             setdiff(order_t, c("test-blocking.R", "test-evaluate.R", "test-activity.R")))

for (m in muts) {
  orig <- readLines(file.path(work, m$file), warn = FALSE)
  writeLines(m$fn(orig), file.path(work, m$file))

  suppressMessages(pkgload::load_all(work, quiet = TRUE, export_all = FALSE))
  killers <- character(0)
  for (tf in order_t) {
    r <- tryCatch({
      invisible(capture.output(suppressMessages(suppressWarnings(
        res <- testthat::test_file(file.path("tests/testthat", tf),
                                   package = "reidentify", reporter = "silent")))))
      d <- as.data.frame(res); sum(d$failed) + sum(d$error)
    }, error = function(e) 1L)
    if (r > 0) killers <- c(killers, sprintf("%s(%d)", tf, r))
  }
  ## README harness, in its own process, exactly as CI runs it
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                 c("--vanilla", "docs/verify-readme-examples.R", "."),
                 stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status"); if (is.null(st)) st <- 0L
  tally <- grep("output unit", out, value = TRUE)

  cat(sprintf("\n%-26s tests: %-22s  readme-harness: %s\n  %s\n  %s\n",
              m$name,
              if (length(killers)) paste("KILLED", paste(killers, collapse = " "))
              else "*** SURVIVED ***",
              if (st != 0L) "KILLED (exit 1)" else "*** SURVIVED (exit 0) ***",
              m$what, if (length(tally)) trimws(tally[1]) else ""))
  writeLines(orig, file.path(work, m$file))
}
