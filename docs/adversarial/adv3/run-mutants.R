## Worker: run a slice of mutants against the test suite.
## usage: Rscript run-mutants.R <repo_root> <worker_index> <n_workers> <ids_file> <out_file>
## Each worker copies the package into its own scratch dir so workers do not
## race on R/.
args <- commandArgs(trailingOnly = TRUE)
root  <- normalizePath(args[1], winslash = "/")
wi    <- as.integer(args[2]); nw <- as.integer(args[3])
ids   <- as.integer(readLines(file.path(root, args[4])))
out   <- file.path(root, args[5])

mut <- readRDS(file.path(root, "docs/adversarial/mutants.rds"))
mine <- ids[seq_along(ids) %% nw == (wi %% nw)]

work <- file.path(tempdir(), paste0("mutwork", wi))
dir.create(work, showWarnings = FALSE, recursive = TRUE)
for (d in c("R", "tests", "man", "inst")) {
  if (dir.exists(file.path(root, d)))
    file.copy(file.path(root, d), work, recursive = TRUE)
}
for (f in c("DESCRIPTION", "NAMESPACE"))
  file.copy(file.path(root, f), work, overwrite = TRUE)

setwd(work)
suppressMessages(library(testthat))

## test-file ordering: cheapest first, but the same-named file first of all.
timing <- c(
  "test-multiattr.R"=20.6,"test-evaluate.R"=18.0,"test-idf.R"=9.6,
  "test-axis-screening.R"=9.2,"test-self-contained.R"=8.9,"test-knowledge.R"=7.6,
  "test-generalized-column-guard.R"=3.9,"test-spatiotemporal.R"=3.9,
  "test-tiebreak-seed.R"=3.6,"test-activity.R"=3.6,"test-layers.R"=3.4,
  "test-statistical-properties.R"=3.1,"test-generalize.R"=2.6,
  "test-scoreboard.R"=2.4,"test-confidence.R"=1.9,"test-blocking.R"=1.8,
  "test-setsim.R"=1.5,"test-assignment.R"=1.3,"test-split-literal.R"=1.3,
  "test-args-and-determinism.R"=1.3,"test-unicity.R"=0.8,
  "test-column-selection.R"=0.8,"test-tie-and-na.R"=0.7,
  "test-master-column-names.R"=0.6,"test-distance-metrics.R"=0.6,
  "test-known-values.R"=0.4,"test-boundary-cases.R"=0.3,
  "test-distribution-distance.R"=0.3,"test-contract.R"=0.2,"test-calc-kl.R"=0.2)
all_tests <- list.files("tests/testthat", pattern = "^test-", full.names = FALSE)
base_order <- all_tests[order(timing[all_tests])]

run_one <- function(m) {
  target <- file.path(work, m$file)
  orig <- readLines(target, warn = FALSE)
  on.exit(writeLines(orig, target), add = TRUE)
  mutated <- orig
  mutated[m$line] <- m$new_line
  writeLines(mutated, target)

  loaded <- tryCatch({
    suppressMessages(suppressWarnings(
      pkgload::load_all(work, quiet = TRUE, export_all = FALSE)))
    TRUE
  }, error = function(e) FALSE)
  if (!loaded) return(list(status = "KILLED", by = "<load_all>"))

  want <- sub("^R/", "test-", m$file)
  ord <- c(want[want %in% base_order], setdiff(base_order, want))
  for (tf in ord) {
    r <- tryCatch({
      invisible(capture.output(suppressMessages(suppressWarnings(
        res <- testthat::test_file(file.path("tests/testthat", tf),
                                   package = "reidentify",
                                   reporter = "silent")))))
      d <- as.data.frame(res)
      c(sum(d$failed), sum(d$error))
    }, error = function(e) c(1, 1))
    if (sum(r) > 0) return(list(status = "KILLED", by = tf))
  }
  list(status = "SURVIVED", by = "")
}

con <- file(out, open = "wt")
for (i in mine) {
  m <- mut[[i]]
  t0 <- Sys.time()
  r <- tryCatch(run_one(m), error = function(e)
    list(status = "ERROR", by = conditionMessage(e)))
  el <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  writeLines(paste(i, r$status, r$by, el, m$file, m$line, m$kind,
                   m$tok, m$new, sep = "\t"), con)
  flush(con)
}
close(con)
