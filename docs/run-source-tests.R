## ---------------------------------------------------------------------------
## Runs the test suite the way a developer does: pkgload::load_all() against
## the working tree, then testthat::test_dir().
##
## This is NOT the same run as `R CMD check` (Issues #63, #65):
##
##   * `R CMD check` tests an installed copy built from a tarball. The source
##     tree is not visible from there, so tests that reach for it take their
##     other branch.
##   * `load_all()` + `test_dir()` sees the working tree, so those branches run.
##
## The counts differ for that reason, and the larger one is the complete run.
## site/index.html quotes the number of passing tests, so this script is also
## what feeds docs/verify-quoted-numbers.R its --tests= value.
##
## Run with:
##   Rscript docs/run-source-tests.R          # run and report
##   Rscript docs/run-source-tests.R --check-figures
## ---------------------------------------------------------------------------

args        <- commandArgs(trailingOnly = TRUE)
do_figures  <- "--check-figures" %in% args
pos         <- setdiff(args, "--check-figures")
pkg_root    <- normalizePath(if (length(pos)) pos[1] else ".", winslash = "/")

## This run is the one that is supposed to see the working tree, so tests that
## take a different branch when they cannot see it must fail here rather than
## quietly pass (#65). test-self-contained.R reads this.
Sys.setenv(REIDENTIFY_REQUIRE_SOURCE_TREE = "1")

suppressMessages(pkgload::load_all(pkg_root, quiet = TRUE))

res <- as.data.frame(testthat::test_dir(
  file.path(pkg_root, "tests", "testthat"),
  reporter = "summary", stop_on_failure = FALSE
))

n_pass <- sum(res$passed); n_fail <- sum(res$failed)
n_err  <- sum(res$error);  n_skip <- sum(res$skipped)

cat("\n==== source-tree run: PASS ", n_pass, " FAIL ", n_fail, " ERROR ", n_err,
    " SKIP ", n_skip, " ====\n", sep = "")

bad <- (n_fail > 0L) || (n_err > 0L)

if (do_figures) {
  cat("\n---- quoted figures ----\n")
  st <- system2("Rscript",
                c(shQuote(file.path(pkg_root, "docs", "verify-quoted-numbers.R")),
                  shQuote(pkg_root), paste0("--tests=", n_pass)))
  if (!identical(st, 0L)) bad <- TRUE
}

if (bad) quit(status = 1L)
