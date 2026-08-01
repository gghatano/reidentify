## ---------------------------------------------------------------------------
## Line coverage of R/, reported and never gated (Issue #86).
##
## WHY THERE IS NO THRESHOLD. Coverage on this package was 98.51% the first
## time it was measured, and the 1.49% that is missing is almost entirely
## defensive `stop()` branches -- guards whose whole purpose is never to fire.
## A number that is already at 98.5% cannot get much better, so a gate on it
## would be a gate that only ever fires on noise, and the way to clear such a
## gate is to write a test for whichever guard is cheapest to reach. That is
## the opposite of what the suite needs.
##
## What coverage IS good for here is the *zero* column: a whole branch that
## nothing in the suite has ever run. Measured on 1438ebc, that column found
## the open-ended generalisation forms ("65 or more", "39 under") had never
## been parsed by any test, and those are exactly the values whose failure mode
## is silent -- an unparsed band reads as an ordinary category, the #40 guard
## stays quiet, and the reported risk comes out fourfold low. So this script
## prints the uncovered lines per file, and leaves the judgement to a reader.
##
## The job DOES fail if the measurement itself breaks, because a coverage
## report that has silently stopped running is worth less than no report
## (docs/lessons-learned.md section 2).
##
## Run with:
##   Rscript .github/scripts/coverage-report.R .
## ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (length(args)) args[1] else ".", winslash = "/")

if (!requireNamespace("covr", quietly = TRUE)) {
  stop("coverage-report.R needs the `covr` package", call. = FALSE)
}

cov <- covr::package_coverage(root, type = "tests", quiet = TRUE)

overall <- covr::percent_coverage(cov)
per_file <- sort(covr::coverage_to_list(cov)$filecoverage)

d <- as.data.frame(cov)
d$file <- basename(as.character(d$filename))
zero <- d[d$value == 0, ]

## ---- stdout: the whole picture, for the job log ----------------------------
cat(sprintf("overall line coverage: %.2f%%\n\n", overall))
cat("per file (lowest first):\n")
for (i in seq_along(per_file)) {
  cat(sprintf("  %-38s %6.2f%%\n", names(per_file)[i], per_file[[i]]))
}

cat("\nlines no test ever reached:\n")
if (nrow(zero) == 0L) {
  cat("  (none)\n")
} else {
  for (f in sort(unique(zero$file))) {
    lines <- sort(unique(zero$first_line[zero$file == f]))
    runs <- split(lines, cumsum(c(1, diff(lines) != 1)))
    cat(sprintf("  %-38s %s\n", f, paste(vapply(
      runs,
      function(r) if (length(r) == 1L) as.character(r)
        else paste0(min(r), "-", max(r)),
      character(1)
    ), collapse = ", ")))
  }
}

## ---- job summary: the short form, so a PR does not need the log ------------
summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY", unset = "")
if (nzchar(summary_file)) {
  con <- file(summary_file, open = "at", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(c(
    sprintf("## Line coverage: %.2f%%", overall),
    "",
    sprintf("%d of %d expressions in `R/` were never evaluated by the suite.",
            nrow(zero), nrow(d)),
    "",
    "This is reported, not enforced: see the note at the top of",
    "`.github/scripts/coverage-report.R` for why there is no threshold.",
    "",
    "| file | coverage | lines never reached |",
    "|---|---:|---|",
    vapply(names(per_file), function(nm) {
      f <- basename(nm)
      lines <- sort(unique(zero$first_line[zero$file == f]))
      sprintf("| `%s` | %.2f%% | %s |", f, per_file[[nm]],
              if (length(lines)) paste(lines, collapse = ", ") else "-")
    }, character(1))
  ), con)
}
