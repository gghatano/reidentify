## Is the Issue-#39 guard in test-self-contained.R actually exercised by CI?
##
## Under `R CMD check` there is no source tree, so package_source_dir() returns
## NULL and verified_library() takes the "installed copy" branch. The branch
## that installs the working tree -- the whole point of #39 -- would then never
## run in CI. Prove it: poison that branch with stop() and see whether
## `R CMD check` (what CI runs) still passes, while the local
## load_all()+test_dir() run fails.
##   usage: Rscript docs/adversarial/ci-deadcode-probe.R <repo_root>
root <- normalizePath(commandArgs(trailingOnly = TRUE)[1], winslash = "/")
work <- file.path(tempdir(), "ci-deadcode")
unlink(work, recursive = TRUE); dir.create(work, recursive = TRUE)
src <- file.path(work, "reidentify")
dir.create(src)
for (d in c("R", "tests", "man", "inst"))
  file.copy(file.path(root, d), src, recursive = TRUE)
for (f in c("DESCRIPTION", "NAMESPACE", "LICENSE"))
  file.copy(file.path(root, f), src, overwrite = TRUE)

tf <- file.path(src, "tests/testthat/test-self-contained.R")
x <- readLines(tf, warn = FALSE)
i <- grep('^    lib <- file\\.path\\(tempdir\\(\\), "reidentify-verified-lib"\\)', x)
stopifnot(length(i) == 1)
x[i] <- paste0('    stop("POISON: the working-tree install branch ran"); ', x[i])
writeLines(x, tf)
cat("poisoned", tf, "at line", i, "\n")

setwd(work)
cat("\n---- 1. what CI runs: R CMD build + R CMD check --no-manual ----\n")
s1 <- system2(file.path(R.home("bin"), "R"),
              c("--vanilla", "CMD", "build", shQuote(src)),
              stdout = TRUE, stderr = TRUE)
tar <- list.files(work, pattern = "[.]tar[.]gz$")
cat("build status:", if (length(tar)) "OK" else "FAILED", "\n")
s2 <- system2(file.path(R.home("bin"), "R"),
              c("--vanilla", "CMD", "check", "--no-manual", shQuote(tar[1])),
              stdout = TRUE, stderr = TRUE)
cat("R CMD check exit status:", if (is.null(attr(s2, "status"))) 0L else attr(s2, "status"), "\n")
cat(paste(grep("^Status|ERROR|WARNING|NOTE|POISON", s2, value = TRUE),
          collapse = "\n"), "\n")

cat("\n---- 2. what a developer runs: load_all() + test_dir() ----\n")
setwd(src)
code <- 'suppressMessages(pkgload::load_all(".", quiet=TRUE, export_all=FALSE));
library(testthat);
r <- test_file("tests/testthat/test-self-contained.R", package="reidentify",
               reporter="silent");
d <- as.data.frame(r);
cat("pass=", sum(d$passed), " fail=", sum(d$failed), " err=", sum(d$error), "\n", sep="")'
f <- tempfile(fileext = ".R"); writeLines(code, f)
out <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(f)),
               stdout = TRUE, stderr = TRUE)
cat(paste(grep("pass=|POISON|Error", out, value = TRUE), collapse = "\n"), "\n")
