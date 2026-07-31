## Environment-sensitivity probe (#55 class).
## Runs the whole suite under a few ambient settings that CI never varies:
## collation locale, options(width), timezone, and stringsAsFactors-era
## defaults. Reports pass/fail per cell.
##   usage: Rscript docs/adversarial/env-matrix.R <repo_root>
setwd(normalizePath(commandArgs(trailingOnly = TRUE)[1]))
suppressMessages(library(testthat))

cells <- list(
  list(name = "baseline",            setup = function() invisible(NULL)),
  list(name = "LC_COLLATE=C",        setup = function() Sys.setlocale("LC_COLLATE", "C")),
  list(name = "LC_ALL=C",            setup = function() suppressWarnings(Sys.setlocale("LC_ALL", "C"))),
  list(name = "options(width=40)",   setup = function() options(width = 40)),
  list(name = "options(width=200)",  setup = function() options(width = 200)),
  list(name = "TZ=Pacific/Kiritimati", setup = function() Sys.setenv(TZ = "Pacific/Kiritimati")),
  list(name = "options(digits=15)",  setup = function() options(digits = 15)),
  list(name = "options(OutDec=',')", setup = function() options(OutDec = ",")),
  list(name = "options(stringsAsFactors=TRUE)",
       setup = function() options(stringsAsFactors = TRUE)),
  list(name = "warn=2 (warnings are errors)", setup = function() options(warn = 2))
)

for (cl in cells) {
  code <- sprintf(
    'suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = FALSE));
     library(testthat);
     (%s)();
     cat("LOCALE:", Sys.getlocale("LC_COLLATE"), " WIDTH:", getOption("width"), "\\n");
     r <- test_dir("tests/testthat", package = "reidentify", reporter = "silent",
                   stop_on_failure = FALSE);
     d <- as.data.frame(r);
     cat("RESULT pass=", sum(d$passed), " fail=", sum(d$failed),
         " err=", sum(d$error), "\\n", sep = "");
     bad <- d[d$failed > 0 | d$error > 0, c("file", "test", "failed", "error")];
     if (nrow(bad)) print(bad)',
    paste(deparse(cl$setup), collapse = " "))
  f <- tempfile(fileext = ".R"); writeLines(code, f)
  cat("\n########## ", cl$name, " ##########\n", sep = "")
  out <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(f)),
                 stdout = TRUE, stderr = TRUE)
  cat(paste(grep("^LOCALE:|^RESULT|failed|error|Error", out, value = TRUE),
            collapse = "\n"), "\n")
}
