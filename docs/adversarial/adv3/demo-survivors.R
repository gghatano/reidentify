## Show the user-visible consequence of mutants that the 2104-test suite
## does not kill.  Works on a throw-away copy of the package so the worktree
## is never modified.
##   usage: Rscript docs/adversarial/demo-survivors.R <repo_root>
root <- normalizePath(commandArgs(trailingOnly = TRUE)[1], winslash = "/")
work <- file.path(tempdir(), "demo-pkg")
unlink(work, recursive = TRUE); dir.create(work, recursive = TRUE)
for (d in c("R", "man")) file.copy(file.path(root, d), work, recursive = TRUE)
for (f in c("DESCRIPTION", "NAMESPACE"))
  file.copy(file.path(root, f), work, overwrite = TRUE)

patch <- function(file, line, old, new) {
  p <- file.path(work, file); x <- readLines(p, warn = FALSE)
  stopifnot(grepl(old, x[line], fixed = TRUE))
  x[line] <- sub(old, new, x[line], fixed = TRUE)
  writeLines(x, p)
}
reload <- function() suppressMessages(
  pkgload::load_all(work, quiet = TRUE, export_all = FALSE))

show <- function(label, expr) {
  cat("\n--", label, "\n")
  r <- tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)),
                warning = function(w) paste("WARNING:", conditionMessage(w)))
  print(r)
}

## ------------------------------------------------------------------ (A)
## R/activity.R:60   if (any(bad))  ->  if (all(bad))
## `bad` marks records whose collapsed column would not parse as numeric.
cat("\n================ (A) activity.R:60  any(bad) -> all(bad) ============\n")
mk <- function() {
  raw <- data.frame(ROW_NUMBER = 1:3,
                    D = c("1:2:3", "2:3:4", "3:4:5"), stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:3,
                     D = c("1:2:3", "x:y:z", "3:4:5"), stringsAsFactors = FALSE)
  list(raw = raw, anon = anon)
}
reload()
fx <- mk(); j <- join_raw_anon_data(fx$raw, fx$anon)
show("ORIGINAL  score_span(mixed good/bad)", score_span(j, "D"))
patch("R/activity.R", 60, "if (any(bad))", "if (all(bad))")
reload()
fx <- mk(); j <- join_raw_anon_data(fx$raw, fx$anon)
show("MUTANT    score_span(mixed good/bad)", utils::head(score_span(j, "D"), 4))
patch("R/activity.R", 60, "if (all(bad))", "if (any(bad))")

## ------------------------------------------------------------------ (B)
## R/activity.R:171  if (total > 0)  ->  if (total >= 0)
cat("\n================ (B) activity.R:171  total > 0 -> total >= 0 =========\n")
mk2 <- function() {
  raw <- data.frame(ROW_NUMBER = 1:2, D = c("a:b", "a:a"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:2, D = c("a:b", "a:a"),
                     stringsAsFactors = FALSE)
  list(raw = raw, anon = anon)
}
reload(); fx <- mk2(); j <- join_raw_anon_data(fx$raw, fx$anon)
show("ORIGINAL  score_profile(bins = 'zzz' -> every record empty)",
     score_profile(j, "D", bins = c("zzz", "qqq")))
patch("R/activity.R", 171, "if (total > 0)", "if (total >= 0)")
reload(); fx <- mk2(); j <- join_raw_anon_data(fx$raw, fx$anon)
show("MUTANT    score_profile(bins = 'zzz' -> every record empty)",
     score_profile(j, "D", bins = c("zzz", "qqq")))
patch("R/activity.R", 171, "if (total >= 0)", "if (total > 0)")

## ------------------------------------------------------------------ (C)
## R/blocking.R:54/55/61 : the "no denominator -> NA, never a reassuring
## number" guards.  Do any tests build a blocking with zero true pairs?
cat("\n================ (C) blocking.R:54/55/61  x > 0 -> x >= 0 ===========\n")
reload()
ni <- getFromNamespace("new_blocking_info", "reidentify")
show("ORIGINAL  new_blocking_info(n_pairs_full=0, n_true_pairs=0)",
     unlist(ni("x", 0, 0, 0, 0, 0, 0, 0)[c("kept_fraction", "reduction", "recall")]))
for (ln in c(54, 55, 61)) {
  x <- readLines(file.path(work, "R/blocking.R"), warn = FALSE)
  x[ln] <- sub("> 0", ">= 0", x[ln], fixed = TRUE)
  writeLines(x, file.path(work, "R/blocking.R"))
}
reload()
ni <- getFromNamespace("new_blocking_info", "reidentify")
show("MUTANT    new_blocking_info(n_pairs_full=0, n_true_pairs=0)",
     unlist(ni("x", 0, 0, 0, 0, 0, 0, 0)[c("kept_fraction", "reduction", "recall")]))
cat("\n(the three guards exist so that 'recall' is NA, not a number, when there",
    "\n is no ground truth -- see the comment at R/blocking.R:57-59)\n")
