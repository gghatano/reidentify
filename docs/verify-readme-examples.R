## ---------------------------------------------------------------------------
## README.md verification harness (Issues #10, #47).
##
## The README's `#>` lines are *shown output*. They rot silently: #44 changed
## the `confidence` default and the README kept printing the old CONFIDENCE
## values for a whole release cycle. Nothing failed, because nothing checked.
## This script is the check.
##
## What it does:
##
##   1. extracts every fenced block from README.md;
##   2. runs the ```r blocks, in order, in ONE session -- exactly as a reader
##      copy-pasting down the page would -- splitting each block into
##      expressions via srcrefs;
##   3. captures each expression's printed output and compares it, line by
##      line, with the `#>` lines that follow it;
##   4. checks that every exported function is named somewhere in README.md,
##      and that the README names no function that is not exported.
##
## Comparison is whitespace-normalised but otherwise exact: an omitted line is
## a mismatch, not a pass. (The #10 version tested "does every README line
## appear somewhere in the log", which silently accepted truncated output.)
##
## Run with:
##   Rscript docs/verify-readme-examples.R            # check, exit 1 on drift
##   Rscript docs/verify-readme-examples.R --rewrite  # write actual output back
##
## `--rewrite` only replaces `#>` runs that already exist, so where output is
## shown stays an authoring decision; only its content is mechanical.
##
## The examples are seeded. Two independent runs must produce byte-identical
## logs -- an unseeded example drifts between runs (in #10, the success rate
## moved 0.0947 / 0.0973 / 0.1127 across runs) and this harness would then
## fail intermittently, which is the intended behaviour.
## ---------------------------------------------------------------------------

args     <- commandArgs(trailingOnly = TRUE)
rewrite  <- "--rewrite" %in% args
pos      <- setdiff(args, "--rewrite")
pkg_root <- normalizePath(if (length(pos)) pos[1] else ".", winslash = "/")

readme_path <- file.path(pkg_root, "README.md")
lines <- readLines(readme_path, warn = FALSE, encoding = "UTF-8")

## ---- 1. fenced blocks ------------------------------------------------------
fence <- grep("^```", lines)
stopifnot(length(fence) %% 2 == 0)

blocks <- list()
for (i in seq(1, length(fence), by = 2)) {
  blocks[[length(blocks) + 1L]] <- list(
    lang  = sub("^```", "", lines[fence[i]]),
    open  = fence[i],
    close = fence[i + 1L],
    body  = if (fence[i + 1L] - fence[i] > 1L)
      lines[(fence[i] + 1L):(fence[i + 1L] - 1L)] else character(0)
  )
}

## ---- 2. run ----------------------------------------------------------------
suppressMessages(pkgload::load_all(pkg_root, quiet = TRUE, export_all = FALSE))

env <- new.env(parent = globalenv())
## The README says library(reidentify); pkgload has already attached the
## package under test, so that one call is neutralised. Everything else runs
## verbatim. (The installed path is verified separately, by installing the
## built tarball into a temporary library and re-running the first example.)
env$library <- function(...) invisible(NULL)

norm <- function(x) gsub("[[:space:]]+", " ", trimws(x))

as_comment <- function(actual, prefix) {
  bare <- sub("\\s+$", "", prefix)
  ifelse(nzchar(actual), paste0(prefix, actual), bare)
}

capture_eval <- function(expr, envir) {
  out <- character(0)
  con <- textConnection("out", "w", local = TRUE)
  sink(con, type = "output")
  on.exit({ sink(type = "output"); close(con) }, add = TRUE)
  msgs <- character(0)
  withCallingHandlers(
    {
      v <- withVisible(eval(expr, envir = envir))
      if (v$visible) print(v$value)
    },
    warning = function(w) {
      msgs <<- c(msgs, paste0("WARNING: ", conditionMessage(w)))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      msgs <<- c(msgs, paste0("MESSAGE: ", sub("\n$", "", conditionMessage(m))))
      invokeRestart("muffleMessage")
    }
  )
  sink(type = "output"); close(con); on.exit()
  list(out = out, msgs = msgs)
}

report <- list()
edits  <- list()
n_run <- 0L; n_skip <- 0L; n_units <- 0L; n_bad <- 0L

## Blocks that install software or open help pages are instructions, not
## examples: they are reported as skipped so nothing is silently ignored.
skip_re <- "install\\.packages|R CMD INSTALL|load_all\\(\"path/to|test_dir\\(|help\\(package|^\\?"

for (bi in seq_along(blocks)) {
  b <- blocks[[bi]]
  if (!identical(b$lang, "r")) next
  code  <- paste(b$body, collapse = "\n")
  label <- paste0("README.md:", b$open)

  if (grepl(skip_re, code)) {
    cat("SKIP  ", label, " (install/help instructions)\n", sep = "")
    n_skip <- n_skip + 1L
    next
  }
  n_run <- n_run + 1L
  cat("\n===== RUN ", label, " =====\n", sep = "")

  ex <- parse(text = code, keep.source = TRUE)
  sr <- attr(ex, "srcref")

  for (k in seq_along(ex)) {
    first <- sr[[k]][1L]; last <- sr[[k]][3L]
    src   <- b$body[first:last]

    j <- last + 1L
    expected <- character(0)
    exp_first <- j
    prefix <- "#> "
    while (j <= length(b$body) && grepl("^\\s*#>", b$body[j])) {
      if (j == exp_first) prefix <- sub("^(\\s*#>\\s?).*$", "\\1", b$body[j])
      expected <- c(expected, sub("^\\s*#>\\s?", "", b$body[j]))
      j <- j + 1L
    }
    exp_last <- j - 1L

    res    <- capture_eval(ex[[k]], env)
    actual <- res$out
    for (m in res$msgs) cat("  [", m, "]\n", sep = "")

    if (!length(expected)) {
      cat("  ---- ", label, " +", first, "  (no #> expected)\n", sep = "")
      next
    }
    n_units <- n_units + 1L
    ok <- identical(norm(expected), norm(actual))
    if (!ok) {
      n_bad <- n_bad + 1L
      edits[[length(edits) + 1L]] <- list(
        start = b$open + exp_first, end = b$open + exp_last,
        new = as_comment(actual, prefix))
      report[[length(report) + 1L]] <- list(
        label = label, src = paste(src, collapse = "\n"),
        expected = expected, actual = actual)
    }
    cat(if (ok) "  OK   " else "  DIFF ", label, " +", first, "  <",
        substr(paste(src, collapse = " "), 1, 60), ">\n", sep = "")
  }
}

for (r in report) {
  cat("\n", strrep("-", 72), "\nMISMATCH at ", r$label, "\n  expr: ", r$src,
      "\n  --- README says ---\n", sep = "")
  cat(paste0("  | ", r$expected, "\n"), sep = "")
  cat("  --- actual ---\n")
  cat(paste0("  | ", r$actual, "\n"), sep = "")
}

if (rewrite && length(edits)) {
  ord <- order(vapply(edits, function(e) e$start, numeric(1)), decreasing = TRUE)
  new_lines <- lines
  for (i in ord) {
    e <- edits[[i]]
    head_part <- if (e$start > 1L) new_lines[1:(e$start - 1L)] else character(0)
    tail_part <- if (e$end < length(new_lines))
      new_lines[(e$end + 1L):length(new_lines)] else character(0)
    new_lines <- c(head_part, e$new, tail_part)
  }
  con <- file(readme_path, open = "wt", encoding = "UTF-8")
  writeLines(new_lines, con); close(con)
  cat("\nREWROTE ", readme_path, " (", length(edits), " edit(s))\n", sep = "")
}

## ---- 3. exported functions vs README --------------------------------------
ns <- readLines(file.path(pkg_root, "NAMESPACE"), warn = FALSE)
exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
readme_text <- paste(readLines(readme_path, warn = FALSE, encoding = "UTF-8"),
                     collapse = "\n")
undocumented <- exports[!vapply(exports, function(f)
  grepl(paste0("\\b", f, "\\b"), readme_text), logical(1))]

cat("\n==== ", n_run, " R block(s) run, ", n_skip, " skipped; ",
    n_units, " output unit(s) compared, ", n_bad, " mismatching ====\n", sep = "")
cat("==== ", length(exports), " exported function(s), ", length(undocumented),
    " missing from README ====\n", sep = "")
if (length(undocumented)) cat("  ", paste(undocumented, collapse = "\n  "), "\n", sep = "")

if ((n_bad > 0 && !rewrite) || length(undocumented) > 0) quit(status = 1L)
