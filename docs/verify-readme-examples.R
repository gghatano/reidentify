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
##   4. captures every warning and message the examples signal and compares the
##      whole list with docs/verify-readme-expected.txt;
##   5. asserts the number of blocks run / skipped and of output units compared
##      against the same file;
##   6. checks that every exported function is named somewhere in README.md.
##
## Comparison is whitespace-normalised but otherwise exact: an omitted line is
## a mismatch, not a pass. (The #10 version tested "does every README line
## appear somewhere in the log", which silently accepted truncated output.)
##
## What this harness does NOT compare: the prose around the blocks, the block
## languages other than ```r, and anything a block writes to a file or a
## device. Everything a reader sees inside a ```r block -- printed output,
## warnings, messages -- is compared.
##
## Why the counts are pinned (#62): the failure mode of a harness is not that
## it goes red, it is that it silently checks less. `skip_re` below used to be
## matched against the whole block *including comments*, so a single
## `# install.packages("dplyr")` comment line took the entire block out of the
## comparison -- turning a real mismatch (exit 1) into a green run (exit 0).
## The regex is now applied to the code only (comments stripped by R's own
## parser), and, because the next such hole will not look like this one, the
## block / skip / unit counts are pinned in a file. Anything that shrinks the
## checked surface has to show up as a diff in that file.
##
## Run with:
##   Rscript docs/verify-readme-examples.R            # check, exit 1 on drift
##   Rscript docs/verify-readme-examples.R --rewrite  # write actual output back
##
## `--rewrite` only replaces `#>` runs that already exist, so where output is
## shown stays an authoring decision; only its content is mechanical. It also
## regenerates docs/verify-readme-expected.txt.
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

## R reads ./.Rprofile and ~/.Rprofile before this script runs, and every one
## of these options changes how values print. Without this, a developer whose
## profile sets width = 120 sees 6 spurious mismatches -- and, worse, a
## --rewrite in that session bakes width-120 output into README.md and breaks
## CI. Pin them here rather than relying on --vanilla, which would also throw
## away the library paths the harness needs.
options(width = 80, digits = 7, OutDec = ".", scipen = 0,
        useFancyQuotes = TRUE, max.print = 99999)

readme_path <- file.path(pkg_root, "README.md")
expect_path <- file.path(pkg_root, "docs", "verify-readme-expected.txt")
lines <- readLines(readme_path, warn = FALSE, encoding = "UTF-8")

## ---- 0. the skip decision, and a self-test of it (#62) ---------------------
## Blocks that install software or open help pages are instructions, not
## examples: they are reported as skipped so nothing is silently ignored.
skip_re <- "install\\.packages|R CMD INSTALL|load_all\\(\"path/to|test_dir\\(|help\\(package|^\\?"

## The skip decision must look at the CODE only. It used to be matched against
## the whole block text, comments included, so `# install.packages("dplyr")`
## -- a comment README:78 and README:88 really do carry -- silently took the
## entire block out of the comparison. Comments are now stripped by R's own
## parser (deparse drops them); an unparseable block falls back to the raw
## text, which is the conservative direction (it may skip, never mis-run).
##
## String literals are deliberately NOT stripped: `load_all("path/to` is a
## pattern about a literal. A real example whose *data* contains one of these
## strings would therefore be skipped -- that would show up as a drop in
## blocks_run, which is pinned.
code_of <- function(body) {
  txt <- paste(body, collapse = "\n")
  ex <- tryCatch(parse(text = txt), error = function(e) NULL)
  if (is.null(ex) || !length(ex)) return(txt)
  paste(vapply(as.list(ex),
               function(e) paste(deparse(e), collapse = "\n"),
               character(1)),
        collapse = "\n")
}

local({
  cases <- list(
    ## the #62 regression: a comment must not disqualify a block
    list(c('# install.packages("dplyr")', 'x <- 1'),                   FALSE),
    list(c('x <- 1  # see install.packages() if this fails'),          FALSE),
    list(c('# run test_dir("tests/testthat") first', 'y <- 2'),        FALSE),
    ## real instructions must still be skipped
    list(c('install.packages("dplyr")'),                               TRUE),
    list(c('pkgload::load_all("path/to/reidentify")'),                 TRUE),
    list(c('testthat::test_dir("tests/testthat")'),                    TRUE),
    list(c('help(package = "reidentify")'),                            TRUE),
    ## unparseable text falls back to the raw match
    list(c('R CMD INSTALL .'),                                         TRUE)
  )
  bad <- vapply(cases, function(cs)
    !identical(grepl(skip_re, code_of(cs[[1]])), cs[[2]]), logical(1))
  if (any(bad)) {
    cat("SELFTEST failed for block(s):\n")
    for (cs in cases[bad])
      cat("  <", paste(cs[[1]], collapse = " / "), "> expected skip=", cs[[2]],
          "\n", sep = "")
    quit(status = 1L)
  }
})

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

report     <- list()
edits      <- list()
conditions <- character(0)
n_run <- 0L; n_skip <- 0L; n_units <- 0L; n_bad <- 0L

for (bi in seq_along(blocks)) {
  b <- blocks[[bi]]
  if (!identical(b$lang, "r")) next
  code  <- paste(b$body, collapse = "\n")
  label <- paste0("README.md:", b$open)

  if (grepl(skip_re, code_of(b$body))) {
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
    if (length(res$msgs))
      conditions <- c(conditions,
                      paste0(label, " +", first, " | ", norm(res$msgs)))

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

## ---- 2b. pinned counts and conditions (#62) --------------------------------
## The point of this section is that the checked surface cannot shrink quietly.
## A block that stops being run, an output unit that stops being compared, a
## warning that appears or disappears -- each is a diff here, not a silent
## change of a number nobody reads.

read_expected <- function(path) {
  if (!file.exists(path)) return(NULL)
  l <- readLines(path, warn = FALSE, encoding = "UTF-8")
  l <- l[!grepl("^\\s*(#|$)", l)]
  key <- sub("^\\s*([a-z_]+)\\s*:.*$", "\\1", l)
  val <- trimws(sub("^\\s*[a-z_]+\\s*:", "", l))
  num <- function(k) {
    v <- val[key == k]
    if (length(v) != 1L) NA_integer_ else suppressWarnings(as.integer(v))
  }
  list(blocks_run = num("blocks_run"), blocks_skipped = num("blocks_skipped"),
       output_units = num("output_units"), conditions = val[key == "condition"])
}

write_expected <- function(path, n_run, n_skip, n_units, conditions) {
  txt <- c(
    "# docs/verify-readme-expected.txt",
    "#",
    "# docs/verify-readme-examples.R が README.md を実行して得るはずの件数と、",
    "# 実行中に出るはずの警告・メッセージ。Issue #62 の対策。",
    "#",
    "# なぜ固定するか: ハーネスの危険な壊れ方は「赤くなる」ことではなく",
    "# 「黙って検査対象が減る」ことである。実際 #62 では、コメント 1 行が",
    "# ブロック 1 つを検査対象から外し、README の改竄が exit 0 になった。",
    "# 唯一の兆候は誰も見ていない 34 → 33 という数字だった。",
    "#",
    "# 意図して README を変えたときは、変更が意図どおりであることを確認して",
    "#   Rscript docs/verify-readme-examples.R . --rewrite",
    "# で再生成する。手で編集してもよいが、減らす方向の編集は理由を書くこと。",
    "",
    paste0("blocks_run: ", n_run),
    paste0("blocks_skipped: ", n_skip),
    paste0("output_units: ", n_units),
    "",
    "# 実行中に signal される警告・メッセージ。順序込みで完全一致を要求する。",
    "# ここに無い警告が出ても、ここにある警告が出なくても落ちる。",
    paste0("condition: ", conditions)
  )
  con <- file(path, open = "wt", encoding = "UTF-8")
  writeLines(txt, con); close(con)
}

n_pin_bad <- 0L
if (rewrite) {
  write_expected(expect_path, n_run, n_skip, n_units, conditions)
  cat("REWROTE ", expect_path, "\n", sep = "")
} else {
  exp_pin <- read_expected(expect_path)
  if (is.null(exp_pin)) {
    n_pin_bad <- n_pin_bad + 1L
    cat("\nPIN   missing expectations file: ", expect_path,
        "\n      regenerate with: Rscript docs/verify-readme-examples.R . --rewrite\n",
        sep = "")
  } else {
    for (k in c("blocks_run", "blocks_skipped", "output_units")) {
      got <- switch(k, blocks_run = n_run, blocks_skipped = n_skip,
                    output_units = n_units)
      want <- exp_pin[[k]]
      if (is.na(want) || !identical(as.integer(want), as.integer(got))) {
        n_pin_bad <- n_pin_bad + 1L
        cat("\nPIN   ", k, ": expected ", if (is.na(want)) "<absent>" else want,
            ", got ", got, "\n", sep = "")
        if (!is.na(want) && got < want)
          cat("      the harness is checking LESS than it used to. ",
              "Find out why before updating the pin.\n", sep = "")
      }
    }
    if (!identical(exp_pin$conditions, conditions)) {
      n_pin_bad <- n_pin_bad + 1L
      cat("\nPIN   warnings/messages differ from ", expect_path, "\n", sep = "")
      for (m in setdiff(conditions, exp_pin$conditions))
        cat("      + unexpected: ", m, "\n", sep = "")
      for (m in setdiff(exp_pin$conditions, conditions))
        cat("      - missing   : ", m, "\n", sep = "")
      if (setequal(exp_pin$conditions, conditions))
        cat("      (same set, different order)\n")
    }
  }
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
cat("==== ", length(conditions), " warning(s)/message(s) signalled, ",
    n_pin_bad, " pinned expectation(s) violated ====\n", sep = "")
cat("==== ", length(exports), " exported function(s), ", length(undocumented),
    " missing from README ====\n", sep = "")
if (length(undocumented)) cat("  ", paste(undocumented, collapse = "\n  "), "\n", sep = "")

if ((n_bad > 0 && !rewrite) || length(undocumented) > 0 || n_pin_bad > 0)
  quit(status = 1L)
