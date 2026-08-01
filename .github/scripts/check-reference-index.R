## ---------------------------------------------------------------------------
## Every exported function must appear in the pkgdown reference index.
##
## pkgdown already refuses to build when a non-internal .Rd topic is missing
## from `reference:` in _pkgdown.yml, so this looks redundant. It is not: that
## check is about *topics*, and it is satisfied by a catch-all section such as
## `- title: everything else / contents: matches(".")`. This one is about
## *exports* -- it reads NAMESPACE and asks whether each exported name is
## reachable from reference/index.html.
##
## The failure it is here to catch: a new export whose .Rd is written but which
## nobody put in a topic group, so the index quietly grows an "everything else"
## drawer, or an export that gets documented on a shared page under a name the
## index never links. Both leave a function that exists but cannot be found.
##
## Run with:
##   Rscript .github/scripts/check-reference-index.R .
## after pkgdown has written the site (destination: pkgdown-site).
## ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[1] else "."

ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
exports <- sort(sub("^export[(](.*)[)]$", "\\1",
                    grep("^export[(]", ns, value = TRUE)))

index_path <- file.path(root, "pkgdown-site", "reference", "index.html")
if (!file.exists(index_path)) {
  stop("no reference index at ", index_path,
       " -- build the site before running this", call. = FALSE)
}

## The index is UTF-8 with Japanese section headings; the hrefs are ASCII.
## Working on bytes keeps the regex out of any locale question.
idx <- readLines(index_path, warn = FALSE)
Encoding(idx) <- "bytes"
hrefs <- unlist(regmatches(
  idx, gregexpr('href="[^"]+[.]html"', idx, useBytes = TRUE)
))
topics <- unique(sub('^href="(.*)[.]html"$', "\\1", hrefs))
topics <- topics[!grepl("^(https?:|[.][.]/)", topics)]
topics <- setdiff(topics, "index")

missing <- setdiff(exports, topics)

cat("exported functions          : ", length(exports), "\n", sep = "")
cat("topics linked from the index: ", length(topics), "\n", sep = "")
cat("extra (S3 methods etc.)     : ",
    paste(sort(setdiff(topics, exports)), collapse = ", "), "\n", sep = "")

if (length(missing)) {
  cat("\n==== ", length(missing),
      " exported function(s) missing from the reference index ====\n", sep = "")
  cat(paste0("  ", missing, collapse = "\n"), "\n", sep = "")
  cat("fix: add them to a `reference:` section in _pkgdown.yml\n")
  quit(status = 1L)
}

cat("\n==== all ", length(exports),
    " exports are in the reference index ====\n", sep = "")
