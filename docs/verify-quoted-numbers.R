## ---------------------------------------------------------------------------
## Checks the "state of the package" numbers that documents quote (Issue #63).
##
## The public page and the method catalogue both quote figures about the
## package itself -- how many tests pass, how many functions are exported.
## Those numbers went stale twice (#47/#48, then again two commits later),
## because nothing recomputed them. This script recomputes them.
##
## It deliberately does NOT check the *measurement* figures on the page
## (45.5% / 11.7% / 7.0% / 94.5% / 91x). Those come from a seeded fixture and
## are reproduced by docs/investigation/site-figures.R; they change only when
## the measurement changes, and that script is the record.
##
## Two kinds of number are handled differently:
##
##   * derivable from the tree (exported functions) -- recomputed here;
##   * derivable only by running something (tests passing) -- pinned in the
##     page and compared by the caller that ran it, via --tests=N;
##   * not derivable at all (closed issues: the count moves without a commit)
##     -- not checked, but the page must say *as of when*, which is checked.
##
## Run with:
##   Rscript docs/verify-quoted-numbers.R .              # offline checks
##   Rscript docs/verify-quoted-numbers.R . --tests=2311 # + the test count
## ---------------------------------------------------------------------------

args     <- commandArgs(trailingOnly = TRUE)
tests_arg <- grep("^--tests=", args, value = TRUE)
pos      <- setdiff(args, tests_arg)
pkg_root <- normalizePath(if (length(pos)) pos[1] else ".", winslash = "/")
tests_n  <- if (length(tests_arg))
  as.integer(sub("^--tests=", "", tests_arg[1])) else NA_integer_

n_bad <- 0L
fail <- function(...) {
  n_bad <<- n_bad + 1L
  cat("FAIL  ", ..., "\n", sep = "")
}
ok <- function(...) cat("ok    ", ..., "\n", sep = "")

## "not in the future" needs a day of slack: CI runs with TZ=UTC and the dates
## in these files are written in the author's local time. A date stamped in
## JST is UTC's tomorrow for nine hours a day, and the first CI run of this
## script failed for exactly that reason -- a check about staleness has no
## business being sensitive to which side of midnight UTC is on. A whole day
## of slack still catches what this is for: a date that was never true.
not_future <- function(d) {
  !is.na(d) && !is.na(as.Date(d, optional = TRUE)) &&
    as.Date(d) <= Sys.Date() + 1L
}

read_utf8 <- function(...) readLines(file.path(pkg_root, ...), warn = FALSE,
                                     encoding = "UTF-8")

## a number written for humans: 2,227 -> 2227
as_num <- function(x) suppressWarnings(as.integer(gsub("[^0-9]", "", x)))

## ---- what the tree actually says -------------------------------------------
ns      <- read_utf8("NAMESPACE")
exports <- length(grep("^export\\(", ns))
cat("== measured ==\n")
cat("  exported functions : ", exports, "\n", sep = "")
if (!is.na(tests_n)) cat("  tests passing      : ", tests_n, "\n", sep = "")
cat("\n== quoted ==\n")

## ---- site/index.html -------------------------------------------------------
site <- read_utf8("site", "index.html")

figure <- function(txt, name) {
  hit <- grep(paste0("data-figure=\"", name, "\""), txt, value = TRUE)
  if (length(hit) != 1L) return(NULL)
  ## the marked element's own text, not the whole line
  sub(paste0("^.*data-figure=\"", name, "\"[^>]*>([^<]*)<.*$"), "\\1", hit)
}

site_tests <- figure(site, "tests")
if (is.null(site_tests)) {
  fail("site/index.html: no unique element marked data-figure=\"tests\"")
} else if (is.na(as_num(site_tests))) {
  fail("site/index.html: data-figure=\"tests\" holds no number: ", site_tests)
} else {
  ok("site/index.html tests    : ", as_num(site_tests), "  <", site_tests, ">")
  site_asof <- figure(site, "tests-asof")
  if (is.null(site_asof) || is.na(as_num(site_asof))) {
    fail("site/index.html: footer is missing data-figure=\"tests-asof\"")
  } else if (!identical(as_num(site_asof), as_num(site_tests))) {
    fail("site/index.html: the table says ", as_num(site_tests),
         " tests but the footer says ", as_num(site_asof))
  }
  if (!is.na(tests_n) && !identical(as_num(site_tests), tests_n)) {
    fail("site/index.html says ", as_num(site_tests), " tests, the suite ran ",
         tests_n, ".\n      fix: set data-figure=\"tests\" and ",
         "data-figure=\"tests-asof\" to ", format(tests_n, big.mark = ","))
  }
}

## The page carries a claim that only a human can refresh; it must at least be
## dated, so a reader can tell how old it is.
site_closed <- figure(site, "closed-issues")
if (is.null(site_closed)) {
  fail("site/index.html: no element marked data-figure=\"closed-issues\"")
} else if (!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", site_closed)) {
  fail("site/index.html: the closed-issue count must say as of when: <",
       site_closed, ">")
} else {
  ok("site/index.html closed   : <", site_closed, ">")
}

site_upd <- figure(site, "updated")
if (is.null(site_upd) || is.na(as.Date(site_upd, optional = TRUE))) {
  fail("site/index.html: footer is missing a data-figure=\"updated\" date")
} else if (!not_future(site_upd)) {
  fail("site/index.html: 最終更新 ", site_upd, " is in the future (today is ",
       format(Sys.Date()), ")")
} else {
  ok("site/index.html updated  : ", site_upd)
}

## ---- docs/investigation/site-figures-log.txt --------------------------------
fig_log <- read_utf8("docs", "investigation", "site-figures-log.txt")
hit <- grep("exported functions", fig_log, value = TRUE)
if (length(hit) != 1L) {
  fail("site-figures-log.txt: no unique 'exported functions' line")
} else if (!identical(as_num(hit), exports)) {
  fail("site-figures-log.txt says ", as_num(hit), " exported functions, ",
       "NAMESPACE has ", exports,
       ".\n      fix: Rscript docs/investigation/site-figures.R ",
       "> docs/investigation/site-figures-log.txt")
} else {
  ok("site-figures-log.txt     : ", as_num(hit), " exported functions")
}

## ---- docs/reid-method-candidates.md ----------------------------------------
## The catalogue calls itself audited, so it has to say what it was audited
## against. "2026-07-31 に突き合わせた" does not survive the next commit (#63):
## the same day can hold a dozen of them. One machine-readable line carries the
## commit, the date and the counts.
cat_md <- read_utf8("docs", "reid-method-candidates.md")
hit <- grep("^\\*\\*照合基準\\*\\*:", cat_md, value = TRUE)
if (length(hit) != 1L) {
  fail("reid-method-candidates.md: expected exactly one line starting with ",
       "**照合基準**:, found ", length(hit))
} else {
  field <- function(k) {
    m <- regmatches(hit, regexpr(paste0(k, "=`?[^ `]+`?"), hit))
    if (!length(m)) NA_character_ else gsub("`", "", sub(paste0(k, "="), "", m))
  }
  commit <- field("commit"); date <- field("date"); ex_q <- field("exports")
  before <- n_bad
  if (is.na(commit) || !grepl("^[0-9a-f]{7,40}$", commit))
    fail("reid-method-candidates.md 照合基準: commit= must be a hex sha, got <",
         commit, ">")
  if (!not_future(date))
    fail("reid-method-candidates.md 照合基準: date= must not be in the future, ",
         "got <", date, "> (today is ", format(Sys.Date()), ")")
  if (is.na(as_num(ex_q)) || !identical(as_num(ex_q), exports))
    fail("reid-method-candidates.md 照合基準 says exports=", ex_q,
         ", NAMESPACE has ", exports,
         ".\n      re-audit the catalogue against the current tree, ",
         "then update commit=/date=/exports=")
  if (identical(n_bad, before))
    ok("reid-method-candidates   : exports=", exports, " commit=", commit,
       " date=", date)
}

cat("\n==== ", n_bad, " quoted figure(s) out of date ====\n", sep = "")
if (n_bad > 0L) quit(status = 1L)
