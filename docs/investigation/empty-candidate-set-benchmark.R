## Issues #101 and #109: an empty candidate set reads as a safe release, and a
## hierarchy passed under the wrong name reads as no hierarchy at all.
##
## Run with:
##   Rscript docs/investigation/empty-candidate-set-benchmark.R
##
## Section 1 measures the defects by re-creating each of them from a *pre-fix*
## reimplementation of the pieces that changed, so the before and after numbers
## come from identical draws in one process. Section 2 is the false-positive
## calibration Issues #40 and #92 established, redone for every surface these
## fixes widen:
##
##   * is_generalization_wildcard() gained the suppression markers a release
##     actually writes. A false positive here matches EVERY raw record, which
##     enlarges candidate sets and LOWERS the reported risk -- the dangerous
##     direction, and the reason bare "NA" and numeric sentinels were left out.
##   * node_matches() now reads the RAW side with the same parser as the ANON
##     side, so "37歳" is 37. A false positive here reads a categorical code as
##     a number.
##   * node_matches(rule = "auto") now reads a trailing "*" as a prefix. A false
##     positive here widens a category into a prefix.
##
## is_generalized_value() -- the detector Issues #40 and #92 calibrated -- is
## NOT touched by either fix, and section 2 checks that its measured rate is
## unchanged rather than assuming it.

suppressWarnings(suppressMessages(pkgload::load_all(".", quiet = TRUE)))

cat("R:", R.version.string, "\n")
cat("date:", format(Sys.time()), "\n")
cat("stringi:", as.character(utils::packageVersion("stringi")), "\n\n")

u <- function(...) intToUtf8(c(...))
SAI <- 0x6B73                       # 歳
IJOU <- c(0x4EE5, 0x4E0A)           # 以上
MIMAN <- c(0x672A, 0x6E80)          # 未満
FUMEI <- u(0x4E0D, 0x660E)          # 不明

## ---------------------------------------------------------------------------
## the pre-fix implementations, so both sides are measured on the same draws
## ---------------------------------------------------------------------------

pre_wildcard <- function(x) {
  is.na(x) | !nzchar(trimws(x)) | grepl("^\\*+$", trimws(x))
}

pre_raw_numeric <- function(vals, units) suppressWarnings(as.numeric(vals))

pre_node_matches <- function(values, node, rule = "auto",
                             units = generalization_units()) {
  if (is.na(node)) {
    return(rep(FALSE, length(values)))
  }
  vals <- as.character(values)
  if (identical(rule, "exact")) {
    return(!is.na(vals) & vals == node)
  }
  if (identical(rule, "prefix")) {
    return(!is.na(vals) & startsWith(vals, sub("\\*+$", "", node)))
  }
  iv <- parse_generalized_interval(node, units)
  if (is.null(iv)) {
    return(!is.na(vals) & vals == node)
  }
  numv <- pre_raw_numeric(vals, units)
  inside <- !is.na(numv) &
    (if (iv$lower_closed) numv >= iv$lower else numv > iv$lower) &
    (if (iv$upper_closed) numv <= iv$upper else numv < iv$upper)
  inside | (!is.na(vals) & vals == node)
}

## The containment path, computed with the pre-fix pieces. Only those pieces
## differ; the surrounding arithmetic is copied unchanged, so the comparison is
## of the fix and nothing else.
pre_score <- function(d, targets, hierarchy = NULL, rules = NULL) {
  units <- generalization_units()
  rule_of <- resolve_containment_rules(targets, rules, "pre")
  per <- vapply(targets, function(t) {
    rv <- as.character(d[[paste0("RAW_", t)]])
    av <- as.character(d[[paste0("ANON_", t)]])
    uv <- unique(rv)
    ug <- unique(av)
    m <- matrix(FALSE, nrow = length(uv), ncol = length(ug))
    for (j in seq_along(ug)) {
      g <- ug[j]
      if (pre_wildcard(g)) {
        m[, j] <- TRUE
        next
      }
      hit <- pre_node_matches(uv, g, rule_of[[t]], units)
      for (dd in descendants_of(hierarchy, t, g)) {
        hit <- hit | pre_node_matches(uv, dd, rule_of[[t]], units)
      }
      m[, j] <- hit
    }
    m[cbind(match(rv, uv), match(av, ug))]
  }, logical(nrow(d)))
  per <- matrix(per, nrow = nrow(d))
  contained <- rowSums(per) == length(targets)
  k <- containment_k(contained, d$ANON_ROW_NUMBER, d$RAW_ROW_NUMBER)
  s <- ifelse(contained, 1 - 1 / k[as.character(d$ANON_ROW_NUMBER)], 1)
  out <- new_reid_scores(d$RAW_ROW_NUMBER, d$ANON_ROW_NUMBER, unname(s))
  attr(out, "candidate_count") <- k
  out
}

ev_of <- function(s) suppressWarnings(reid_evaluate(s, seeds = 1:10, top_k = 1))

line <- function(label, s) {
  ev <- ev_of(s)
  k <- attr(s, "candidate_count")
  cat(sprintf("  %-26s success=%.6f  lift=%6.2fx  empty candidate sets: %3d/%d\n",
              label, ev$success_analytic, ev$lift, sum(k == 0), length(k)))
}

## ---------------------------------------------------------------------------
## 1. the defects
## ---------------------------------------------------------------------------

cat("== #101 (a): a unit written on the RAW side only ==\n")
cat("   ANON publishes 5-year bands as \"30歳以上35歳未満\"; RAW carries the age.\n")

age_fixture <- function(raw_fmt, marker = NULL, people = 200, seed = 20260802) {
  set.seed(seed)
  age <- sample(20:79, people, replace = TRUE)
  sex <- sample(c("M", "F"), people, replace = TRUE)
  raw <- data.frame(ROW_NUMBER = seq_len(people), AGE = raw_fmt(age), SEX = sex,
                    stringsAsFactors = FALSE)
  lo <- floor(age / 5) * 5
  band <- paste0(lo, u(SAI), u(IJOU), lo + 5, u(SAI), u(MIMAN))
  if (!is.null(marker)) {
    band[seq_len(30)] <- marker
  }
  anon <- data.frame(ROW_NUMBER = raw$ROW_NUMBER, AGE = band, SEX = sex,
                     stringsAsFactors = FALSE)
  join_raw_anon_data(raw, anon)
}

d_plain <- age_fixture(as.character)
d_unit <- age_fixture(function(a) paste0(a, u(SAI)))
line("RAW = 37       (before)", pre_score(d_plain, c("AGE", "SEX")))
line("RAW = 37       (after)",
     suppressWarnings(score_containment(d_plain, c("AGE", "SEX"))))
line("RAW = 37 + sai (before)", pre_score(d_unit, c("AGE", "SEX")))
line("RAW = 37 + sai (after)",
     suppressWarnings(score_containment(d_unit, c("AGE", "SEX"))))

cat("\n   what the other guards said about the broken case, before the fix:\n")
ev <- ev_of(pre_score(d_unit, c("AGE", "SEX")))
cat(sprintf("     blocked=%s  n_true_missing=%d  truth_coverage=%g  n_zero_candidate=%d\n",
            ev$blocked, ev$n_true_missing, ev$truth_coverage, ev$n_zero_candidate))
cat("   -- all three shape guards report a healthy join. Only the count that\n")
cat("      score_containment() had been attaching since #20 saw it.\n")

cat("\n== #101 (b): the suppression marker a release writes (30 of 200 rows) ==\n")
for (m in list("*", "-", "?", "N/A", "unknown", FUMEI, "999")) {
  dm <- age_fixture(as.character, marker = m)
  line(paste0(sprintf("%-9s", m), " (before)"), pre_score(dm, c("AGE", "SEX")))
  line(paste0(sprintf("%-9s", m), " (after)"),
       suppressWarnings(score_containment(dm, c("AGE", "SEX"))))
}
cat("   \"999\" is deliberately still empty: a numeric sentinel cannot be told\n")
cat("   apart from a measurement, so it is warned about, not silently widened.\n")

cat("\n== #101 (c): what the user is now told about a case that is still empty ==\n")
d999 <- age_fixture(as.character, marker = "999")
withCallingHandlers({
  s999 <- score_containment(d999, c("AGE", "SEX"))
  NULL
}, warning = function(cnd) {
  cat("  warning: ", conditionMessage(cnd), "\n", sep = "")
  invokeRestart("muffleWarning")
})
cat("  reid_evaluate() print(), verbatim:\n")
out <- capture.output(print(ev_of(s999)))
cat(paste0("    | ", out, collapse = "\n"), "\n")

## ---------------------------------------------------------------------------

cat("\n== #109 (1): a hierarchy whose attribute name does not match the column ==\n")
set.seed(20260803)
people <- 300
zips <- paste0(sprintf("%05d", sample(10000:10059, people, replace = TRUE)),
               sprintf("%02d", sample(0:99, people, replace = TRUE)))
raw <- data.frame(ROW_NUMBER = seq_len(people), ZIP = zips,
                  SEX = sample(c("M", "F"), people, replace = TRUE),
                  stringsAsFactors = FALSE)
h_of <- function(a) generalization_hierarchy(data.frame(
  attribute = a, value = unique(zips), parent = substr(unique(zips), 1, 5),
  stringsAsFactors = FALSE
))
anon <- data.frame(ROW_NUMBER = raw$ROW_NUMBER, ZIP = substr(zips, 1, 5),
                   SEX = raw$SEX, stringsAsFactors = FALSE)
d2 <- join_raw_anon_data(raw, anon)

for (a in c("ZIP", "zip", "POSTCODE")) {
  line(paste0("attribute=", a, " (before)"),
       pre_score(d2, c("ZIP", "SEX"), hierarchy = h_of(a)))
}
line("hierarchy=NULL (before)", pre_score(d2, c("ZIP", "SEX")))
cat("   -- \"zip\", \"POSTCODE\" and NULL agree to the last digit: the hierarchy\n")
cat("      was ignored and nothing said so.\n\n")
for (a in c("ZIP", "zip", "POSTCODE")) {
  s <- tryCatch(suppressWarnings(score_containment(d2, c("ZIP", "SEX"),
                                                   hierarchy = h_of(a))),
                error = function(e) e)
  if (inherits(s, "error")) {
    cat(sprintf("  %-26s ERROR: %s...\n", paste0("attribute=", a, " (after)"),
                substr(conditionMessage(s), 1, 96)))
  } else {
    line(paste0("attribute=", a, " (after)"), s)
  }
}

cat("\n== #109 (2): a masked column under the default rules ==\n")
anon_mask <- data.frame(ROW_NUMBER = raw$ROW_NUMBER,
                        ZIP = paste0(substr(zips, 1, 5), "**"),
                        SEX = raw$SEX, stringsAsFactors = FALSE)
d3 <- join_raw_anon_data(raw, anon_mask)
line("rules=NULL     (before)", pre_score(d3, c("ZIP", "SEX")))
line("rules=prefix   (before)",
     pre_score(d3, c("ZIP", "SEX"), rules = c(ZIP = "prefix")))
line("rules=NULL     (after)",
     suppressWarnings(score_containment(d3, c("ZIP", "SEX"))))
line("rules=prefix   (after)",
     suppressWarnings(score_containment(d3, c("ZIP", "SEX"),
                                        rules = c(ZIP = "prefix"))))
cat("   generalization_evidence() had been choosing rule = \"",
    if (is_generalization_mask("135****")) "prefix" else "auto",
    "\" for these\n   values all along; score_containment() was the only side that did not.\n",
    sep = "")

## ---------------------------------------------------------------------------
## 2. false-positive calibration
## ---------------------------------------------------------------------------

## 200,000 at length 2, where the whole alphabet is only 3,844 strings and the
## parse is cached by unique(); fewer above it, where almost every draw is
## distinct and the parser runs once per draw. The rates being compared here are
## between 0.1% and 3%, and 20,000 draws resolve those to better than 0.1 pp --
## the #40 protocol's 50,000 buys nothing at that scale and costs minutes.
draw_n <- c("2" = 200000L, "3" = 50000L, "4" = 20000L, "6" = 20000L)
lens <- c(2L, 3L, 4L, 6L)
wide <- "[A-Za-z0-9+~.,*-]"
jp_pool <- intToUtf8(c(0x3041:0x3093, 0x30A1:0x30F6,
                       0x4E00:0x4E80, 0x5E74, 0x6708, 0x65E5, 0x6B73,
                       0x5186, 0x4EBA, 0x4EE3, 0x53F0), multiple = TRUE)

## Drawn once and reused by every table below, so the four surfaces are all
## measured on exactly the same strings and the rates can be compared with each
## other and not only with their own "before".
DRAWS <- list()
for (kind in c("default", "wide", "jp")) {
  for (len in lens) {
    ## The Japanese draws are the slow surface -- every one of them takes the
    ## non-ASCII path through the parser -- so 10,000 throughout.
    n <- if (identical(kind, "jp")) 10000L else draw_n[[as.character(len)]]
    set.seed(20260801 + len)
    DRAWS[[paste0(kind, len)]] <- if (identical(kind, "default")) {
      stringi::stri_rand_strings(n, length = len)
    } else if (identical(kind, "wide")) {
      stringi::stri_rand_strings(n, length = len, pattern = wide)
    } else {
      pool <- c(jp_pool, as.character(0:9), as.character(0:9))
      apply(matrix(sample(pool, n * len, replace = TRUE), ncol = len), 1L,
            paste, collapse = "")
    }
  }
}

rate_table <- function(title, note, before_fn, after_fn, extra_fn = NULL,
                       extra_label = "") {
  cat("\n== ", title, " ==\n", sep = "")
  cat("   ", note, "\n", sep = "")
  for (kind in c("default", "wide", "jp")) {
    label <- switch(kind,
                    default = "[A-Za-z0-9]       ",
                    wide = "[A-Za-z0-9+~.,*-] ",
                    jp = "japanese + digits  ")
    for (len in lens) {
      s <- DRAWS[[paste0(kind, len)]]
      b <- mean(before_fn(s))
      a <- mean(after_fn(s))
      extra <- if (is.null(extra_fn)) {
        ""
      } else {
        e <- mean(extra_fn(s))
        sprintf("  %s %.4f%% (%+.4f pp)", extra_label, 100 * e, 100 * (e - b))
      }
      cat(sprintf("  %s len %d (n = %6d):  before %.4f%%  after %.4f%%  delta %+.4f pp%s\n",
                  label, len, length(s), 100 * b, 100 * a, 100 * (a - b), extra))
    }
  }
}

cat("\n\n=====================================================================\n")
cat("false-positive calibration (the Issue #40 / #92 protocol)\n")
cat("=====================================================================\n")

## is_generalized_value() -- the detector Issues #40 and #92 calibrated -- is
## not measured again here. Re-running it under a different seed would produce a
## different number and prove nothing; the regression test that actually binds
## is that the #92 benchmark reproduces its committed log byte for byte:
##
##   Rscript docs/investigation/japanese-generalization-benchmark.R \
##     > /tmp/after.txt && diff docs/investigation/japanese-generalization-benchmark-log.txt /tmp/after.txt
##
## (only the "date:" line differs). That covers the detector on all three
## alphabets, the per-column share against GENERALIZATION_SHARE_THRESHOLD, and
## the fixture columns.

rate_table(
  "is_generalization_wildcard(): read as \"suppressed, matches anything\"",
  "a false positive here matches every RAW record and LOWERS reported risk",
  pre_wildcard,
  is_generalization_wildcard
)

## The rejected variant is measured beside the chosen one. Reading a one-letter
## ASCII unit turns digit + {m, g, y} into a number, which is 30 of the 3844
## two-character strings over [A-Za-z0-9]; the chosen variant holds those back.
raw_numeric_all_units <- function(vals) {
  units <- generalization_units()
  out <- suppressWarnings(as.numeric(vals))
  todo <- is.na(out) & !is.na(vals)
  if (any(todo)) {
    u <- unique(vals[todo])
    pt <- vapply(u, function(v) {
      iv <- parse_generalized_interval(v, units)
      if (is.null(iv) || !isTRUE(iv$lower == iv$upper) ||
          !isTRUE(iv$lower_closed) || !isTRUE(iv$upper_closed)) {
        return(NA_real_)
      }
      iv$lower
    }, numeric(1), USE.NAMES = FALSE)
    out[todo] <- pt[match(vals[todo], u)]
  }
  out
}

rate_table(
  "node_matches(): share of RAW values that now read as a number",
  "as.numeric() before; as.numeric() then a point-interval parse after",
  function(s) !is.na(pre_raw_numeric(s, generalization_units())),
  function(s) !is.na(gen_raw_numeric(s, generalization_units())),
  function(s) !is.na(raw_numeric_all_units(s)),
  "| rejected variant, all units:"
)

rate_table(
  "is_generalization_mask(): share that rule = \"auto\" now reads as a prefix",
  "unchanged function; measured because \"auto\" newly acts on it",
  function(s) rep(FALSE, length(s)),
  is_generalization_mask
)

cat("\n== per-column share, the quantity the #40 guard thresholds on ==\n")
cat("   (2000 draws of 40 two-character strings, as create_dummy_master_data()\n")
cat("    produces; the guard needs a share of at least ",
    format(GENERALIZATION_SHARE_THRESHOLD), ")\n", sep = "")
set.seed(20260840)
sh_detect <- numeric(2000)
sh_wild <- numeric(2000)
sh_num <- numeric(2000)
for (i in seq_len(2000)) {
  s <- stringi::stri_rand_strings(40L, length = 2L)
  sh_detect[i] <- mean(is_generalized_value(s))
  sh_wild[i] <- mean(is_generalization_wildcard(s))
  sh_num[i] <- mean(!is.na(gen_raw_numeric(s, generalization_units())) &
                      is.na(pre_raw_numeric(s, generalization_units())))
}
cat(sprintf("  is_generalized_value      : max %.4f  mean %.6f  columns over threshold %d\n",
            max(sh_detect), mean(sh_detect),
            sum(sh_detect >= GENERALIZATION_SHARE_THRESHOLD)))
cat(sprintf("  is_generalization_wildcard: max %.4f  mean %.6f\n",
            max(sh_wild), mean(sh_wild)))
cat(sprintf("  newly numeric on RAW side : max %.4f  mean %.6f\n",
            max(sh_num), mean(sh_num)))

cat("\n== the values deliberately left out of the wildcard list ==\n")
for (v in c("NA", "na", "Na", "999", "-1", "0", "none", "None", "M", "F")) {
  cat(sprintf("  %-6s wildcard = %s\n", v, is_generalization_wildcard(v)))
}
cat("  \"NA\" in any casing is 4 of the 3844 two-character strings over\n")
cat("  [A-Za-z0-9] = 0.104%, four times the whole #40 budget for the detector.\n")

cat("\n== the fixture columns the rest of the suite uses ==\n")
q <- create_dummy_qi_data(people = 400, seed = 1)
set.seed(1)
mst <- create_dummy_master_data(people = 400)
show_cols <- function(tag, x) {
  for (nm in setdiff(names(x), "ROW_NUMBER")) {
    v <- as.character(x[[nm]])
    cat(sprintf("  %s %-13s region %.4f  wildcard %.4f  newly numeric %.4f\n",
                tag, nm,
                mean(is_generalized_value(v)),
                mean(is_generalization_wildcard(v)),
                mean(!is.na(gen_raw_numeric(v, generalization_units())) &
                       is.na(pre_raw_numeric(v, generalization_units())))))
  }
}
show_cols("qi ", q)
show_cols("mst", mst)
