## Issue #92 verification: does the generalisation detector see the way a
## Japanese release actually writes a band, and does seeing it cost anything in
## false positives?
##
## Run with:
##   Rscript docs/investigation/japanese-generalization-benchmark.R
##
## Two questions, and the second is the binding one.
##
##   1. RECALL. "65歳以上" is the ordinary way to write a top-coded age band in
##      Japanese. Before this fix strip_unit() only removed a unit from the
##      *end* of the value, so "65以上" was read as [65, Inf) and "65歳以上"
##      was not read at all -- is_generalized_value() returned FALSE, the
##      Issue #40 guard stayed quiet, and score_char() edit-distanced a number
##      against a band. This section lists every notation the parser is
##      expected to handle and prints the interval it produces.
##
##   2. PRECISION. Issue #40 measured what happens when the detector is too
##      eager: reading "8s" as [8, 18) made the check fire on a column of
##      random two-character codes and stopped test-statistical-properties.R,
##      a test with nothing to do with generalised data. That calibration has
##      to be redone here, because every notation added widens the surface.
##      This section measures the per-value false-positive rate and the
##      per-column share on random strings, and compares it with the rate the
##      code before the fix produced on exactly the same draws.
##
## The pre-fix parser is reimplemented at the bottom of this file rather than
## checked out, so the two rates are measured on identical inputs in one
## process.

suppressWarnings(suppressMessages(pkgload::load_all(".", quiet = TRUE)))

cat("R:", R.version.string, "\n")
cat("date:", format(Sys.time()), "\n")
cat("stringi:", as.character(utils::packageVersion("stringi")), "\n\n")

u <- function(...) intToUtf8(c(...))

SAI    <- 0x6B73          # 歳
SAI2   <- 0x624D          # 才
DAI    <- 0x4EE3          # 代
BAND   <- 0x53F0          # 台
EN     <- 0x5186          # 円
NIN    <- 0x4EBA          # 人
NEN    <- 0x5E74          # 年
NICHI  <- 0x65E5          # 日
MAN    <- 0x4E07          # 万
OKU    <- 0x5104          # 億
IJOU   <- c(0x4EE5, 0x4E0A)  # 以上
IKA    <- c(0x4EE5, 0x4E0B)  # 以下
MIMAN  <- c(0x672A, 0x6E80)  # 未満
CHOU   <- 0x8D85             # 超
INAI   <- c(0x4EE5, 0x5185)  # 以内
IKOU   <- c(0x4EE5, 0x964D)  # 以降
IZEN   <- c(0x4EE5, 0x524D)  # 以前
MADE   <- c(0x307E, 0x3067)  # まで
KARA   <- c(0x304B, 0x3089)  # から
MITSU  <- 0x6E80             # 満 (prefix)
ZENHAN <- c(0x524D, 0x534A)  # 前半
KOUHAN <- c(0x5F8C, 0x534A)  # 後半
WAVE   <- 0x301C             # 〜
PCT    <- 0xFF05             # ％

## ---------------------------------------------------------------------------
## 1. recall: the notations a Japanese release uses
## ---------------------------------------------------------------------------

expected <- list(
  ## age, the case Issue #92 is about
  list(u(0x36, 0x35, SAI, IJOU),                       "[65,Inf)"),
  list(u(0x33, 0x30, SAI, MIMAN),                      "(-Inf,30)"),
  list(u(0x36, 0x35, SAI, IKA),                        "(-Inf,65]"),
  list(u(0x36, 0x35, SAI, CHOU),                       "(65,Inf)"),
  list(u(0x32, 0x30, SAI, WAVE, 0x32, 0x39, SAI),      "[20,29]"),
  list(u(0x32, 0x30, SAI, 0x2D, 0x32, 0x39, SAI),      "[20,29]"),
  list(u(0x32, 0x30, DAI),                             "[20,30)"),
  list(u(0x32, 0x30, SAI, DAI),                        "[20,30)"),
  list(u(0x32, 0x30, DAI, ZENHAN),                     "[20,25)"),
  list(u(0x32, 0x30, DAI, KOUHAN),                     "[25,30)"),
  list(u(0x31, 0x39, 0x39, 0x30, NEN, DAI),            "[1990,2000)"),
  list(u(0x33, 0x30, SAI, 0x2B),                       "[30,Inf)"),
  list(u(0x33, 0x30, SAI, 0xFF0B),                     "[30,Inf)"),
  list(u(0x32, 0x30, SAI2, IJOU),                      "[20,Inf)"),
  list(u(MITSU, 0x32, 0x30, SAI, IJOU),                "[20,Inf)"),
  list(u(0x32, 0x30, SAI, IJOU, 0x33, 0x30, SAI, MIMAN), "[20,30)"),
  list(u(0x32, 0x30, SAI, IJOU, 0x32, 0x39, SAI, IKA), "[20,29]"),
  list(u(0x32, 0x30, SAI, KARA, 0x32, 0x39, SAI),      "[20,29]"),
  list(u(0x32, 0x30, SAI, KARA, 0x32, 0x39, SAI, MADE), "[20,29]"),
  list(u(0x32, 0x39, SAI, MADE),                       "(-Inf,29]"),
  list(u(0x36, 0x35, SAI, WAVE),                       "[65,Inf)"),
  list(u(WAVE, 0x36, 0x34, SAI),                       "(-Inf,64]"),
  ## money
  list(u(0x31, 0x30, 0x30, 0x30, EN, IJOU),            "[1000,Inf)"),
  list(u(0x31, 0x2C, 0x30, 0x30, 0x30, EN, IJOU),      "[1000,Inf)"),
  list(u(0x35, MAN, EN, MIMAN),                        "(-Inf,50000)"),
  list(u(0x31, OKU, EN, IJOU),                         "[1e+08,Inf)"),
  list(u(0x31, OKU, 0x32, 0x30, 0x30, 0x30, MAN, EN, IJOU), "[1.2e+08,Inf)"),
  list(u(0x33, 0x30, 0x30, 0x30, EN, BAND),            "[3000,4000)"),
  list(u(0x32, 0x30, MAN, EN, BAND),                   "[2e+05,3e+05)"),
  ## other counters
  list(u(0x31, 0x30, 0x30, NIN, IJOU),                 "[100,Inf)"),
  list(u(0x33, 0x30, NICHI, INAI),                     "(-Inf,30]"),
  list(u(0x32, 0x30, 0x32, 0x30, NEN, IKOU),           "[2020,Inf)"),
  list(u(0x32, 0x30, 0x32, 0x30, NEN, IZEN),           "(-Inf,2020]"),
  list(u(0x31, 0x30, 0x25, IJOU),                      "[10,Inf)"),
  list(u(0x31, 0x30, PCT, IJOU),                       "[10,Inf)"),
  ## fullwidth digits and punctuation
  list(u(0xFF16, 0xFF15, SAI, IJOU),                   "[65,Inf)"),
  list(u(0xFF13, 0xFF10, DAI),                         "[30,40)"),
  list(u(0xFF12, 0xFF10, SAI, 0xFF5E, 0xFF12, 0xFF19, SAI), "[20,29]"),
  ## ASCII forms that already worked and must keep working
  list("[30,40)", "[30,40)"),
  list("30s",     "[30,40)"),
  list("30-39",   "[30,39]"),
  list("30+",     "[30,Inf)"),
  list("30+yrs",  "[30,Inf)"),
  list("30-39kg", "[30,39]")
)

iv_str <- function(x) {
  iv <- parse_generalized_interval(x)
  if (is.null(iv)) {
    return(NA_character_)
  }
  sprintf("%s%s,%s%s",
          if (iv$lower_closed) "[" else "(",
          format(iv$lower), format(iv$upper),
          if (iv$upper_closed) "]" else ")")
}

cat("== notations recognised ==\n")
bad <- 0L
for (e in expected) {
  got <- iv_str(e[[1]])
  ok <- identical(got, e[[2]])
  if (!ok) bad <- bad + 1L
  cat(sprintf("  %-22s -> %-16s %-16s %s  region=%s\n",
              e[[1]], if (is.na(got)) "NOT PARSED" else got,
              paste0("(want ", e[[2]], ")"),
              if (ok) "ok" else "MISMATCH",
              is_generalized_value(e[[1]])))
}
cat(sprintf("  %d of %d notations parse as expected\n\n",
            length(expected) - bad, length(expected)))

cat("== notations deliberately NOT read as regions ==\n")
negatives <- c("37", "37.0", "-5", "M", "F", "3S", "30S", "Z001", "8s", "7s",
               "1:2:3", "a-b", "30kg", "35",
               u(0x6771, 0x4EAC, 0x90FD),                    # 東京都
               u(0x4E0D, 0x660E),                            # 不明
               u(0x305D, 0x306E, 0x4ED6),                    # その他
               u(0x4E09, 0x5341, SAI, IJOU),                 # 三十歳以上
               u(0x36, 0x35, SAI, IJOU, 0x306E, 0x65B9),     # 65歳以上の方
               u(0x33, 0x6708, 0x31, 0x35, NICHI),           # 3月15日
               u(0x32, 0x30, 0x32, 0x34, NEN))               # 2024年
for (v in negatives) {
  cat(sprintf("  %-14s region=%-6s interval=%s\n", v,
              is_generalized_value(v),
              if (is.na(iv_str(v))) "-" else iv_str(v)))
}
cat("\n")

## ---------------------------------------------------------------------------
## 2. precision: the Issue #40 calibration, redone
## ---------------------------------------------------------------------------
##
## The pre-fix detector, reimplemented so both can be measured on the same
## draws. This is R/generalize.R as of commit 8cadf9f, cut down to the parts
## that decide is_generalized_value().

prefix_units <- c(
  vapply(c(0x6B73, 0x624D, 0x5E74, 0x5186, 0x4EBA,
           0x4EF6, 0x56DE, 0x65E5, 0x6708, 0xFF05),
         intToUtf8, character(1)),
  "%", "yr", "yrs", "y", "years", "kg", "km", "cm", "m", "g"
)
PRE_NUM <- "[-+]?[0-9]+(?:\\.[0-9]+)?"
PRE_DASH <- paste0("[-~", intToUtf8(c(0x2013, 0x2014, 0x301C, 0xFF5E)), "]")
PRE_DECADE <- intToUtf8(0x4EE3)
PRE_OR_MORE <- intToUtf8(c(0x4EE5, 0x4E0A))
PRE_OR_LESS <- intToUtf8(c(0x4EE5, 0x4E0B))
PRE_UNDER <- intToUtf8(c(0x672A, 0x6E80))

pre_parse <- function(x) {
  if (is.na(x)) return(NULL)
  s <- trimws(as.character(x))
  if (!nzchar(s)) return(NULL)
  num <- function(v) if (!nzchar(v)) NA_real_ else as.numeric(v)
  m <- regmatches(s, regexec(
    paste0("^([[(])\\s*(", PRE_NUM, ")?\\s*,\\s*(", PRE_NUM, ")?\\s*([])])$"), s))[[1]]
  if (length(m) == 5) {
    lo <- num(m[3]); hi <- num(m[4])
    return(list(lower = if (is.na(lo)) -Inf else lo,
                upper = if (is.na(hi)) Inf else hi))
  }
  su <- s
  for (one in prefix_units[order(nchar(prefix_units), decreasing = TRUE)]) {
    if (nzchar(one) && endsWith(su, one)) {
      su <- trimws(substr(su, 1L, nchar(su) - nchar(one)))
      break
    }
  }
  m <- regmatches(s, regexec(paste0("^(", PRE_NUM, ")\\s*(", PRE_DECADE, "|s)$"), s))[[1]]
  if (length(m) == 3) return(list(lower = as.numeric(m[2]), upper = as.numeric(m[2]) + 10))
  if (grepl(paste0("^", PRE_NUM, "$"), su)) {
    v <- as.numeric(su); return(list(lower = v, upper = v))
  }
  m <- regmatches(su, regexec(paste0("^(", PRE_NUM, ")\\s*(\\+|", PRE_OR_MORE, ")$"), su))[[1]]
  if (length(m) == 3) return(list(lower = as.numeric(m[2]), upper = Inf))
  m <- regmatches(su, regexec(paste0("^(", PRE_NUM, ")\\s*(", PRE_OR_LESS, ")$"), su))[[1]]
  if (length(m) == 3) return(list(lower = -Inf, upper = as.numeric(m[2])))
  m <- regmatches(su, regexec(paste0("^(", PRE_NUM, ")\\s*(", PRE_UNDER, ")$"), su))[[1]]
  if (length(m) == 3) return(list(lower = -Inf, upper = as.numeric(m[2])))
  unsigned <- "[0-9]+(?:\\.[0-9]+)?"
  m <- regmatches(su, regexec(paste0("^(", unsigned, ")\\s*", PRE_DASH, "\\s*(", unsigned, ")$"), su))[[1]]
  if (length(m) == 3) return(list(lower = as.numeric(m[2]), upper = as.numeric(m[3])))
  m <- regmatches(su, regexec(paste0("^(", unsigned, ")\\s*", PRE_DASH, "$"), su))[[1]]
  if (length(m) == 2) return(list(lower = as.numeric(m[2]), upper = Inf))
  m <- regmatches(su, regexec(paste0("^", PRE_DASH, "\\s*(", unsigned, ")$"), su))[[1]]
  if (length(m) == 2) return(list(lower = -Inf, upper = as.numeric(m[2])))
  NULL
}

pre_is_generalized <- function(x) {
  v <- as.character(x)
  out <- rep(FALSE, length(v))
  known <- !is.na(v)
  if (!any(known)) return(out)
  s <- trimws(v[known])
  uq <- unique(s)
  hit <- !is.na(uq) & grepl("\\*$", uq)
  odd_decade <- grepl(paste0("^(", PRE_NUM, ")\\s*(", PRE_DECADE, "|s)$"), uq) &
    !grepl(paste0("^[-+]?[0-9]*0\\s*(", PRE_DECADE, "|s)$"), uq)
  for (i in which(!hit & !odd_decade)) {
    iv <- pre_parse(uq[i])
    hit[i] <- !is.null(iv) && isTRUE(iv$lower < iv$upper)
  }
  out[known] <- hit[match(s, uq)]
  out
}

## sanity: the reimplementation reproduces the defect and the #40 calibration
stopifnot(
  isTRUE(pre_is_generalized("30s")),
  isFALSE(pre_is_generalized("8s")),
  isFALSE(pre_is_generalized(u(0x36, 0x35, SAI, IJOU))),   # 65歳以上: the bug
  isTRUE(pre_is_generalized(u(0x36, 0x35, IJOU)))          # 65以上: worked
)

## n is 200,000 at length 2 -- where the whole alphabet is only 3,844 strings
## and the parse is cached by unique() -- and 50,000 above it, where almost
## every draw is distinct and the parser runs once per draw.
draw_n <- c("2" = 200000L, "3" = 50000L, "4" = 50000L, "6" = 50000L)

cat("== per-value false-positive rate on random strings ==\n")
cat("   (stri_rand_strings default alphabet [A-Za-z0-9])\n")
set.seed(20260801)
for (len in c(2L, 3L, 4L, 6L)) {
  n <- draw_n[[as.character(len)]]
  s <- stringi::stri_rand_strings(n, length = len)
  before <- mean(pre_is_generalized(s))
  after <- mean(is_generalized_value(s))
  cat(sprintf("  length %d (n = %6d):  before %.4f%%   after %.4f%%   %s\n",
              len, n, 100 * before, 100 * after,
              if (identical(before, after)) "unchanged" else "CHANGED"))
}

cat("\n== per-value false-positive rate on a wider alphabet ==\n")
cat("   (letters, digits and the punctuation an interval could use)\n")
wide <- "[A-Za-z0-9+~.,*-]"
set.seed(20260802)
for (len in c(2L, 3L, 4L, 6L)) {
  n <- draw_n[[as.character(len)]]
  s <- stringi::stri_rand_strings(n, length = len, pattern = wide)
  before <- mean(pre_is_generalized(s))
  after <- mean(is_generalized_value(s))
  cat(sprintf("  length %d (n = %6d):  before %.4f%%   after %.4f%%   delta %+.4f pp\n",
              len, n, 100 * before, 100 * after, 100 * (after - before)))
}

cat("\n== per-value false-positive rate on random Japanese text ==\n")
cat("   (hiragana, katakana and common kanji -- a categorical column written\n")
cat("    in Japanese, which is the surface this fix newly touches)\n")
set.seed(20260805)
jp_pool <- intToUtf8(c(0x3041:0x3093, 0x30A1:0x30F6,
                       0x4E00:0x4E80, 0x5E74, 0x6708, 0x65E5, 0x6B73,
                       0x5186, 0x4EBA, 0x4EE3, 0x53F0),
                     multiple = TRUE)
digits <- as.character(0:9)
for (len in c(2L, 3L, 4L)) {
  n <- 50000L
  s <- vapply(seq_len(n), function(i) {
    paste(sample(c(jp_pool, digits, digits), len, replace = TRUE), collapse = "")
  }, character(1))
  before <- mean(pre_is_generalized(s))
  after <- mean(is_generalized_value(s))
  cat(sprintf("  length %d (n = %6d):  before %.4f%%   after %.4f%%   delta %+.4f pp\n",
              len, n, 100 * before, 100 * after, 100 * (after - before)))
}

cat("\n== per-column share, the quantity the guard thresholds on ==\n")
cat("   (2000 draws of 40 two-character strings, as create_dummy_master_data()\n")
cat("    produces; the guard needs a share of at least ",
    format(GENERALIZATION_SHARE_THRESHOLD), ")\n", sep = "")
set.seed(20260803)
shares_before <- numeric(2000)
shares_after <- numeric(2000)
for (i in seq_len(2000)) {
  s <- stringi::stri_rand_strings(40L, length = 2L)
  shares_before[i] <- mean(pre_is_generalized(s))
  shares_after[i] <- mean(is_generalized_value(s))
}
cat(sprintf("  before: max share %.4f  mean %.6f  columns over threshold %d\n",
            max(shares_before), mean(shares_before),
            sum(shares_before >= GENERALIZATION_SHARE_THRESHOLD)))
cat(sprintf("  after : max share %.4f  mean %.6f  columns over threshold %d\n",
            max(shares_after), mean(shares_after),
            sum(shares_after >= GENERALIZATION_SHARE_THRESHOLD)))

cat("\n== the fixture columns the rest of the suite uses ==\n")
q <- create_dummy_qi_data(people = 400, seed = 1)
for (nm in setdiff(names(q), "ROW_NUMBER")) {
  cat(sprintf("  %-14s share flagged = %.4f\n", nm,
              mean(is_generalized_value(as.character(q[[nm]])))))
}
set.seed(1)
mst <- create_dummy_master_data(people = 400)
for (nm in setdiff(names(mst), "ROW_NUMBER")) {
  cat(sprintf("  %-14s share flagged = %.4f\n", nm,
              mean(is_generalized_value(as.character(mst[[nm]])))))
}

## ---------------------------------------------------------------------------
## 3. what the fix is worth: the guard on a release written in Japanese
## ---------------------------------------------------------------------------

cat("\n== the under-report Issue #40 closed, on a Japanese-written release ==\n")
set.seed(20260804)
people <- 200
raw <- data.frame(
  ROW_NUMBER = seq_len(people),
  AGE = sample(20:79, people, replace = TRUE),
  SEX = sample(c("M", "F"), people, replace = TRUE),
  stringsAsFactors = FALSE
)
## "20歳以上25歳未満" -- exactly how a Japanese release writes an age bin
band <- function(a) {
  lo <- floor(a / 5) * 5
  ifelse(lo >= 75,
         paste0("75", u(SAI), u(IJOU)),
         paste0(lo, u(SAI), u(IJOU), lo + 5, u(SAI), u(MIMAN)))
}
anon <- data.frame(ROW_NUMBER = raw$ROW_NUMBER, AGE = band(raw$AGE),
                   SEX = raw$SEX, stringsAsFactors = FALSE)
d <- join_raw_anon_data(raw, anon)

cat("  sample published values: ",
    paste(head(unique(anon$AGE), 3), collapse = ", "), "\n", sep = "")
cat("  detected as regions (before the fix): ",
    format(mean(pre_is_generalized(anon$AGE))), "\n", sep = "")
cat("  detected as regions (after the fix):  ",
    format(mean(is_generalized_value(anon$AGE))), "\n", sep = "")

cat("  score_char(AGE) now: ",
    tryCatch({
      score_char(d, "AGE")
      "RETURNED A SCORE (guard silent)"
    }, error = function(e) paste0("stopped -- ", substr(conditionMessage(e), 1, 90), " ...")),
    "\n", sep = "")

targets <- c("AGE", "SEX")
cont <- match_greedy(score_containment(d, targets), seed = 1L)
rate_cont <- mean(cont$RAW_ROW_NUMBER == cont$ANON_ROW_NUMBER)
ch <- match_greedy(combine_scores(lapply(targets, function(t) {
  score_char(d, t, generalized = "ignore")
})), seed = 1L)
rate_char <- mean(ch$RAW_ROW_NUMBER == ch$ANON_ROW_NUMBER)
cat(sprintf("  score_containment  success = %.4f\n", rate_cont))
cat(sprintf("  score_char (misuse) success = %.4f  -> %.1fx under-report\n",
            rate_char, rate_cont / rate_char))
