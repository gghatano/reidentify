## Issue #40: score_char() did not stop on a generalised column. It computed
## adist("37", "[30,40)") = 6 -- the length of the bracket string -- and handed
## back a table of plausible-looking scores. On the generalisation benchmark
## that reported a success rate of 0.1017 where score_containment() reported
## 0.4450: a fourfold under-report with no error anywhere, which is the failure
## direction docs/lessons-learned.md section 2 is about.
##
## These tests pin down three things:
##   1. the generalised column is now refused, by every value-comparison score;
##   2. an ordinary column is untouched (a false positive here would break
##      working analyses, so the detector has to be quiet on normal data);
##   3. the escape hatch still produces the old number, so the under-report is
##      reachable deliberately and never by accident.

gen_fixture <- function(people = 60, seed = 42) {
  set.seed(seed)
  raw <- data.frame(
    ROW_NUMBER = seq_len(people),
    AGE = sample(20:69, people, replace = TRUE),
    SEX = sample(c("M", "F"), people, replace = TRUE),
    NAME = paste0("name", seq_len(people)),
    stringsAsFactors = FALSE
  )
  anon <- data.frame(
    ROW_NUMBER = raw$ROW_NUMBER,
    AGE = sprintf("[%d,%d)", floor(raw$AGE / 10) * 10, floor(raw$AGE / 10) * 10 + 10),
    SEX = raw$SEX,
    NAME = raw$NAME,
    stringsAsFactors = FALSE
  )
  join_raw_anon_data(raw, anon)
}

## ---------------------------------------------------------------------------
## the detector itself
## ---------------------------------------------------------------------------

test_that("is_generalized_value() marks regions and leaves values alone", {
  expect_true(all(is_generalized_value(
    c("[30,40)", "(30,40]", "30s", "30-39", "0-100", "[65,)", "*", "**", "135****")
  )))
  expect_false(any(is_generalized_value(
    c("37", "37.0", "-5", "M", "Z001", "chiyoda", "tokyo", "1:2:3", "a-b", "")
  )))
})

test_that("is_generalized_value() treats a bare number as a value, not a region", {
  ## "35" parses as the degenerate interval [35, 35]. Calling that a
  ## generalisation would flag every numeric-looking character column.
  expect_false(is_generalized_value("35"))
  expect_true(is_generalized_value("35-36"))
})

test_that("is_generalized_value() reports NA as FALSE", {
  ## A missing value can mean anything; treating it as a generalisation would
  ## flag every column that merely has a gap in it.
  expect_equal(is_generalized_value(c(NA, "30s", NA_character_)),
               c(FALSE, TRUE, FALSE))
})

test_that("is_generalized_value() only reads a multiple of ten as a decade", {
  ## parse_generalized_interval() reads "8s" as [8, 18) -- correct for
  ## containment, wrong as evidence of generalisation. Random two-character
  ## strings hit digit + "s" 0.26% of the time, and that is what first made
  ## this check stop test-statistical-properties.R, a test with nothing to do
  ## with generalised data.
  expect_false(is_generalized_value("8s"))
  expect_false(is_generalized_value("7s"))
  expect_true(is_generalized_value("30s"))
  expect_true(is_generalized_value("60s"))
  ## the containment parser is unchanged: "8s" is still a usable node
  expect_true(node_matches("11", "8s"))
})

test_that("random two-character strings are not called generalised", {
  set.seed(99)
  s <- stringi::stri_rand_strings(4000, length = 2)
  ## measured: 0.0245% -- allow generous headroom, but not 1%
  expect_lt(mean(is_generalized_value(s)), 0.01)
})

test_that("the guard needs a region-shaped column, not one stray region value", {
  ## exactly the shape of the false positive found in the existing suite:
  ## a column of ordinary two-character codes, one of which happens to parse
  ## as an interval.
  raw <- data.frame(ROW_NUMBER = 1:10,
                    CHAR = c("11", "ab", "cd", "ef", "gh",
                             "ij", "kl", "mn", "op", "qr"),
                    stringsAsFactors = FALSE)
  anon <- raw
  anon$CHAR[1] <- "10-20"
  d <- join_raw_anon_data(raw, anon)
  expect_true(is_generalized_value("10-20"))
  expect_no_error(score_char(d, "CHAR"))
  expect_no_warning(score_char(d, "CHAR"))

  ## ... and it does fire once the column really is regions
  anon2 <- raw
  anon2$CHAR <- "10-20"
  d2 <- join_raw_anon_data(raw, anon2)
  expect_error(score_char(d2, "CHAR"), regexp = "generalised")
})

test_that("the message reports the share of published values that are regions", {
  d <- gen_fixture()
  expect_error(score_char(d, "AGE"), regexp = "100% of its published values are regions",
               fixed = FALSE)
})

test_that("is_generalized_value() is vectorised and keeps the input length", {
  x <- rep(c("30s", "35", NA), times = 4)
  expect_length(is_generalized_value(x), length(x))
  expect_equal(is_generalized_value(x), rep(c(TRUE, FALSE, FALSE), times = 4))
})

## ---------------------------------------------------------------------------
## the guard: generalised columns are refused
## ---------------------------------------------------------------------------

test_that("score_char() stops on a generalised column and names score_containment()", {
  d <- gen_fixture()
  expect_error(score_char(d, "AGE"), regexp = "score_char\\(\\)")
  expect_error(score_char(d, "AGE"), regexp = "score_containment")
  ## the message must show the evidence, not just assert it
  expect_error(score_char(d, "AGE"), regexp = "falls inside ANON")
})

test_that("score_num() on a generalised column says so instead of 'non-numeric argument'", {
  d <- gen_fixture()
  ## it always stopped -- but with base R's "non-numeric argument to binary
  ## operator", which names neither the function, the column, nor the fix.
  expect_error(score_num(d, "AGE"), regexp = "score_num\\(\\)")
  expect_error(score_num(d, "AGE"), regexp = "AGE")
  expect_error(score_num(d, "AGE"), regexp = "score_containment")
  ## and it is no longer base R's message
  expect_false(grepl("non-numeric argument to binary operator",
                     tryCatch(score_num(d, "AGE"), error = conditionMessage),
                     fixed = TRUE))
})

test_that("score_num_rank() and score_dist() stop on a generalised column", {
  d <- gen_fixture()
  expect_error(score_num_rank(d, "AGE"), regexp = "score_num_rank\\(\\)")
  expect_error(score_num_rank(d, "AGE"), regexp = "score_containment")
  expect_error(score_dist(d, "AGE"), regexp = "score_dist\\(\\)")
  expect_error(score_dist(d, "AGE"), regexp = "score_containment")
})

test_that("the reid_by_*() wrappers inherit the guard and name themselves", {
  d <- gen_fixture()
  expect_error(reid_by_char(d, "AGE"), regexp = "reid_by_char\\(\\)")
  expect_error(reid_by_num(d, "AGE"), regexp = "reid_by_num\\(\\)")
  expect_error(reid_by_num_rank(d, "AGE"), regexp = "reid_by_num_rank\\(\\)")
  expect_error(reid_by_dist(d, "AGE"), regexp = "reid_by_dist\\(\\)")
})

test_that("a suppressed (masked) ANON column is refused too", {
  raw <- data.frame(ROW_NUMBER = 1:5, ZIP = c("1350001", "1350002", "1360003",
                                              "1360004", "1400005"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:5, ZIP = c("135****", "135****", "136****",
                                               "136****", "140****"),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)
  expect_error(score_char(d, "ZIP"), regexp = "generalised")

  anon2 <- anon
  anon2$ZIP <- "*"
  d2 <- join_raw_anon_data(raw, anon2)
  expect_error(score_char(d2, "ZIP"), regexp = "generalised")
})

## ---------------------------------------------------------------------------
## the escape hatches
## ---------------------------------------------------------------------------

test_that("generalized = 'warn' computes the score but says what it is doing", {
  d <- gen_fixture()
  expect_warning(s <- score_char(d, "AGE", generalized = "warn"),
                 regexp = "score_containment")
  expect_s3_class(s, "reid_scores")
  expect_equal(nrow(s), nrow(d))
})

test_that("generalized = 'ignore' is silent and reproduces the pre-fix numbers", {
  d <- gen_fixture()
  expect_silent(s <- score_char(d, "AGE", generalized = "ignore"))
  ## the same edit distances as before the fix: RAW "37" vs ANON "[30,40)"
  i <- which(d$RAW_ROW_NUMBER == d$ANON_ROW_NUMBER)[1]
  expect_equal(s$SCORE[i],
               as.numeric(utils::adist(as.character(d$ANON_AGE[i]),
                                       as.character(d$RAW_AGE[i]))[[1]]))
})

test_that("the wrappers pass `generalized` through", {
  d <- gen_fixture()
  expect_warning(reid_by_char(d, "AGE", generalized = "warn"))
  expect_no_error(reid_by_char(d, "AGE", generalized = "ignore"))
})

## ---------------------------------------------------------------------------
## no false positives: ordinary columns must be untouched
## ---------------------------------------------------------------------------

test_that("ordinary character columns still score exactly as before", {
  d <- gen_fixture()
  expect_no_error(score_char(d, "SEX"))
  expect_no_warning(score_char(d, "NAME"))

  ## identical to the pre-fix result: the guard must not touch the score
  s <- score_char(d, "NAME")
  raw_v <- as.character(d$RAW_NAME)
  anon_v <- as.character(d$ANON_NAME)
  expect_equal(s$SCORE,
               unname(mapply(function(x, y) utils::adist(x, y)[[1]],
                             anon_v, raw_v)))
})

test_that("the dummy fixtures the rest of the suite uses are not flagged", {
  q <- create_dummy_qi_data(people = 40, seed = 1)
  j <- join_raw_anon_data(q, q)
  expect_no_error(score_char(j, "ZIP"))
  expect_no_error(score_char(j, "SEX"))
  expect_no_error(score_num(j, "AGE"))
  expect_no_error(score_num_rank(j, "VISIT_COUNT"))
})

test_that("a distribution column is not mistaken for a generalisation", {
  raw <- data.frame(ROW_NUMBER = 1:4, D = c("1:2", "2:3", "3:4", "4:5"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:4, D = c("1:2.1", "2:3.1", "3:4.1", "5:6.1"),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)
  expect_no_error(score_dist(d, "D"))
  expect_no_error(score_char(d, "D"))
})

test_that("a hyphenated code column is not flagged when both sides carry it", {
  ## "100-8111" is structurally indistinguishable from the range [100, 8111],
  ## so is_generalized_value() does call it a region. The guard needs a second
  ## fact before it fires -- a RAW value that falls *inside* the region and is
  ## not equal to it -- and a postcode column has none, because the two sides
  ## carry the same strings.
  expect_true(is_generalized_value("100-8111"))
  zips <- c("100-8111", "150-0001", "100-0005", "231-0023")
  d <- join_raw_anon_data(
    data.frame(ROW_NUMBER = 1:4, ZIP = zips, stringsAsFactors = FALSE),
    data.frame(ROW_NUMBER = 1:4, ZIP = zips, stringsAsFactors = FALSE)
  )
  expect_no_error(score_char(d, "ZIP"))
  expect_no_warning(score_char(d, "ZIP"))
})

test_that("a column already binned on BOTH sides is compared literally, not refused", {
  ## RAW and ANON both hold "[30,40)": the attacker's knowledge is as coarse as
  ## the release, so literal comparison is the right thing and there is no
  ## region-containing-a-value mismatch to report.
  bins <- c("[20,30)", "[30,40)", "[40,50)", "[30,40)")
  d <- join_raw_anon_data(
    data.frame(ROW_NUMBER = 1:4, AGE = bins, stringsAsFactors = FALSE),
    data.frame(ROW_NUMBER = 1:4, AGE = bins, stringsAsFactors = FALSE)
  )
  expect_no_error(score_char(d, "AGE"))
  expect_no_warning(score_char(d, "AGE"))
})

## ---------------------------------------------------------------------------
## the cross-cutting finding: rank() on a character column
## ---------------------------------------------------------------------------

test_that("score_num_rank() refuses a plain character column instead of ranking it lexicographically", {
  ## Found while checking the other scores for the same defect: rank() accepts
  ## a character vector and orders it alphabetically, so a categorical column
  ## -- including one generalised into named categories, which no structural
  ## test can recognise -- came back as a full table of plausible rank gaps.
  d <- join_raw_anon_data(
    data.frame(ROW_NUMBER = 1:4, AREA = c("chiyoda", "shinjuku", "yokohama", "kobe"),
               stringsAsFactors = FALSE),
    data.frame(ROW_NUMBER = 1:4, AREA = c("tokyo", "tokyo", "kanagawa", "hyogo"),
               stringsAsFactors = FALSE)
  )
  expect_error(score_num_rank(d, "AREA"), regexp = "not\\s+numeric")
  expect_error(score_num_rank(d, "AREA"), regexp = "lexicographic")
  expect_error(reid_by_num_rank(d, "AREA"), regexp = "reid_by_num_rank\\(\\)")
})

test_that("score_num() refuses a plain character column with a message that names it", {
  d <- gen_fixture()
  expect_error(score_num(d, "SEX"), regexp = "score_num\\(\\)")
  expect_error(score_num(d, "SEX"), regexp = "SEX")
  ## not generalised, so it must NOT point at score_containment()
  expect_error(score_num(d, "SEX"), regexp = "score_char")
})

## ---------------------------------------------------------------------------
## the number the issue is about
## ---------------------------------------------------------------------------

test_that("the fourfold under-report is no longer reachable without asking for it", {
  d <- gen_fixture(people = 200, seed = 7)

  ## what the correct score reports
  cont <- match_greedy(score_containment(d, c("AGE", "SEX")), seed = 1L)
  rate_containment <- mean(cont$RAW_ROW_NUMBER == cont$ANON_ROW_NUMBER)

  ## what the misuse reported, now only reachable on purpose
  ch <- match_greedy(
    combine_scores(lapply(c("AGE", "SEX"), function(t) {
      score_char(d, t, generalized = "ignore")
    })),
    seed = 1L
  )
  rate_char <- mean(ch$RAW_ROW_NUMBER == ch$ANON_ROW_NUMBER)

  expect_lt(rate_char, rate_containment)
  ## and the misuse is not silently available
  expect_error(combine_scores(lapply(c("AGE", "SEX"), function(t) score_char(d, t))))
})
