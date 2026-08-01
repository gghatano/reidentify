## Issue #92: the way a Japanese release actually writes a band.
##
## strip_unit() removed a known unit only from the *end* of the value, so
## "65以上" was read as [65, Inf) and "65歳以上" was not read at all. The second
## is the ordinary Japanese form and the first is not; the consequence was
## therefore not an occasional miss but the normal case:
##
##   is_generalized_value("65歳以上")  -> FALSE
##   -> the Issue #40 guard stays quiet
##   -> score_char() edit-distances "37" against "65歳以上"
##   -> the release reports a success rate several times below the real one,
##      with no error and no warning (docs/lessons-learned.md section 2).
##
## Every string here is built from Unicode code points, for the same reason
## R/generalize.R builds its own that way: the file has to read and behave
## identically whatever locale and source encoding R was started with, and the
## CI matrix runs both Linux and Windows.

jp <- function(...) intToUtf8(c(...))

SAI    <- 0x6B73             # 歳   years old
SAI2   <- 0x624D             # 才   years old, informal
DAI    <- 0x4EE3             # 代   "the thirties"
BAND   <- 0x53F0             # 台   "in the 3000s"
EN     <- 0x5186             # 円   yen
NIN    <- 0x4EBA             # 人   people
NEN    <- 0x5E74             # 年   year
NICHI  <- 0x65E5             # 日   day
MAN    <- 0x4E07             # 万   10^4
OKU    <- 0x5104             # 億   10^8
IJOU   <- c(0x4EE5, 0x4E0A)  # 以上 or more
IKA    <- c(0x4EE5, 0x4E0B)  # 以下 or less
MIMAN  <- c(0x672A, 0x6E80)  # 未満 under
CHOU   <- 0x8D85             # 超   over, endpoint excluded
INAI   <- c(0x4EE5, 0x5185)  # 以内 within
IKOU   <- c(0x4EE5, 0x964D)  # 以降 from ... onwards
IZEN   <- c(0x4EE5, 0x524D)  # 以前 up to and including
MADE   <- c(0x307E, 0x3067)  # まで up to
KARA   <- c(0x304B, 0x3089)  # から from
MITSU  <- 0x6E80             # 満   prefix of 満20歳
ZENHAN <- c(0x524D, 0x534A)  # 前半 early half
KOUHAN <- c(0x5F8C, 0x534A)  # 後半 late half
WAVE   <- 0x301C             # 〜
PCT    <- 0xFF05             # ％

jp_iv <- function(x) {
  iv <- parse_generalized_interval(x)
  if (is.null(iv)) {
    return(NA_character_)
  }
  sprintf("%s%s,%s%s",
          if (iv$lower_closed) "[" else "(",
          format(iv$lower), format(iv$upper),
          if (iv$upper_closed) "]" else ")")
}

## ---------------------------------------------------------------------------
## the form the issue is about
## ---------------------------------------------------------------------------

test_that("a unit between the number and the suffix no longer hides the band", {
  ## The whole issue in four lines: "65以上" already worked, "65歳以上" is what
  ## a release writes, and before the fix only the first was read.
  expect_equal(jp_iv(jp(0x36, 0x35, IJOU)), "[65,Inf)")
  expect_equal(jp_iv(jp(0x36, 0x35, SAI, IJOU)), "[65,Inf)")
  expect_true(is_generalized_value(jp(0x36, 0x35, SAI, IJOU)))

  expect_equal(jp_iv(jp(0x33, 0x30, SAI, MIMAN)), "(-Inf,30)")
  expect_true(is_generalized_value(jp(0x33, 0x30, SAI, MIMAN)))
})

test_that("the unit is removed at every position it can occupy", {
  ## "20歳～29歳" carries it twice; only the trailing one used to go.
  expect_equal(jp_iv(jp(0x32, 0x30, SAI, WAVE, 0x32, 0x39, SAI)), "[20,29]")
  expect_equal(jp_iv(jp(0x32, 0x30, SAI, 0x2D, 0x32, 0x39, SAI)), "[20,29]")
  expect_equal(jp_iv(jp(0x33, 0x30, SAI, 0x2B)), "[30,Inf)")
  expect_equal(jp_iv(jp(0x32, 0x30, SAI2, IJOU)), "[20,Inf)")
})

test_that("the decade forms read with and without the unit", {
  expect_equal(jp_iv(jp(0x32, 0x30, DAI)), "[20,30)")
  expect_equal(jp_iv(jp(0x32, 0x30, SAI, DAI)), "[20,30)")
  expect_equal(jp_iv(jp(0x32, 0x30, DAI, ZENHAN)), "[20,25)")
  expect_equal(jp_iv(jp(0x32, 0x30, DAI, KOUHAN)), "[25,30)")
  expect_true(all(is_generalized_value(c(
    jp(0x32, 0x30, SAI, DAI),
    jp(0x32, 0x30, DAI, ZENHAN),
    jp(0x32, 0x30, DAI, KOUHAN)
  ))))

  ## "1990年代" is the same construction with 年 as the unit: the 1990s.
  expect_equal(jp_iv(jp(0x31, 0x39, 0x39, 0x30, NEN, DAI)), "[1990,2000)")
  expect_equal(jp_iv(jp(0x31, 0x39, 0x39, 0x30, NEN, DAI, ZENHAN)),
               "[1990,1995)")
})

test_that("money notation reads, including grouping and the myriad scale", {
  expect_equal(jp_iv(jp(0x31, 0x30, 0x30, 0x30, EN, IJOU)), "[1000,Inf)")
  ## a thousands separator is not a decimal point and not an interval comma
  expect_equal(jp_iv(jp(0x31, 0x2C, 0x30, 0x30, 0x30, EN, IJOU)), "[1000,Inf)")
  ## "5万円" is 50000 yen, and read literally is not a number at all
  expect_equal(jp_iv(jp(0x35, MAN, EN, MIMAN)), "(-Inf,50000)")
  expect_equal(jp_iv(jp(0x31, OKU, EN, IJOU)), "[1e+08,Inf)")
  expect_equal(jp_iv(jp(0x31, OKU, 0x32, 0x30, 0x30, 0x30, MAN, EN, IJOU)),
               "[1.2e+08,Inf)")
  expect_true(all(is_generalized_value(c(
    jp(0x31, 0x2C, 0x30, 0x30, 0x30, EN, IJOU),
    jp(0x35, MAN, EN, MIMAN)
  ))))
})

test_that("the two-sided band, the commonest way to write an age bin, reads", {
  expect_equal(jp_iv(jp(0x32, 0x30, SAI, IJOU, 0x33, 0x30, SAI, MIMAN)),
               "[20,30)")
  expect_equal(jp_iv(jp(0x32, 0x30, SAI, IJOU, 0x32, 0x39, SAI, IKA)),
               "[20,29]")
  ## the endpoints are not decoration: 以下 keeps the last record, 未満 drops it
  band <- jp(0x32, 0x30, SAI, IJOU, 0x33, 0x30, SAI, MIMAN)
  expect_equal(node_matches(c("19", "20", "29", "30"), band),
               c(FALSE, TRUE, TRUE, FALSE))
})

test_that("the remaining bound words read, and keep their endpoint", {
  expect_equal(jp_iv(jp(0x36, 0x35, SAI, IKA)), "(-Inf,65]")
  expect_equal(jp_iv(jp(0x36, 0x35, SAI, CHOU)), "(65,Inf)")
  expect_equal(jp_iv(jp(0x33, 0x30, NICHI, INAI)), "(-Inf,30]")
  expect_equal(jp_iv(jp(0x32, 0x30, 0x32, 0x30, NEN, IKOU)), "[2020,Inf)")
  expect_equal(jp_iv(jp(0x32, 0x30, 0x32, 0x30, NEN, IZEN)), "(-Inf,2020]")
  expect_equal(jp_iv(jp(0x32, 0x39, SAI, MADE)), "(-Inf,29]")

  ## "65歳以上" includes 65; "65歳超" does not. One record's worth of
  ## candidates at the band edge, in the direction that matters.
  expect_true(node_matches("65", jp(0x36, 0x35, SAI, IJOU)))
  expect_false(node_matches("65", jp(0x36, 0x35, SAI, CHOU)))
})

test_that("prose separators read: から, まで, and the 満 prefix", {
  expect_equal(jp_iv(jp(0x32, 0x30, SAI, KARA, 0x32, 0x39, SAI)), "[20,29]")
  expect_equal(jp_iv(jp(0x32, 0x30, SAI, KARA, 0x32, 0x39, SAI, MADE)),
               "[20,29]")
  expect_equal(jp_iv(jp(MITSU, 0x32, 0x30, SAI, IJOU)), "[20,Inf)")
  ## "満" as a prefix must not be confused with the "満" of "未満"
  expect_equal(jp_iv(jp(0x33, 0x30, SAI, MIMAN)), "(-Inf,30)")
})

test_that("the magnitude band 台 uses the place value of its leading digit", {
  expect_equal(jp_iv(jp(0x33, 0x30, 0x30, 0x30, EN, BAND)), "[3000,4000)")
  expect_equal(jp_iv(jp(0x32, 0x30, MAN, EN, BAND)), "[2e+05,3e+05)")
  ## and at the scale of an age it names the same band 代 does
  expect_equal(jp_iv(jp(0x32, 0x30, BAND)), jp_iv(jp(0x32, 0x30, DAI)))
})

test_that("other counters read", {
  expect_equal(jp_iv(jp(0x31, 0x30, 0x30, NIN, IJOU)), "[100,Inf)")
  expect_equal(jp_iv(jp(0x31, 0x30, 0x25, IJOU)), "[10,Inf)")
  expect_equal(jp_iv(jp(0x31, 0x30, PCT, IJOU)), "[10,Inf)")
})

test_that("fullwidth digits and punctuation read as their ASCII equivalents", {
  ## A release mixes the two freely, and a form recognised in only one width
  ## is a form the guard misses half the time.
  expect_equal(jp_iv(jp(0xFF16, 0xFF15, SAI, IJOU)), "[65,Inf)")
  expect_equal(jp_iv(jp(0xFF13, 0xFF10, DAI)), "[30,40)")
  expect_equal(jp_iv(jp(0x33, 0x30, SAI, 0xFF0B)), "[30,Inf)")
  expect_equal(jp_iv(jp(0xFF12, 0xFF10, SAI, 0xFF5E, 0xFF12, 0xFF19, SAI)),
               "[20,29]")
  expect_true(all(is_generalized_value(c(
    jp(0xFF16, 0xFF15, SAI, IJOU), jp(0xFF13, 0xFF10, DAI)
  ))))
})

## ---------------------------------------------------------------------------
## the guard: the whole point of reading these forms
## ---------------------------------------------------------------------------

jp_fixture <- function(people = 60, seed = 42) {
  set.seed(seed)
  raw <- data.frame(
    ROW_NUMBER = seq_len(people),
    AGE = sample(20:69, people, replace = TRUE),
    SEX = sample(c("M", "F"), people, replace = TRUE),
    stringsAsFactors = FALSE
  )
  lo <- floor(raw$AGE / 10) * 10
  anon <- data.frame(
    ROW_NUMBER = raw$ROW_NUMBER,
    ## "20歳以上30歳未満"
    AGE = paste0(lo, jp(SAI), jp(IJOU), lo + 10, jp(SAI), jp(MIMAN)),
    SEX = raw$SEX,
    stringsAsFactors = FALSE
  )
  join_raw_anon_data(raw, anon)
}

test_that("score_char() stops on a column written the Japanese way", {
  d <- jp_fixture()
  expect_error(score_char(d, "AGE"), regexp = "score_char\\(\\)")
  expect_error(score_char(d, "AGE"), regexp = "score_containment")
  expect_error(score_char(d, "AGE"), regexp = "falls inside ANON")
  expect_error(score_char(d, "AGE"),
               regexp = "100% of its published values are regions")
})

test_that("score_num(), score_num_rank() and score_dist() stop as well", {
  d <- jp_fixture()
  for (f in list(score_num, score_num_rank, score_dist)) {
    expect_error(f(d, "AGE"), regexp = "score_containment")
  }
})

test_that("the guard fires for the top-coded band on its own", {
  ## "65歳以上" as the last bin is where a real release top-codes, and it is
  ## the single form the issue names.
  raw <- data.frame(ROW_NUMBER = 1:6, AGE = c(21, 34, 47, 66, 71, 88),
                    stringsAsFactors = FALSE)
  anon <- data.frame(
    ROW_NUMBER = 1:6,
    AGE = c(jp(0x32, 0x30, DAI), jp(0x33, 0x30, DAI), jp(0x34, 0x30, DAI),
            rep(jp(0x36, 0x35, SAI, IJOU), 3)),
    stringsAsFactors = FALSE
  )
  d <- join_raw_anon_data(raw, anon)
  expect_error(score_char(d, "AGE"), regexp = "score_containment")

  ## and containment reads it: 66, 71 and 88 all survive the top-coded band
  cc <- containment_counts(d, "AGE")
  expect_true(all(cc$TRUTH_CONTAINED))
  expect_equal(cc$N_CONTAINED[cc$ANON_ROW_NUMBER == 4], 3)
})

test_that("the escape hatches still work on a Japanese column", {
  d <- jp_fixture()
  expect_warning(s <- score_char(d, "AGE", generalized = "warn"),
                 regexp = "score_containment")
  expect_s3_class(s, "reid_scores")
  expect_silent(score_char(d, "AGE", generalized = "ignore"))
})

test_that("containment on a Japanese release beats the misuse it replaces", {
  d <- jp_fixture(people = 200, seed = 7)
  cont <- match_greedy(score_containment(d, c("AGE", "SEX")), seed = 1L)
  ch <- match_greedy(
    combine_scores(lapply(c("AGE", "SEX"), function(t) {
      score_char(d, t, generalized = "ignore")
    })),
    seed = 1L
  )
  expect_gt(mean(cont$RAW_ROW_NUMBER == cont$ANON_ROW_NUMBER),
            mean(ch$RAW_ROW_NUMBER == ch$ANON_ROW_NUMBER))
})

## ---------------------------------------------------------------------------
## precision: the Issue #40 calibration, which this change must not move
## ---------------------------------------------------------------------------

test_that("an ASCII value is parsed exactly as it was before Issue #92", {
  ## Everything added here is either non-ASCII or needs "+", "-" or "~", so a
  ## value with no non-ASCII character takes the pre-#92 path by construction.
  ## These are the readings the old code produced, pinned unchanged.
  expect_equal(jp_iv("[30,40)"), "[30,40)")
  expect_equal(jp_iv("30s"), "[30,40)")
  expect_equal(jp_iv("30-39"), "[30,39]")
  expect_equal(jp_iv("30+"), "[30,Inf)")
  expect_equal(jp_iv("30+yrs"), "[30,Inf)")
  expect_equal(jp_iv("30-39kg"), "[30,39]")
  expect_equal(jp_iv("30kg"), "[30,30]")
  expect_equal(jp_iv("35"), "[35,35]")
  expect_equal(jp_iv("-39"), "[-39,-39]")
  for (v in c("tokyo", "M", "F", "3S", "30S", "Z001", "abc", "a-b", "*", "")) {
    expect_true(is.na(jp_iv(v)), info = v)
  }
})

test_that("units are still only removed where a digit precedes them", {
  ## Removing "m" from anywhere would turn "0ms" into the decade "0s"; the
  ## digit anchor and the decade being read on the unstripped value both stop
  ## that. Neither may be relaxed without redoing the measurement below.
  expect_false(is_generalized_value("0ms"))
  expect_false(is_generalized_value("0gs"))
  expect_false(is_generalized_value("0ys"))
  expect_false(is_generalized_value("8s"))
  expect_true(is_generalized_value("30s"))
})

test_that("the false-positive rate on random strings has not moved", {
  ## Issue #40 measured 0.0245% on stri_rand_strings(n, 2) and set
  ## GENERALIZATION_SHARE_THRESHOLD from it. Re-measured here, on the same
  ## generator, because every notation added widens the surface.
  ## docs/investigation/japanese-generalization-benchmark.R runs this at
  ## n = 200,000 against a reimplementation of the pre-fix detector and reports
  ## the two rates as identical.
  set.seed(99)
  s <- stringi::stri_rand_strings(4000, length = 2)
  expect_lt(mean(is_generalized_value(s)), 0.01)

  set.seed(101)
  s3 <- stringi::stri_rand_strings(4000, length = 3)
  expect_lt(mean(is_generalized_value(s3)), 0.01)
})

test_that("a Japanese categorical column is not read as a set of regions", {
  ## 千代田区 starts with 千, the character for 1000. Nothing may be read out of
  ## it: the scale expansion only fires after a digit.
  vals <- c(jp(0x5343, 0x4EE3, 0x7530, 0x533A),   # 千代田区
            jp(0x6771, 0x4EAC, 0x90FD),           # 東京都
            jp(0x795E, 0x5948, 0x5DDD, 0x770C),   # 神奈川県
            jp(0x4E0D, 0x660E),                   # 不明
            jp(0x305D, 0x306E, 0x4ED6),           # その他
            jp(0x7537, 0x6027), jp(0x5973, 0x6027))
  expect_false(any(is_generalized_value(vals)))

  d <- join_raw_anon_data(
    data.frame(ROW_NUMBER = 1:7, AREA = vals, stringsAsFactors = FALSE),
    data.frame(ROW_NUMBER = 1:7, AREA = vals, stringsAsFactors = FALSE)
  )
  expect_no_error(score_char(d, "AREA"))
  expect_no_warning(score_char(d, "AREA"))
})

test_that("a Japanese column that is already binned on BOTH sides is compared literally", {
  ## The attacker's knowledge is as coarse as the release, so there is no
  ## region-containing-a-value mismatch and the guard must stay quiet.
  bins <- c(jp(0x32, 0x30, DAI), jp(0x33, 0x30, DAI),
            jp(0x34, 0x30, DAI), jp(0x33, 0x30, DAI))
  d <- join_raw_anon_data(
    data.frame(ROW_NUMBER = 1:4, AGE = bins, stringsAsFactors = FALSE),
    data.frame(ROW_NUMBER = 1:4, AGE = bins, stringsAsFactors = FALSE)
  )
  expect_no_error(score_char(d, "AGE"))
  expect_no_warning(score_char(d, "AGE"))
})

test_that("a date written in Japanese is not read as a range", {
  ## "3月15日" strips to "315", a bare number: a value, not a region. The
  ## failure to avoid is the reverse of the issue's -- flagging a column that
  ## merely carries units.
  expect_false(is_generalized_value(jp(0x33, 0x6708, 0x31, 0x35, NICHI)))
  expect_false(is_generalized_value(jp(0x32, 0x30, 0x32, 0x34, NEN)))
  expect_false(is_generalized_value(jp(0x36, 0x35, SAI)))
})

## ---------------------------------------------------------------------------
## what is still not read, recorded so it is a known gap and not a surprise
## ---------------------------------------------------------------------------

test_that("kanji numerals are not read, and say so by returning NULL", {
  ## "三十歳以上" is 65歳以上 written in kanji digits. It is rare in a data
  ## file and reading it would need a numeral parser; recorded here so that the
  ## silence is a documented gap rather than an assumption.
  expect_null(parse_generalized_interval(jp(0x4E09, 0x5341, SAI, IJOU)))
  expect_false(is_generalized_value(jp(0x4E09, 0x5341, SAI, IJOU)))
})

test_that("a band wrapped in prose is not read", {
  ## "65歳以上の方" ("people aged 65 and over") is a label, not a value. The
  ## patterns are anchored at both ends on purpose: matching a band anywhere
  ## inside a string would read one out of any sentence that mentions an age.
  ## A column written like this is a gap the guard cannot close.
  expect_null(parse_generalized_interval(
    jp(0x36, 0x35, SAI, IJOU, 0x306E, 0x65B9)
  ))
})

test_that("a categorical generalisation is still invisible, as documented", {
  ## Nothing about "東京都" says it contains "千代田区"; that lives in a
  ## declared hierarchy. Repeated here because a reader of this file could
  ## otherwise take "Japanese notation is handled" to mean more than it does.
  expect_false(is_generalized_value(jp(0x6771, 0x4EAC, 0x90FD)))
  d <- join_raw_anon_data(
    data.frame(ROW_NUMBER = 1:2,
               AREA = c(jp(0x5343, 0x4EE3, 0x7530, 0x533A),
                        jp(0x6A2A, 0x6D5C, 0x5E02)),
               stringsAsFactors = FALSE),
    data.frame(ROW_NUMBER = 1:2,
               AREA = c(jp(0x6771, 0x4EAC, 0x90FD),
                        jp(0x795E, 0x5948, 0x5DDD, 0x770C)),
               stringsAsFactors = FALSE)
  )
  expect_no_error(score_char(d, "AREA"))
})
