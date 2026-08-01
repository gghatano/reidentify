## Issue #32: `split` is a literal separator, not a regular expression.
##
## strsplit()'s default treats its `split` argument as a regex. Before the
## fix a separator that happened to be a metacharacter misbehaved, and in
## the worst case did so *silently*: distribution_distance("123", "132",
## split = "|") returned 0 -- two different amounts scored as the same
## distribution -- because "|" is an empty alternation and split the value
## between every character. These tests pin the literal behaviour.

DD <- reidentify:::distribution_distance
PV <- reidentify:::parse_dist_values
KL <- reidentify:::calc_KL

## every regex metacharacter that could plausibly be picked as a separator
META <- c("|", ".", "$", "^", "*", "+", "?", "(", ")", "[", "]", "{", "}", "\\")

test_that("parse_dist_values() splits on metacharacter separators literally", {
  for (sep in META) {
    str <- paste(c("1", "22", "333"), collapse = sep)
    expect_equal(
      PV(str, sep, "x"), c(1, 22, 333),
      info = paste0("split = ", encodeString(sep, quote = '"'))
    )
  }
})

test_that("parse_dist_values() handles multi-character and regex-shaped separators", {
  expect_equal(PV("1||2||3", "||", "x"), c(1, 2, 3))
  expect_equal(PV("1.*2.*3", ".*", "x"), c(1, 2, 3))
  expect_equal(PV("1[0]2[0]3", "[0]", "x"), c(1, 2, 3))
})

test_that("the silent mis-parse of Issue #32 is gone: '123' vs '132' are different", {
  ## the regression that motivated the issue. Pre-fix this was 0.
  expect_gt(DD("123", "132", split = "|"), 0)
  ## and it agrees with the same numbers written with the default separator
  expect_equal(DD("123", "132", split = "|"), DD("123", "132", split = ":"))
})

test_that("distances are identical whichever literal separator is used", {
  a <- c(1, 2, 3, 10)
  b <- c(4, 5, 6, 40)
  ref_dd <- DD(paste(a, collapse = ":"), paste(b, collapse = ":"))
  ref_kl <- KL(paste(a, collapse = ":"), paste(b, collapse = ":"))

  for (sep in META) {
    x <- paste(a, collapse = sep)
    y <- paste(b, collapse = sep)
    lab <- paste0("split = ", encodeString(sep, quote = '"'))
    expect_equal(DD(x, y, split = sep), ref_dd, info = lab)
    expect_equal(KL(x, y, split = sep), ref_kl, info = lab)
  }
})

test_that("the default split = ':' is unchanged", {
  expect_equal(PV("1:2:3", ":", "x"), c(1, 2, 3))
  expect_equal(DD("1:2:3", "4:5:6"), DD("1:2:3", "4:5:6", split = ":"))
  ## a value that contains no separator at all is one number, not its digits
  expect_equal(PV("123", ":", "x"), 123)
})

test_that("a wrong separator still errors loudly rather than mis-parsing", {
  ## ":"-separated data read back with "|" is now a single unparseable token
  expect_error(PV("1:2:3", "|", "x"), regexp = "numeric")
})

test_that("score_dist() accepts metacharacter separators end to end", {
  vals <- c("1:2:3", "4:5:6", "7:8:9", "10:50:90")
  raw <- data.frame(ROW_NUMBER = 1:4, D = vals, stringsAsFactors = FALSE)
  d_ref <- join_raw_anon_data(raw, raw)
  ref_scores <- score_dist(d_ref, "D")
  ref_match <- match_greedy(ref_scores, seed = 1)

  for (sep in META) {
    alt <- raw
    alt$D <- gsub(":", sep, alt$D, fixed = TRUE)
    d <- join_raw_anon_data(alt, alt)
    lab <- paste0("split = ", encodeString(sep, quote = '"'))

    s <- score_dist(d, "D", split = sep)
    expect_equal(s$SCORE, ref_scores$SCORE, info = lab)

    ## and the whole attack, not only the score, is unaffected by which
    ## literal separator the column happens to use
    m <- match_greedy(s, seed = 1)
    expect_equal(m$RAW_ROW_NUMBER, ref_match$RAW_ROW_NUMBER, info = lab)
    expect_equal(m$RESULT, ref_match$RESULT, info = lab)
  }
})

test_that("transform_transaction_to_master(collapse=) round-trips through split=", {
  ## the producing side uses paste(collapse=), which is literal; the reading
  ## side is now literal too, so any separator survives the round trip.
  dat <- data.frame(
    ROW_NUMBER = 1:6,
    ID = rep(c("a", "b"), each = 3),
    AMT = c(10, 20, 30, 11, 21, 31),
    stringsAsFactors = FALSE
  )
  ref <- transform_transaction_to_master(dat, DYNAMIC_NUM = "AMT")
  ref_s <- score_dist(join_raw_anon_data(ref, ref), "AMT_DIST")
  ref_m <- match_greedy(ref_s, seed = 1)

  for (sep in META) {
    m <- transform_transaction_to_master(dat, collapse = sep, DYNAMIC_NUM = "AMT")
    lab <- paste0("collapse/split = ", encodeString(sep, quote = '"'))
    s <- score_dist(join_raw_anon_data(m, m), "AMT_DIST", split = sep)
    expect_equal(s$SCORE, ref_s$SCORE, info = lab)

    r <- match_greedy(s, seed = 1)
    expect_equal(r$RAW_ROW_NUMBER, ref_m$RAW_ROW_NUMBER, info = lab)
    expect_equal(r$RESULT, ref_m$RESULT, info = lab)
  }
})

test_that("score_by_knowledge() passes `split` through literally", {
  mk <- function(sep) {
    raw <- data.frame(
      ROW_NUMBER = 1:4,
      D = c("1:2:3", "4:5:6", "7:8:9", "10:50:90"),
      stringsAsFactors = FALSE
    )
    anon <- data.frame(
      ROW_NUMBER = 1:4,
      D = c("1:2:4", "4:5:7", "7:8:20", "10:50:99"),
      stringsAsFactors = FALSE
    )
    raw$D <- gsub(":", sep, raw$D, fixed = TRUE)
    anon$D <- gsub(":", sep, anon$D, fixed = TRUE)
    join_raw_anon_data(raw, anon)
  }
  k <- attacker_knowledge("S", quasi_identifiers = c(D = "dist"))

  s_ref <- score_by_knowledge(mk(":"), k, split = ":", normalize = "none")
  expect_gt(diff(range(s_ref$SCORE)), 0) # the scores actually vary

  for (sep in META) {
    expect_equal(
      score_by_knowledge(mk(sep), k, split = sep, normalize = "none")$SCORE,
      s_ref$SCORE,
      info = paste0("split = ", encodeString(sep, quote = '"'))
    )
  }
})

test_that("validate_split() rejects separators that cannot work", {
  expect_error(PV("1:2:3", "", "x"), regexp = "empty string")
  expect_error(PV("1:2:3", c(":", ";"), "x"), regexp = "single non-NA character")
  expect_error(PV("1:2:3", NA_character_, "x"), regexp = "single non-NA character")
  expect_error(PV("1:2:3", 1, "x"), regexp = "single non-NA character")
  expect_error(PV("1:2:3", NULL, "x"), regexp = "single non-NA character")
})
