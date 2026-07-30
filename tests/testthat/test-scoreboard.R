## Tests for Scoreboard-RH (#23).
##
## The claim this file has to support is the one that makes the attack a
## threat rather than a curiosity: an attacker holding a *fragment* of
## somebody's history -- a few items out of a dozen, values remembered
## approximately -- reidentifies them anyway, at a rate nowhere near the
## random baseline. Everything else here exists to keep the three ingredients
## honest: sparse support weighting, near matching, and the refusal rule.

## ---------------------------------------------------------------------------
## fixtures
## ---------------------------------------------------------------------------

## Transaction-shaped sparse data: `people` records over `items` columns, each
## record holding `held` items with an integer value 1..5, NA everywhere else.
## `zipf` skews item popularity, which is the shape real transaction data has
## and the condition under which rarity weighting has anything to work with.
make_sparse <- function(people, items, held, seed, zipf = 0) {
  set.seed(seed)
  p <- if (zipf == 0) rep(1, items) else (seq_len(items))^(-zipf)
  m <- matrix(NA_real_, nrow = people, ncol = items)
  for (i in seq_len(people)) {
    picked <- sample.int(items, held, prob = p / sum(p))
    m[i, picked] <- sample.int(5, held, replace = TRUE)
  }
  df <- as.data.frame(m)
  names(df) <- paste0("I", seq_len(items))
  cbind(ROW_NUMBER = seq_len(people), df)
}

## the released table: values nudged by +/- `noise`, `drop` items withheld
perturb <- function(dat, noise, drop, seed) {
  set.seed(seed + 5000)
  out <- dat
  for (i in seq_len(nrow(out))) {
    cols <- 1 + which(!is.na(unlist(out[i, -1])))
    if (noise > 0) {
      out[i, cols] <- unlist(out[i, cols]) +
        sample(c(-noise, 0, noise), length(cols), replace = TRUE)
    }
    if (drop > 0 && length(cols) > drop) out[i, sample(cols, drop)] <- NA
  }
  out
}

## the attacker's fragment: only `known` of each person's items survive
fragment <- function(dat, known, seed) {
  set.seed(seed + 9000)
  out <- dat
  for (i in seq_len(nrow(out))) {
    cols <- 1 + which(!is.na(unlist(out[i, -1])))
    if (length(cols) > known) out[i, setdiff(cols, sample(cols, known))] <- NA
  }
  out
}

item_names <- function(items) paste0("I", seq_len(items))

## success rate of the attack, averaged over `reps` fixtures
attack_rate <- function(people, items, held, known, noise, drop,
                        weight = "inv_log", tolerance = 0, zipf = 0,
                        reps = 4) {
  mean(vapply(seq_len(reps), function(s) {
    full <- make_sparse(people, items, held, s, zipf)
    anon <- perturb(full, noise, drop, s)
    aux <- fragment(full, known, s)
    d <- join_raw_anon_data(aux, anon)
    sc <- score_scoreboard(d, item_names(items), tolerance = tolerance,
                           weight = weight)
    mean(match_scoreboard_rh(sc, seed = s)$RESULT)
  }, numeric(1)))
}

## ---------------------------------------------------------------------------
## 1. the score itself
## ---------------------------------------------------------------------------

test_that("score_scoreboard() adds up the weights of the attributes that agree", {
  ## Two records, two items. I1 is held by both records (support 2), I2 by one
  ## (support 1). ANON is used for the support counts by default.
  anon <- data.frame(ROW_NUMBER = 1:2, I1 = c(4, 4), I2 = c(7, NA))
  aux <- data.frame(ROW_NUMBER = 1:2, I1 = c(4, 1), I2 = c(7, NA))
  d <- join_raw_anon_data(aux, anon)

  s <- score_scoreboard(d, c("I1", "I2"), weight = "inv_log")
  expect_equal(attr(s, "score_type"), "similarity")

  w1 <- idf_weight(2, 2, method = "inv_log")   # support 2 of 2 records
  w2 <- idf_weight(1, 2, method = "inv_log")   # support 1 of 2 records

  got <- function(a, r) s$SCORE[s$ANON_ROW_NUMBER == a & s$RAW_ROW_NUMBER == r]

  ## aux 1 knows I1 = 4 and I2 = 7
  expect_equal(got(1, 1), w1 + w2)   # anon 1 agrees on both
  expect_equal(got(2, 1), w1)        # anon 2 agrees on I1, has no I2
  ## aux 2 knows only I1 = 1, which matches nobody
  expect_equal(got(1, 2), 0)
  expect_equal(got(2, 2), 0)
})

test_that("a rarer attribute is worth more than a common one", {
  ## I1 held by all 3 records, I2 by one
  anon <- data.frame(ROW_NUMBER = 1:3, I1 = c(2, 2, 2), I2 = c(9, NA, NA))
  d <- join_raw_anon_data(anon, anon)
  s <- score_scoreboard(d, c("I1", "I2"), weight = "idf")

  common_only <- s$SCORE[s$ANON_ROW_NUMBER == 2 & s$RAW_ROW_NUMBER == 2]
  both <- s$SCORE[s$ANON_ROW_NUMBER == 1 & s$RAW_ROW_NUMBER == 1]

  ## "idf" gives an attribute every record has weight exactly 0: agreeing on
  ## it rules nothing out
  expect_equal(common_only, 0)
  expect_gt(both, 0)
})

test_that("NA means 'this record has no such attribute' and contributes nothing", {
  anon <- data.frame(ROW_NUMBER = 1:2, I1 = c(NA, NA), I2 = c(3, 8))
  aux <- data.frame(ROW_NUMBER = 1:2, I1 = c(NA, NA), I2 = c(3, 8))
  d <- join_raw_anon_data(aux, anon)

  ## no error, unlike the other score_*() functions
  s <- expect_silent(score_scoreboard(d, c("I1", "I2")))
  expect_false(anyNA(s$SCORE))

  ## an all-NA attribute can never manufacture agreement
  only_na <- score_scoreboard(d, "I1")
  expect_true(all(only_na$SCORE == 0))
})

test_that("attributes outside the attacker's knowledge are not scored, and aux_side selects whose knowledge that is", {
  ## aux knows I1 only; anon holds both
  anon <- data.frame(ROW_NUMBER = 1:2, I1 = c(1, 2), I2 = c(5, 6))
  aux <- data.frame(ROW_NUMBER = 1:2, I1 = c(1, 2), I2 = c(NA, NA))
  d <- join_raw_anon_data(aux, anon)

  from_raw <- score_scoreboard(d, c("I1", "I2"), aux_side = "raw")
  ## same as scoring I1 alone, because I2 is outside the attacker's support
  i1_only <- score_scoreboard(d, "I1")
  expect_equal(from_raw$SCORE, i1_only$SCORE)

  ## with the support taken from the ANON side instead, I2 is in scope -- and
  ## contributes nothing anyway, since the RAW side has no value to agree with
  from_anon <- score_scoreboard(d, c("I1", "I2"), aux_side = "anon")
  expect_equal(from_anon$SCORE, i1_only$SCORE)
})

## ---------------------------------------------------------------------------
## 2. near matching
## ---------------------------------------------------------------------------

test_that("tolerance admits near matches and 'linear' grades them", {
  anon <- data.frame(ROW_NUMBER = 1:3, I1 = c(5, 6, 8))
  aux <- data.frame(ROW_NUMBER = 1:3, I1 = c(5, 5, 5))
  d <- join_raw_anon_data(aux, anon)

  exact <- score_scoreboard(d, "I1", tolerance = 0)
  step <- score_scoreboard(d, "I1", tolerance = 2, partial = "step")
  linear <- score_scoreboard(d, "I1", tolerance = 2, partial = "linear")

  pick <- function(s, a, r) s$SCORE[s$ANON_ROW_NUMBER == a & s$RAW_ROW_NUMBER == r]
  w <- max(step$SCORE)

  ## aux 1 holds 5; anon 2 holds 6, a gap of 1
  expect_equal(pick(exact, 2, 1), 0)
  expect_equal(pick(step, 2, 1), w)          # inside tolerance: full credit
  expect_equal(pick(linear, 2, 1), w * 0.5)  # halfway to the tolerance
  ## anon 3 holds 8, a gap of 3: outside either way
  expect_equal(pick(step, 3, 1), 0)
  expect_equal(pick(linear, 3, 1), 0)
})

test_that("tolerance is ignored for non-numeric columns, where agreement can only be exact", {
  anon <- data.frame(ROW_NUMBER = 1:2, C = c("aa", "ab"), stringsAsFactors = FALSE)
  aux <- data.frame(ROW_NUMBER = 1:2, C = c("aa", "aa"), stringsAsFactors = FALSE)
  d <- join_raw_anon_data(aux, anon)

  expect_equal(
    score_scoreboard(d, "C", tolerance = 0)$SCORE,
    score_scoreboard(d, "C", tolerance = 5)$SCORE
  )
})

test_that("near matching is what rescues the attack when the release perturbs values", {
  ## The released table shifts every value by +/- 1. Exact matching throws
  ## most of that away; a tolerance of 1 does not.
  exact <- attack_rate(60, 40, 10, known = 4, noise = 1, drop = 2, tolerance = 0)
  near <- attack_rate(60, 40, 10, known = 4, noise = 1, drop = 2, tolerance = 1)

  expect_gt(near, 2 * exact)
})

## ---------------------------------------------------------------------------
## 3. the claim: a fragment of the history is enough
## ---------------------------------------------------------------------------

test_that("knowing a few of a person's items reidentifies them far above the random baseline", {
  ## Verification criterion from #23. 60 people => random guessing is 1/60.
  baseline <- 1 / 60
  for (known in c(4, 3, 2)) {
    rate <- attack_rate(60, 40, 10, known = known, noise = 0, drop = 0,
                        tolerance = 0)
    expect_gt(rate, 20 * baseline, label = paste("rate with", known, "items known"))
  }
})

test_that("the attack degrades gracefully as the attacker's fragment shrinks", {
  rates <- vapply(c(4, 2, 1), function(k) {
    attack_rate(60, 40, 10, known = k, noise = 0, drop = 0, tolerance = 0)
  }, numeric(1))

  expect_true(all(diff(rates) <= 0))    # monotone decreasing
  expect_gt(rates[1], 0.9)              # 4 items of 10: near certain
  expect_gt(rates[3], 5 / 60)           # even 1 item beats the baseline
})

test_that("reid_evaluate() agrees that the attack beats both baselines by a wide margin", {
  full <- make_sparse(60, 40, 10, seed = 3)
  anon <- perturb(full, 1, 2, 3)
  aux <- fragment(full, 3, 3)
  d <- join_raw_anon_data(aux, anon)
  sc <- score_scoreboard(d, item_names(40), tolerance = 1)

  e <- reid_evaluate(sc, seeds = 1:3, confidence = "margin")
  expect_gt(e$lift, 20)
  expect_gt(e$success_analytic, max(e$baseline$rate) * 20)
  expect_gt(e$top_k$hit_rate[e$top_k$k == 1], 0.5)
})

## ---------------------------------------------------------------------------
## 4. rarity weighting -- and the condition under which it pays
## ---------------------------------------------------------------------------

test_that("rarity weighting beats unweighted overlap when item popularity is skewed", {
  ## Skew is the precondition, not a detail: if every item is about as
  ## popular as every other, the weights are all about equal and there is
  ## nothing for the weighting to exploit. Measured on the flat fixture the
  ## gain is a few per cent; here it is not.
  none <- attack_rate(60, 40, 10, known = 3, noise = 1, drop = 2,
                      weight = "none", tolerance = 1, zipf = 1.6)
  weighted <- attack_rate(60, 40, 10, known = 3, noise = 1, drop = 2,
                          weight = "inv_log", tolerance = 1, zipf = 1.6)

  expect_gt(weighted, none * 1.1)
})

## ---------------------------------------------------------------------------
## 5. the RH refusal rule
## ---------------------------------------------------------------------------

test_that("match_scoreboard_rh() is match_greedy() with margin confidence", {
  full <- make_sparse(40, 30, 8, seed = 7)
  d <- join_raw_anon_data(fragment(full, 3, 7), perturb(full, 1, 2, 7))
  sc <- score_scoreboard(d, item_names(30), tolerance = 1)

  expect_identical(
    match_scoreboard_rh(sc, phi = 0.5, seed = 2),
    match_greedy(sc, seed = 2, confidence = "margin", min_confidence = 0.5)
  )
})

test_that("raising phi trades recall for precision", {
  full <- make_sparse(80, 40, 10, seed = 11)
  d <- join_raw_anon_data(fragment(full, 3, 11), perturb(full, 1, 3, 11))
  sc <- score_scoreboard(d, item_names(40), tolerance = 1)
  ecc <- reid_confidence(sc, method = "margin")$CONFIDENCE

  summarise <- function(phi) {
    m <- match_scoreboard_rh(sc, phi = phi, seed = 1)
    att <- sum(!is.na(m$RAW_ROW_NUMBER))
    c(precision = if (att > 0) sum(m$RESULT) / att else NA_real_,
      recall = mean(m$RESULT),
      coverage = att / nrow(m))
  }

  wide <- summarise(0)
  picky <- summarise(stats::quantile(ecc, 0.75, names = FALSE))

  expect_gt(picky[["precision"]], wide[["precision"]])
  expect_lt(picky[["recall"]], wide[["recall"]])
  expect_lt(picky[["coverage"]], wide[["coverage"]])
})

test_that("declining keeps the row, so refusing to guess cannot inflate the reported rate", {
  full <- make_sparse(40, 30, 8, seed = 13)
  d <- join_raw_anon_data(fragment(full, 2, 13), perturb(full, 1, 2, 13))
  sc <- score_scoreboard(d, item_names(30), tolerance = 1)
  ecc <- reid_confidence(sc, method = "margin")$CONFIDENCE

  m <- match_scoreboard_rh(sc, phi = stats::quantile(ecc, 0.6, names = FALSE))
  expect_equal(nrow(m), 40)
  expect_gt(sum(is.na(m$RAW_ROW_NUMBER)), 0)
  expect_true(all(!m$RESULT[is.na(m$RAW_ROW_NUMBER)]))
})

test_that("match_scoreboard_rh() can impose the one-to-one constraint as well", {
  full <- make_sparse(40, 30, 8, seed = 17)
  d <- join_raw_anon_data(fragment(full, 3, 17), perturb(full, 1, 2, 17))
  sc <- score_scoreboard(d, item_names(30), tolerance = 1)

  m <- match_scoreboard_rh(sc, assignment = "optimal", seed = 1)
  used <- m$RAW_ROW_NUMBER[!is.na(m$RAW_ROW_NUMBER)]
  expect_false(anyDuplicated(used) > 0)
  expect_equal(nrow(m), 40)
})

## ---------------------------------------------------------------------------
## 6. validation
## ---------------------------------------------------------------------------

test_that("score_scoreboard() rejects malformed input instead of guessing", {
  anon <- data.frame(ROW_NUMBER = 1:2, I1 = c(1, 2), I2 = c(3, 4))
  d <- join_raw_anon_data(anon, anon)

  expect_error(score_scoreboard(d, character(0)), "at least one")
  expect_error(score_scoreboard(d, c("I1", "I1")), "more than once")
  expect_error(score_scoreboard(d, "NOPE"), "NOPE")
  expect_error(score_scoreboard(d, "I1", tolerance = -1), "non-negative")
  expect_error(score_scoreboard(d, "I1", tolerance = NA), "non-missing")
  expect_error(score_scoreboard(d, c("I1", "I2"), tolerance = c(1, 2)),
               "named by target")
  expect_error(score_scoreboard(d, c("I1", "I2"), tolerance = c(I1 = 1)),
               "no entry for target")
})

test_that("a per-target tolerance is matched to its column by name, not by position", {
  anon <- data.frame(ROW_NUMBER = 1:2, I1 = c(5, 6), I2 = c(5, 6))
  aux <- data.frame(ROW_NUMBER = 1:2, I1 = c(5, 5), I2 = c(5, 5))
  d <- join_raw_anon_data(aux, anon)

  ## tolerant on I2 only: aux 1 vs anon 2 should score on I2 but not I1
  s <- score_scoreboard(d, c("I1", "I2"), tolerance = c(I2 = 1, I1 = 0))
  loose <- score_scoreboard(d, "I2", tolerance = 1)
  expect_equal(s$SCORE[s$ANON_ROW_NUMBER == 2 & s$RAW_ROW_NUMBER == 1],
               loose$SCORE[loose$ANON_ROW_NUMBER == 2 & loose$RAW_ROW_NUMBER == 1])
})

test_that("match_scoreboard_rh() validates phi", {
  anon <- data.frame(ROW_NUMBER = 1:2, I1 = c(1, 2))
  sc <- score_scoreboard(join_raw_anon_data(anon, anon), "I1")

  expect_error(match_scoreboard_rh(sc, phi = -1), "non-negative")
  expect_error(match_scoreboard_rh(sc, phi = "high"), "non-negative")
  expect_error(match_scoreboard_rh(sc, assignment = "stable"), "should be one of")
})
