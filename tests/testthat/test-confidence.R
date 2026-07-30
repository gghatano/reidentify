## Tests for margin / eccentricity confidence (#16).
##
## The point of #16 is resolution. The tie-based confidence is a correct
## probability but collapses almost every record onto 1, so reid_evaluate()'s
## precision-recall sweep degenerates to a single point equal to the overall
## success rate -- the metric that exists to show the shape of the risk shows
## none of it. These tests pin (a) the arithmetic, (b) that the resolution
## really improves, (c) that raising the threshold raises precision, and
## (d) that a threshold on the wrong scale cannot quietly return "nothing was
## reidentified".

make_noisy_pair <- function(n, noise, seed) {
  set.seed(seed)
  raw <- data.frame(
    ROW_NUMBER = seq_len(n),
    A = round(stats::runif(n, 0, 100), 2),
    B = round(stats::runif(n, 0, 100), 2)
  )
  anon <- raw
  anon$A <- anon$A + stats::rnorm(n, 0, noise)
  anon$B <- anon$B + stats::rnorm(n, 0, noise)
  list(raw = raw, anon = anon)
}

two_attribute_scores <- function(raw, anon) {
  d <- join_raw_anon_data(raw, anon)
  combine_scores(list(score_num(d, "A"), score_num(d, "B")))
}

## ---------------------------------------------------------------------------
## arithmetic
## ---------------------------------------------------------------------------

test_that("reid_confidence() computes the eccentricity by hand on a known table", {
  ## one ANON record, candidate scores 1, 4, 6, 9
  sc <- new_reid_scores(
    raw_row_number = 1:4,
    anon_row_number = rep(1L, 4),
    score = c(1, 4, 6, 9)
  )
  cf <- reid_confidence(sc, method = "margin")

  expect_equal(nrow(cf), 1)
  expect_equal(cf$BEST_SCORE, 1)
  expect_equal(cf$SECOND_SCORE, 4)
  expect_equal(cf$MARGIN, 3)
  expect_equal(cf$SD_SCORE, stats::sd(c(1, 4, 6, 9)))
  expect_equal(cf$ECCENTRICITY, 3 / stats::sd(c(1, 4, 6, 9)))
  expect_equal(cf$CONFIDENCE, cf$ECCENTRICITY)
  expect_equal(cf$TIE_SIZE, 1)
})

test_that("a tie at the top gives margin 0, not the gap to the next distinct score", {
  ## Scores 2, 2, 20: the attacker is flipping a coin between the two best.
  ## Taking the gap to the third value (18) would report that coin flip as a
  ## very confident win.
  sc <- new_reid_scores(
    raw_row_number = 1:3,
    anon_row_number = rep(1L, 3),
    score = c(2, 2, 20)
  )
  cf <- reid_confidence(sc, method = "margin")

  expect_equal(cf$TIE_SIZE, 2)
  expect_equal(cf$SECOND_SCORE, 2)
  expect_equal(cf$MARGIN, 0)
  expect_equal(cf$ECCENTRICITY, 0)
})

test_that("reid_confidence() is unchanged for method = 'tie' and matches match_greedy()", {
  raw <- data.frame(ROW_NUMBER = 1:6, V = c(1, 1, 2, 2, 3, 3))
  sc <- score_num(join_raw_anon_data(raw, raw), "V")

  cf <- reid_confidence(sc, method = "tie")
  expect_equal(cf$CONFIDENCE, rep(0.5, 6))
  expect_equal(cf$CONFIDENCE, match_greedy(sc)$CONFIDENCE)
})

test_that("all candidates identical means zero margin and zero eccentricity", {
  sc <- new_reid_scores(
    raw_row_number = 1:3,
    anon_row_number = rep(1L, 3),
    score = c(5, 5, 5)
  )
  cf <- reid_confidence(sc, method = "margin")
  expect_equal(cf$MARGIN, 0)
  expect_equal(cf$SD_SCORE, 0)
  expect_equal(cf$ECCENTRICITY, 0)
})

test_that("a single candidate has nothing to be confused with", {
  sc <- new_reid_scores(raw_row_number = 1, anon_row_number = 1, score = 3)
  cf <- reid_confidence(sc, method = "margin")
  expect_equal(cf$N_CANDIDATES, 1)
  expect_equal(cf$ECCENTRICITY, Inf)
  expect_equal(reid_confidence(sc, method = "tie")$CONFIDENCE, 1)
})

test_that("confidence does not depend on whether the score is stored as a distance or a similarity", {
  fx <- make_noisy_pair(40, 3, seed = 2)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  sim <- new_reid_scores(sc$RAW_ROW_NUMBER, sc$ANON_ROW_NUMBER, -sc$SCORE,
                         score_type = "similarity")

  expect_equal(reid_confidence(sc, "margin"), reid_confidence(sim, "margin"))
  expect_equal(reid_confidence(sc, "tie"), reid_confidence(sim, "tie"))
})

test_that("reid_confidence() rejects NA scores rather than ranking around them", {
  sc <- new_reid_scores(1:3, rep(1L, 3), c(1, NA, 3))
  expect_error(reid_confidence(sc), "contains NA")
})

## ---------------------------------------------------------------------------
## resolution and discrimination -- the reason #16 exists
## ---------------------------------------------------------------------------

test_that("margin confidence gives one threshold per record where tie confidence gives one in total", {
  fx <- make_noisy_pair(60, 8, seed = 3)
  sc <- two_attribute_scores(fx$raw, fx$anon)

  e_tie <- reid_evaluate(sc, seeds = 1:3, confidence = "tie")
  e_margin <- reid_evaluate(sc, seeds = 1:3, confidence = "margin")

  ## continuous scores => every best candidate is unique => tie confidence is
  ## 1 everywhere and the sweep degenerates
  expect_equal(nrow(e_tie$precision_recall), 1)
  expect_equal(e_tie$precision_recall$precision, e_tie$success_analytic)

  expect_gt(nrow(e_margin$precision_recall), 40)
})

test_that("eccentricity separates correct guesses from incorrect ones", {
  fx <- make_noisy_pair(150, 1, seed = 4)
  sc <- two_attribute_scores(fx$raw, fx$anon)

  cf <- reid_confidence(sc, method = "margin")
  m <- match_greedy(sc, seed = 1)
  correct <- m$RESULT[match(cf$ANON_ROW_NUMBER, m$ANON_ROW_NUMBER)]

  expect_gt(mean(cf$ECCENTRICITY[correct]), 3 * mean(cf$ECCENTRICITY[!correct]))
})

test_that("raising the threshold raises precision and lowers recall", {
  ## The verification criterion stated on #16.
  ##
  ## Measured over six fixtures rather than one, deliberately. On a single
  ## draw the top of the curve is a ratio over a handful of records and swings
  ## wildly: at n = 200 the same construction gave rank correlations between
  ## -0.07 and -0.98 depending only on the fixture seed, and one fixture in
  ## ten showed no precision gain at all. Asserting on one draw would either
  ## be flaky or would have to be weakened until it tested nothing.
  ratios <- numeric(0)
  cors <- numeric(0)

  for (s in 1:6) {
    fx <- make_noisy_pair(120, 4, seed = s)
    sc <- two_attribute_scores(fx$raw, fx$anon)
    pr <- reid_evaluate(sc, seeds = 1:3, confidence = "margin")$precision_recall

    ## thresholds descend, so coverage and recall must ascend -- exactly, on
    ## every fixture, because that part is arithmetic rather than sampling
    expect_false(is.unsorted(pr$threshold * -1))
    expect_false(is.unsorted(pr$coverage))
    expect_false(is.unsorted(pr$recall))

    ## precision is a ratio of two noisy counts, so it is not monotone row by
    ## row; what has to hold is that it trends up as coverage falls, once the
    ## denominator is big enough to mean anything
    stable <- pr[pr$n_attacked >= 12, ]
    cors <- c(cors, stats::cor(stable$coverage, stable$precision,
                               method = "spearman"))

    band <- pr[pr$coverage <= 0.25 & pr$n_attacked >= 10, ]
    ratios <- c(ratios, mean(band$precision) / pr$precision[nrow(pr)])
  }

  expect_lt(max(cors), -0.7)
  ## attacking the confident quarter is worth substantially more per guess
  ## than attacking everyone
  expect_gt(min(ratios), 1.15)
  expect_gt(mean(ratios), 1.4)
})

test_that("reid_evaluate() reports which confidence measure it used and keeps the analytic quantities unchanged", {
  fx <- make_noisy_pair(50, 4, seed = 6)
  sc <- two_attribute_scores(fx$raw, fx$anon)

  e_tie <- reid_evaluate(sc, seeds = 1:3, confidence = "tie")
  e_margin <- reid_evaluate(sc, seeds = 1:3, confidence = "margin")

  expect_equal(e_tie$confidence, "tie")
  expect_equal(e_margin$confidence, "margin")

  ## the confidence measure changes only how records are *ranked*; the risk
  ## itself must be identical
  expect_equal(e_tie$success_analytic, e_margin$success_analytic)
  expect_equal(e_tie$max_risk, e_margin$max_risk)
  expect_equal(sort(e_tie$per_record$RISK), sort(e_margin$per_record$RISK))
  expect_equal(e_tie$top_k, e_margin$top_k)
})

test_that("per-record output carries the margin columns whichever measure is selected", {
  fx <- make_noisy_pair(30, 4, seed = 7)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  e <- reid_evaluate(sc, seeds = 1:3)

  expect_true(all(c("MARGIN", "ECCENTRICITY") %in% names(e$per_record)))
  expect_false(anyNA(e$per_record$MARGIN))
})

## ---------------------------------------------------------------------------
## threshold filtering in the assignment layer
## ---------------------------------------------------------------------------

test_that("match_greedy() defaults are unchanged by the new arguments", {
  raw <- data.frame(ROW_NUMBER = 1:6, V = c(1, 1, 2, 2, 3, 3))
  sc <- score_num(join_raw_anon_data(raw, raw), "V")

  expect_identical(match_greedy(sc, seed = 2),
                   match_greedy(sc, seed = 2, confidence = "tie", min_confidence = 0))
  expect_equal(match_greedy(sc)$CONFIDENCE, rep(0.5, 6))
})

test_that("min_confidence declines low-confidence records without dropping their rows", {
  fx <- make_noisy_pair(80, 5, seed = 8)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  cf <- reid_confidence(sc, method = "margin")
  thr <- stats::quantile(cf$CONFIDENCE, 0.7, names = FALSE)

  m <- match_greedy(sc, seed = 1, confidence = "margin", min_confidence = thr)

  expect_equal(nrow(m), 80)                 # trial count untouched
  expect_gt(sum(is.na(m$RAW_ROW_NUMBER)), 0)
  expect_true(all(!m$RESULT[is.na(m$RAW_ROW_NUMBER)]))
  expect_true(all(m$CONFIDENCE[!is.na(m$RAW_ROW_NUMBER)] >= thr))
})

test_that("declining raises precision among the records still guessed", {
  fx <- make_noisy_pair(200, 4, seed = 9)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  cf <- reid_confidence(sc, method = "margin")

  prec <- function(m) {
    att <- sum(!is.na(m$RAW_ROW_NUMBER))
    if (att == 0) NA_real_ else sum(m$RESULT) / att
  }

  all_in <- match_greedy(sc, seed = 1, confidence = "margin")
  picky <- match_greedy(sc, seed = 1, confidence = "margin",
                        min_confidence = stats::quantile(cf$CONFIDENCE, 0.8,
                                                         names = FALSE))
  expect_gt(prec(picky), prec(all_in))
})

test_that("a threshold that rejects everything warns instead of reporting a quiet zero", {
  fx <- make_noisy_pair(30, 5, seed = 10)
  sc <- two_attribute_scores(fx$raw, fx$anon)

  ## 1.5 is Narayanan & Shmatikov's constant, and it is on the wrong scale here
  expect_warning(
    m <- match_greedy(sc, seed = 1, confidence = "margin", min_confidence = 1.5),
    "rejected every one of"
  )
  expect_equal(sum(m$RESULT), 0)
  expect_equal(nrow(m), 30)
})

test_that("match_optimal() takes the same confidence arguments", {
  fx <- make_noisy_pair(40, 4, seed = 11)
  sc <- two_attribute_scores(fx$raw, fx$anon)

  m <- match_optimal(sc, seed = 1, confidence = "margin")
  cf <- reid_confidence(sc, method = "margin")
  expect_equal(m$CONFIDENCE, cf$CONFIDENCE[match(m$ANON_ROW_NUMBER, cf$ANON_ROW_NUMBER)])

  thr <- stats::quantile(cf$CONFIDENCE, 0.6, names = FALSE)
  picky <- match_optimal(sc, seed = 1, confidence = "margin", min_confidence = thr)
  expect_equal(nrow(picky), 40)
  expect_gt(sum(is.na(picky$RAW_ROW_NUMBER)), 0)
})

test_that("a record the padding already declined keeps confidence 0 under either measure", {
  ## more ANON than RAW: some records cannot be matched at all
  set.seed(12)
  raw <- data.frame(ROW_NUMBER = 1:5, A = stats::runif(5, 0, 100),
                    B = stats::runif(5, 0, 100))
  anon <- data.frame(ROW_NUMBER = 1:9, A = stats::runif(9, 0, 100),
                     B = stats::runif(9, 0, 100))
  sc <- two_attribute_scores(raw, anon)

  m <- match_optimal(sc, seed = 1, confidence = "margin")
  declined <- is.na(m$RAW_ROW_NUMBER)
  expect_gt(sum(declined), 0)
  expect_true(all(m$CONFIDENCE[declined] == 0))
})

test_that("min_confidence is validated", {
  fx <- make_noisy_pair(10, 3, seed = 13)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  expect_error(match_greedy(sc, min_confidence = "high"), "single number")
  expect_error(match_greedy(sc, min_confidence = c(1, 2)), "single number")
  expect_error(match_greedy(sc, confidence = "eccentric"), "should be one of")
})
