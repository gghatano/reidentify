## Tests for the evaluation metrics added by #12.
##
## Every metric here is checked against a hand calculation on a fixture whose
## correct answer can be written down without running the code, because the
## whole point of #12 is that the numbers this package reports have to be
## trustworthy on their own terms. A metric that is only checked against
## itself would reproduce its own bug.
##
## The fixtures are built so the arithmetic is obvious:
##
##   uniq3_tied3 : V = c(10, 20, 30, 40, 40, 40), ANON an exact copy of RAW.
##                 Records 1-3 have a unique nearest neighbour (risk 1).
##                 Records 4-6 are mutually indistinguishable (risk 1/3).
##                 => exact success rate = (3*1 + 3*(1/3)) / 6 = 4/6
##
##   all_tied    : V = c(1, 1, 2, 2, 3, 3). Every record ties with exactly one
##                 other => risk 1/2 everywhere, exact success rate 1/2.

make_uniq3_tied3 <- function() {
  raw <- data.frame(ROW_NUMBER = 1:6, V = c(10, 20, 30, 40, 40, 40))
  join_raw_anon_data(raw, raw)
}

make_all_tied <- function() {
  raw <- data.frame(ROW_NUMBER = 1:6, V = c(1, 1, 2, 2, 3, 3))
  join_raw_anon_data(raw, raw)
}

make_unique <- function(n = 5) {
  raw <- data.frame(ROW_NUMBER = seq_len(n), V = seq_len(n) * 10)
  join_raw_anon_data(raw, raw)
}

## ---------------------------------------------------------------------------
## success rate: exact value, and the simulation agreeing with it
## ---------------------------------------------------------------------------

test_that("success_analytic matches the hand-computed expectation", {
  e <- reid_evaluate(score_num(make_uniq3_tied3(), "V"), seeds = 1:10)
  expect_equal(e$success_analytic, 4 / 6)

  e2 <- reid_evaluate(score_num(make_all_tied(), "V"), seeds = 1:10)
  expect_equal(e2$success_analytic, 1 / 2)

  e3 <- reid_evaluate(score_num(make_unique(), "V"), seeds = 1:10)
  expect_equal(e3$success_analytic, 1)
})

test_that("the simulated success rate converges on the exact one (cross-check between two independent routes)", {
  e <- reid_evaluate(score_num(make_uniq3_tied3(), "V"), seeds = 1:400)
  expect_equal(e$success_mean, e$success_analytic, tolerance = 0.03)

  e2 <- reid_evaluate(score_num(make_all_tied(), "V"), seeds = 1:400)
  expect_equal(e2$success_mean, e2$success_analytic, tolerance = 0.03)
})

test_that("the point estimate is reported with its spread, not on its own", {
  e <- reid_evaluate(score_num(make_all_tied(), "V"), seeds = 1:40)

  expect_equal(nrow(e$per_seed), 40)
  expect_setequal(names(e$per_seed), c("seed", "success", "trial", "rate"))
  expect_gt(e$success_sd, 0)
  expect_true(e$success_min <= e$success_mean)
  expect_true(e$success_mean <= e$success_max)

  ## a collision-free column has no spread at all
  e2 <- reid_evaluate(score_num(make_unique(), "V"), seeds = 1:10)
  expect_equal(e2$success_sd, 0)
  expect_equal(e2$success_mean, 1)
})

## ---------------------------------------------------------------------------
## baselines
## ---------------------------------------------------------------------------

test_that("the random baseline is 1/n for an n x n cross join", {
  for (n in c(4, 5, 20)) {
    e <- reid_evaluate(score_num(make_unique(n), "V"), seeds = 1:5)
    expect_equal(e$baseline$rate[e$baseline$method == "random"], 1 / n)
  }
})

test_that("the mode baseline names a single RAW record, so it can identify at most one", {
  e <- reid_evaluate(score_num(make_uniq3_tied3(), "V"), seeds = 1:5)
  mode_rate <- e$baseline$rate[e$baseline$method == "mode"]
  expect_true(mode_rate %in% c(0, 1 / 6))
})

test_that("lift is the exact success rate over the random baseline, and a real attack beats 1", {
  e <- reid_evaluate(score_num(make_uniq3_tied3(), "V"), seeds = 1:5)
  expect_equal(e$lift, e$success_analytic / e$baseline$rate[e$baseline$method == "random"])
  expect_equal(e$lift, (4 / 6) / (1 / 6))
  expect_gt(e$lift, 1)
})

test_that("a score that carries no information does not beat the random baseline", {
  ## a constant column: every RAW record is equally close to every ANON record
  raw <- data.frame(ROW_NUMBER = 1:8, V = rep(1, 8))
  d <- join_raw_anon_data(raw, raw)

  e <- reid_evaluate(score_num(d, "V"), seeds = 1:50)
  expect_equal(e$success_analytic, 1 / 8)
  expect_equal(e$baseline$rate[e$baseline$method == "random"], 1 / 8)
  expect_equal(e$lift, 1)
  expect_equal(e$max_risk, 1 / 8)
})

## ---------------------------------------------------------------------------
## top-k
## ---------------------------------------------------------------------------

test_that("top-k hit rate matches the hand calculation", {
  e <- reid_evaluate(score_num(make_uniq3_tied3(), "V"), seeds = 1:5, top_k = c(1, 2, 3, 6))

  ## k=1: 3*1 + 3*(1/3) = 4  -> 4/6
  ## k=2: 3*1 + 3*(2/3) = 5  -> 5/6
  ## k=3: 3*1 + 3*(3/3) = 6  -> 1
  expect_equal(e$top_k$hit_rate[e$top_k$k == 1], 4 / 6)
  expect_equal(e$top_k$hit_rate[e$top_k$k == 2], 5 / 6)
  expect_equal(e$top_k$hit_rate[e$top_k$k == 3], 1)
  expect_equal(e$top_k$hit_rate[e$top_k$k == 6], 1)
})

test_that("top-k is non-decreasing in k and reaches 1 when k covers every candidate", {
  d <- make_all_tied()
  e <- reid_evaluate(score_num(d, "V"), seeds = 1:5, top_k = 1:6)

  expect_false(is.unsorted(e$top_k$hit_rate))
  expect_equal(e$top_k$hit_rate[e$top_k$k == 6], 1)
})

test_that("top-k at k = 1 is exactly the single-guess success rate", {
  for (d in list(make_uniq3_tied3(), make_all_tied(), make_unique())) {
    e <- reid_evaluate(score_num(d, "V"), seeds = 1:5, top_k = 1)
    expect_equal(e$top_k$hit_rate[e$top_k$k == 1], e$success_analytic)
  }
})

test_that("top_k values beyond the candidate count are dropped rather than reported as 1", {
  e <- reid_evaluate(score_num(make_unique(5), "V"), seeds = 1:5, top_k = c(1, 5, 10))
  expect_equal(e$top_k$k, c(1, 5))
})

## ---------------------------------------------------------------------------
## precision-recall
## ---------------------------------------------------------------------------

test_that("the precision-recall sweep separates 'few records, high precision' from the overall mean", {
  ## confidence = "tie" is explicit because the hand calculation below is the
  ## 1/k one. The default became "margin" in #44; the margin version of this
  ## same fixture is the next test.
  e <- reid_evaluate(score_num(make_uniq3_tied3(), "V"), seeds = 1:5,
                     confidence = "tie")
  pr <- e$precision_recall

  ## Two distinct confidence levels: 1 (records 1-3) and 1/3 (records 4-6).
  expect_equal(nrow(pr), 2)

  ## Attacking only the confident half: 3 of 6 records, all correct.
  expect_equal(pr$n_attacked[1], 3)
  expect_equal(pr$coverage[1], 0.5)
  expect_equal(pr$precision[1], 1)
  expect_equal(pr$recall[1], 0.5)

  ## Attacking everything collapses to the plain success rate.
  expect_equal(pr$coverage[2], 1)
  expect_equal(pr$precision[2], e$success_analytic)
  expect_equal(pr$recall[2], e$success_analytic)

  ## This is the whole point of the metric: precision is much better than the
  ## headline rate if the attacker is allowed to pick their targets.
  expect_gt(pr$precision[1], e$success_analytic)
})

test_that("the default sweep ('margin', #44) refines the tie sweep rather than replacing it", {
  ## Same fixture, default confidence. V = c(10, 20, 30, 40, 40, 40):
  ##
  ##   record 1: margin 10, sd 12.649 => eccentricity 0.7906
  ##   record 2: margin 10, sd  8.165 => eccentricity 1.2247
  ##   record 3: margin 10, sd  6.325 => eccentricity 1.5811
  ##   records 4-6: tied three ways at the top => margin 0 => eccentricity 0
  ##
  ## so the sweep has 4 rows where "tie" had 2. This is the change #44 made
  ## the default: the same risk, reported at a resolution that shows the
  ## shape of it.
  tie <- reid_evaluate(score_num(make_uniq3_tied3(), "V"), seeds = 1:5,
                       confidence = "tie")
  e <- reid_evaluate(score_num(make_uniq3_tied3(), "V"), seeds = 1:5)
  pr <- e$precision_recall

  expect_equal(e$confidence, "margin")
  expect_equal(nrow(pr), 4)
  expect_equal(nrow(tie$precision_recall), 2)

  expect_equal(pr$threshold, c(1.5811388, 1.2247449, 0.7905694, 0), tolerance = 1e-6)
  expect_equal(pr$n_attacked, c(1, 2, 3, 6))
  expect_equal(pr$precision, c(1, 1, 1, e$success_analytic))
  expect_equal(pr$recall, c(1, 2, 3, 4) / 6)

  ## The two rows the tie sweep reported are still there, at rows 3 and 4:
  ## "margin" adds thresholds, it does not move the ones "tie" could see.
  expect_equal(pr[c(3, 4), c("n_attacked", "coverage", "precision", "recall")],
               tie$precision_recall[, c("n_attacked", "coverage", "precision", "recall")],
               ignore_attr = TRUE)

  ## and the risk itself is untouched by the choice of measure
  expect_equal(e$success_analytic, tie$success_analytic)
  expect_equal(e$max_risk, tie$max_risk)
  expect_equal(e$top_k, tie$top_k)
})

test_that("lowering the threshold increases coverage and recall monotonically", {
  set.seed(11)
  raw <- data.frame(ROW_NUMBER = 1:12, V = c(1, 1, 2, 2, 2, 3, 4, 5, 6, 7, 8, 8))
  d <- join_raw_anon_data(raw, raw)
  pr <- reid_evaluate(score_num(d, "V"), seeds = 1:5)$precision_recall

  expect_false(is.unsorted(pr$threshold * -1))
  expect_false(is.unsorted(pr$coverage))
  expect_false(is.unsorted(pr$recall))
  expect_true(all(pr$precision >= 0 & pr$precision <= 1))
  expect_true(all(pr$recall >= 0 & pr$recall <= 1))
})

## ---------------------------------------------------------------------------
## per-record risk
## ---------------------------------------------------------------------------

test_that("per-record risk is exact, and the empirical rate computed over seeds agrees with it", {
  d <- make_uniq3_tied3()
  e <- reid_evaluate(score_num(d, "V"), seeds = 1:400)
  pr <- e$per_record[order(e$per_record$ANON_ROW_NUMBER), ]

  expect_equal(pr$RISK, c(1, 1, 1, 1 / 3, 1 / 3, 1 / 3))
  expect_equal(pr$TIE_SIZE, c(1, 1, 1, 3, 3, 3))
  expect_equal(pr$TRUE_RANK, rep(1, 6))
  expect_equal(pr$EMPIRICAL_RATE, pr$RISK, tolerance = 0.1)
})

test_that("risk is spread across the tie group instead of concentrating on its first member (#3)", {
  ## Before the random tie-break, every success landed on whichever record led
  ## its tie group, so per-record risk was 1 for a few records and 0 for the
  ## rest even though they are indistinguishable in the data.
  d <- make_all_tied()
  e <- reid_evaluate(score_num(d, "V"), seeds = 1:200)

  expect_equal(e$per_record$RISK, rep(0.5, 6))
  expect_true(all(e$per_record$EMPIRICAL_RATE > 0))
  expect_equal(e$per_record$EMPIRICAL_RATE, rep(0.5, 6), tolerance = 0.15)
})

test_that("max_risk picks out the single most exposed record", {
  e <- reid_evaluate(score_num(make_uniq3_tied3(), "V"), seeds = 1:5)
  expect_equal(e$max_risk, 1)
  expect_equal(e$max_risk, max(e$per_record$RISK))

  ## per_record is ordered by decreasing risk, so the worst case is row 1
  expect_equal(e$per_record$RISK[1], e$max_risk)
  expect_false(is.unsorted(rev(e$per_record$RISK)))
})

test_that("mean per-record risk equals the overall success rate", {
  for (d in list(make_uniq3_tied3(), make_all_tied(), make_unique())) {
    e <- reid_evaluate(score_num(d, "V"), seeds = 1:5)
    expect_equal(mean(e$per_record$RISK), e$success_analytic)
  }
})

test_that("an ANON record with no counterpart in RAW gets risk 0 and rank NA, and drags the rate down", {
  raw <- data.frame(ROW_NUMBER = 1:4, V = c(10, 20, 30, 40))
  anon <- data.frame(ROW_NUMBER = 1:6, V = c(10, 20, 30, 40, 50, 60))
  d <- join_raw_anon_data(raw, anon)

  e <- reid_evaluate(score_num(d, "V"), seeds = 1:10)
  pr <- e$per_record[order(e$per_record$ANON_ROW_NUMBER), ]

  expect_equal(pr$RISK, c(1, 1, 1, 1, 0, 0))
  expect_true(all(is.na(pr$TRUE_RANK[5:6])))
  expect_equal(e$success_analytic, 4 / 6)
  ## the random baseline must account for them too: 4 of 6 records can be hit
  expect_equal(e$baseline$rate[e$baseline$method == "random"], (4 * (1 / 4)) / 6)
})

## ---------------------------------------------------------------------------
## structure, validation, printing
## ---------------------------------------------------------------------------

test_that("reid_evaluate() reports the size of the problem", {
  d <- make_uniq3_tied3()
  e <- reid_evaluate(score_num(d, "V"), seeds = 1:5)

  expect_s3_class(e, "reid_evaluation")
  expect_equal(e$n_anon, 6)
  expect_equal(e$n_raw, 6)
  expect_equal(e$n_pairs, 36)
})

test_that("reid_evaluate() works on a combined score and on every score_*() function", {
  set.seed(71)
  dat <- create_dummy_transaction_data(people = 12, size = 3)
  dat$CHAR_STATIC <- paste("CHAR", dat$ID, sep = "")
  m <- transform_transaction_to_master(
    dat,
    ROW_NUMBER = "ROW_NUMBER", STATIC_NUM = "NUM_STATIC",
    STATIC_CHAR = "CHAR_STATIC", DYNAMIC_NUM = c("BIN", "NUM_DYNAMIC"),
    DYNAMIC_CHAR = "CHAR"
  )
  d <- join_raw_anon_data(m, m)

  for (s in list(
    score_num(d, "NUM_DYNAMIC_MEAN"),
    score_char(d, "CHAR_STATIC"),
    score_dist(d, "NUM_DYNAMIC_DIST"),
    score_num_rank(d, "NUM_DYNAMIC_MEAN"),
    ## The two columns are on different scales on purpose; this test is about
    ## reid_evaluate(), not about the scale check (#57).
    combine_scores(list(score_num(d, "BIN_MEAN"), score_num(d, "NUM_DYNAMIC_MEAN")),
                   scale_check = "none")
  )) {
    e <- reid_evaluate(s, seeds = 1:5)
    expect_s3_class(e, "reid_evaluation")
    expect_equal(mean(e$per_record$RISK), e$success_analytic)
    expect_true(e$success_analytic >= 0 && e$success_analytic <= 1)
  }
})

test_that("reid_evaluate() validates its arguments", {
  d <- make_unique()
  s <- score_num(d, "V")

  expect_error(reid_evaluate(s, seeds = 1), regexp = "at least 2")
  expect_error(reid_evaluate(s, seeds = c(1, 1, 2)), regexp = "duplicate")
  expect_error(reid_evaluate(data.frame(A = 1)), regexp = "missing score-layer column")

  bad <- s
  bad$SCORE[1] <- NA
  expect_error(reid_evaluate(bad), regexp = "NA")
})

test_that("print.reid_evaluation() shows the baseline next to the rate, and returns invisibly", {
  e <- reid_evaluate(score_num(make_uniq3_tied3(), "V"), seeds = 1:5)

  out <- capture.output(res <- print(e))
  expect_true(any(grepl("success rate", out)))
  expect_true(any(grepl("baseline", out)))
  expect_true(any(grepl("top-k hit rate", out)))
  expect_true(any(grepl("max per-record risk", out)))
  expect_true(any(grepl("precision", out)))
  expect_identical(res, e)
})

test_that("reid_result() is untouched: it still returns the same text", {
  ## #12 asked for a structured object; adding one must not change the
  ## existing character-scalar API that callers and tests already rely on.
  d <- make_unique()
  txt <- reid_result(reid_by_num(d, "V"), method = "num")
  expect_type(txt, "character")
  expect_length(txt, 1)
  expect_match(txt, "5 / 5", fixed = TRUE)
})
