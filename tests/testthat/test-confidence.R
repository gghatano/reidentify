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
  expect_equal(cf$CONFIDENCE, match_greedy(sc, confidence = "tie")$CONFIDENCE)
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

test_that("two candidates are two candidates: the Inf case is only the one-candidate case", {
  ## `Inf` is reserved for "there is no runner-up at all". Widening that test by
  ## one -- `n_candidates <= 2` instead of `< 2`, and the matching bounds on
  ## SECOND_SCORE and SD_SCORE -- makes every record that has been narrowed to
  ## a straight coin flip report the *highest possible* confidence. Nothing
  ## errors: the precision-recall sweep in reid_evaluate() simply puts those
  ## records at the top of the ranking, where they are right half the time.
  sc <- new_reid_scores(raw_row_number = 1:2, anon_row_number = c(1L, 1L),
                        score = c(2, 8))
  cf <- reid_confidence(sc, method = "margin")

  expect_equal(cf$N_CANDIDATES, 2)
  expect_equal(cf$SECOND_SCORE, 8)
  expect_equal(cf$SD_SCORE, stats::sd(c(2, 8)))
  expect_equal(cf$ECCENTRICITY, 6 / stats::sd(c(2, 8)))
  expect_true(is.finite(cf$ECCENTRICITY))

  ## and a two-way tie at two candidates is the coin flip, not a certainty
  tied <- reid_confidence(new_reid_scores(1:2, c(1L, 1L), c(4, 4)),
                          method = "margin")
  expect_equal(tied$ECCENTRICITY, 0)
  expect_equal(reid_confidence(new_reid_scores(1:2, c(1L, 1L), c(4, 4)),
                               method = "tie")$CONFIDENCE, 0.5)
})

test_that("reid_confidence() returns one row per ANON record in ANON_ROW_NUMBER order", {
  ## The documented return contract. match_greedy(), match_optimal() and
  ## reid_per_anon() all re-align by ANON_ROW_NUMBER, so a reordering here does
  ## not break them -- which is exactly why nothing would notice.
  sc <- new_reid_scores(
    raw_row_number = rep(1:3, times = 3),
    anon_row_number = rep(c(30L, 10L, 20L), each = 3),
    score = c(1, 5, 9, 2, 6, 8, 3, 4, 7)
  )
  cf <- reid_confidence(sc)
  expect_equal(cf$ANON_ROW_NUMBER, c(10L, 20L, 30L))
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

test_that("the confidence default is 'margin' (#44), and 'tie' is still one argument away", {
  ## This test used to assert the opposite -- that the default was still
  ## "tie" -- and was inverted deliberately by #44, not because it broke.
  ## The decision: the precision-recall sweep is a primary metric, and "tie"
  ## gives it one threshold, so the resolution has to be on by default.
  raw <- data.frame(ROW_NUMBER = 1:6, V = c(1, 1, 2, 2, 3, 3))
  sc <- score_num(join_raw_anon_data(raw, raw), "V")

  expect_identical(match_greedy(sc, seed = 2),
                   match_greedy(sc, seed = 2, confidence = "margin",
                                min_confidence = 0))
  expect_identical(match_optimal(sc, seed = 2),
                   match_optimal(sc, seed = 2, confidence = "margin"))
  expect_equal(reid_evaluate(sc, seeds = 1:3)$confidence, "margin")
  expect_equal(reid_confidence(sc)$CONFIDENCE, reid_confidence(sc, "margin")$CONFIDENCE)

  ## Everything ties two-for-first here, so the eccentricity is 0 where the
  ## old default reported 0.5. Same coin flip, different summary of it.
  expect_equal(match_greedy(sc)$CONFIDENCE, rep(0, 6))
  expect_equal(match_greedy(sc, confidence = "tie")$CONFIDENCE, rep(0.5, 6))

  ## The assignment itself is untouched by the change: only CONFIDENCE moves.
  expect_identical(match_greedy(sc, seed = 2)$RAW_ROW_NUMBER,
                   match_greedy(sc, seed = 2, confidence = "tie")$RAW_ROW_NUMBER)
  expect_identical(match_greedy(sc, seed = 2)$RESULT,
                   match_greedy(sc, seed = 2, confidence = "tie")$RESULT)
})

test_that("min_confidence declines low-confidence records without dropping their rows", {
  fx <- make_noisy_pair(80, 5, seed = 8)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  cf <- reid_confidence(sc, method = "margin")
  thr <- stats::quantile(cf$CONFIDENCE, 0.7, names = FALSE)

  ## The "rejected every one of" warning below says the result is "an
  ## unconditional zero rather than a measurement". If it also fired on an
  ## ordinary partial decline it would be noise, and a reader who has learned
  ## to ignore it would miss the case it exists for.
  expect_no_warning(
    m <- match_greedy(sc, seed = 1, confidence = "margin", min_confidence = thr)
  )

  expect_equal(nrow(m), 80)                 # trial count untouched
  expect_gt(sum(is.na(m$RAW_ROW_NUMBER)), 0)
  expect_true(all(!m$RESULT[is.na(m$RAW_ROW_NUMBER)]))
  expect_true(all(m$CONFIDENCE[!is.na(m$RAW_ROW_NUMBER)] >= thr))
})

test_that("a record sitting exactly on min_confidence is attacked, not declined", {
  ## `min_confidence` is documented as "records scoring below this decline to
  ## guess". Off by one in the *inclusive* direction (`<=` instead of `<`)
  ## drops the record at the threshold, so the attacker makes one fewer claim
  ## and the measured reidentification rate comes out lower than the attack
  ## really achieves -- with no error, on a threshold the caller chose from the
  ## observed values exactly as the documentation tells them to
  ## (docs/lessons-learned.md section 2).
  fx <- make_noisy_pair(40, 5, seed = 21)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  cf <- reid_confidence(sc, method = "margin")

  ## an observed value, so a record really does sit on the threshold
  thr <- sort(cf$CONFIDENCE)[10]
  on_thr <- cf$ANON_ROW_NUMBER[cf$CONFIDENCE == thr]
  expect_gte(length(on_thr), 1)

  m <- match_greedy(sc, seed = 1, confidence = "margin", min_confidence = thr)
  expect_true(all(!is.na(m$RAW_ROW_NUMBER[m$ANON_ROW_NUMBER %in% on_thr])))
  ## ... and the records just below it really were declined, so the assertion
  ## above is not passing because the threshold did nothing
  below <- cf$ANON_ROW_NUMBER[cf$CONFIDENCE < thr]
  expect_gt(length(below), 0)
  expect_true(all(is.na(m$RAW_ROW_NUMBER[m$ANON_ROW_NUMBER %in% below])))
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

## ---------------------------------------------------------------------------
## Issue #61: tie detection needs a tolerance, or the risk depends on the units
##
## score_num() is |raw - anon|. In exact arithmetic, multiplying every value by
## c > 0 multiplies every score by c and leaves the tie structure -- and so
## every risk figure -- unchanged. In doubles it does not:
##
##   42.3 - 41.2 = 1.0999999999999943
##   43.4 - 42.3 = 1.1000000000000014
##
## Equal as reals, 7.1e-15 apart as doubles. Measured on the 200-record fixture
## below (docs/adversarial/adv2-02-probe.R C3), with the exact == comparison
## that predates #61:
##
##                       integer units   1/10 units
##   success_analytic       0.494167      0.504167
##   max per-record risk    0.5           1.0
##   TIE_SIZE > 1           198           108
##   precision_recall rows  3             93
##
## 43 records with a true risk of 0.5 were reported as 0 and 47 as 1, so the
## error does not even have a safe direction.
## ---------------------------------------------------------------------------

fp_score_table <- function(v_raw, v_anon) {
  score_num(join_raw_anon_data(
    data.frame(ROW_NUMBER = seq_along(v_raw), V = v_raw),
    data.frame(ROW_NUMBER = seq_along(v_anon), V = v_anon)
  ), "V")
}

fp_symmetric_decoys <- function(n = 200, seed = 11) {
  ## Every ANON record sits exactly halfway between its own RAW record and a
  ## decoy, so every true risk is exactly 1/2 and the answer is known without
  ## running anything.
  set.seed(seed)
  base <- sample(seq(1000, 99999), n)
  delta <- sample(c(3, 5, 7, 11), n, TRUE)
  list(raw = c(base, base + 2 * delta), anon = base + delta)
}

test_that("the tie structure survives a change of units (#61)", {
  f <- fp_symmetric_decoys()
  s_int <- fp_score_table(f$raw, f$anon)
  s_dec <- fp_score_table(f$raw / 10, f$anon / 10)

  e_int <- reid_evaluate(s_int, seeds = 1:5, top_k = 1)
  e_dec <- reid_evaluate(s_dec, seeds = 1:5, top_k = 1)

  expect_equal(e_dec$success_analytic, e_int$success_analytic)
  expect_equal(e_dec$max_risk, e_int$max_risk)
  expect_equal(nrow(e_dec$precision_recall), nrow(e_int$precision_recall))

  ord_i <- order(e_int$per_record$ANON_ROW_NUMBER)
  ord_d <- order(e_dec$per_record$ANON_ROW_NUMBER)
  expect_equal(e_dec$per_record$RISK[ord_d], e_int$per_record$RISK[ord_i])
  expect_equal(e_dec$per_record$TIE_SIZE[ord_d], e_int$per_record$TIE_SIZE[ord_i])

  c_int <- reid_confidence(s_int)
  c_dec <- reid_confidence(s_dec)
  expect_equal(c_dec$TIE_SIZE, c_int$TIE_SIZE)
  expect_equal(c_dec$ECCENTRICITY, c_int$ECCENTRICITY)
})

test_that("tolerance = 0 reproduces the pre-#61 numbers exactly", {
  ## The point of keeping the old path reachable: this test is also the record
  ## of what the defect looked like.
  f <- fp_symmetric_decoys()
  s_int <- fp_score_table(f$raw, f$anon)
  s_dec <- fp_score_table(f$raw / 10, f$anon / 10)

  old_int <- reid_evaluate(s_int, seeds = 1:5, top_k = 1, tolerance = 0)
  old_dec <- reid_evaluate(s_dec, seeds = 1:5, top_k = 1, tolerance = 0)

  expect_equal(round(old_int$success_analytic, 6), 0.494167)
  expect_equal(round(old_dec$success_analytic, 6), 0.504167)
  expect_equal(old_int$max_risk, 0.5)
  expect_equal(old_dec$max_risk, 1)
  expect_equal(nrow(old_int$precision_recall), 3)
  expect_equal(nrow(old_dec$precision_recall), 93)

  ## The new default agrees with the integer-unit answer, which is the one the
  ## arithmetic says is right.
  new_dec <- reid_evaluate(s_dec, seeds = 1:5, top_k = 1)
  expect_equal(round(new_dec$success_analytic, 6), 0.494167)
  expect_equal(new_dec$max_risk, 0.5)
  expect_equal(nrow(new_dec$precision_recall), 3)
})

test_that("the exact-tie case is the one the tolerance has to reproduce (#61)", {
  ## 3 records; ANON 2 sits exactly between RAW 2 and RAW 3. Written as
  ## integers there is no representation error at all, so this fixture pins
  ## the answer the 1/10 version has to match.
  s <- fp_score_table(c(100, 412, 434), c(100, 423, 434))
  cf <- reid_confidence(s)
  expect_equal(cf$TIE_SIZE, c(1, 2, 1))
  expect_equal(cf$MARGIN[2], 0)

  cf_dec <- reid_confidence(fp_score_table(c(10.0, 41.2, 43.4),
                                           c(10.0, 42.3, 43.4)))
  expect_equal(cf_dec$TIE_SIZE, c(1, 2, 1))
  expect_equal(cf_dec$MARGIN[2], 0)

  ## Without the tolerance, the 1/10 version claims a unique winner on a
  ## margin of 7.1e-15.
  cf_old <- reid_confidence(fp_score_table(c(10.0, 41.2, 43.4),
                                           c(10.0, 42.3, 43.4)), tolerance = 0)
  expect_equal(cf_old$TIE_SIZE, c(1, 1, 1))
  expect_gt(cf_old$MARGIN[2], 0)
  expect_lt(cf_old$MARGIN[2], 1e-12)
})

test_that("the tolerance does not fuse genuinely distinct candidates (#61)", {
  ## 1e-7 relative apart is ten times the default tolerance: these stay apart.
  expect_equal(length(unique(snap_tied_values(c(1, 1 + 1e-7, 1 + 2e-7, 5)))), 4L)
  ## 1e-12 apart is representation noise: these fuse.
  expect_equal(length(unique(snap_tied_values(c(1, 1 + 1e-12, 5)))), 2L)
  ## min() is preserved, which is what keeps BEST_SCORE and the identity of the
  ## winning candidate unchanged.
  v <- c(3, 3 + 1e-13, 1, 1 + 1e-13)
  expect_equal(min(snap_tied_values(v)), min(v))
  ## Inf gets its own group rather than being fused with a finite score.
  expect_equal(snap_tied_values(c(1, Inf, Inf, 2)), c(1, Inf, Inf, 2))
  ## A table with no near-ties comes back unchanged.
  expect_equal(snap_tied_values(c(0, 10, 20, 30)), c(0, 10, 20, 30))
  ## tolerance = 0 is exactly the old comparison.
  expect_equal(snap_tied_values(c(1, 1 + 1e-15), tolerance = 0), c(1, 1 + 1e-15))
})

test_that("the tolerance leaves ordinary fixtures alone -- no false positives (#61)", {
  fixtures <- list(
    score_num(join_raw_anon_data(
      data.frame(ROW_NUMBER = 1:6, V = c(10, 20, 30, 40, 40, 40)),
      data.frame(ROW_NUMBER = 1:6, V = c(10, 20, 30, 40, 40, 40))), "V"),
    score_num(join_raw_anon_data(
      data.frame(ROW_NUMBER = 1:6, V = c(1, 1, 2, 2, 3, 3)),
      data.frame(ROW_NUMBER = 1:6, V = c(1, 1, 2, 2, 3, 3))), "V")
  )
  set.seed(3)
  rawv <- runif(60, 0, 1000)
  fixtures[[3]] <- fp_score_table(rawv, rawv + rnorm(60, 0, 0.5))

  for (s in fixtures) {
    a <- reid_evaluate(s, seeds = 1:5, top_k = 1, tolerance = 0)
    b <- reid_evaluate(s, seeds = 1:5, top_k = 1)
    expect_equal(b$success_analytic, a$success_analytic)
    expect_equal(b$max_risk, a$max_risk)
    expect_equal(nrow(b$precision_recall), nrow(a$precision_recall))
    expect_equal(reid_confidence(s)$TIE_SIZE,
                 reid_confidence(s, tolerance = 0)$TIE_SIZE)
  }
})

test_that("match_greedy and match_optimal use the same notion of 'tied' (#61)", {
  f <- fp_symmetric_decoys(n = 40, seed = 5)
  s <- fp_score_table(f$raw / 10, f$anon / 10)

  g <- match_greedy(s, seed = 1, confidence = "tie")
  o <- match_optimal(s, seed = 1, confidence = "tie")
  expect_lt(sum(g$CONFIDENCE == 1), nrow(g))
  expect_lt(sum(o$CONFIDENCE == 1), nrow(o))

  ## With tolerance = 0 both report far more unique winners: the same defect,
  ## in both functions.
  g0 <- match_greedy(s, seed = 1, confidence = "tie", tolerance = 0)
  o0 <- match_optimal(s, seed = 1, confidence = "tie", tolerance = 0)
  expect_gt(sum(g0$CONFIDENCE == 1), sum(g$CONFIDENCE == 1))
  expect_gt(sum(o0$CONFIDENCE == 1), sum(o$CONFIDENCE == 1))
})

test_that("tolerance is validated (#61)", {
  s <- fp_score_table(c(1, 2, 3), c(1, 2, 3))
  for (bad in list(-1, NA_real_, c(1, 2), "x", Inf)) {
    expect_error(reid_evaluate(s, seeds = 1:3, tolerance = bad), regexp = "tolerance")
    expect_error(reid_confidence(s, tolerance = bad), regexp = "tolerance")
    expect_error(match_greedy(s, tolerance = bad), regexp = "tolerance")
    expect_error(match_optimal(s, tolerance = bad), regexp = "tolerance")
  }
})
