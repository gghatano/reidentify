## Phase 5: statistical correctness properties (adversarially requested).
##
## Identity (ANON == RAW exact copy => success == trial) is already covered
## by test-column-selection.R / test-tie-and-na.R for all 4 score_*()
## functions, so it is not duplicated here.
##
## This file covers:
##  (1) uninformativeness: replacing the ANON target column with independent
##      random noise should push the number of successful reidentifications
##      down near the "random guess" baseline (~1 correct match out of N by
##      chance), not somewhere systematically higher. This is a statistical
##      property, so it is checked as an average over several seeds with a
##      loose upper bound rather than a single-seed exact value.
##  (2) monotonicity: increasing the amount of perturbation applied to the
##      ANON target column should (loosely) decrease the number of
##      successful reidentifications. Checked as "success at the smallest
##      perturbation > success at the largest perturbation", averaged over a
##      few reps per perturbation level, rather than strict monotonicity at
##      every intermediate step (noise makes strict monotonicity unreliable).
##
## score_dist() is intentionally not exercised here: distribution_distance()
## is known (out of scope for this phase, see docs) to correlate ~0.99 with
## raw record-count differences rather than genuine distribution shape, which
## would make an informativeness/monotonicity test against it either
## meaningless or flaky for reasons unrelated to what's being tested.

test_that("score_num(): independent random noise in ANON$NUM reidentifies close to the chance baseline (mean over 10 seeds, N = 30)", {
  n <- 30
  seeds <- 1:10

  successes <- vapply(seeds, function(s) {
    set.seed(s)
    raw <- create_dummy_master_data(n)
    anon <- raw
    anon$NUM <- runif(n) # independent of raw$NUM: carries no information
    d <- join_raw_anon_data(raw, anon)
    sum(match_greedy(score_num(d, "NUM"))$RESULT)
  }, numeric(1))

  ## chance baseline for matching N independent draws is ~1 correct guess;
  ## allow a loose margin rather than asserting an exact value.
  expect_lt(mean(successes), 5)
})

test_that("score_char(): independent random noise in ANON$CHAR reidentifies close to the chance baseline (mean over 10 seeds, N = 30)", {
  n <- 30
  seeds <- 1:10

  successes <- vapply(seeds, function(s) {
    set.seed(s)
    raw <- create_dummy_master_data(n)
    anon <- raw
    anon$CHAR <- stringi::stri_rand_strings(n, length = 2) # independent
    d <- join_raw_anon_data(raw, anon)
    sum(match_greedy(score_char(d, "CHAR"))$RESULT)
  }, numeric(1))

  expect_lt(mean(successes), 5)
})

test_that("score_num_rank(): independent random noise in ANON$NUM reidentifies close to the chance baseline (mean over 10 seeds, N = 30)", {
  n <- 30
  seeds <- 1:10

  successes <- vapply(seeds, function(s) {
    set.seed(s)
    raw <- create_dummy_master_data(n)
    anon <- raw
    anon$NUM <- runif(n) # independent of raw$NUM
    d <- join_raw_anon_data(raw, anon)
    sum(match_greedy(score_num_rank(d, "NUM"))$RESULT)
  }, numeric(1))

  expect_lt(mean(successes), 5)
})

test_that("score_num(): increasing Gaussian noise added to ANON$NUM decreases success (min-noise mean > max-noise mean, over 8 reps per level)", {
  n <- 40
  sigmas <- c(1e-6, 1e-2, 1, 50)
  reps <- 8

  mean_success_at <- function(sigma, sigma_idx) {
    mean(vapply(seq_len(reps), function(i) {
      set.seed(sigma_idx * 1000L + i)
      raw <- create_dummy_master_data(n)
      anon <- raw
      anon$NUM <- raw$NUM + rnorm(n, sd = sigma)
      d <- join_raw_anon_data(raw, anon)
      sum(match_greedy(score_num(d, "NUM"))$RESULT)
    }, numeric(1)))
  }

  successes <- vapply(
    seq_along(sigmas),
    function(i) mean_success_at(sigmas[i], i),
    numeric(1)
  )

  ## loose monotonicity: the least-perturbed level clearly beats the most
  ## perturbed level; intermediate levels are not required to be strictly
  ## ordered.
  expect_gt(successes[1], successes[length(successes)])
})

test_that("score_num_rank(): increasing Gaussian noise added to ANON$NUM decreases success (min-noise mean > max-noise mean, over 8 reps per level)", {
  n <- 40
  sigmas <- c(1e-6, 1e-2, 1, 50)
  reps <- 8

  mean_success_at <- function(sigma, sigma_idx) {
    mean(vapply(seq_len(reps), function(i) {
      set.seed(sigma_idx * 2000L + i)
      raw <- create_dummy_master_data(n)
      anon <- raw
      anon$NUM <- raw$NUM + rnorm(n, sd = sigma)
      d <- join_raw_anon_data(raw, anon)
      sum(match_greedy(score_num_rank(d, "NUM"))$RESULT)
    }, numeric(1)))
  }

  successes <- vapply(
    seq_along(sigmas),
    function(i) mean_success_at(sigmas[i], i),
    numeric(1)
  )

  expect_gt(successes[1], successes[length(successes)])
})

test_that("score_char(): increasing the fraction of ANON$CHAR values replaced by independent random strings decreases success (min-perturbation mean > max-perturbation mean, over 8 reps per level)", {
  n <- 40
  fractions <- c(0, 0.3, 0.7, 1.0)
  reps <- 8

  mean_success_at <- function(frac, frac_idx) {
    mean(vapply(seq_len(reps), function(i) {
      set.seed(frac_idx * 3000L + i)
      raw <- create_dummy_master_data(n)
      anon <- raw
      k <- round(n * frac)
      if (k > 0) {
        replace_idx <- sample(seq_len(n), k)
        anon$CHAR[replace_idx] <- stringi::stri_rand_strings(k, length = 2)
      }
      d <- join_raw_anon_data(raw, anon)
      sum(match_greedy(score_char(d, "CHAR"))$RESULT)
    }, numeric(1)))
  }

  successes <- vapply(
    seq_along(fractions),
    function(i) mean_success_at(fractions[i], i),
    numeric(1)
  )

  expect_gt(successes[1], successes[length(successes)])
})
