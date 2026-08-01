## Phase 5: output-contract tests (adversarially requested), carried over to
## the three-layer API when the reid_by_*() wrappers were removed in 3.0.0.
##
## These pin down the parts of the API surface that callers rely on but that
## were previously only checked implicitly:
##  - whichever of the four scores is used, the assignment layer always
##    returns a data frame with ANON_ROW_NUMBER / RAW_ROW_NUMBER / RESULT,
##    RESULT is logical, and there is exactly one row per ANON record.
##  - the reported success can never exceed the number of trials, on an
##    identity, a pure-noise and a record-suppressed fixture alike.
##
## The two assertions about reid_result()'s *text* went with the function.
## What they were protecting -- "success <= trial", and a trial count that
## matches the number of ANON records rather than a tie-inflated row count --
## is asserted below against match_greedy() and reid_evaluate(), which are
## where those numbers are produced now.

make_identity_fixture <- function(people = 15, seed = 42) {
  set.seed(seed)
  raw <- create_dummy_master_data(people)
  d <- join_raw_anon_data(raw, raw)
  d
}

test_that("all 4 scores assign to a data frame with ANON_ROW_NUMBER/RAW_ROW_NUMBER/RESULT, RESULT is logical", {
  d <- make_identity_fixture()

  m_num <- match_greedy(score_num(d, "NUM"))
  m_char <- match_greedy(score_char(d, "CHAR"))
  m_rank <- match_greedy(score_num_rank(d, "NUM"))

  set.seed(1)
  dat <- create_dummy_transaction_data(people = 15, size = 3)
  m <- transform_transaction_to_master(
    dat,
    STATIC_NUM = "NUM_STATIC", DYNAMIC_NUM = "NUM_DYNAMIC", DYNAMIC_CHAR = "CHAR"
  )
  d_dist <- join_raw_anon_data(m, m)
  m_dist <- match_greedy(score_dist(d_dist, "NUM_DYNAMIC_DIST"))

  for (r in list(num = m_num, char = m_char, rank = m_rank, dist = m_dist)) {
    expect_true(is.data.frame(r))
    expect_true(all(c("ANON_ROW_NUMBER", "RAW_ROW_NUMBER", "RESULT") %in% names(r)))
    expect_type(r$RESULT, "logical")
    ## exactly one row per ANON record: no duplicated ANON_ROW_NUMBER
    expect_false(anyDuplicated(r$ANON_ROW_NUMBER) > 0)
  }
})

test_that("success <= trial always holds (identity, subset, and independent-noise fixtures)", {
  check_success_le_trial <- function(scores, n_anon_expected) {
    m <- match_greedy(scores, seed = 1)
    success <- sum(m$RESULT)
    trial <- nrow(m)

    expect_true(success <= trial)
    ## the trial count is the number of ANON records, not a tie-inflated row
    ## count -- the failure reid_result()'s duplicate guard used to catch
    expect_equal(trial, n_anon_expected)
    expect_equal(length(unique(m$ANON_ROW_NUMBER)), n_anon_expected)

    ## and the analytic rate reid_evaluate() reports lives in [0, 1] over the
    ## same denominator
    e <- reid_evaluate(scores, seeds = 1:3, top_k = 1)
    expect_true(e$success_analytic >= 0 && e$success_analytic <= 1)
    expect_equal(unique(e$per_seed$trial), n_anon_expected)
  }

  ## identity: success == trial
  d1 <- make_identity_fixture()
  check_success_le_trial(score_num(d1, "NUM"), 15)

  ## independent noise: success should be well below trial, but the
  ## invariant success <= trial must hold regardless
  set.seed(7)
  raw <- create_dummy_master_data(25)
  anon <- raw
  anon$NUM <- runif(25)
  d2 <- join_raw_anon_data(raw, anon)
  check_success_le_trial(score_num(d2, "NUM"), 25)

  ## subset (record-suppressed) ANON
  d3 <- join_raw_anon_data(raw, raw[1:10, ])
  check_success_le_trial(score_num(d3, "NUM"), 10)
})

test_that("a duplicated candidate pair is refused rather than silently taking twice the tie-break share (defense-in-depth regression, see phase 3 and Issue #60)", {
  ## reid_result() used to catch this after the fact, on its *input* data
  ## frame. The check moved upstream, to the score table, where it can also
  ## stop the analytic rate and the random baseline from being wrong in the
  ## same direction (which is what made the old cross-check blind to it).
  bad <- data.frame(
    RAW_ROW_NUMBER = c(1, 2, 2, 3),
    ANON_ROW_NUMBER = c(1, 2, 2, 3),
    SCORE = c(0, 0, 0, 0)
  )

  expect_error(match_greedy(bad), regexp = "duplicated")
  expect_error(reid_evaluate(bad, seeds = 1:2), regexp = "duplicated")

  ## the same table without the repeat is accepted
  good <- data.frame(
    RAW_ROW_NUMBER = c(1, 2, 3),
    ANON_ROW_NUMBER = c(1, 2, 3),
    SCORE = c(0, 0, 0)
  )
  m <- expect_no_error(match_greedy(good))
  expect_equal(nrow(m), 3)
  expect_equal(sum(m$RESULT), 3)
})
