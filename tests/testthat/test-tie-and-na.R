## Regression tests for phase 3, carried over to the three-layer API when the
## reid_by_*() wrappers were removed in 3.0.0.
##
## defect D: the distribution attack had no tie-handling step at all (unlike
## the num / char / rank ones), so a discrete/low-cardinality distribution
## column produced several result rows per ANON record. The summary counted
## `trial` via nrow(), so this silently inflated the denominator and
## under-reported the reidentification rate -- the worst possible failure mode
## for a safety-checking tool.
##
## defect E: distribution_distance() (and calc_KL()) coerced their input to
## numeric with as.numeric(), which silently turns a non-numeric/character
## column into all-NA distances. The distribution attack then silently
## reported "0 / 0", which reads as "could not be reidentified = safe" when in
## fact the column was simply the wrong type.
##
## Shared fix: resolve_min_distance_ties() -- now reached through
## match_greedy(), the single assignment entry point every score feeds into --
## always collapses ties down to exactly one row per ANON_ROW_NUMBER, and
## errors instead of silently shrinking the result if DISTANCE could not be
## computed. distribution_distance()/calc_KL() error with a specific message
## when a value cannot be parsed as numeric.
##
## The old "reid_result() rejects a duplicated ANON_ROW_NUMBER" defence has a
## successor, not a copy: the duplicate check moved to the *input* side, as
## validate_unique_candidate_pairs(), and is asserted in test-evaluate.R and
## test-assignment.R for match_greedy() / match_optimal() / reid_evaluate() /
## combine_scores(). The output side is covered structurally instead -- every
## assignment here is checked to have exactly one row per ANON record, which
## is the property the reid_result() guard existed to detect after the fact.

make_master_30 <- function(seed = 71) {
  set.seed(seed)
  dat <- create_dummy_transaction_data(people = 30, size = 4)
  dat$NUM_STATIC_2 <- dat$NUM_STATIC + 1
  dat$NUM_DYNAMIC_2 <- dat$NUM_DYNAMIC + 1
  dat$CHAR_STATIC <- paste("CHAR", dat$ID, sep = "")

  transform_transaction_to_master(
    dat,
    ROW_NUMBER = "ROW_NUMBER",
    STATIC_NUM = c("NUM_STATIC", "NUM_STATIC_2"),
    DYNAMIC_NUM = c("BIN", "NUM_DYNAMIC", "NUM_DYNAMIC_2"),
    STATIC_CHAR = "CHAR_STATIC",
    DYNAMIC_CHAR = "CHAR"
  )
}

## ANON is an exact copy of RAW => the true reidentification rate is 100%.
make_identity_join_30 <- function(seed = 71) {
  m <- make_master_30(seed = seed)
  join_raw_anon_data(m, m)
}

n_expected <- function(d) length(unique(d$ANON_ROW_NUMBER))

test_that("score_dist() + match_greedy() on BIN_DIST (many ties) does not inflate the row count / trial denominator", {
  d <- make_identity_join_30()
  n <- n_expected(d)
  expect_equal(n, 30)

  ## Sanity check on the fixture: BIN_DIST really does have heavy collisions
  ## for this data (this is what triggered defect D), so this test is
  ## actually exercising the tie-handling path and not accidentally passing
  ## because every value happens to be unique.
  expect_true(length(unique(d$RAW_BIN_DIST)) < n)

  m <- match_greedy(score_dist(d, "BIN_DIST"), seed = 1)
  ## defect D used to produce more than one row per ANON record here
  ## (108 rows for a 30-person identity join); the tie-handling must always
  ## produce exactly one row per ANON record.
  expect_equal(nrow(m), n)
  expect_equal(length(unique(m$ANON_ROW_NUMBER)), n)

  ## and the reported trial count (the denominator) is the true number of
  ## ANON records, not an inflated tie-count -- defect D used to make this
  ## 108, not 30.
  e <- reid_evaluate(score_dist(d, "BIN_DIST"), seeds = 1:3, top_k = 1)
  expect_equal(unique(e$per_seed$trial), 30)
  expect_equal(e$n_anon, 30)
})

test_that("all 4 score functions give exactly one row per ANON record after assignment, including tie-heavy/constant columns", {
  d <- make_identity_join_30()
  n <- n_expected(d)

  ## NUM_STATIC is literally constant (10) for every record, so every
  ## RAW/ANON pair is tied at SCORE == 0 -- the strongest possible stress
  ## test for missing tie-handling.
  cases <- list(
    num = score_num(d, "NUM_STATIC"),
    char = score_char(d, "CHAR_STATIC"),
    dist = score_dist(d, "BIN_DIST"),
    rank = score_num_rank(d, "NUM_STATIC")
  )

  for (nm in names(cases)) {
    m <- match_greedy(cases[[nm]], seed = 1)
    expect_equal(nrow(m), n, info = nm)
    expect_equal(length(unique(m$ANON_ROW_NUMBER)), n, info = nm)
  }
})

test_that("score_dist() errors on a non-numeric distribution column instead of silently producing an all-NA score", {
  d <- make_identity_join_30()

  ## CHAR_DIST is a distribution of random 2-letter strings ("ab:cd:..."),
  ## not numbers; before the fix, distribution_distance() silently coerced
  ## this to all-NA distances via as.numeric(), and the summary reported
  ## "0 / 0" with no error or warning.
  expect_error(
    score_dist(d, "CHAR_DIST"),
    regexp = "numeric"
  )
})

test_that("distribution_distance() and calc_KL() reject non-numeric distribution strings directly", {
  expect_error(distribution_distance("1:2:3", "a:b:c"), regexp = "numeric")
  expect_error(distribution_distance("a:b:c", "1:2:3"), regexp = "numeric")
  expect_error(calc_KL("1:2:3", "a:b:c"), regexp = "numeric")

  ## still works for genuinely numeric input
  expect_equal(distribution_distance("1:2:3", "1:2:3"), 0)
})

test_that("resolve_min_distance_ties() errors rather than silently shrinking the result when DISTANCE is NA", {
  ## The direct successor of the reid_result() duplicate guard: the failure it
  ## defended against was "the result has the wrong number of rows and the
  ## rate is computed from it anyway". Both directions are refused here.

  ## every DISTANCE missing: nothing was measured at all
  all_na <- data.frame(
    RAW_ROW_NUMBER = c(1, 2, 1, 2),
    ANON_ROW_NUMBER = c(1, 1, 2, 2),
    DISTANCE = NA_real_
  )
  expect_error(resolve_min_distance_ties(all_na), regexp = "could not be computed")

  ## one ANON record with no computable distance: it would silently vanish
  ## from the result, shrinking the denominator
  some_na <- data.frame(
    RAW_ROW_NUMBER = c(1, 2, 1, 2),
    ANON_ROW_NUMBER = c(1, 1, 2, 2),
    DISTANCE = c(1, 2, NA_real_, NA_real_)
  )
  expect_error(resolve_min_distance_ties(some_na), regexp = "dropped")

  ## a well-formed input still collapses to exactly one row per ANON record
  ok <- data.frame(
    RAW_ROW_NUMBER = c(1, 2, 1, 2),
    ANON_ROW_NUMBER = c(1, 1, 2, 2),
    DISTANCE = c(0, 3, 4, 0)
  )
  got <- resolve_min_distance_ties(ok, seed = 1)
  expect_equal(nrow(got), 2)
  expect_equal(got$ANON_ROW_NUMBER, c(1, 2))
  expect_equal(got$RAW_ROW_NUMBER, c(1, 2))
})

test_that("identity check: ANON is an exact copy of RAW => every one of the 30 records is found by all 4 score functions", {
  d <- make_identity_join_30()

  ## NUM_DYNAMIC_DIST/NUM_DYNAMIC_MEAN/CHAR_STATIC are (for this data)
  ## effectively collision-free, unlike the deliberately low-cardinality
  ## BIN_DIST/NUM_STATIC columns used above, so the "true match" is
  ## unambiguous and success should equal trial for all 4.
  cases <- list(
    num = score_num(d, "NUM_DYNAMIC_MEAN"),
    char = score_char(d, "CHAR_STATIC"),
    dist = score_dist(d, "NUM_DYNAMIC_DIST"),
    rank = score_num_rank(d, "NUM_DYNAMIC_MEAN")
  )

  for (nm in names(cases)) {
    m <- match_greedy(cases[[nm]], seed = 1)
    expect_equal(nrow(m), 30, info = nm)
    expect_equal(sum(m$RESULT), 30, info = nm)
  }
})
