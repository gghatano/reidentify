## Phase 5: degenerate/boundary-case regression tests (adversarially
## requested), carried over to the three-layer API when the reid_by_*()
## wrappers were removed in 3.0.0.
##
## Covers: people = 1, invalid `people` arguments to the dummy-data
## generators, RAW/ANON with different row counts (record suppression),
## explicit NA handling in the target column (must error, not silently
## mis-answer -- see the phase 5 fix for the rank score's
## rank(..., na.last = TRUE) defect), and a clear error message for a
## nonexistent `target` column (see the phase 5 fix adding
## check_raw_anon_columns_exist(), now reached through
## reid_prefixed_columns() from every score_*() function).

## -----------------------------------------------------------------------
## people = 1 (smallest possible non-degenerate input)
## -----------------------------------------------------------------------

test_that("people = 1: create_dummy_master_data() and the full score/assign pipeline all work", {
  m <- create_dummy_master_data(1)
  expect_equal(nrow(m), 1)

  d <- join_raw_anon_data(m, m)
  expect_equal(nrow(d), 1) # 1x1 cross join

  m_num <- match_greedy(score_num(d, "NUM"))
  m_char <- match_greedy(score_char(d, "CHAR"))
  m_rank <- match_greedy(score_num_rank(d, "NUM"))

  expect_equal(nrow(m_num), 1)
  expect_true(m_num$RESULT)
  expect_true(m_char$RESULT)
  expect_true(m_rank$RESULT)
})

test_that("people = 1: create_dummy_transaction_data() works and transforms to a 1-row master", {
  t <- create_dummy_transaction_data(people = 1, size = 3)
  expect_equal(nrow(t), 3)
  expect_equal(length(unique(t$ID)), 1)

  m <- transform_transaction_to_master(
    t,
    STATIC_NUM = "NUM_STATIC", DYNAMIC_NUM = "NUM_DYNAMIC", DYNAMIC_CHAR = "CHAR"
  )
  expect_equal(nrow(m), 1)
})

## -----------------------------------------------------------------------
## invalid `people`/`size` arguments must error
## -----------------------------------------------------------------------

test_that("create_dummy_master_data() errors on invalid `people`", {
  expect_error(create_dummy_master_data(0))
  expect_error(create_dummy_master_data(-1))
  expect_error(create_dummy_master_data("a"))
})

test_that("create_dummy_transaction_data() errors on invalid `people`/`size`", {
  expect_error(create_dummy_transaction_data(people = 0))
  expect_error(create_dummy_transaction_data(people = -1))
  expect_error(create_dummy_transaction_data(people = "a"))
  expect_error(create_dummy_transaction_data(people = 10, size = 0))
  expect_error(create_dummy_transaction_data(people = 10, size = -1))
  expect_error(create_dummy_transaction_data(people = 10, size = "a"))
})

## -----------------------------------------------------------------------
## RAW and ANON with different row counts (simulated record suppression)
## -----------------------------------------------------------------------

test_that("RAW and ANON with different row counts (ANON is a strict subset of RAW) do not crash any of the 3 pointwise scores", {
  set.seed(1)
  raw <- create_dummy_master_data(20)
  anon <- raw[1:12, ] # 8 of the 20 RAW records were "suppressed" out of ANON

  d <- join_raw_anon_data(raw, anon)
  expect_equal(nrow(d), 20 * 12)

  m_num <- expect_no_error(match_greedy(score_num(d, "NUM")))
  m_char <- expect_no_error(match_greedy(score_char(d, "CHAR")))
  m_rank <- expect_no_error(match_greedy(score_num_rank(d, "NUM")))

  ## exactly one row per surviving ANON record for all 3. score_num() and
  ## score_char() compare RAW/ANON values pointwise (absolute difference /
  ## edit distance), so an ANON record that is an exact copy of a RAW record
  ## is still a correct match even when ANON is missing other RAW records.
  expect_equal(nrow(m_num), 12)
  expect_equal(sum(m_num$RESULT), 12)
  expect_equal(nrow(m_char), 12)
  expect_equal(sum(m_char$RESULT), 12)

  ## score_num_rank() is *not* expected to be exact here: the RAW rank is
  ## computed over all 20 RAW records but the ANON rank only over the 12
  ## surviving ANON records, so ranks are relative to differently-sized/
  ## composed populations and need not line up even for an identical value.
  ## Only "doesn't crash and returns one row per ANON record" is asserted.
  expect_equal(nrow(m_rank), 12)
  expect_equal(length(unique(m_rank$ANON_ROW_NUMBER)), 12)
})

## -----------------------------------------------------------------------
## NA in the target column: must error or be explicitly handled, never
## silently produce a wrong-but-plausible-looking answer.
## -----------------------------------------------------------------------

test_that("score_num(): NA in ANON's target column errors instead of silently mismatching", {
  set.seed(1)
  raw <- create_dummy_master_data(10)
  anon <- raw
  anon$NUM[3] <- NA
  d <- join_raw_anon_data(raw, anon)

  ## abs(RAW - ANON) is NA for every candidate of ANON record 3, so that
  ## record has no computable distance at all. It must not be dropped from
  ## the result (which would shrink the denominator and raise the reported
  ## rate); resolve_min_distance_ties() stops instead.
  expect_error(match_greedy(score_num(d, "NUM")))
})

test_that("score_char(): NA in ANON's target column errors instead of silently mismatching", {
  set.seed(1)
  raw <- create_dummy_master_data(10)
  anon <- raw
  anon$CHAR[3] <- NA
  d <- join_raw_anon_data(raw, anon)

  expect_error(match_greedy(score_char(d, "CHAR")))
})

test_that("score_num_rank(): NA in the target column errors instead of silently assigning it a real rank (regression test for the rank(na.last = TRUE) defect)", {
  set.seed(1)
  raw <- create_dummy_master_data(10)
  anon <- raw
  anon$NUM[3] <- NA
  d <- join_raw_anon_data(raw, anon)

  ## before the phase 5 fix, this silently produced a table with
  ## ANON_ROW_NUMBER 3 confidently (SCORE == 0) matched to the wrong RAW
  ## record, because rank(..., na.last = TRUE) assigns NA a real rank
  ## instead of propagating it. The guard is in compute_num_ranks(), so it
  ## fires in the score layer, before any assignment is made.
  expect_error(
    score_num_rank(d, "NUM"),
    regexp = "NA|missing"
  )

  ## same defect, NA on the RAW side instead of ANON
  raw2 <- create_dummy_master_data(10)
  raw2$NUM[5] <- NA
  d2 <- join_raw_anon_data(raw2, raw2)
  expect_error(score_num_rank(d2, "NUM"), regexp = "NA|missing")
})

test_that("score_dist(): an explicit NA in the target distribution column errors instead of silently mismatching", {
  set.seed(1)
  dat <- create_dummy_transaction_data(people = 10, size = 3)
  m <- transform_transaction_to_master(
    dat,
    STATIC_NUM = "NUM_STATIC", DYNAMIC_NUM = "NUM_DYNAMIC", DYNAMIC_CHAR = "CHAR"
  )
  m$NUM_DYNAMIC_DIST[2] <- NA
  d <- join_raw_anon_data(m, m)

  expect_error(score_dist(d, "NUM_DYNAMIC_DIST"), regexp = "missing")
})

## -----------------------------------------------------------------------
## nonexistent `target` column: error message must be clear
## -----------------------------------------------------------------------

test_that("all 4 score functions give a clear error naming the missing column when `target` doesn't exist", {
  set.seed(1)
  raw <- create_dummy_master_data(10)
  d <- join_raw_anon_data(raw, raw)

  ## the error must name the actual (RAW_/ANON_-prefixed) column that could
  ## not be found, not just a generic/base-R indexing failure.
  for (fn in list(score_num, score_char, score_dist, score_num_rank)) {
    expect_error(fn(d, "NOPE"), regexp = "RAW_NOPE")
    expect_error(fn(d, "NOPE"), regexp = "ANON_NOPE")
  }

  ## the same guard covers a nonexistent `row_number`, which is looked up the
  ## same way and is the argument callers get wrong more often
  expect_error(score_num(d, "NUM", row_number = "NOPE"), regexp = "RAW_NOPE")
})
