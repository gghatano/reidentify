## Phase 5: degenerate/boundary-case regression tests (adversarially requested).
##
## Covers: people = 1, invalid `people` arguments to the dummy-data
## generators, RAW/ANON with different row counts (record suppression),
## explicit NA handling in the reid_by_*() target column (must error, not
## silently mis-answer -- see phase 5 fix for reid_by_num_rank()'s
## rank(..., na.last = TRUE) defect), and a clear error message for a
## nonexistent `target` column (see phase 5 fix adding
## check_raw_anon_columns_exist() to all 4 reid_by_*() functions).

## -----------------------------------------------------------------------
## people = 1 (smallest possible non-degenerate input)
## -----------------------------------------------------------------------

test_that("people = 1: create_dummy_master_data() and the full reid_by_*() pipeline all work", {
  m <- create_dummy_master_data(1)
  expect_equal(nrow(m), 1)

  d <- join_raw_anon_data(m, m)
  expect_equal(nrow(d), 1) # 1x1 cross join

  r_num <- reid_by_num(d, "NUM")
  r_char <- reid_by_char(d, "CHAR")
  r_rank <- reid_by_num_rank(d, "NUM")

  expect_equal(nrow(r_num), 1)
  expect_true(r_num$RESULT)
  expect_true(r_char$RESULT)
  expect_true(r_rank$RESULT)

  expect_match(reid_result(r_num, method = "num"), "1 / 1", fixed = TRUE)
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

test_that("RAW and ANON with different row counts (ANON is a strict subset of RAW) do not crash any of the 4 reid_by_*() functions", {
  set.seed(1)
  raw <- create_dummy_master_data(20)
  anon <- raw[1:12, ] # 12 of the 20 RAW records were "suppressed" out of ANON

  d <- join_raw_anon_data(raw, anon)
  expect_equal(nrow(d), 20 * 12)

  r_num <- expect_no_error(reid_by_num(d, "NUM"))
  r_char <- expect_no_error(reid_by_char(d, "CHAR"))
  r_rank <- expect_no_error(reid_by_num_rank(d, "NUM"))

  ## exactly one row per surviving ANON record for all 3. reid_by_num() and
  ## reid_by_char() compare RAW/ANON values pointwise (absolute difference /
  ## edit distance), so an ANON record that is an exact copy of a RAW record
  ## is still a correct match even when ANON is missing other RAW records.
  expect_equal(nrow(r_num), 12)
  expect_equal(sum(r_num$RESULT), 12)
  expect_equal(nrow(r_char), 12)
  expect_equal(sum(r_char$RESULT), 12)

  ## reid_by_num_rank() is *not* expected to be exact here: RAW_RANK is
  ## computed over all 20 RAW records but ANON_RANK only over the 12
  ## surviving ANON records, so ranks are relative to differently-sized/
  ## composed populations and need not line up even for an identical value.
  ## Only "doesn't crash and returns one row per ANON record" is asserted.
  expect_equal(nrow(r_rank), 12)
  expect_equal(length(unique(r_rank$ANON_ROW_NUMBER)), 12)
})

## -----------------------------------------------------------------------
## NA in the target column: must error or be explicitly handled, never
## silently produce a wrong-but-plausible-looking answer.
## -----------------------------------------------------------------------

test_that("reid_by_num(): NA in ANON's target column errors instead of silently mismatching", {
  set.seed(1)
  raw <- create_dummy_master_data(10)
  anon <- raw
  anon$NUM[3] <- NA
  d <- join_raw_anon_data(raw, anon)

  expect_error(reid_by_num(d, "NUM"))
})

test_that("reid_by_char(): NA in ANON's target column errors instead of silently mismatching", {
  set.seed(1)
  raw <- create_dummy_master_data(10)
  anon <- raw
  anon$CHAR[3] <- NA
  d <- join_raw_anon_data(raw, anon)

  expect_error(reid_by_char(d, "CHAR"))
})

test_that("reid_by_num_rank(): NA in the target column errors instead of silently assigning it a real rank (regression test for the rank(na.last = TRUE) defect)", {
  set.seed(1)
  raw <- create_dummy_master_data(10)
  anon <- raw
  anon$NUM[3] <- NA
  d <- join_raw_anon_data(raw, anon)

  ## before the phase 5 fix, this silently returned a data frame with
  ## ANON_ROW_NUMBER 3 confidently (DISTANCE == 0) matched to the wrong RAW
  ## record, because rank(..., na.last = TRUE) assigns NA a real rank
  ## instead of propagating it.
  expect_error(
    reid_by_num_rank(d, "NUM"),
    regexp = "NA|missing"
  )

  ## same defect, NA on the RAW side instead of ANON
  raw2 <- create_dummy_master_data(10)
  raw2$NUM[5] <- NA
  d2 <- join_raw_anon_data(raw2, raw2)
  expect_error(reid_by_num_rank(d2, "NUM"), regexp = "NA|missing")
})

test_that("reid_by_dist(): an explicit NA in the target distribution column errors instead of silently mismatching", {
  set.seed(1)
  dat <- create_dummy_transaction_data(people = 10, size = 3)
  m <- transform_transaction_to_master(
    dat,
    STATIC_NUM = "NUM_STATIC", DYNAMIC_NUM = "NUM_DYNAMIC", DYNAMIC_CHAR = "CHAR"
  )
  m$NUM_DYNAMIC_DIST[2] <- NA
  d <- join_raw_anon_data(m, m)

  expect_error(reid_by_dist(d, "NUM_DYNAMIC_DIST"), regexp = "missing")
})

## -----------------------------------------------------------------------
## nonexistent `target` column: error message must be clear
## -----------------------------------------------------------------------

test_that("all 4 reid_by_*() functions give a clear error naming the missing column when `target` doesn't exist", {
  set.seed(1)
  raw <- create_dummy_master_data(10)
  d <- join_raw_anon_data(raw, raw)

  ## the error must name the actual (RAW_/ANON_-prefixed) column that could
  ## not be found, not just a generic/base-R indexing failure.
  expect_error(reid_by_num(d, "NOPE"), regexp = "NOPE")
  expect_error(reid_by_char(d, "NOPE"), regexp = "NOPE")
  expect_error(reid_by_dist(d, "NOPE"), regexp = "NOPE")
  expect_error(reid_by_num_rank(d, "NOPE"), regexp = "NOPE")
})
