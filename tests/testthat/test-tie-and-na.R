## Regression tests for phase 3:
##
## defect D: reid_by_dist() had no tie-handling step at all (unlike
## reid_by_num / reid_by_char / reid_by_num_rank), so a discrete/low-
## cardinality distribution column produced several result rows per ANON
## record. reid_result() counts `trial` via nrow(), so this silently
## inflated the denominator and under-reported the reidentification rate
## -- the worst possible failure mode for a safety-checking tool.
##
## defect E: distribution_distance() (and the unused calc_KL()) coerced
## their input to numeric with as.numeric(), which silently turns a
## non-numeric/character column into all-NA distances. reid_by_dist() then
## silently reported "0 / 0", which reads as "could not be reidentified =
## safe" when in fact the column was simply the wrong type.
##
## Shared fix: resolve_min_distance_ties() (used by all 4 reid_by_*()
## functions) always collapses ties down to exactly one row per
## ANON_ROW_NUMBER, and errors instead of silently shrinking the result if
## DISTANCE could not be computed. distribution_distance()/calc_KL() now
## error with a specific message when a value cannot be parsed as numeric.
## reid_result() now errors if handed a data frame with duplicated
## ANON_ROW_NUMBER, as a defense in depth against this class of bug
## recurring.

make_master_30 <- function(seed = 71) {
  set.seed(seed)
  dat <- suppressWarnings(create_dummy_transaction_data(people = 30, size = 4))
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

test_that("reid_by_dist on BIN_DIST (many ties) no longer inflates the row count / trial denominator", {
  d <- make_identity_join_30()
  n <- n_expected(d)
  expect_equal(n, 30)

  ## Sanity check on the fixture: BIN_DIST really does have heavy collisions
  ## for this data (this is what triggers defect D), so this test is
  ## actually exercising the tie-handling path and not accidentally passing
  ## because every value happens to be unique.
  expect_true(length(unique(d$RAW_BIN_DIST)) < n)

  r <- reid_by_dist(d, "BIN_DIST")
  ## defect D used to produce more than one row per ANON record here
  ## (108 rows for a 30-person identity join); the fix must always produce
  ## exactly one row per ANON record.
  expect_equal(nrow(r), n)
  expect_equal(length(unique(r$ANON_ROW_NUMBER)), n)

  txt <- reid_result(r, method = "dist/BIN")
  ## trial (the denominator) must be the true number of ANON records, not
  ## an inflated tie-count (defect D used to make this 108, not 30).
  expect_equal(as.numeric(sub(".*/\\s*", "", txt)), 30)
})

test_that("all 4 reid_by_*() functions return exactly one row per ANON record, including tie-heavy/constant columns", {
  d <- make_identity_join_30()
  n <- n_expected(d)

  ## NUM_STATIC is literally constant (10) for every record, so every
  ## RAW/ANON pair is tied at DISTANCE == 0 -- the strongest possible stress
  ## test for missing tie-handling.
  r_num <- reid_by_num(d, "NUM_STATIC")
  expect_equal(nrow(r_num), n)
  expect_equal(length(unique(r_num$ANON_ROW_NUMBER)), n)

  r_char <- reid_by_char(d, "CHAR_STATIC")
  expect_equal(nrow(r_char), n)
  expect_equal(length(unique(r_char$ANON_ROW_NUMBER)), n)

  r_dist <- reid_by_dist(d, "BIN_DIST")
  expect_equal(nrow(r_dist), n)
  expect_equal(length(unique(r_dist$ANON_ROW_NUMBER)), n)

  r_rank <- reid_by_num_rank(d, "NUM_STATIC")
  expect_equal(nrow(r_rank), n)
  expect_equal(length(unique(r_rank$ANON_ROW_NUMBER)), n)
})

test_that("reid_by_dist errors on a non-numeric distribution column instead of silently returning 0 / 0", {
  d <- make_identity_join_30()

  ## CHAR_DIST is a distribution of random 2-letter strings ("ab:cd:..."),
  ## not numbers; before the fix, distribution_distance() silently coerced
  ## this to all-NA distances via as.numeric(), and reid_result() reported
  ## "0 / 0" with no error or warning.
  expect_error(
    reid_by_dist(d, "CHAR_DIST"),
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

test_that("reid_result errors when ANON_ROW_NUMBER has duplicates (the exact shape defect D used to produce)", {
  ## Simulate the pre-fix defect D output shape: no tie-handling at all, so
  ## more than one candidate RAW row survives for a given ANON row.
  bad <- data.frame(
    RAW_ROW_NUMBER = c(1, 2, 2, 3),
    ANON_ROW_NUMBER = c(1, 2, 2, 3),
    RESULT = c(TRUE, TRUE, TRUE, TRUE)
  )

  expect_error(reid_result(bad, method = "broken"), regexp = "duplicate")

  ## a data frame without duplicates still works normally
  good <- data.frame(
    RAW_ROW_NUMBER = c(1, 2, 3),
    ANON_ROW_NUMBER = c(1, 2, 3),
    RESULT = c(TRUE, TRUE, FALSE)
  )
  expect_no_error(reid_result(good, method = "ok"))
  expect_match(reid_result(good, method = "ok"), "2 / 3", fixed = TRUE)
})

test_that("identity check: ANON is an exact copy of RAW => success == trial == 30 for all 4 reid functions", {
  d <- make_identity_join_30()

  ## NUM_DYNAMIC_DIST/NUM_DYNAMIC_MEAN/CHAR_STATIC are (for this data)
  ## effectively collision-free, unlike the deliberately low-cardinality
  ## BIN_DIST/NUM_STATIC columns used above, so the "true match" is
  ## unambiguous and success should equal trial for all 4 functions.
  r_num <- reid_by_num(d, "NUM_DYNAMIC_MEAN")
  r_char <- reid_by_char(d, "CHAR_STATIC")
  r_dist <- reid_by_dist(d, "NUM_DYNAMIC_DIST")
  r_rank <- reid_by_num_rank(d, "NUM_DYNAMIC_MEAN")

  expect_equal(sum(r_num$RESULT), 30)
  expect_equal(sum(r_char$RESULT), 30)
  expect_equal(sum(r_dist$RESULT), 30)
  expect_equal(sum(r_rank$RESULT), 30)

  expect_match(reid_result(r_num, method = "num"), "30 / 30", fixed = TRUE)
  expect_match(reid_result(r_char, method = "char"), "30 / 30", fixed = TRUE)
  expect_match(reid_result(r_dist, method = "dist"), "30 / 30", fixed = TRUE)
  expect_match(reid_result(r_rank, method = "rank"), "30 / 30", fixed = TRUE)
})
