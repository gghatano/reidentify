## Expected-value tests (Issue #7).
##
## The other test files assert structural invariants (one row per ANON
## record, no decoy-column leakage, self-containedness). This file pins down
## *numeric* results that can be computed by hand, so that a future change to
## a distance definition has to be a deliberate, visible edit rather than a
## silent drift.
##
## NB: the distribution_distance() / calc_KL() expectations below encode the
## CURRENT definitions. Issues #4 (calc_KL sum-normalisation) and #5
## (quantile-vector based distance) intentionally change them; those issues
## are expected to update the corresponding expectations here.

test_that("join_raw_anon_data cross-joins and prefixes column names", {
  raw <- data.frame(ROW_NUMBER = 1:3, V = c(1, 2, 3))
  anon <- data.frame(ROW_NUMBER = 1:2, V = c(9, 8))

  d <- join_raw_anon_data(raw, anon)

  expect_equal(nrow(d), 3 * 2)
  expect_setequal(names(d), c("RAW_ROW_NUMBER", "RAW_V", "ANON_ROW_NUMBER", "ANON_V"))
  ## every RAW row must be paired with every ANON row exactly once
  expect_equal(nrow(unique(d[, c("RAW_ROW_NUMBER", "ANON_ROW_NUMBER")])), 6)
})

test_that("reid_by_num computes |RAW - ANON| and picks the nearest RAW record", {
  raw <- data.frame(ROW_NUMBER = 1:3, V = c(10, 20, 30))
  anon <- data.frame(ROW_NUMBER = 1:3, V = c(11, 21, 31))
  d <- join_raw_anon_data(raw, anon)

  r <- reid_by_num(d, "V")
  r <- r[order(r$ANON_ROW_NUMBER), ]

  ## nearest RAW to each ANON value is the diagonal, at distance 1
  expect_equal(r$RAW_ROW_NUMBER, c(1, 2, 3))
  expect_equal(r$DISTANCE, c(1, 1, 1))
  expect_true(all(r$RESULT))
  expect_equal(reid_result(r, method = "num"), " method: num , success / trial :  3 / 3")
})

test_that("reid_by_char uses Levenshtein (adist) distance", {
  ## adist("aa","ab") == 1, adist("aa","ba") == 1, adist("aa","aa") == 0
  raw <- data.frame(ROW_NUMBER = 1:2, T = c("aa", "zz"), stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:2, T = c("aa", "zz"), stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  r <- reid_by_char(d, "T")
  r <- r[order(r$ANON_ROW_NUMBER), ]

  expect_equal(r$DISTANCE, c(0, 0))
  expect_true(all(r$RESULT))

  ## one-character edit => distance 1
  expect_equal(as.integer(adist("aa", "ab")[[1]]), 1L)
})

test_that("distribution_distance returns the squared L2 distance between quantile vectors", {
  ## Updated by Issue #5: both sides are reduced to n_quantiles evenly spaced
  ## quantiles first. "1:2" vs "3:4" is a constant shift of 2, so every one of
  ## the 10 quantiles differs by 2 => 10 * 2^2 = 40.
  ## (Before #5 this was an element-wise subtraction giving 4 + 4 = 8.)
  expect_equal(distribution_distance("1:2", "3:4"), 40)
  ## identical inputs => 0
  expect_equal(distribution_distance("1:2:3", "1:2:3"), 0)
  ## symmetric
  expect_equal(
    distribution_distance("1:2", "3:4"),
    distribution_distance("3:4", "1:2")
  )
})

test_that("distribution_distance honours a custom split character", {
  expect_equal(distribution_distance("1,2", "3,4", split = ","), 40)
})

test_that("calc_KL returns 0 for identical distributions", {
  expect_equal(as.numeric(suppressMessages(calc_KL("1:2:3:4", "1:2:3:4"))), 0)
})

test_that("transform_transaction_to_master computes MAX/MEAN/MEDIAN/MIN, ROWCOUNT and DIST correctly", {
  dat <- data.frame(
    ROW_NUMBER = 1:3,
    ID = c(1, 1, 2),
    NUM_STATIC = c(10, 10, 20),
    NUM_DYNAMIC = c(1, 3, 5),
    CHAR = c("b", "a", "c"),
    stringsAsFactors = FALSE
  )

  m <- transform_transaction_to_master(
    dat,
    ROW_NUMBER = "ROW_NUMBER", ID = "ID",
    STATIC_NUM = "NUM_STATIC",
    DYNAMIC_NUM = "NUM_DYNAMIC",
    DYNAMIC_CHAR = "CHAR"
  )
  m <- m[order(m$ID), ]

  expect_equal(nrow(m), 2)

  ## ID 1 has NUM_DYNAMIC = {1, 3}; ID 2 has {5}
  ##
  ## NB: the statistic columns are named MAX/MEAN/MEDIAN/MIN *without* the
  ## source column prefix here, because summarise_all() only prefixes when
  ## 2+ columns remain after grouping and DYNAMIC_NUM is a single column.
  ## With 2+ DYNAMIC_NUM columns they become NUM_DYNAMIC_MAX etc. instead.
  ## That inconsistency is tracked as Issue #26; this test pins the current
  ## behaviour so the fix there is a visible change.
  expect_equal(m$MAX, c(3, 5))
  expect_equal(m$MEAN, c(2, 5))
  expect_equal(m$MEDIAN, c(2, 5))
  expect_equal(m$MIN, c(1, 5))

  ## ROWCOUNT is the number of transaction rows per ID
  expect_equal(m$ROWCOUNT, c(2, 1))

  ## ROW_NUMBER is the minimum row number per ID
  expect_equal(m$ROW_NUMBER, c(1, 3))

  ## DIST columns are sorted and colon-joined
  expect_equal(m$NUM_DYNAMIC_DIST, c("1:3", "5"))
  expect_equal(m$CHAR_DIST, c("a:b", "c"))

  ## STATIC_NUM is carried through unchanged
  expect_equal(m$NUM_STATIC, c(10, 20))
})

test_that("transform_transaction_to_master honours a custom collapse character", {
  dat <- data.frame(
    ROW_NUMBER = 1:2, ID = c(1, 1),
    NUM_DYNAMIC = c(1, 2), CHAR = c("a", "b"),
    stringsAsFactors = FALSE
  )

  m <- transform_transaction_to_master(
    dat,
    ROW_NUMBER = "ROW_NUMBER", ID = "ID", collapse = "-",
    DYNAMIC_NUM = "NUM_DYNAMIC", DYNAMIC_CHAR = "CHAR"
  )

  expect_equal(m$NUM_DYNAMIC_DIST, "1-2")
  expect_equal(m$CHAR_DIST, "a-b")
})

test_that("create_dummy_* are reproducible under a fixed seed and validate their arguments", {
  set.seed(42)
  a <- suppressWarnings(create_dummy_master_data(people = 7))
  set.seed(42)
  b <- suppressWarnings(create_dummy_master_data(people = 7))

  expect_equal(a, b)
  expect_equal(nrow(a), 7)
  expect_setequal(names(a), c("ROW_NUMBER", "ID", "NUM", "BIN", "CHAR"))
  ## documented invariant of the dummy generator
  expect_equal(a$ID, a$ROW_NUMBER + 10000)

  set.seed(1)
  t1 <- suppressWarnings(create_dummy_transaction_data(people = 5, size = 3))
  expect_equal(nrow(t1), 5 * 3)
  expect_setequal(
    names(t1),
    c("ROW_NUMBER", "ID", "NUM_STATIC", "NUM_DYNAMIC", "BIN", "CHAR")
  )

  expect_error(create_dummy_master_data(people = 0), regexp = "people")
  expect_error(create_dummy_master_data(people = "x"), regexp = "people")
  expect_error(create_dummy_transaction_data(people = 5, size = 0), regexp = "size")
  expect_error(join_raw_anon_data(1, 2), regexp = "data frame")
})

test_that("reid_result formats the success / trial text and counts successes", {
  d <- data.frame(
    RAW_ROW_NUMBER = c(1, 2, 3, 4),
    ANON_ROW_NUMBER = c(1, 2, 3, 4),
    RESULT = c(TRUE, FALSE, TRUE, TRUE)
  )

  expect_equal(reid_result(d, method = "m"), " method: m , success / trial :  3 / 4")
})
