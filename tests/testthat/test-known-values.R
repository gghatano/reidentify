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

test_that("score_num computes |RAW - ANON| and match_greedy picks the nearest RAW record", {
  raw <- data.frame(ROW_NUMBER = 1:3, V = c(10, 20, 30))
  anon <- data.frame(ROW_NUMBER = 1:3, V = c(11, 21, 31))
  d <- join_raw_anon_data(raw, anon)

  s <- score_num(d, "V")
  r <- match_greedy(s)

  ## nearest RAW to each ANON value is the diagonal, at distance 1
  expect_equal(r$RAW_ROW_NUMBER, c(1, 2, 3))
  expect_true(all(r$RESULT))

  ## the score of each chosen pair really is 1
  chosen <- match(
    paste(r$ANON_ROW_NUMBER, r$RAW_ROW_NUMBER),
    paste(s$ANON_ROW_NUMBER, s$RAW_ROW_NUMBER)
  )
  expect_equal(s$SCORE[chosen], c(1, 1, 1))

  ## success == trial == 3
  expect_equal(sum(r$RESULT), 3)
  expect_equal(nrow(r), 3)
})

test_that("score_char uses Levenshtein (adist) distance", {
  ## adist("aa","ab") == 1, adist("aa","ba") == 1, adist("aa","aa") == 0
  raw <- data.frame(ROW_NUMBER = 1:2, T = c("aa", "zz"), stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:2, T = c("aa", "zz"), stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  s <- score_char(d, "T")
  r <- match_greedy(s)

  chosen <- match(
    paste(r$ANON_ROW_NUMBER, r$RAW_ROW_NUMBER),
    paste(s$ANON_ROW_NUMBER, s$RAW_ROW_NUMBER)
  )
  expect_equal(s$SCORE[chosen], c(0, 0))
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
  ## NB: these used to be named MAX/MEAN/MEDIAN/MIN *without* the source
  ## column prefix, because summarise_all() only prefixed when 2+ columns
  ## remained after grouping and DYNAMIC_NUM is a single column here. Issue
  ## #26 made the naming independent of the column count, so they are now
  ## NUM_DYNAMIC_MAX etc. -- the same names a 2+ column call produces. This
  ## test was written to pin the old behaviour so that the fix showed up as a
  ## visible change; this is that change.
  expect_equal(m$NUM_DYNAMIC_MAX, c(3, 5))
  expect_equal(m$NUM_DYNAMIC_MEAN, c(2, 5))
  expect_equal(m$NUM_DYNAMIC_MEDIAN, c(2, 5))
  expect_equal(m$NUM_DYNAMIC_MIN, c(1, 5))

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

test_that("the success rate counts successes over ANON records, hand-computed", {
  ## The successor of the reid_result() text test: what it pinned down was
  ## "success is the number of TRUE results and trial is the number of ANON
  ## records". reid_evaluate() reports those two numbers per seed.
  ##
  ## Fixture: 4 records, ANON == RAW on V for 3 of them and a value nobody
  ## else is near for the fourth -- which lands on the nearest RAW record and
  ## gets it wrong. So success 3, trial 4, rate 0.75, with no ties anywhere.
  raw <- data.frame(ROW_NUMBER = 1:4, V = c(10, 20, 30, 40))
  anon <- data.frame(ROW_NUMBER = 1:4, V = c(10, 20, 30, 21))
  d <- join_raw_anon_data(raw, anon)

  m <- match_greedy(score_num(d, "V"))
  expect_equal(sum(m$RESULT), 3)
  expect_equal(nrow(m), 4)

  e <- reid_evaluate(score_num(d, "V"), seeds = 1:3, top_k = 1)
  expect_equal(unique(e$per_seed$success), 3)
  expect_equal(unique(e$per_seed$trial), 4)
  expect_equal(e$success_analytic, 0.75)
})
