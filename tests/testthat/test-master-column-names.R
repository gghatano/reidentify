## Regression tests for #26: transform_transaction_to_master()'s aggregate
## column names must not depend on how many columns were passed.
##
## dplyr::summarise_all() with a named function list only prefixes the result
## with the source column name when 2+ columns survive grouping. With a single
## column it used the bare function names, so
##
##   DYNAMIC_NUM = "NUM_DYNAMIC"                  -> MAX, MEAN, MEDIAN, MIN
##   DYNAMIC_NUM = c("BIN", "NUM_DYNAMIC")        -> BIN_MAX, NUM_DYNAMIC_MAX, ...
##
## and downstream code could not hard-code either. R/tmp/dev.R assumed
## NUM_DYNAMIC_MEAN and so was broken for the single-column call (#27).

make_tran <- function(people = 8, size = 3, seed = 71) {
  set.seed(seed)
  dat <- create_dummy_transaction_data(people = people, size = size)
  dat$NUM_DYNAMIC_2 <- dat$NUM_DYNAMIC + 1
  dat$CHAR_2 <- paste0(dat$CHAR, "x")
  dat
}

stat_names <- c("MAX", "MEAN", "MEDIAN", "MIN")

test_that("DYNAMIC_NUM produces <col>_<statistic> for one column and for several alike", {
  dat <- make_tran()

  m1 <- transform_transaction_to_master(dat, DYNAMIC_NUM = "NUM_DYNAMIC")
  m2 <- transform_transaction_to_master(dat, DYNAMIC_NUM = c("NUM_DYNAMIC", "NUM_DYNAMIC_2"))

  expect_true(all(paste("NUM_DYNAMIC", stat_names, sep = "_") %in% names(m1)))
  expect_true(all(paste("NUM_DYNAMIC", stat_names, sep = "_") %in% names(m2)))
  expect_true(all(paste("NUM_DYNAMIC_2", stat_names, sep = "_") %in% names(m2)))

  ## the bare function names must not appear at all any more
  expect_false(any(stat_names %in% names(m1)))
  expect_false(any(stat_names %in% names(m2)))
})

test_that("the columns a single-column call shares with a multi-column call are named identically", {
  dat <- make_tran()

  m1 <- transform_transaction_to_master(dat, DYNAMIC_NUM = "NUM_DYNAMIC")
  m2 <- transform_transaction_to_master(dat, DYNAMIC_NUM = c("NUM_DYNAMIC", "NUM_DYNAMIC_2"))

  shared <- intersect(names(m1), names(m2))
  expect_true(all(paste("NUM_DYNAMIC", stat_names, sep = "_") %in% shared))
  expect_true("NUM_DYNAMIC_DIST" %in% shared)

  ## and they hold the same values, not just the same names
  for (cn in shared) {
    expect_equal(m1[[cn]], m2[[cn]], info = cn)
  }
})

test_that("DIST columns are named <col>_DIST regardless of the column count and kind", {
  dat <- make_tran()

  expect_true("NUM_DYNAMIC_DIST" %in% names(transform_transaction_to_master(dat, DYNAMIC_NUM = "NUM_DYNAMIC")))
  expect_true("CHAR_DIST" %in% names(transform_transaction_to_master(dat, DYNAMIC_CHAR = "CHAR")))

  m <- transform_transaction_to_master(dat, DYNAMIC_CHAR = c("CHAR", "CHAR_2"))
  expect_true(all(c("CHAR_DIST", "CHAR_2_DIST") %in% names(m)))

  ## the bare "DIST" name is gone
  expect_false("DIST" %in% names(transform_transaction_to_master(dat, DYNAMIC_CHAR = "CHAR")))
  expect_false("DIST" %in% names(transform_transaction_to_master(dat, DYNAMIC_NUM = "NUM_DYNAMIC")))
})

test_that("a single DYNAMIC_NUM column now works with a hard-coded downstream column name", {
  ## This is the concrete breakage from the issue: code written against the
  ## multi-column naming failed on a single-column call.
  dat <- make_tran()
  m <- transform_transaction_to_master(dat, ROW_NUMBER = "ROW_NUMBER", DYNAMIC_NUM = "NUM_DYNAMIC")
  d <- join_raw_anon_data(m, m)

  expect_no_error(reid_by_num(d, "NUM_DYNAMIC_MEAN"))
  expect_equal(
    sum(reid_by_num(d, "NUM_DYNAMIC_MEAN")$RESULT),
    length(unique(d$ANON_ROW_NUMBER))
  )
})

test_that("the aggregate values themselves are correct, not merely consistently named", {
  dat <- data.frame(
    ROW_NUMBER = 1:5,
    ID = c("a", "a", "a", "b", "b"),
    V = c(1, 3, 5, 10, 20),
    stringsAsFactors = FALSE
  )

  m <- transform_transaction_to_master(dat, DYNAMIC_NUM = "V")
  m <- m[order(m$ID), ]

  expect_equal(m$V_MAX, c(5, 20))
  expect_equal(m$V_MEAN, c(3, 15))
  expect_equal(m$V_MEDIAN, c(3, 15))
  expect_equal(m$V_MIN, c(1, 10))
  expect_equal(m$V_DIST, c("1:3:5", "10:20"))
  expect_equal(m$ROWCOUNT, c(3, 2))
  expect_equal(m$ROW_NUMBER, c(1, 4))
})

test_that("the statistic-major column order is unchanged for multi-column calls", {
  dat <- make_tran()
  m <- transform_transaction_to_master(
    dat,
    STATIC_NUM = "NUM_STATIC",
    DYNAMIC_NUM = c("BIN", "NUM_DYNAMIC", "NUM_DYNAMIC_2"),
    DYNAMIC_CHAR = "CHAR"
  )

  expect_identical(
    names(m),
    c("ID", "NUM_STATIC",
      "BIN_MAX", "NUM_DYNAMIC_MAX", "NUM_DYNAMIC_2_MAX",
      "BIN_MEAN", "NUM_DYNAMIC_MEAN", "NUM_DYNAMIC_2_MEAN",
      "BIN_MEDIAN", "NUM_DYNAMIC_MEDIAN", "NUM_DYNAMIC_2_MEDIAN",
      "BIN_MIN", "NUM_DYNAMIC_MIN", "NUM_DYNAMIC_2_MIN",
      "BIN_DIST", "NUM_DYNAMIC_DIST", "NUM_DYNAMIC_2_DIST", "CHAR_DIST",
      "ROWCOUNT", "ROW_NUMBER")
  )
})

test_that("calls with no dynamic columns at all still work", {
  dat <- make_tran()

  m <- transform_transaction_to_master(dat)
  expect_setequal(names(m), c("ID", "ROWCOUNT", "ROW_NUMBER"))
  expect_equal(nrow(m), length(unique(dat$ID)))

  m2 <- transform_transaction_to_master(dat, STATIC_NUM = "NUM_STATIC")
  expect_setequal(names(m2), c("ID", "NUM_STATIC", "ROWCOUNT", "ROW_NUMBER"))
})

test_that("a non-default collapse character is honoured in the DIST columns", {
  dat <- data.frame(
    ROW_NUMBER = 1:3, ID = c("a", "a", "b"), V = c(1, 2, 9),
    stringsAsFactors = FALSE
  )
  m <- transform_transaction_to_master(dat, DYNAMIC_NUM = "V", collapse = ";")
  m <- m[order(m$ID), ]
  expect_equal(m$V_DIST, c("1;2", "9"))
})
