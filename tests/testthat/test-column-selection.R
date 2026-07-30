## Regression tests for the "silent wrong column" defect (phase 2).
##
## reid_by_num / reid_by_char / reid_by_dist / reid_by_num_rank build a
## column name by pasting "RAW_"/"ANON_" onto `target`, then used to hand
## that name to dplyr::select()/pull() as a *bare backtick symbol*
## (e.g. `` `raw_target` ``). tidyselect's legacy fallback first looks for a
## column literally named "raw_target" before falling back to using the
## variable's value as a column name. If the data happens to contain a
## column literally named "raw_target"/"anon_target"/"raw_row_number"/
## "anon_row_number", the functions silently pick that decoy column
## instead of the intended one, with no error or warning.
##
## These tests build a master data set via transform_transaction_to_master()
## and join a copy of it against itself (join_raw_anon_data(m, m)), which
## is a perfect "anonymization" of itself: every function under test should
## report success == trial == number of rows, both with and without decoy
## columns present.

make_master <- function(people = 20, size = 4, seed = 71) {
  set.seed(seed)
  dat <- create_dummy_transaction_data(people = people, size = size)
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

make_identity_join <- function(people = 20, size = 4, seed = 71) {
  m <- make_master(people = people, size = size, seed = seed)
  join_raw_anon_data(m, m)
}

add_decoy_columns <- function(d) {
  ## decoy columns whose *names* collide with the internal helper
  ## variables (raw_target / anon_target / raw_row_number /
  ## anon_row_number), with values that would produce a wrong answer if
  ## silently picked up instead of the intended column.
  d$raw_target <- -999
  d$anon_target <- 999
  d$raw_row_number <- -999
  d$anon_row_number <- 999
  d
}

## join_raw_anon_data() cross-joins RAW and ANON (merge() with no common
## column names left after the RAW_/ANON_ prefixing), so nrow(d) is
## nrow(master)^2. The number of correct re-identifications when ANON is an
## exact copy of RAW is the number of distinct ANON rows, not nrow(d).
n_expected <- function(d) length(unique(d$ANON_ROW_NUMBER))

test_that("reid_by_num: decoy columns named raw_target/anon_target do not change the result", {
  d <- make_identity_join()
  g <- add_decoy_columns(d)

  r_clean <- reid_by_num(d, "NUM_DYNAMIC_MEAN")
  r_decoy <- reid_by_num(g, "NUM_DYNAMIC_MEAN")

  expect_equal(sum(r_clean$RESULT), n_expected(d))
  expect_equal(sum(r_decoy$RESULT), n_expected(d))
  expect_equal(r_clean$RESULT, r_decoy$RESULT)
  expect_equal(r_clean$RAW, r_decoy$RAW)
  expect_equal(r_clean$ANON, r_decoy$ANON)
})

test_that("reid_by_char: decoy columns named raw_target/anon_target do not change the result", {
  d <- make_identity_join()
  g <- add_decoy_columns(d)

  r_clean <- reid_by_char(d, "CHAR_STATIC")
  r_decoy <- reid_by_char(g, "CHAR_STATIC")

  expect_equal(sum(r_clean$RESULT), n_expected(d))
  expect_equal(sum(r_decoy$RESULT), n_expected(d))
  expect_equal(r_clean$RESULT, r_decoy$RESULT)
  expect_equal(r_clean$DISTANCE, r_decoy$DISTANCE)
})

test_that("reid_by_dist: decoy columns named raw_target/anon_target do not change the result", {
  d <- make_identity_join()
  g <- add_decoy_columns(d)

  r_clean <- reid_by_dist(d, "NUM_DYNAMIC_DIST")
  r_decoy <- reid_by_dist(g, "NUM_DYNAMIC_DIST")

  expect_equal(sum(r_clean$RESULT), n_expected(d))
  expect_equal(sum(r_decoy$RESULT), n_expected(d))
  expect_equal(r_clean$RESULT, r_decoy$RESULT)
  expect_equal(r_clean$DISTANCE, r_decoy$DISTANCE)
})

test_that("reid_by_num_rank: decoy columns named raw_target/anon_target/raw_row_number/anon_row_number do not change the result", {
  d <- make_identity_join()
  g <- add_decoy_columns(d)

  r_clean <- reid_by_num_rank(d, "NUM_DYNAMIC_MEAN")
  r_decoy <- reid_by_num_rank(g, "NUM_DYNAMIC_MEAN")

  expect_equal(sum(r_clean$RESULT), n_expected(d))
  expect_equal(sum(r_decoy$RESULT), n_expected(d))
  expect_equal(r_clean$RESULT, r_decoy$RESULT)
  expect_equal(r_clean$DISTANCE, r_decoy$DISTANCE)
})

test_that("identity check: ANON is an exact copy of RAW => success == trial == nrow(master) for all 4 reid functions", {
  d <- make_identity_join()

  expect_equal(sum(reid_by_num(d, "NUM_DYNAMIC_MEAN")$RESULT), n_expected(d))
  expect_equal(sum(reid_by_char(d, "CHAR_STATIC")$RESULT), n_expected(d))
  expect_equal(sum(reid_by_dist(d, "NUM_DYNAMIC_DIST")$RESULT), n_expected(d))
  expect_equal(sum(reid_by_num_rank(d, "NUM_DYNAMIC_MEAN")$RESULT), n_expected(d))
})

test_that("normal calls to the 4 reid functions raise no tidyselect deprecation warnings", {
  d <- make_identity_join()

  expect_no_warning(reid_by_num(d, "NUM_DYNAMIC_MEAN"))
  expect_no_warning(reid_by_char(d, "CHAR_STATIC"))
  expect_no_warning(reid_by_dist(d, "NUM_DYNAMIC_DIST"))
  expect_no_warning(reid_by_num_rank(d, "NUM_DYNAMIC_MEAN"))
})

test_that("transform_transaction_to_master works when only some of STATIC_NUM/STATIC_CHAR/DYNAMIC_NUM/DYNAMIC_CHAR are given", {
  set.seed(71)
  dat <- create_dummy_transaction_data(people = 10, size = 3)

  ## only STATIC_NUM given, the rest left at their NULL default
  m1 <- expect_no_warning(
    transform_transaction_to_master(dat, ROW_NUMBER = "ROW_NUMBER", STATIC_NUM = "NUM_STATIC")
  )
  expect_true(is.data.frame(m1))
  expect_true(all(c("ID", "NUM_STATIC", "ROWCOUNT", "ROW_NUMBER") %in% names(m1)))

  ## only DYNAMIC_NUM given
  ## NB: this used to assert the bare name "MEAN". dplyr::summarise_all() with
  ## a named function list only prefixed the result with the source column
  ## name when 2+ columns remained after grouping, so a single column produced
  ## MEAN/MAX/... verbatim while two produced NUM_DYNAMIC_MEAN/... That
  ## inconsistency is Issue #26; the naming is now always <col>_<statistic>.
  m2 <- expect_no_warning(
    transform_transaction_to_master(dat, ROW_NUMBER = "ROW_NUMBER", DYNAMIC_NUM = "NUM_DYNAMIC")
  )
  expect_true(is.data.frame(m2))
  expect_true("NUM_DYNAMIC_MEAN" %in% names(m2))

  ## only DYNAMIC_CHAR given (same naming rule, see #26)
  m3 <- expect_no_warning(
    transform_transaction_to_master(dat, ROW_NUMBER = "ROW_NUMBER", DYNAMIC_CHAR = "CHAR")
  )
  expect_true(is.data.frame(m3))
  expect_true("CHAR_DIST" %in% names(m3))

  ## only STATIC_CHAR given
  m4 <- expect_no_warning(
    transform_transaction_to_master(dat, ROW_NUMBER = "ROW_NUMBER", STATIC_CHAR = "CHAR")
  )
  expect_true(is.data.frame(m4))
  expect_true("CHAR" %in% names(m4))

  ## none of the 4 optional args given at all
  m5 <- expect_no_warning(
    transform_transaction_to_master(dat, ROW_NUMBER = "ROW_NUMBER")
  )
  expect_true(is.data.frame(m5))
  expect_equal(nrow(m5), length(unique(dat$ID)))
})
