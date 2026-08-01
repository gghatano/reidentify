## Regression tests for the "silent wrong column" defect (phase 2), carried
## over to the three-layer API when the reid_by_*() wrappers were removed in
## 3.0.0.
##
## The score functions build a column name by pasting "RAW_"/"ANON_" onto
## `target`, then used to hand that name to dplyr::select()/pull() as a *bare
## backtick symbol* (e.g. `` `raw_target` ``). tidyselect's legacy fallback
## first looks for a column literally named "raw_target" before falling back to
## using the variable's value as a column name. If the data happens to contain
## a column literally named "raw_target"/"anon_target"/"raw_row_number"/
## "anon_row_number", the functions silently pick that decoy column instead of
## the intended one, with no error or warning.
##
## The lookup now goes through reid_prefixed_columns() + `[[`, which is what
## every score_*() function uses, so this is where the regression is pinned.
##
## These tests build a master data set via transform_transaction_to_master()
## and join a copy of it against itself (join_raw_anon_data(m, m)), which
## is a perfect "anonymization" of itself: every function under test should
## reidentify every ANON record, both with and without decoy columns present.

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

## the four score functions, each with a target column of the right type
score_cases <- function() {
  list(
    score_num = list(fn = score_num, target = "NUM_DYNAMIC_MEAN"),
    score_char = list(fn = score_char, target = "CHAR_STATIC"),
    score_dist = list(fn = score_dist, target = "NUM_DYNAMIC_DIST"),
    score_num_rank = list(fn = score_num_rank, target = "NUM_DYNAMIC_MEAN")
  )
}

test_that("decoy columns named raw_target/anon_target/raw_row_number/anon_row_number do not change any score table", {
  d <- make_identity_join()
  g <- add_decoy_columns(d)

  for (nm in names(score_cases())) {
    case <- score_cases()[[nm]]
    s_clean <- case$fn(d, case$target)
    s_decoy <- case$fn(g, case$target)

    expect_equal(s_clean$SCORE, s_decoy$SCORE, info = nm)
    expect_equal(s_clean$RAW_ROW_NUMBER, s_decoy$RAW_ROW_NUMBER, info = nm)
    expect_equal(s_clean$ANON_ROW_NUMBER, s_decoy$ANON_ROW_NUMBER, info = nm)

    ## the decoy values (-999 / 999) would show up as a wrong answer here
    m_clean <- match_greedy(s_clean, seed = 1)
    m_decoy <- match_greedy(s_decoy, seed = 1)
    expect_equal(sum(m_clean$RESULT), n_expected(d), info = nm)
    expect_equal(sum(m_decoy$RESULT), n_expected(d), info = nm)
    expect_equal(m_clean$RESULT, m_decoy$RESULT, info = nm)
    expect_equal(m_clean$RAW_ROW_NUMBER, m_decoy$RAW_ROW_NUMBER, info = nm)
  }
})

test_that("identity check: ANON is an exact copy of RAW => every ANON record is reidentified by all 4 score functions", {
  d <- make_identity_join()

  for (nm in names(score_cases())) {
    case <- score_cases()[[nm]]
    m <- match_greedy(case$fn(d, case$target), seed = 1)
    expect_equal(nrow(m), n_expected(d), info = nm)
    expect_equal(sum(m$RESULT), n_expected(d), info = nm)
  }
})

test_that("normal calls to the 4 score functions raise no tidyselect deprecation warnings", {
  d <- make_identity_join()

  for (nm in names(score_cases())) {
    case <- score_cases()[[nm]]
    expect_no_warning(match_greedy(case$fn(d, case$target)))
  }
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
