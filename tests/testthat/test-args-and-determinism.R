## Regression tests for phase 4:
##
## defect 1: reid_by_num() / reid_by_char() / reid_by_dist() /
## reid_by_num_rank() accepted a `row_number` argument, built
## raw_row_number/anon_row_number from it, and then never used those
## variables -- they went on to reference the literal columns
## RAW_ROW_NUMBER/ANON_ROW_NUMBER regardless. Passing a data set whose row
## number column was renamed (e.g. to "RECORD_ID") crashed with
## "Can't select columns that don't exist." The fix wires `row_number`
## through to the actual column lookup in all 4 functions, and always
## reports the result using canonical RAW_ROW_NUMBER/ANON_ROW_NUMBER output
## columns (so reid_result()'s defaults keep working unchanged).
##
## defect 2: reid_by_num_rank() used rank(..., ties.method = "random"),
## so results for tie-heavy columns were different on every run. The fix
## switches to ties.method = "min" (deterministic; ties collapse to the
## same rank rather than an arbitrary distinct one).
##
## defect 3: create_dummy_master_data()/create_dummy_transaction_data()
## used the deprecated tibble::data_frame(); replaced with tibble::tibble().

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

## -----------------------------------------------------------------------
## row_number argument actually works, and is backward compatible
## -----------------------------------------------------------------------

test_that("row_number argument: all 4 reid_by_*() work with a renamed row-number column and match the default-named result", {
  m <- make_master_30()
  d_default <- join_raw_anon_data(m, m)

  m_renamed <- dplyr::rename(m, RECORD_ID = ROW_NUMBER)
  d_renamed <- join_raw_anon_data(m_renamed, m_renamed)

  ## reid_by_num
  r_default <- reid_by_num(d_default, "NUM_DYNAMIC_MEAN")
  r_renamed <- expect_no_error(
    reid_by_num(d_renamed, "NUM_DYNAMIC_MEAN", row_number = "RECORD_ID")
  )
  expect_identical(
    reid_result(r_default, method = "num"),
    reid_result(r_renamed, method = "num")
  )

  ## reid_by_char
  r_default <- reid_by_char(d_default, "CHAR_STATIC")
  r_renamed <- expect_no_error(
    reid_by_char(d_renamed, "CHAR_STATIC", row_number = "RECORD_ID")
  )
  expect_identical(
    reid_result(r_default, method = "char"),
    reid_result(r_renamed, method = "char")
  )

  ## reid_by_dist
  r_default <- reid_by_dist(d_default, "NUM_DYNAMIC_DIST")
  r_renamed <- expect_no_error(
    reid_by_dist(d_renamed, "NUM_DYNAMIC_DIST", row_number = "RECORD_ID")
  )
  expect_identical(
    reid_result(r_default, method = "dist"),
    reid_result(r_renamed, method = "dist")
  )

  ## reid_by_num_rank
  r_default <- reid_by_num_rank(d_default, "NUM_DYNAMIC_MEAN")
  r_renamed <- expect_no_error(
    reid_by_num_rank(d_renamed, "NUM_DYNAMIC_MEAN", row_number = "RECORD_ID")
  )
  expect_identical(
    reid_result(r_default, method = "rank"),
    reid_result(r_renamed, method = "rank")
  )
})

test_that("row_number argument: previously this crashed with 'Can't select columns that don't exist'", {
  m <- make_master_30()
  m_renamed <- dplyr::rename(m, RECORD_ID = ROW_NUMBER)
  d_renamed <- join_raw_anon_data(m_renamed, m_renamed)

  expect_false("RAW_ROW_NUMBER" %in% names(d_renamed))
  expect_true("RAW_RECORD_ID" %in% names(d_renamed))

  expect_no_error(reid_by_num(d_renamed, "NUM_DYNAMIC_MEAN", row_number = "RECORD_ID"))
  expect_no_error(reid_by_char(d_renamed, "CHAR_STATIC", row_number = "RECORD_ID"))
  expect_no_error(reid_by_dist(d_renamed, "NUM_DYNAMIC_DIST", row_number = "RECORD_ID"))
  expect_no_error(reid_by_num_rank(d_renamed, "NUM_DYNAMIC_MEAN", row_number = "RECORD_ID"))
})

test_that("row_number argument: output always uses canonical RAW_ROW_NUMBER/ANON_ROW_NUMBER so reid_result() defaults work unchanged", {
  m <- make_master_30()
  m_renamed <- dplyr::rename(m, RECORD_ID = ROW_NUMBER)
  d_renamed <- join_raw_anon_data(m_renamed, m_renamed)

  r_num <- reid_by_num(d_renamed, "NUM_DYNAMIC_MEAN", row_number = "RECORD_ID")
  r_char <- reid_by_char(d_renamed, "CHAR_STATIC", row_number = "RECORD_ID")
  r_dist <- reid_by_dist(d_renamed, "NUM_DYNAMIC_DIST", row_number = "RECORD_ID")
  r_rank <- reid_by_num_rank(d_renamed, "NUM_DYNAMIC_MEAN", row_number = "RECORD_ID")

  expect_true(all(c("RAW_ROW_NUMBER", "ANON_ROW_NUMBER") %in% names(r_num)))
  expect_true(all(c("RAW_ROW_NUMBER", "ANON_ROW_NUMBER") %in% names(r_char)))
  expect_true(all(c("RAW_ROW_NUMBER", "ANON_ROW_NUMBER") %in% names(r_dist)))
  expect_true(all(c("RAW_ROW_NUMBER", "ANON_ROW_NUMBER") %in% names(r_rank)))

  ## reid_result() with its *default* raw_row_number/anon_row_number args
  ## (i.e. no special-casing needed by the caller) works on all 4.
  expect_no_error(reid_result(r_num, method = "num"))
  expect_no_error(reid_result(r_char, method = "char"))
  expect_no_error(reid_result(r_dist, method = "dist"))
  expect_no_error(reid_result(r_rank, method = "rank"))

  n <- length(unique(d_renamed$ANON_RECORD_ID))
  expect_match(reid_result(r_num, method = "num"), paste(n, "/", n), fixed = TRUE)
  expect_match(reid_result(r_char, method = "char"), paste(n, "/", n), fixed = TRUE)
  expect_match(reid_result(r_dist, method = "dist"), paste(n, "/", n), fixed = TRUE)
  expect_match(reid_result(r_rank, method = "rank"), paste(n, "/", n), fixed = TRUE)
})

## -----------------------------------------------------------------------
## reid_by_num_rank() determinism
## -----------------------------------------------------------------------

test_that("reid_by_num_rank() is deterministic on a heavily-tied column (BIN_MEAN), 5 runs identical", {
  d <- make_identity_join_30()

  ## sanity check on the fixture: BIN_MEAN really does have ties for this
  ## data, so this actually exercises the tie-handling path.
  n <- length(unique(d$ANON_ROW_NUMBER))
  expect_true(length(unique(d$RAW_BIN_MEAN)) < n)

  runs <- lapply(1:5, function(i) reid_by_num_rank(d, "BIN_MEAN"))
  for (i in 2:5) {
    expect_identical(runs[[1]], runs[[i]])
  }
})

test_that("reid_by_num_rank() is deterministic on a fully-constant column (NUM_STATIC), 5 runs identical", {
  d <- make_identity_join_30()

  ## NUM_STATIC is constant (10) for every record -- every value ties.
  expect_equal(length(unique(d$RAW_NUM_STATIC)), 1)

  runs <- lapply(1:5, function(i) reid_by_num_rank(d, "NUM_STATIC"))
  for (i in 2:5) {
    expect_identical(runs[[1]], runs[[i]])
  }
})

test_that("reid_by_num_rank() determinism holds for the exact reproduction snippet in the task (BIN_MEAN)", {
  d <- make_identity_join_30()
  a <- reid_by_num_rank(d, "BIN_MEAN")
  b <- reid_by_num_rank(d, "BIN_MEAN")
  expect_identical(a, b)
})

## -----------------------------------------------------------------------
## tibble::data_frame() -> tibble::tibble() deprecation removal
## -----------------------------------------------------------------------

test_that("create_dummy_master_data() and create_dummy_transaction_data() emit no warnings", {
  expect_no_warning(m <- create_dummy_master_data(5))
  expect_no_warning(t <- create_dummy_transaction_data(people = 5, size = 2))

  expect_true(is.data.frame(m))
  expect_true(is.data.frame(t))
})

test_that("create_dummy_master_data() output schema is unchanged (column names/types/row count)", {
  set.seed(1)
  m <- create_dummy_master_data(5)

  expect_true(tibble::is_tibble(m))
  expect_equal(nrow(m), 5)
  expect_identical(names(m), c("ROW_NUMBER", "ID", "NUM", "BIN", "CHAR"))
  expect_type(m$ROW_NUMBER, "integer")
  expect_type(m$ID, "double")
  expect_type(m$NUM, "double")
  expect_type(m$BIN, "double")
  expect_type(m$CHAR, "character")
})

test_that("create_dummy_transaction_data() output schema is unchanged (column names/types/row count)", {
  set.seed(1)
  t <- create_dummy_transaction_data(people = 5, size = 3)

  expect_true(tibble::is_tibble(t))
  expect_equal(nrow(t), 15)
  expect_identical(names(t), c("ROW_NUMBER", "ID", "NUM_STATIC", "NUM_DYNAMIC", "BIN", "CHAR"))
  expect_type(t$ROW_NUMBER, "integer")
  expect_type(t$ID, "integer")
  expect_type(t$NUM_STATIC, "double")
  expect_type(t$NUM_DYNAMIC, "double")
  expect_type(t$BIN, "double")
  expect_type(t$CHAR, "character")
})
