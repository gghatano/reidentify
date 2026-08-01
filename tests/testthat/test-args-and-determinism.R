## Regression tests for phase 4, carried over to the three-layer API when the
## reid_by_*() wrappers were removed in 3.0.0.
##
## defect 1: the reid_by_*() wrappers accepted a `row_number` argument, built
## raw_row_number/anon_row_number from it, and then never used those
## variables -- they went on to reference the literal columns
## RAW_ROW_NUMBER/ANON_ROW_NUMBER regardless. Passing a data set whose row
## number column was renamed (e.g. to "RECORD_ID") crashed with
## "Can't select columns that don't exist." The fix wired `row_number`
## through to the actual column lookup, and it now lives in
## reid_prefixed_columns() -- shared by every score_*() function, which is
## where these tests reach it. The output columns are always named
## RAW_ROW_NUMBER/ANON_ROW_NUMBER whatever `row_number` was, so the
## assignment and evaluation layers need no special-casing.
##
## defect 2: the rank score used rank(..., ties.method = "random"), so
## results for tie-heavy columns were different on every run. The fix
## switches to ties.method = "min" (deterministic; ties collapse to the
## same rank rather than an arbitrary distinct one). compute_num_ranks(),
## reached here through score_num_rank(), is what carries it.
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

## Score tables are compared as *sets* of candidate pairs: join_raw_anon_data()
## is built on merge(), whose row order depends on the column names it was
## handed, and renaming the row-number column changes those.
sorted_scores <- function(s) {
  s <- as.data.frame(s)
  s[order(s$ANON_ROW_NUMBER, s$RAW_ROW_NUMBER), , drop = FALSE][
    , c("ANON_ROW_NUMBER", "RAW_ROW_NUMBER", "SCORE")
  ]
}

## -----------------------------------------------------------------------
## row_number argument actually works, and is backward compatible
## -----------------------------------------------------------------------

test_that("row_number argument: all 4 score_*() work with a renamed row-number column and give the same score table as the default-named one", {
  m <- make_master_30()
  d_default <- join_raw_anon_data(m, m)

  m_renamed <- dplyr::rename(m, RECORD_ID = ROW_NUMBER)
  d_renamed <- join_raw_anon_data(m_renamed, m_renamed)

  cases <- list(
    num = list(fn = score_num, target = "NUM_DYNAMIC_MEAN"),
    char = list(fn = score_char, target = "CHAR_STATIC"),
    dist = list(fn = score_dist, target = "NUM_DYNAMIC_DIST"),
    rank = list(fn = score_num_rank, target = "NUM_DYNAMIC_MEAN")
  )

  for (nm in names(cases)) {
    fn <- cases[[nm]]$fn
    tgt <- cases[[nm]]$target

    s_default <- fn(d_default, tgt)
    s_renamed <- expect_no_error(fn(d_renamed, tgt, row_number = "RECORD_ID"))

    expect_equal(
      sorted_scores(s_renamed), sorted_scores(s_default),
      ignore_attr = TRUE, info = nm
    )

    ## and the assignment reached through them is the same, seed for seed
    a <- match_greedy(s_default, seed = 3)
    b <- match_greedy(s_renamed, seed = 3)
    expect_equal(a$ANON_ROW_NUMBER, b$ANON_ROW_NUMBER, info = nm)
    expect_equal(a$RAW_ROW_NUMBER, b$RAW_ROW_NUMBER, info = nm)
    expect_equal(a$RESULT, b$RESULT, info = nm)
  }
})

test_that("row_number argument: previously this crashed with 'Can't select columns that don't exist'", {
  m <- make_master_30()
  m_renamed <- dplyr::rename(m, RECORD_ID = ROW_NUMBER)
  d_renamed <- join_raw_anon_data(m_renamed, m_renamed)

  expect_false("RAW_ROW_NUMBER" %in% names(d_renamed))
  expect_true("RAW_RECORD_ID" %in% names(d_renamed))

  expect_no_error(score_num(d_renamed, "NUM_DYNAMIC_MEAN", row_number = "RECORD_ID"))
  expect_no_error(score_char(d_renamed, "CHAR_STATIC", row_number = "RECORD_ID"))
  expect_no_error(score_dist(d_renamed, "NUM_DYNAMIC_DIST", row_number = "RECORD_ID"))
  expect_no_error(score_num_rank(d_renamed, "NUM_DYNAMIC_MEAN", row_number = "RECORD_ID"))
})

test_that("row_number argument: the score table always uses canonical RAW_ROW_NUMBER/ANON_ROW_NUMBER, so the assignment and evaluation layers need no special-casing", {
  m <- make_master_30()
  m_renamed <- dplyr::rename(m, RECORD_ID = ROW_NUMBER)
  d_renamed <- join_raw_anon_data(m_renamed, m_renamed)

  scores <- list(
    num = score_num(d_renamed, "NUM_DYNAMIC_MEAN", row_number = "RECORD_ID"),
    char = score_char(d_renamed, "CHAR_STATIC", row_number = "RECORD_ID"),
    dist = score_dist(d_renamed, "NUM_DYNAMIC_DIST", row_number = "RECORD_ID"),
    rank = score_num_rank(d_renamed, "NUM_DYNAMIC_MEAN", row_number = "RECORD_ID")
  )

  n <- length(unique(d_renamed$ANON_RECORD_ID))

  for (nm in names(scores)) {
    s <- scores[[nm]]
    expect_identical(
      names(s), c("RAW_ROW_NUMBER", "ANON_ROW_NUMBER", "SCORE"),
      info = nm
    )

    ## the assignment layer's defaults work unchanged on it: one row per ANON
    ## record, every one of them found (ANON is an exact copy of RAW)
    m_out <- expect_no_error(match_greedy(s))
    expect_equal(nrow(m_out), n, info = nm)
    expect_equal(sum(m_out$RESULT), n, info = nm)
  }
})

## -----------------------------------------------------------------------
## score_num_rank() determinism (ties.method = "min")
## -----------------------------------------------------------------------

test_that("score_num_rank() is deterministic on a heavily-tied column (BIN_MEAN), 5 runs identical", {
  d <- make_identity_join_30()

  ## sanity check on the fixture: BIN_MEAN really does have ties for this
  ## data, so this actually exercises the tie-handling path.
  n <- length(unique(d$ANON_ROW_NUMBER))
  expect_true(length(unique(d$RAW_BIN_MEAN)) < n)

  runs <- lapply(1:5, function(i) score_num_rank(d, "BIN_MEAN"))
  for (i in 2:5) {
    expect_identical(runs[[1]], runs[[i]])
  }

  ## and the whole attack, score plus assignment, is reproducible with it
  attacks <- lapply(1:5, function(i) match_greedy(score_num_rank(d, "BIN_MEAN"), seed = 1))
  for (i in 2:5) {
    expect_identical(attacks[[1]], attacks[[i]])
  }
})

test_that("score_num_rank() is deterministic on a fully-constant column (NUM_STATIC), 5 runs identical", {
  d <- make_identity_join_30()

  ## NUM_STATIC is constant (10) for every record -- every value ties.
  expect_equal(length(unique(d$RAW_NUM_STATIC)), 1)

  runs <- lapply(1:5, function(i) score_num_rank(d, "NUM_STATIC"))
  for (i in 2:5) {
    expect_identical(runs[[1]], runs[[i]])
  }
})

test_that("score_num_rank() determinism holds for the exact reproduction snippet in the task (BIN_MEAN)", {
  d <- make_identity_join_30()
  a <- score_num_rank(d, "BIN_MEAN")
  b <- score_num_rank(d, "BIN_MEAN")
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
