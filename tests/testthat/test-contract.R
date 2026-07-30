## Phase 5: output-contract tests (adversarially requested).
##
## These pin down the parts of the API surface that callers (and
## reid_result()) rely on but that were previously only checked implicitly:
##  - all 4 reid_by_*() functions always return a data frame with
##    ANON_ROW_NUMBER / RAW_ROW_NUMBER / RESULT columns, and RESULT is
##    logical.
##  - reid_result() always returns a length-1 character vector, and its
##    reported success is always <= trial.

make_identity_fixture <- function(people = 15, seed = 42) {
  set.seed(seed)
  raw <- create_dummy_master_data(people)
  d <- join_raw_anon_data(raw, raw)
  d
}

test_that("all 4 reid_by_*() functions return a data frame with ANON_ROW_NUMBER/RAW_ROW_NUMBER/RESULT, RESULT is logical", {
  d <- make_identity_fixture()

  r_num <- reid_by_num(d, "NUM")
  r_char <- reid_by_char(d, "CHAR")
  r_rank <- reid_by_num_rank(d, "NUM")

  set.seed(1)
  dat <- create_dummy_transaction_data(people = 15, size = 3)
  m <- transform_transaction_to_master(
    dat,
    STATIC_NUM = "NUM_STATIC", DYNAMIC_NUM = "NUM_DYNAMIC", DYNAMIC_CHAR = "CHAR"
  )
  d_dist <- join_raw_anon_data(m, m)
  r_dist <- reid_by_dist(d_dist, "NUM_DYNAMIC_DIST")

  for (r in list(r_num = r_num, r_char = r_char, r_rank = r_rank, r_dist = r_dist)) {
    expect_true(is.data.frame(r))
    expect_true(all(c("ANON_ROW_NUMBER", "RAW_ROW_NUMBER", "RESULT") %in% names(r)))
    expect_type(r$RESULT, "logical")
    ## exactly one row per ANON record: no duplicated ANON_ROW_NUMBER
    expect_false(anyDuplicated(r$ANON_ROW_NUMBER) > 0)
  }
})

test_that("reid_result() always returns a length-1 character vector", {
  d <- make_identity_fixture()
  r_num <- reid_by_num(d, "NUM")

  txt <- reid_result(r_num, method = "num")
  expect_type(txt, "character")
  expect_length(txt, 1)
})

test_that("reid_result(): success <= trial always holds (identity, subset, and independent-noise fixtures)", {
  check_success_le_trial <- function(dat_reid_result) {
    txt <- reid_result(dat_reid_result, method = "check")
    m <- regmatches(txt, regexpr("[0-9]+\\s*/\\s*[0-9]+", txt))
    parts <- as.numeric(strsplit(gsub("\\s", "", m), "/")[[1]])
    expect_true(parts[1] <= parts[2])
    parts
  }

  ## identity: success == trial
  d1 <- make_identity_fixture()
  check_success_le_trial(reid_by_num(d1, "NUM"))

  ## independent noise: success should be well below trial, but the
  ## invariant success <= trial must hold regardless
  set.seed(7)
  raw <- create_dummy_master_data(25)
  anon <- raw
  anon$NUM <- runif(25)
  d2 <- join_raw_anon_data(raw, anon)
  check_success_le_trial(reid_by_num(d2, "NUM"))

  ## subset (record-suppressed) ANON
  d3 <- join_raw_anon_data(raw, raw[1:10, ])
  check_success_le_trial(reid_by_num(d3, "NUM"))
})

test_that("reid_result(): duplicate-ANON_ROW_NUMBER input still errors rather than silently reporting success <= trial with a wrong trial count (defense-in-depth regression, see phase 3)", {
  bad <- data.frame(
    RAW_ROW_NUMBER = c(1, 2, 2, 3),
    ANON_ROW_NUMBER = c(1, 2, 2, 3),
    RESULT = c(TRUE, TRUE, TRUE, TRUE)
  )
  expect_error(reid_result(bad, method = "broken"), regexp = "duplicate")
})
