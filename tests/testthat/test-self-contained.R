## These tests verify that reidentify's 8 exported functions work when
## dplyr/magrittr are NOT attached (only loaded as Imports). This guards
## against unqualified dplyr/magrittr calls (pull(), n(), %<>%, ...)
## silently depending on the caller having attached those packages.

make_fixture <- function() {
  raw <- data.frame(
    ROW_NUMBER = 1:5, VAL = c(1, 2, 3, 4, 5),
    TXT = c("aa", "bb", "cc", "dd", "ee"),
    D = c("1:2", "2:3", "3:4", "4:5", "5:6"),
    stringsAsFactors = FALSE
  )
  anon <- data.frame(
    ROW_NUMBER = 1:5, VAL = c(1.1, 2.1, 3.1, 4.1, 5.1),
    TXT = c("aa", "bb", "cc", "dd", "ex"),
    D = c("1:2.1", "2:3.1", "3:4.1", "4:5.1", "5:6.1"),
    stringsAsFactors = FALSE
  )
  list(raw = raw, anon = anon)
}

test_that("dplyr/magrittr are not attached during testthat run", {
  # sanity check for the in-process test below: if this ever fails, the
  # in-process assertions no longer prove anything about self-containedness.
  expect_false("package:dplyr" %in% search())
  expect_false("package:magrittr" %in% search())
})

test_that("all 8 exported functions work in-process without dplyr/magrittr attached", {
  fx <- make_fixture()
  dra <- join_raw_anon_data(fx$raw, fx$anon)
  expect_true(is.data.frame(dra))

  r_num <- reid_by_num(dra, "VAL")
  expect_true(is.data.frame(r_num))

  r_rank <- reid_by_num_rank(dra, "VAL")
  expect_true(is.data.frame(r_rank))

  r_char <- reid_by_char(dra, "TXT")
  expect_true(is.data.frame(r_char))

  r_dist <- reid_by_dist(dra, "D")
  expect_true(is.data.frame(r_dist))

  txt <- reid_result(r_num, method = "x")
  expect_type(txt, "character")

  master_dummy <- create_dummy_master_data(5)
  expect_equal(nrow(master_dummy), 5)

  tran <- create_dummy_transaction_data(5, 2)
  expect_true(is.data.frame(tran))

  master <- transform_transaction_to_master(
    tran,
    STATIC_NUM = "NUM_STATIC", DYNAMIC_NUM = "NUM_DYNAMIC", DYNAMIC_CHAR = "CHAR"
  )
  expect_true(is.data.frame(master))
})

test_that("all 8 exported functions work in a fresh Rscript subprocess without attaching dplyr/magrittr", {
  # This is the decisive check: a brand-new R session that only does
  # library(reidentify) (no dplyr/magrittr attach at all), run out-of-process
  # so nothing testthat itself loads can mask a missing import.
  code <- paste(
    "library(reidentify)",
    "raw <- data.frame(ROW_NUMBER = 1:5, VAL = c(1,2,3,4,5),",
    "                   TXT = c('aa','bb','cc','dd','ee'),",
    "                   D = c('1:2','2:3','3:4','4:5','5:6'), stringsAsFactors = FALSE)",
    "anon <- data.frame(ROW_NUMBER = 1:5, VAL = c(1.1,2.1,3.1,4.1,5.1),",
    "                   TXT = c('aa','bb','cc','dd','ex'),",
    "                   D = c('1:2.1','2:3.1','3:4.1','4:5.1','5:6.1'), stringsAsFactors = FALSE)",
    "dra <- join_raw_anon_data(raw, anon)",
    "r <- reid_by_num(dra, 'VAL')",
    "reid_by_num_rank(dra, 'VAL')",
    "reid_by_char(dra, 'TXT')",
    "reid_by_dist(dra, 'D')",
    "reid_result(r, method = 'x')",
    "create_dummy_master_data(5)",
    "t <- create_dummy_transaction_data(5, 2)",
    "transform_transaction_to_master(t, STATIC_NUM = 'NUM_STATIC', DYNAMIC_NUM = 'NUM_DYNAMIC', DYNAMIC_CHAR = 'CHAR')",
    "stopifnot(!('package:dplyr' %in% search()))",
    "stopifnot(!('package:magrittr' %in% search()))",
    "cat('ALL OK\\n')",
    sep = "\n"
  )

  rscript <- file.path(R.home("bin"), "Rscript")
  out_file <- tempfile()
  err_file <- tempfile()
  on.exit(unlink(c(out_file, err_file)), add = TRUE)

  status <- system2(
    rscript,
    args = c("--vanilla", "-e", shQuote(code)),
    stdout = out_file,
    stderr = err_file
  )

  stdout_txt <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
  stderr_txt <- paste(readLines(err_file, warn = FALSE), collapse = "\n")

  expect_equal(status, 0L, info = paste("stderr:\n", stderr_txt))
  expect_match(stdout_txt, "ALL OK", fixed = TRUE)
  expect_false(grepl("could not find function", stderr_txt, fixed = TRUE))
})
