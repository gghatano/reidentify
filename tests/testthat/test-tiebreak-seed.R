## Regression tests for Issue #3: tie-breaking must not depend on input row
## order, must be reproducible from a seed, and the run-to-run spread must be
## reportable.
##
## Before the fix, ties were resolved by keeping RAW_ROW_NUMBER[1] -- the
## first tied candidate in input order. On a 50-person fixture with a
## 3-valued column that made the reported success rate range over
## [0.02, 0.14] purely as a function of how the input rows happened to be
## ordered, and it concentrated every possible success onto the 3 records
## that led their tie group.

make_tie_heavy <- function(people = 50, seed = 71) {
  set.seed(seed)
  raw <- suppressWarnings(create_dummy_master_data(people = people))
  ## ANON is an exact copy => the true match always exists, and BIN has only
  ## 3 distinct values so every ANON record has a large tie group.
  join_raw_anon_data(raw, raw)
}

test_that("the same seed gives an identical result", {
  d <- make_tie_heavy()

  a <- reid_by_num(d, "BIN", seed = 42)
  b <- reid_by_num(d, "BIN", seed = 42)

  expect_identical(a, b)
})

test_that("different seeds actually change which tied candidate is chosen", {
  d <- make_tie_heavy()

  rates <- vapply(1:20, function(s) mean(reid_by_num(d, "BIN", seed = s)$RESULT), numeric(1))

  expect_gt(length(unique(rates)), 1)
})

test_that("input row order no longer changes the result for a fixed seed", {
  d <- make_tie_heavy()

  baseline <- reid_by_num(d, "BIN", seed = 7)

  set.seed(1)
  for (i in 1:10) {
    shuffled <- d[sample(nrow(d)), ]
    got <- reid_by_num(shuffled, "BIN", seed = 7)
    ## row.names differ after subsetting; compare the payload only
    expect_equal(got$ANON_ROW_NUMBER, baseline$ANON_ROW_NUMBER)
    expect_equal(got$RAW_ROW_NUMBER, baseline$RAW_ROW_NUMBER)
    expect_equal(got$RESULT, baseline$RESULT)
  }
})

test_that("output is ordered by ANON_ROW_NUMBER regardless of input order", {
  d <- make_tie_heavy()

  r <- reid_by_num(d, "BIN", seed = 5)
  expect_false(is.unsorted(r$ANON_ROW_NUMBER))

  r2 <- reid_by_num(d[sample(nrow(d)), ], "BIN", seed = 5)
  expect_identical(r$ANON_ROW_NUMBER, r2$ANON_ROW_NUMBER)
})

test_that("passing seed= does not disturb the caller's RNG stream", {
  d <- make_tie_heavy(people = 20)

  set.seed(99)
  before <- runif(3)

  set.seed(99)
  invisible(reid_by_num(d, "BIN", seed = 12345))
  after <- runif(3)

  expect_identical(before, after)
})

test_that("all 4 reid_by_*() accept seed= and stay reproducible", {
  d <- make_tie_heavy(people = 20)

  expect_identical(reid_by_num(d, "BIN", seed = 3), reid_by_num(d, "BIN", seed = 3))
  expect_identical(reid_by_char(d, "CHAR", seed = 3), reid_by_char(d, "CHAR", seed = 3))
  expect_identical(reid_by_num_rank(d, "BIN", seed = 3), reid_by_num_rank(d, "BIN", seed = 3))

  raw <- data.frame(
    ROW_NUMBER = 1:10, ID = 1:10,
    D = rep(c("1:2:3", "4:5:6"), length.out = 10), stringsAsFactors = FALSE
  )
  dd <- join_raw_anon_data(raw, raw)
  expect_identical(reid_by_dist(dd, "D", seed = 3), reid_by_dist(dd, "D", seed = 3))
})

test_that("random tie-breaking is unbiased: the mean rate matches mean(1 / tie_size)", {
  d <- make_tie_heavy()

  tie_sizes <- tapply(
    abs(d$RAW_BIN - d$ANON_BIN), d$ANON_ROW_NUMBER,
    function(v) sum(v == min(v))
  )
  theoretical <- mean(1 / tie_sizes)

  st <- reid_stability(reid_by_num, d, "BIN", seeds = 1:200)

  ## sampling error over 200 seeds is small; allow a generous tolerance
  expect_equal(st$mean, theoretical, tolerance = 0.02)
})

test_that("every record becomes reachable, not just the head of each tie group", {
  d <- make_tie_heavy()

  ever_hit <- rowSums(vapply(
    1:100,
    function(s) as.integer(reid_by_num(d, "BIN", seed = s)$RESULT),
    numeric(50)
  ))

  ## The deterministic RAW_ROW_NUMBER[1] rule could only ever succeed on the
  ## 3 records leading the 3 BIN tie groups; random tie-breaking must spread
  ## the chance across essentially all records.
  expect_gt(sum(ever_hit > 0), 40)
})

test_that("reid_stability reports mean, sd and range over seeds", {
  d <- make_tie_heavy()

  st <- reid_stability(reid_by_num, d, "BIN", seeds = 1:30)

  expect_s3_class(st, "reid_stability")
  expect_setequal(
    names(st),
    c("per_seed", "mean", "sd", "min", "max", "trial", "n_seeds")
  )
  expect_equal(nrow(st$per_seed), 30)
  expect_setequal(names(st$per_seed), c("seed", "success", "trial", "rate"))
  expect_equal(st$n_seeds, 30)
  expect_equal(st$trial, 50)

  ## a tie-heavy column must show real spread
  expect_gt(st$sd, 0)
  expect_true(st$min <= st$mean && st$mean <= st$max)

  ## printing works and returns invisibly
  expect_output(print(st), "reid stability over 30 tie-break seeds")
})

test_that("reid_stability reports sd 0 for a collision-free column", {
  d <- make_tie_heavy()

  ## NUM is continuous, so ANON == RAW matches uniquely: no ties, no spread
  st <- reid_stability(reid_by_num, d, "NUM", seeds = 1:10)

  expect_equal(st$mean, 1)
  expect_equal(st$sd, 0)
})

test_that("reid_stability validates its seeds argument", {
  d <- make_tie_heavy(people = 10)

  expect_error(reid_stability(reid_by_num, d, "BIN", seeds = 1), regexp = "at least 2")
  expect_error(reid_stability(reid_by_num, d, "BIN", seeds = c(1, 1, 2)), regexp = "duplicate")
})
