## Regression tests for Issue #5: distribution_distance() must compare the
## *shape* of two distributions, not their record counts, and must not depend
## on the order the values were written in.

test_that("record-count difference no longer leaks into the distance", {
  set.seed(123)
  pop <- runif(500)

  mk <- function(k) paste(sort(sample(pop, k)), collapse = ":")

  ## All of these are samples from the same population, so the shape is the
  ## same and only n differs. The distance must stay near 0 throughout.
  ref <- mk(10)
  ks <- c(10, 11, 13, 16, 20, 30, 50)
  dists <- vapply(ks, function(k) distribution_distance(ref, mk(k)), numeric(1))

  expect_true(all(dists < 0.5))

  ## The old implementation produced a correlation of 0.99 between the count
  ## difference and the distance; there must be no such trend now.
  expect_lt(abs(cor(ks - 10, dists)), 0.8)
})

test_that("identical shapes at very different sample sizes give a near-zero distance", {
  base <- seq(0, 1, length.out = 200)

  q10 <- paste(quantile(base, probs = seq(0, 1, length.out = 10)), collapse = ":")
  q40 <- paste(quantile(base, probs = seq(0, 1, length.out = 40)), collapse = ":")
  q200 <- paste(base, collapse = ":")

  expect_lt(distribution_distance(q10, q40), 1e-6)
  expect_lt(distribution_distance(q10, q200), 1e-6)
  expect_lt(distribution_distance(q40, q200), 1e-6)
})

test_that("the same multiset in a different order has distance 0", {
  ## The old implementation returned 6 here: with equal lengths it never
  ## sorted, so it subtracted element-wise in input order.
  expect_equal(distribution_distance("3:1:2", "1:2:3"), 0)
  expect_equal(distribution_distance("5:5:1", "1:5:5"), 0)
  expect_equal(distribution_distance("9:1", "1:9"), 0)
})

test_that("the distance is symmetric", {
  expect_equal(
    distribution_distance("1:2:3", "7:8:9:10"),
    distribution_distance("7:8:9:10", "1:2:3")
  )
  expect_equal(
    distribution_distance("3:1:2", "9:1"),
    distribution_distance("9:1", "3:1:2")
  )
})

test_that("identical inputs give 0 and shifted distributions give a positive distance", {
  expect_equal(distribution_distance("1:2:3", "1:2:3"), 0)
  expect_gt(distribution_distance("1:2:3", "4:5:6"), 0)

  ## a constant shift of c moves every quantile by c => n_quantiles * c^2
  expect_equal(distribution_distance("1:2:3", "3:4:5", n_quantiles = 10), 10 * 4)
})

test_that("shape differences still dominate count differences", {
  set.seed(7)
  same_shape <- distribution_distance(
    paste(sort(runif(10)), collapse = ":"),
    paste(sort(runif(50)), collapse = ":")
  )
  diff_shape <- distribution_distance(
    paste(sort(runif(10)), collapse = ":"),
    paste(sort(runif(10) * 3 + 5), collapse = ":")
  )

  expect_lt(same_shape, diff_shape)
})

test_that("single-element distributions are handled", {
  expect_equal(distribution_distance("5", "5"), 0)
  expect_equal(distribution_distance("5", "7", n_quantiles = 10), 10 * 4)
  expect_equal(distribution_distance("5", "5:5:5"), 0)
})

test_that("n_quantiles is validated and scales the result", {
  expect_error(distribution_distance("1:2", "3:4", n_quantiles = 1), regexp = "n_quantiles")
  expect_error(distribution_distance("1:2", "3:4", n_quantiles = NA), regexp = "n_quantiles")

  ## constant offset of 2 => n_quantiles * 4
  expect_equal(distribution_distance("1:2", "3:4", n_quantiles = 5), 5 * 4)
  expect_equal(distribution_distance("1:2", "3:4", n_quantiles = 20), 20 * 4)
})

test_that("score_dist still reidentifies an exact copy perfectly", {
  set.seed(71)
  dat <- suppressWarnings(create_dummy_transaction_data(people = 30, size = 4))
  m <- transform_transaction_to_master(
    dat,
    ROW_NUMBER = "ROW_NUMBER", ID = "ID",
    DYNAMIC_NUM = "NUM_DYNAMIC", DYNAMIC_CHAR = "CHAR"
  )
  d <- join_raw_anon_data(m, m)

  r <- match_greedy(score_dist(d, "NUM_DYNAMIC_DIST"), seed = 1)

  expect_equal(nrow(r), 30)
  expect_equal(sum(r$RESULT), 30)
})
