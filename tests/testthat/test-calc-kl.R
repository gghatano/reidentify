## Regression tests for Issue #4: calc_KL() must actually compute a
## Kullback-Leibler divergence.
##
## The old implementation normalised by max() instead of sum(), so it was
## handed vectors that were not probability distributions. It could return
## negative values, disagreed with the true divergence, and even picked a
## different nearest candidate.

kl_ref <- function(xv, yv) {
  p <- xv / sum(xv)
  q <- yv / sum(yv)
  sum(p * log2(p / q))
}

quiet_kl <- function(...) as.numeric(suppressMessages(calc_KL(...)))

test_that("calc_KL matches the hand-computed sum-normalised value", {
  ## x = 1:2:3:4, y = 2:2:2:2 -> 0.1535607 bits (the old code returned
  ## -1.311278 here)
  expect_equal(quiet_kl("1:2:3:4", "2:2:2:2"), kl_ref(c(1, 2, 3, 4), c(2, 2, 2, 2)))
  expect_equal(quiet_kl("1:2:3:4", "2:2:2:2"), 0.1535607, tolerance = 1e-6)
})

test_that("calc_KL is zero for identical distributions", {
  expect_equal(quiet_kl("1:2:3:4", "1:2:3:4"), 0)
  expect_equal(quiet_kl("5:5:5", "5:5:5"), 0)
})

test_that("calc_KL is non-negative, as a divergence must be", {
  set.seed(5)
  vals <- vapply(1:50, function(i) {
    a <- paste(round(runif(6) * 10 + 1, 3), collapse = ":")
    b <- paste(round(runif(6) * 10 + 1, 3), collapse = ":")
    quiet_kl(a, b)
  }, numeric(1))

  ## the old implementation produced 11 negative values out of these 50
  expect_true(all(vals >= 0))
  expect_true(all(is.finite(vals)))
})

test_that("calc_KL is scale invariant (counts vs proportions)", {
  ## multiplying either side by a constant leaves the distribution unchanged
  expect_equal(quiet_kl("1:2:3:4", "2:2:2:2"), quiet_kl("2:4:6:8", "1:1:1:1"))
  expect_equal(quiet_kl("1:2:3:4", "2:2:2:2"), quiet_kl("10:20:30:40", "7:7:7:7"))
})

test_that("calc_KL is asymmetric (it is a divergence, not a metric)", {
  ab <- quiet_kl("1:2:3:4", "4:3:2:1")
  ba <- quiet_kl("4:3:2:1", "1:2:3:4")

  expect_gt(ab, 0)
  expect_gt(ba, 0)
  ## for this pair the two directions happen to coincide by symmetry of the
  ## reversal, so use a genuinely asymmetric pair to assert the property
  expect_false(isTRUE(all.equal(
    quiet_kl("1:1:8", "3:3:4"),
    quiet_kl("3:3:4", "1:1:8")
  )))
})

test_that("zero denominators are guarded instead of producing Inf by default", {
  ## y has zero mass where x has mass => the true KL is infinite
  v <- quiet_kl("1:1:1", "1:1:0")
  expect_true(is.finite(v))
  expect_gt(v, 0)

  ## opting out of the guard gives the mathematically exact Inf
  expect_equal(quiet_kl("1:1:1", "1:1:0", epsilon = 0), Inf)

  ## a smaller guard means a larger (more faithful) divergence
  expect_gt(
    quiet_kl("1:1:1", "1:1:0", epsilon = 1e-12),
    quiet_kl("1:1:1", "1:1:0", epsilon = 1e-05)
  )
})

test_that("calc_KL rejects inputs it cannot interpret as distributions", {
  ## different support lengths would previously be recycled silently by rbind
  expect_error(calc_KL("1:2:3", "1:2"), regexp = "same support")

  expect_error(calc_KL("1:-2:3", "1:2:3"), regexp = "non-negative")
  expect_error(calc_KL("0:0:0", "1:2:3"), regexp = "positive value")

  ## non-numeric input is still rejected by parse_dist_values()
  expect_error(calc_KL("1:2:3", "a:b:c"), regexp = "numeric")
})

test_that("calc_KL ranks candidates the same way the reference definition does", {
  ## The old max()-normalised version had a rank correlation of only 0.79
  ## against the true divergence and chose a different argmin.
  set.seed(9)
  tgt_v <- round(runif(6) * 10 + 1, 3)
  tgt <- paste(tgt_v, collapse = ":")
  cand_v <- replicate(8, round(runif(6) * 10 + 1, 3), simplify = FALSE)

  got <- vapply(cand_v, function(cv) quiet_kl(tgt, paste(cv, collapse = ":")), numeric(1))
  ref <- vapply(cand_v, function(cv) kl_ref(tgt_v, cv), numeric(1))

  expect_equal(got, ref)
  expect_equal(rank(got), rank(ref))
  expect_equal(which.min(got), which.min(ref))
})
