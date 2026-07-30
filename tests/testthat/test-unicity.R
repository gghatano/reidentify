## Tests for unicity measurement (#21).
##
## The issue's acceptance criteria are that unicity increases with p and
## approaches 100% once every attribute is known. Both are checked below, but
## the load-bearing tests are the hand-computed ones: a monotone curve that is
## monotone for the wrong reason would still pass a monotonicity check.

## A x B is a perfect 2x2 grid, so:
##   p = 1 : {A} -> 0, {B} -> 0, {C} -> 0                       mean 0
##   p = 2 : {A,B} -> 1, {A,C} -> 0, {B,C} -> 0                 mean 1/3
##   p = 3 : {A,B,C} -> 1                                       mean 1
make_grid <- function() {
  data.frame(A = c(1, 1, 2, 2), B = c(1, 2, 1, 2), C = c(1, 1, 1, 1))
}

make_wide <- function(people = 40, n_attr = 10, seed = 3) {
  set.seed(seed)
  cols <- lapply(seq_len(n_attr), function(i) sample.int(3L, people, replace = TRUE))
  names(cols) <- paste0("A", seq_len(n_attr))
  as.data.frame(cols)
}

## ---------------------------------------------------------------------------
## unicity_fraction(): the primitive
## ---------------------------------------------------------------------------

test_that("unicity_fraction() counts records with no twin, on hand-computed cases", {
  d <- make_grid()

  expect_equal(unicity_fraction(d, "A"), 0)
  expect_equal(unicity_fraction(d, "B"), 0)
  expect_equal(unicity_fraction(d, "C"), 0)
  expect_equal(unicity_fraction(d, c("A", "B")), 1)
  expect_equal(unicity_fraction(d, c("A", "C")), 0)
  expect_equal(unicity_fraction(d, c("A", "B", "C")), 1)
})

test_that("unicity_fraction() handles partial uniqueness", {
  ## values 1, 2, 3, 3 -> the first two are unique, the last two are not
  d <- data.frame(V = c(1, 2, 3, 3))
  expect_equal(unicity_fraction(d, "V"), 0.5)

  ## 1, 1, 1, 2 -> only the last is unique
  d2 <- data.frame(V = c(1, 1, 1, 2))
  expect_equal(unicity_fraction(d2, "V"), 0.25)
})

test_that("unicity_fraction() does not confuse different value combinations", {
  ## Naive string concatenation without a separator would map ("ab", "c") and
  ## ("a", "bc") onto the same key and over-report collisions.
  d <- data.frame(
    X = c("ab", "a"), Y = c("c", "bc"),
    stringsAsFactors = FALSE
  )
  expect_equal(unicity_fraction(d, c("X", "Y")), 1)
})

test_that("unicity_fraction() edge cases", {
  d <- make_grid()

  ## no attributes: nothing distinguishes anybody
  expect_equal(unicity_fraction(d, character(0)), 0)
  expect_equal(unicity_fraction(d[1, ], character(0)), 1)

  ## empty data
  expect_equal(unicity_fraction(d[0, ], "A"), 0)

  expect_error(unicity_fraction(d, "NOPE"), regexp = "not found")
  expect_error(unicity_fraction("nope", "A"), regexp = "must be a data frame")
})

## ---------------------------------------------------------------------------
## unicity(): the curve
## ---------------------------------------------------------------------------

test_that("unicity() reproduces the hand-computed curve exactly", {
  u <- unicity(make_grid(), attributes = c("A", "B", "C"))

  expect_equal(u$p, 1:3)
  expect_equal(u$n_subsets, c(3, 3, 1))
  expect_true(all(u$exhaustive))
  expect_equal(u$unicity_mean, c(0, 1 / 3, 1))
  expect_equal(u$unicity_min, c(0, 0, 1))
  expect_equal(u$unicity_max, c(0, 1, 1))
})

test_that("unicity increases with p and reaches 1 when every attribute is known", {
  d <- create_dummy_qi_data(people = 50, seed = 1)
  u <- unicity(d, attributes = c("AGE", "ZIP", "SEX", "VISIT_COUNT", "SPEND_MEAN", "SPEND_DIST"))

  ## monotone non-decreasing (exhaustive enumeration, so this is exact)
  expect_false(is.unsorted(u$unicity_mean))
  ## and it really does grow, not just fail to shrink
  expect_lt(u$unicity_mean[1], u$unicity_mean[nrow(u)])
  ## all attributes known => everybody is unique
  expect_equal(u$unicity_mean[nrow(u)], 1)
  expect_true(all(u$unicity_mean >= 0 & u$unicity_mean <= 1))
})

test_that("the curve is monotone on several independently generated data sets", {
  for (s in 1:5) {
    d <- create_dummy_qi_data(people = 40, seed = s)
    u <- unicity(d, attributes = c("AGE", "ZIP", "SEX", "VISIT_COUNT"))
    expect_false(is.unsorted(u$unicity_mean), info = paste("seed", s))
  }
})

test_that("unicity is 1 everywhere when every record is already distinct on any single attribute", {
  d <- data.frame(X = 1:10, Y = 101:110)
  u <- unicity(d, attributes = c("X", "Y"))
  expect_equal(u$unicity_mean, c(1, 1))
})

test_that("unicity is 0 everywhere when every record is identical", {
  d <- data.frame(X = rep(1, 6), Y = rep(2, 6))
  u <- unicity(d, attributes = c("X", "Y"))
  expect_equal(u$unicity_mean, c(0, 0))
  expect_equal(u$unicity_max, c(0, 0))
})

## ---------------------------------------------------------------------------
## exhaustive vs sampled
## ---------------------------------------------------------------------------

test_that("small p are enumerated exhaustively and flagged as exact", {
  u <- unicity(make_wide(n_attr = 10), attributes = paste0("A", 1:10), n_samples = 100)

  ## choose(10, 1) = 10 and choose(10, 2) = 45 both fit in n_samples = 100
  expect_true(u$exhaustive[u$p == 1])
  expect_equal(u$n_subsets[u$p == 1], 10)
  expect_true(u$exhaustive[u$p == 2])
  expect_equal(u$n_subsets[u$p == 2], 45)

  ## choose(10, 5) = 252 does not
  expect_false(u$exhaustive[u$p == 5])
  expect_lte(u$n_subsets[u$p == 5], 100)
})

test_that("sampled rows draw distinct subsets and stay within n_samples", {
  u <- unicity(make_wide(n_attr = 12), attributes = paste0("A", 1:12),
               p = 6, n_samples = 20)
  expect_false(u$exhaustive)
  expect_lte(u$n_subsets, 20)
  expect_gt(u$n_subsets, 1)
})

test_that("unicity() is reproducible from its seed and leaves the caller's RNG stream alone", {
  d <- make_wide(n_attr = 12)
  attrs <- paste0("A", 1:12)

  expect_equal(
    unicity(d, attrs, p = 6, n_samples = 20, seed = 42),
    unicity(d, attrs, p = 6, n_samples = 20, seed = 42)
  )
  expect_false(isTRUE(all.equal(
    unicity(d, attrs, p = 6, n_samples = 20, seed = 42)$unicity_mean,
    unicity(d, attrs, p = 6, n_samples = 20, seed = 7)$unicity_mean
  )))

  set.seed(99)
  before <- runif(3)
  set.seed(99)
  invisible(unicity(d, attrs, p = 6, n_samples = 20, seed = 12345))
  after <- runif(3)
  expect_identical(before, after)
})

test_that("the exhaustive mean does not depend on n_samples", {
  d <- make_wide(n_attr = 6)
  attrs <- paste0("A", 1:6)

  a <- unicity(d, attrs, p = 2, n_samples = 100)
  b <- unicity(d, attrs, p = 2, n_samples = 1000)
  expect_true(a$exhaustive && b$exhaustive)
  expect_equal(a$unicity_mean, b$unicity_mean)
})

## ---------------------------------------------------------------------------
## arguments and reporting shape
## ---------------------------------------------------------------------------

test_that("unicity() returns a plain data frame ready to drop into a report", {
  u <- unicity(make_grid(), attributes = c("A", "B", "C"))

  expect_true(is.data.frame(u))
  expect_false(inherits(u, "tbl_df"))
  expect_identical(
    names(u),
    c("p", "n_subsets", "exhaustive", "unicity_mean", "unicity_sd",
      "unicity_min", "unicity_max")
  )
  expect_identical(rownames(u), as.character(1:3))
})

test_that("p can be restricted to a subset of sizes", {
  u <- unicity(make_grid(), attributes = c("A", "B", "C"), p = c(2, 3))
  expect_equal(u$p, c(2, 3))
  expect_equal(u$unicity_mean, c(1 / 3, 1))

  ## duplicates and disorder are normalised
  u2 <- unicity(make_grid(), attributes = c("A", "B", "C"), p = c(3, 2, 3))
  expect_equal(u2$p, c(2, 3))
})

test_that("unicity() validates its arguments", {
  d <- make_grid()

  expect_error(unicity("nope", "A"), regexp = "must be a data frame")
  expect_error(unicity(d, character(0)), regexp = "at least one column")
  expect_error(unicity(d, c("A", "A")), regexp = "duplicates")
  expect_error(unicity(d, c("A", "NOPE")), regexp = "not found")
  expect_error(unicity(d, c("A", "B"), p = 0), regexp = "must lie between")
  expect_error(unicity(d, c("A", "B"), p = 3), regexp = "must lie between")
  expect_error(unicity(d, c("A", "B"), n_samples = 0), regexp = "n_samples")
})

test_that("unicity is a LOWER bound on the attack's expected success rate, not an upper one", {
  ## This is the relationship it is easy to get backwards. Unicity counts a
  ## record that shares its attribute values with m - 1 others as 0, because it
  ## cannot be pinned down with certainty. An attacker guessing among the m
  ## still wins it with probability 1/m, so the expected success rate sits
  ## *above* unicity, never below it.
  ##
  ## Measured on this fixture: unicity 0.950, attack 0.975 -- the 2 records
  ## that share an (AGE, ZIP, SEX) combination contribute 0 to unicity and
  ## 1/2 each to the attack.
  d <- create_dummy_qi_data(people = 40, seed = 4)
  attrs <- c("AGE", "ZIP", "SEX")

  u_full <- unicity_fraction(d, attrs)

  j <- join_raw_anon_data(d, d)
  k <- attacker_knowledge("M", quasi_identifiers = c(AGE = "num", ZIP = "char", SEX = "char"))
  e <- reid_evaluate(score_by_knowledge(j, k), seeds = 1:20)

  expect_gte(e$success_analytic, u_full - 1e-9)
  expect_equal(u_full, 0.95)
  expect_equal(e$success_analytic, 0.975)

  ## every record that is unique on the attributes is identified with
  ## certainty, i.e. risk exactly 1
  expect_equal(mean(e$per_record$RISK == 1), u_full)
})
