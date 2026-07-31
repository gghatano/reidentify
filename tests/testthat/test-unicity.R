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

## ---------------------------------------------------------------------------
## #58: the key must not flatten distinct records onto one value
##
## Every failure below reported a *lower* unicity than the truth. A safety
## tool erring towards "looks anonymous" is the failure mode of
## docs/lessons-learned.md section 2, so each case is pinned down here.
## ---------------------------------------------------------------------------

test_that("a value containing the field separator does not collide", {
  ## The reproduction from #58: unicity(A) = 1 but unicity(A, B) = 0, which is
  ## mathematically impossible -- adding an attribute can only refine classes.
  d <- data.frame(A = c("x", "x\ry"), B = c("y\rz", "z"),
                  stringsAsFactors = FALSE)

  expect_equal(unicity_fraction(d, "A"), 1)
  expect_equal(unicity_fraction(d, "B"), 1)
  expect_equal(unicity_fraction(d, c("A", "B")), 1)

  ## and the other control characters, plus non-ASCII
  for (sep in c("\r", "\n", "\t", "\u3000")) {
    dd <- data.frame(A = c("x", paste0("x", sep, "y")),
                     B = c(paste0("y", sep, "z"), "z"),
                     stringsAsFactors = FALSE)
    expect_equal(unicity_fraction(dd, c("A", "B")), 1, info = sep)
  }

  non_ascii <- data.frame(A = c("\u3042", "\u3044"), B = c("\u3046", "\u3046"),
                          stringsAsFactors = FALSE)
  expect_equal(unicity_fraction(non_ascii, c("A", "B")), 1)
})

test_that("doubles that print the same are still distinct records", {
  ## as.character() prints 15 significant digits, so both pairs printed
  ## identically and every record looked non-unique.
  expect_equal(unicity_fraction(data.frame(V = c(0.1 + 0.2, 0.3)), "V"), 1)
  expect_equal(unicity_fraction(data.frame(V = c(1e15, 1e15 + 1)), "V"), 1)

  ## exactly equal values are still a tie, of course
  expect_equal(unicity_fraction(data.frame(V = c(0.3, 0.3)), "V"), 0)
})

test_that("unicity agrees with the score layer on what \"the same value\" means", {
  ## The package must not hold two definitions of equality that disagree on
  ## real data: reid_evaluate() called these two records distinguishable
  ## (TIE_SIZE 1, success 1) while unicity_fraction() called them a tie.
  v <- c(0.1 + 0.2, 0.3)
  d <- data.frame(ROW_NUMBER = 1:2, V = v)
  j <- join_raw_anon_data(d, d)

  expect_equal(unicity_fraction(data.frame(V = v), "V"), 1)
  expect_equal(reid_evaluate(score_num(j, "V"), seeds = 1:2)$success_analytic, 1)
  expect_equal(reid_confidence(score_num(j, "V"))$TIE_SIZE, c(1, 1))
})

test_that("NA is not the string \"NA\"", {
  d <- data.frame(A = c(NA, "NA"), B = c("z", "z"), stringsAsFactors = FALSE)
  expect_equal(unicity_fraction(d, c("A", "B")), 1)

  ## NA is a value in its own right: two NAs on the same column tie
  d2 <- data.frame(A = c(NA, NA), B = c("z", "z"), stringsAsFactors = FALSE)
  expect_equal(unicity_fraction(d2, c("A", "B")), 0)

  ## NA and NaN are different values, and neither is the string
  d3 <- data.frame(V = c(NA_real_, NaN))
  expect_equal(unicity_fraction(d3, "V"), 1)
})

test_that("unicity is monotone under adding attributes (property test)", {
  ## unicity(S) <= unicity(T) whenever S is contained in T. This is not an
  ## empirical tendency: the classes of T refine those of S, and refining a
  ## class of size 1 cannot destroy it. Any key collision breaks it, which is
  ## how #58 was caught, so it is fixed here as a property.
  ##
  ## NEGATIVE CONTROL. A generator of "ordinary" random columns does NOT catch
  ## this: it was run against the pre-#58 implementation and produced 0
  ## violations in 2698 checks, because a monotonicity break needs two rows
  ## whose *concatenations* coincide while the rows differ -- independent draws
  ## from disjoint alphabets essentially never do that. The alphabet below puts
  ## the separator at the start, the end and the middle of the values, so the
  ## field boundary can shift between rows. Against the pre-#58
  ## implementation this generator produces 33 violations in 2827 checks; the
  ## fixed one produces 0. A property test that cannot fail is not a test.
  set.seed(58)

  ## values that can shift the field boundary: "a\r" + sep + "\rb" and
  ## "a" + sep + "\r\rb" both flatten to "a\r\r\rb"
  alphabet <- c("", "a", "\r", "a\r", "\ra", "\r\r")

  draw_column <- function(n) {
    switch(
      sample.int(6L, 1L),
      sample(alphabet, n, replace = TRUE),
      sample(alphabet, n, replace = TRUE),
      sample(alphabet, n, replace = TRUE),
      sample(alphabet, n, replace = TRUE),
      sample(c(1, 2, 0.1 + 0.2, 0.3, 1e15, 1e15 + 1, NA), n, replace = TRUE),
      sample(c("a", "NA", NA), n, replace = TRUE)
    )
  }

  ## Violations are collected rather than asserted one by one, so a failure
  ## names the data set and the pair that broke instead of drowning in a few
  ## thousand identical expectations.
  violations <- character(0)
  n_checks <- 0L

  for (i in seq_len(200)) {
    n <- sample(2:8, 1)
    p <- sample(2:4, 1)
    dat <- as.data.frame(
      stats::setNames(lapply(seq_len(p), function(k) draw_column(n)),
                      paste0("C", seq_len(p))),
      stringsAsFactors = FALSE
    )
    cols <- names(dat)

    subsets <- unlist(
      lapply(seq_along(cols), function(k) {
        apply(utils::combn(length(cols), k), 2, function(idx) list(cols[idx]))
      }),
      recursive = FALSE
    )
    subsets <- lapply(subsets, function(x) x[[1]])

    for (s in subsets) {
      u_s <- unicity_fraction(dat, s)
      for (extra in setdiff(cols, s)) {
        u_t <- unicity_fraction(dat, c(s, extra))
        n_checks <- n_checks + 1L
        if (u_t < u_s - 1e-12) {
          violations <- c(violations, paste0(
            "data set ", i, ": unicity(", paste(s, collapse = ","), ") = ", u_s,
            " > unicity(", paste(c(s, extra), collapse = ","), ") = ", u_t))
        }
      }
    }
  }

  expect_gt(n_checks, 1000L)
  expect_equal(violations, character(0))
})

test_that("unicity() rejects a matrix column rather than mis-encoding it", {
  d <- data.frame(A = 1:3)
  d$M <- matrix(1:6, nrow = 3)
  expect_error(unicity_fraction(d, c("A", "M")), regexp = "one value per record")
})
