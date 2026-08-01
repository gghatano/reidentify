## Regression tests for phase 6:
##
## defect 1: calc_KL() normalized its input by dividing by the *maximum*
## element instead of the *sum*, so the result was not a true probability
## distribution (does not sum to 1). The KL divergence formula is only
## guaranteed non-negative (and 0 for identical inputs) for genuine
## probability distributions, so this could -- and did -- return negative
## values, e.g. calc_KL("1:1:10", "1:1:1") ~= -0.664. Fixed by normalizing
## by sum instead of max. Also, philentropy::KL() prints an informational
## message to the console on every call; since calc_KL() is an internal
## helper, that message is now suppressed.
##
## defect 2: distribution_distance() returned the raw sum of squared
## differences between two (mean-padded, length-matched) numeric vectors,
## so the distance scaled up with the number of (padded) elements being
## compared. Two people with the same distribution *shape* but different
## record counts ended up systematically farther apart than two same-
## record-count people, purely as an artifact of vector length. Fixed by
## using the mean squared difference (MSE) instead of the sum, which
## normalizes for the (post-padding, common) length of the two vectors.

test_that("calc_KL(): is non-negative, including the case that used to return a negative value", {
  ## this exact case previously returned approximately -0.664
  kl_val <- reidentify:::calc_KL("1:1:10", "1:1:1")
  expect_gte(kl_val, 0)

  ## a handful of other cases: always non-negative
  expect_gte(reidentify:::calc_KL("1:2:3", "1:2:3"), 0)
  expect_gte(reidentify:::calc_KL("1:2:3", "3:2:1"), 0)
  expect_gte(reidentify:::calc_KL("1:2:3", "2:4:6"), 0)
  expect_gte(reidentify:::calc_KL("5:5:5:5", "1:1:1:20"), 0)
})

test_that("calc_KL(): d(x, x) == 0", {
  expect_equal(reidentify:::calc_KL("1:2:3", "1:2:3"), 0)
  expect_equal(reidentify:::calc_KL("1:1:10", "1:1:10"), 0)
})

test_that("calc_KL(): does not print anything to the console (philentropy::KL()'s metric message is suppressed)", {
  expect_silent(reidentify:::calc_KL("1:2:3", "3:2:1"))
  expect_silent(reidentify:::calc_KL("1:1:10", "1:1:1"))
})

test_that("calc_KL(): errors (rather than silently returning NaN) when a distribution's elements are all zero", {
  expect_error(reidentify:::calc_KL("0:0:0", "1:2:3"), regexp = "zero")
  expect_error(reidentify:::calc_KL("1:2:3", "0:0:0"), regexp = "zero")
})

test_that("distribution_distance(): symmetry d(x, y) == d(y, x)", {
  DD <- reidentify:::distribution_distance

  expect_equal(DD("1:2:3", "3:5:9"), DD("3:5:9", "1:2:3"))
  expect_equal(DD("1:2:3:4:5", "10:20"), DD("10:20", "1:2:3:4:5"))

  set.seed(11)
  for (i in 1:20) {
    x <- paste(sort(runif(sample(2:15, 1))), collapse = ":")
    y <- paste(sort(runif(sample(2:15, 1))), collapse = ":")
    expect_equal(DD(x, y), DD(y, x))
  }
})

test_that("distribution_distance(): self-distance d(x, x) == 0", {
  DD <- reidentify:::distribution_distance

  expect_equal(DD("1:2:3", "1:2:3"), 0)
  expect_equal(DD("0.1:0.4:0.9", "0.1:0.4:0.9"), 0)

  set.seed(12)
  for (i in 1:20) {
    x <- paste(sort(runif(sample(2:15, 1))), collapse = ":")
    expect_equal(DD(x, x), 0)
  }
})

test_that("distribution_distance(): same shape, different record count -> (near) 0, smaller than same-count-different-shape", {
  DD <- reidentify:::distribution_distance

  ## identical constant shape (all 0.5), 4 elements vs 14 elements: the two
  ## distributions are the same shape, just observed with a different
  ## number of records for that person.
  same_shape_diff_count <- DD(
    paste(rep(0.5, 4), collapse = ":"),
    paste(rep(0.5, 14), collapse = ":")
  )
  expect_equal(same_shape_diff_count, 0)

  ## same record count (4 vs 4), clearly different shape
  same_count_diff_shape <- DD(
    paste(rep(0.1, 4), collapse = ":"),
    paste(rep(0.9, 4), collapse = ":")
  )
  expect_true(same_count_diff_shape > 0)
  expect_true(same_shape_diff_count < same_count_diff_shape)
})

test_that("distribution_distance(): dependence on record-count difference is reduced relative to the pre-fix implementation", {
  DD <- reidentify:::distribution_distance

  ## pre-fix implementation, kept locally only for this comparison: same
  ## mean-fill/length-matching logic, but the raw *sum* of squared
  ## differences instead of the mean.
  dd_old <- function(x, y, split = ":") {
    x_list <- as.numeric(strsplit(x, split = split)[[1]])
    y_list <- as.numeric(strsplit(y, split = split)[[1]])
    x_length <- length(x_list)
    y_length <- length(y_list)
    diff_x_y <- x_length - y_length
    if (diff_x_y == 0) {
    } else if (diff_x_y > 0) {
      y_list <- sort(c(y_list, rep(mean(y_list), diff_x_y)))
    } else {
      x_list <- sort(c(x_list, rep(mean(x_list), -1 * diff_x_y)))
    }
    sum((x_list - y_list)^2)
  }

  mk <- function(v) paste(v, collapse = ":")

  set.seed(71)
  n <- 400
  lx <- sample(2:20, n, TRUE)
  ly <- sample(2:20, n, TRUE)
  d_new <- numeric(n)
  d_old <- numeric(n)
  ld <- numeric(n)
  for (i in 1:n) {
    x <- sort(runif(lx[i]))
    y <- sort(runif(ly[i]))
    d_new[i] <- DD(mk(x), mk(y))
    d_old[i] <- dd_old(mk(x), mk(y))
    ld[i] <- abs(lx[i] - ly[i])
  }

  ## same-record-count pairs vs very-different-record-count pairs: the
  ## pre-fix implementation shows about a ~3.9x gap; the ratio should now
  ## be clearly smaller. Threshold is loose on purpose (this is a
  ## statistical property, not an exact value).
  ratio_old <- mean(d_old[ld >= 10]) / mean(d_old[ld == 0])
  ratio_new <- mean(d_new[ld >= 10]) / mean(d_new[ld == 0])

  expect_true(ratio_old > 3) # sanity check on the fixture / pre-fix baseline
  expect_true(ratio_new < ratio_old - 1)

  ## correlation between distance and |record-count difference| should also
  ## be clearly reduced (pre-fix ~0.63)
  cor_old <- cor(d_old, ld)
  cor_new <- cor(d_new, ld)
  expect_true(cor_old > 0.5)
  expect_true(cor_new < cor_old - 0.2)
})

test_that("score_dist(): identity join (ANON == RAW) still reidentifies every record (success == trial == n)", {
  set.seed(71)
  dat <- create_dummy_transaction_data(people = 30, size = 4)
  dat$NUM_STATIC_2 <- dat$NUM_STATIC + 1
  dat$NUM_DYNAMIC_2 <- dat$NUM_DYNAMIC + 1
  dat$CHAR_STATIC <- paste("CHAR", dat$ID, sep = "")

  m <- transform_transaction_to_master(
    dat,
    ROW_NUMBER = "ROW_NUMBER",
    STATIC_NUM = c("NUM_STATIC", "NUM_STATIC_2"),
    DYNAMIC_NUM = c("BIN", "NUM_DYNAMIC", "NUM_DYNAMIC_2"),
    STATIC_CHAR = "CHAR_STATIC",
    DYNAMIC_CHAR = "CHAR"
  )
  d <- join_raw_anon_data(m, m)

  s <- score_dist(d, "NUM_DYNAMIC_DIST")
  r <- match_greedy(s, seed = 1)
  expect_equal(nrow(r), 30)
  expect_equal(sum(r$RESULT), 30)

  ## the trial count is the number of ANON records, not a tie-inflated one
  e <- reid_evaluate(s, seeds = 1:3, top_k = 1)
  expect_equal(unique(e$per_seed$trial), 30)
})
