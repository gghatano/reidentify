## Tests for the globally optimal assignment rule introduced by #15.
##
## Three things have to be pinned down, in decreasing order of how badly a
## regression would hurt:
##
##  1. match_optimal() really returns an optimal assignment. Checked against
##     brute-force enumeration on small instances -- if the solver call is
##     wired up wrongly (transposed, wrong orientation, cost shifted
##     inconsistently) the result is still a plausible-looking assignment with
##     a plausible-looking success rate, and nothing else would catch it.
##  2. The one-to-one constraint is actually enforced, and the sampling /
##     dummy-padding path does not quietly drop or duplicate ANON records.
##     A dropped record shrinks the trial count and inflates the reported
##     rate; a duplicated one does the opposite (docs/lessons-learned.md
##     section 2).
##  3. The documented behaviour under a *false* one-to-one premise holds:
##     match_optimal() under-reports relative to match_greedy(). That is a
##     property users are warned about, so it is pinned rather than fixed.

## ---------------------------------------------------------------------------
## helpers
## ---------------------------------------------------------------------------

## build a score table straight from a cost matrix (rows = ANON, cols = RAW)
scores_from_matrix <- function(cost, score_type = "distance") {
  n_anon <- nrow(cost)
  n_raw <- ncol(cost)
  anon <- rep(seq_len(n_anon), times = n_raw)
  raw <- rep(seq_len(n_raw), each = n_anon)
  new_reid_scores(
    raw_row_number = raw,
    anon_row_number = anon,
    score = cost[cbind(anon, raw)],
    score_type = score_type
  )
}

## all permutations of seq_len(n), one per row
all_perms <- function(n) {
  if (n == 1L) return(matrix(1L, 1L, 1L))
  sub <- all_perms(n - 1L)
  do.call(rbind, lapply(seq_len(n), function(i) {
    cbind(i, matrix(setdiff(seq_len(n), i)[sub], nrow = nrow(sub)))
  }))
}

## two numeric attributes, ANON = RAW + gaussian noise
make_noisy_pair <- function(n, noise, seed) {
  set.seed(seed)
  raw <- data.frame(
    ROW_NUMBER = seq_len(n),
    A = round(stats::runif(n, 0, 100), 2),
    B = round(stats::runif(n, 0, 100), 2)
  )
  anon <- raw
  anon$A <- anon$A + stats::rnorm(n, 0, noise)
  anon$B <- anon$B + stats::rnorm(n, 0, noise)
  list(raw = raw, anon = anon)
}

two_attribute_scores <- function(raw, anon) {
  d <- join_raw_anon_data(raw, anon)
  combine_scores(list(score_num(d, "A"), score_num(d, "B")))
}

## RAW and ANON drawn from the same population with a controlled overlap
make_partial_overlap <- function(pop, n_raw, n_anon, overlap, noise, seed) {
  set.seed(seed)
  pop_dat <- data.frame(
    ROW_NUMBER = seq_len(pop),
    A = round(stats::runif(pop, 0, 100), 2),
    B = round(stats::runif(pop, 0, 100), 2)
  )
  ids <- sample(pop)
  both <- ids[seq_len(overlap)]
  only_raw <- ids[overlap + seq_len(n_raw - overlap)]
  only_anon <- ids[overlap + (n_raw - overlap) + seq_len(n_anon - overlap)]
  raw <- pop_dat[pop_dat$ROW_NUMBER %in% c(both, only_raw), ]
  anon <- pop_dat[pop_dat$ROW_NUMBER %in% c(both, only_anon), ]
  anon$A <- anon$A + stats::rnorm(nrow(anon), 0, noise)
  anon$B <- anon$B + stats::rnorm(nrow(anon), 0, noise)
  list(raw = raw, anon = anon)
}

## ---------------------------------------------------------------------------
## 1. optimality
## ---------------------------------------------------------------------------

test_that("match_optimal() finds the brute-force optimum on small square instances", {
  set.seed(4242)
  for (trial in seq_len(25)) {
    n <- sample(2:6, 1)
    cost <- matrix(round(stats::runif(n * n, 0, 9)), n, n)
    m <- match_optimal(scores_from_matrix(cost), seed = trial)

    got <- sum(cost[cbind(m$ANON_ROW_NUMBER, m$RAW_ROW_NUMBER)])
    best <- min(apply(all_perms(n), 1, function(p) sum(cost[cbind(seq_len(n), p)])))

    expect_equal(got, best,
                 info = paste("trial", trial, "n", n, "got", got, "best", best))
  }
})

test_that("match_optimal() finds the brute-force optimum on rectangular instances (fewer ANON than RAW)", {
  set.seed(99)
  for (trial in seq_len(15)) {
    n_anon <- sample(2:4, 1)
    n_raw <- n_anon + sample(1:3, 1)
    cost <- matrix(round(stats::runif(n_anon * n_raw, 0, 9)), n_anon, n_raw)
    m <- match_optimal(scores_from_matrix(cost), seed = trial)

    got <- sum(cost[cbind(m$ANON_ROW_NUMBER, m$RAW_ROW_NUMBER)])
    ## enumerate every injective map from ANON rows to RAW columns
    combos <- utils::combn(n_raw, n_anon)
    best <- min(apply(combos, 2, function(cols) {
      min(apply(all_perms(n_anon), 1, function(p) sum(cost[cbind(seq_len(n_anon), cols[p])])))
    }))
    expect_equal(got, best, info = paste("trial", trial))
  }
})

test_that("match_optimal() maximises a similarity score instead of minimising it", {
  ## the greedy pick here is wrong for ANON 2, and only the joint constraint
  ## makes both assignments right
  sim <- matrix(c(0.9, 0.8, 0.1, 0.2), nrow = 2, ncol = 2)
  m <- match_optimal(scores_from_matrix(sim, score_type = "similarity"), seed = 1)

  expect_equal(m$ANON_ROW_NUMBER, c(1, 2))
  expect_equal(m$RAW_ROW_NUMBER, c(1, 2))
  expect_true(all(m$RESULT))
})

test_that("match_optimal() beats match_greedy() where greedy's independent picks collide", {
  ## ANON 1 and ANON 2 both prefer RAW 1; only ANON 1 can have it.
  cost <- matrix(c(0, 1, 5, 2), nrow = 2, ncol = 2)
  sc <- scores_from_matrix(cost)

  g <- match_greedy(sc, seed = 1)
  o <- match_optimal(sc, seed = 1)

  expect_equal(g$RAW_ROW_NUMBER, c(1, 1))   # collision: RAW 1 claimed twice
  expect_equal(o$RAW_ROW_NUMBER, c(1, 2))   # one-to-one
  expect_equal(sum(g$RESULT), 1)
  expect_equal(sum(o$RESULT), 2)
})

## ---------------------------------------------------------------------------
## 2. contract
## ---------------------------------------------------------------------------

test_that("match_optimal() returns the same schema as match_greedy(), one ANON-ordered row per ANON record", {
  fx <- make_noisy_pair(20, 2, seed = 5)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  m <- match_optimal(sc, seed = 1)

  expect_identical(
    names(m),
    c("ANON_ROW_NUMBER", "RAW_ROW_NUMBER", "CONFIDENCE", "RESULT")
  )
  expect_type(m$RESULT, "logical")
  expect_true(is.numeric(m$CONFIDENCE))
  expect_equal(nrow(m), 20)
  expect_false(anyDuplicated(m$ANON_ROW_NUMBER) > 0)
  expect_false(is.unsorted(m$ANON_ROW_NUMBER))
  expect_false(anyNA(m$RESULT))
})

test_that("match_optimal() never uses the same RAW record twice", {
  fx <- make_noisy_pair(40, 6, seed = 8)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  for (s in 1:5) {
    m <- match_optimal(sc, seed = s)
    used <- m$RAW_ROW_NUMBER[!is.na(m$RAW_ROW_NUMBER)]
    expect_false(anyDuplicated(used) > 0)
  }
})

test_that("match_optimal() CONFIDENCE reduces to match_greedy()'s 1/k when the constraint does not bind", {
  ## V unique: every ANON record's argmin is unique and distinct, so the
  ## one-to-one constraint costs nothing and both rules agree.
  raw <- data.frame(ROW_NUMBER = 1:5, V = c(10, 20, 30, 40, 50))
  d <- join_raw_anon_data(raw, raw)
  m <- match_optimal(score_num(d, "V"), seed = 1)

  expect_equal(m$RAW_ROW_NUMBER, 1:5)
  expect_equal(m$CONFIDENCE, rep(1, 5))
  expect_true(all(m$RESULT))
})

test_that("match_optimal() is reproducible for a fixed seed", {
  fx <- make_noisy_pair(30, 5, seed = 12)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  expect_identical(match_optimal(sc, seed = 7), match_optimal(sc, seed = 7))
})

test_that("match_optimal() does not disturb the caller's RNG stream", {
  raw <- data.frame(ROW_NUMBER = 1:6, V = c(1, 1, 2, 2, 3, 3))
  sc <- score_num(join_raw_anon_data(raw, raw), "V")

  set.seed(123)
  before <- stats::runif(3)
  set.seed(123)
  invisible(match_optimal(sc, seed = 9))
  after <- stats::runif(3)

  expect_equal(before, after)
})

test_that("match_optimal() treats a candidate pair absent from the score table as forbidden", {
  ## ANON 2 is only ever offered RAW 2; ANON 1 is offered both.
  sc <- new_reid_scores(
    raw_row_number = c(1, 2, 2),
    anon_row_number = c(1, 1, 2),
    score = c(5, 0, 3)
  )
  m <- match_optimal(sc, seed = 1)

  expect_equal(m$ANON_ROW_NUMBER, c(1, 2))
  ## RAW 2 is ANON 1's favourite but ANON 2 has no alternative, so ANON 1 must
  ## fall back to the only pair it was offered.
  expect_equal(m$RAW_ROW_NUMBER, c(1, 2))
})

test_that("match_optimal() declines rather than guessing a pair that was never a candidate", {
  ## Two ANON records, one RAW record: one of them cannot be matched at all.
  sc <- new_reid_scores(
    raw_row_number = c(1, 1),
    anon_row_number = c(1, 2),
    score = c(0, 4)
  )
  m <- match_optimal(sc, seed = 1)

  expect_equal(nrow(m), 2)
  expect_equal(sum(is.na(m$RAW_ROW_NUMBER)), 1)
  expect_equal(m$RAW_ROW_NUMBER[m$ANON_ROW_NUMBER == 1], 1)
  expect_equal(m$CONFIDENCE[is.na(m$RAW_ROW_NUMBER)], 0)
  expect_false(m$RESULT[is.na(m$RAW_ROW_NUMBER)])
})

## ---------------------------------------------------------------------------
## 3. sampling rate and dummy padding
## ---------------------------------------------------------------------------

test_that("sampling_rate = 1 adds no dummy columns when RAW is at least as large as ANON", {
  fx <- make_noisy_pair(25, 4, seed = 21)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  m <- match_optimal(sc, sampling_rate = 1, seed = 3)
  expect_false(anyNA(m$RAW_ROW_NUMBER))
})

test_that("sampling_rate controls the fraction of ANON records actually guessed", {
  fx <- make_partial_overlap(400, 60, 60, 40, 3, seed = 31)
  sc <- two_attribute_scores(fx$raw, fx$anon)

  for (sr in c(0.25, 0.5, 0.75)) {
    m <- match_optimal(sc, sampling_rate = sr, seed = 2)
    coverage <- mean(!is.na(m$RAW_ROW_NUMBER))
    expect_equal(coverage, sr, tolerance = 0.05,
                 info = paste("sampling_rate", sr, "coverage", coverage))
    ## declining must never invent or lose an ANON record
    expect_equal(nrow(m), 60)
  }
})

test_that("declined records are reported as NA / 0 confidence / FALSE, never as a silent success", {
  fx <- make_partial_overlap(400, 50, 50, 20, 3, seed = 44)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  m <- match_optimal(sc, sampling_rate = 0.4, seed = 1)

  declined <- is.na(m$RAW_ROW_NUMBER)
  expect_gt(sum(declined), 0)
  expect_true(all(m$CONFIDENCE[declined] == 0))
  expect_true(all(!m$RESULT[declined]))
  expect_true(all(m$CONFIDENCE[!declined] > 0))
})

test_that("an explicit dummy_cost overrides the derived rejection threshold", {
  fx <- make_partial_overlap(400, 50, 50, 25, 3, seed = 52)
  sc <- two_attribute_scores(fx$raw, fx$anon)

  ## cost 0 => a dummy is at least as good as any real candidate, so every
  ## record that *can* decline does
  cheap <- match_optimal(sc, sampling_rate = 0.5, dummy_cost = 0, seed = 1)
  ## a very large cost => dummies are never worth taking
  dear <- match_optimal(sc, sampling_rate = 0.5, dummy_cost = 1e6, seed = 1)

  expect_gt(mean(is.na(cheap$RAW_ROW_NUMBER)), mean(is.na(dear$RAW_ROW_NUMBER)))
  expect_equal(mean(is.na(dear$RAW_ROW_NUMBER)), 0)
})

test_that("padding raises precision among the records actually guessed under partial overlap", {
  ## The documented trade: declining costs recall and buys precision. This is
  ## the property that justifies the sampling_rate argument existing at all.
  prec <- function(m) {
    att <- sum(!is.na(m$RAW_ROW_NUMBER))
    if (att == 0) NA_real_ else sum(m$RESULT) / att
  }

  unpadded <- numeric(0)
  padded <- numeric(0)
  for (s in 1:8) {
    fx <- make_partial_overlap(400, 80, 80, 48, 3, seed = s)
    sc <- two_attribute_scores(fx$raw, fx$anon)
    unpadded <- c(unpadded, prec(match_optimal(sc, seed = s)))
    padded <- c(padded, prec(match_optimal(sc, sampling_rate = 0.6, seed = s)))
  }

  expect_gt(mean(padded, na.rm = TRUE), mean(unpadded, na.rm = TRUE))
})

test_that("a false one-to-one premise makes match_optimal() under-report relative to match_greedy()", {
  ## Documented, deliberately pinned: this is why match_optimal() must not be
  ## the reference when the overlap is partial or unknown
  ## (docs/lessons-learned.md section 2 -- a safety tool's failures point the
  ## safe-looking way).
  g <- numeric(0)
  o <- numeric(0)
  for (s in 1:8) {
    fx <- make_partial_overlap(400, 80, 80, 40, 3, seed = s)
    sc <- two_attribute_scores(fx$raw, fx$anon)
    g <- c(g, mean(match_greedy(sc, seed = s)$RESULT))
    o <- c(o, mean(match_optimal(sc, seed = s)$RESULT))
  }
  expect_lt(mean(o), mean(g))
})

test_that("match_optimal() beats match_greedy() when the one-to-one premise holds", {
  g <- numeric(0)
  o <- numeric(0)
  for (s in 1:8) {
    fx <- make_noisy_pair(80, 3, seed = s)
    sc <- two_attribute_scores(fx$raw, fx$anon)
    g <- c(g, mean(match_greedy(sc, seed = s)$RESULT))
    o <- c(o, mean(match_optimal(sc, seed = s)$RESULT))
  }
  expect_gt(mean(o), mean(g))
})

## ---------------------------------------------------------------------------
## 4. blocking, solver backend and size guards
## ---------------------------------------------------------------------------

test_that("block splits the problem and still returns exactly one row per ANON record", {
  fx <- make_noisy_pair(24, 3, seed = 61)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  blk <- ((sc$ANON_ROW_NUMBER - 1) %/% 8) + 1

  m <- match_optimal(sc, seed = 1, block = blk)
  expect_equal(nrow(m), 24)
  expect_equal(m$ANON_ROW_NUMBER, 1:24)
  expect_false(anyNA(m$RESULT))
})

test_that("blocking on an exact partition of the candidate pairs reproduces the unblocked optimum", {
  ## Two groups with no cross-group candidate pairs: splitting there is exact,
  ## so the blocked and unblocked results must agree exactly.
  cost <- matrix(stats::runif(1), 1, 1)  # placeholder, replaced below
  set.seed(77)
  c1 <- matrix(round(stats::runif(16, 0, 9)), 4, 4)
  c2 <- matrix(round(stats::runif(16, 0, 9)), 4, 4)

  sc1 <- scores_from_matrix(c1)
  sc2 <- scores_from_matrix(c2)
  sc2$ANON_ROW_NUMBER <- sc2$ANON_ROW_NUMBER + 4
  sc2$RAW_ROW_NUMBER <- sc2$RAW_ROW_NUMBER + 4

  sc <- new_reid_scores(
    raw_row_number = c(sc1$RAW_ROW_NUMBER, sc2$RAW_ROW_NUMBER),
    anon_row_number = c(sc1$ANON_ROW_NUMBER, sc2$ANON_ROW_NUMBER),
    score = c(sc1$SCORE, sc2$SCORE)
  )
  blk <- ifelse(sc$ANON_ROW_NUMBER <= 4, "a", "b")

  whole <- match_optimal(sc, seed = 5)
  split_up <- match_optimal(sc, seed = 5, block = blk)

  cost_of <- function(m) {
    key <- paste(m$ANON_ROW_NUMBER, m$RAW_ROW_NUMBER, sep = "\r")
    sum(sc$SCORE[match(key, paste(sc$ANON_ROW_NUMBER, sc$RAW_ROW_NUMBER, sep = "\r"))])
  }
  expect_equal(cost_of(whole), cost_of(split_up))
  expect_equal(nrow(split_up), 8)
  expect_false(is.null(cost))
})

test_that("block rejects an ANON record spread over more than one block", {
  fx <- make_noisy_pair(6, 2, seed = 71)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  blk <- rep(c("a", "b"), length.out = nrow(sc))
  expect_error(match_optimal(sc, block = blk), "same `block`")
})

test_that("block validates its length and rejects NA", {
  fx <- make_noisy_pair(4, 2, seed = 72)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  expect_error(match_optimal(sc, block = c("a", "b")), "one entry per row")
  bad <- rep("a", nrow(sc))
  bad[1] <- NA
  expect_error(match_optimal(sc, block = bad), "must not contain NA")
})

test_that("the solver backend is pluggable and unknown names are rejected", {
  expect_true("clue" %in% names(reid_lsap_solvers()))

  fx <- make_noisy_pair(5, 1, seed = 81)
  sc <- two_attribute_scores(fx$raw, fx$anon)
  expect_error(match_optimal(sc, solver = "hungarian"), "unknown `solver`")
  expect_error(match_optimal(sc, solver = c("clue", "clue")), "unknown `solver`")
})

test_that("match_optimal() warns above warn_size and errors above max_size, once per call", {
  fx <- make_noisy_pair(6, 2, seed = 91)
  sc <- two_attribute_scores(fx$raw, fx$anon)

  expect_warning(match_optimal(sc, warn_size = 3), "largest sub-problem")
  expect_error(match_optimal(sc, max_size = 3), "exceeds `max_size`")
  expect_silent(match_optimal(sc, warn_size = NULL, max_size = NULL))

  ## a blocked call must not emit one warning per block
  blk <- ((sc$ANON_ROW_NUMBER - 1) %/% 3) + 1
  w <- testthat::capture_warnings(match_optimal(sc, block = blk, warn_size = 2))
  expect_equal(length(w), 1)
})

## ---------------------------------------------------------------------------
## 5. input validation
## ---------------------------------------------------------------------------

test_that("match_optimal() rejects malformed input instead of guessing", {
  fx <- make_noisy_pair(5, 1, seed = 101)
  sc <- two_attribute_scores(fx$raw, fx$anon)

  expect_error(match_optimal(sc[0, ]), "no rows")
  expect_error(match_optimal(sc, sampling_rate = 0), "sampling_rate")
  expect_error(match_optimal(sc, sampling_rate = 1.5), "sampling_rate")
  expect_error(match_optimal(sc, sampling_rate = NA), "sampling_rate")
  expect_error(match_optimal(sc, dummy_cost = "cheap"), "dummy_cost")
  expect_error(match_optimal(data.frame(A = 1)), "missing score-layer column")

  na_scores <- sc
  na_scores$SCORE[1] <- NA
  expect_error(match_optimal(na_scores), "contains NA")

  dup <- new_reid_scores(
    raw_row_number = c(1, 1),
    anon_row_number = c(1, 1),
    score = c(0, 1)
  )
  expect_error(match_optimal(dup), "duplicated")
})
