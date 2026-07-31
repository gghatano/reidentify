## Tests for multi-attribute integration (#14): normalisation, the weighted
## sum over several attributes, and the Mahalanobis option.
##
## The two acceptance criteria on the issue -- "a multi-attribute attack beats
## a single-attribute one" and "on correlated data Mahalanobis beats the plain
## weighted sum" -- are checked at the bottom, on the same generators that
## docs/investigation/multiattr-benchmark.R measures across seeds.

simple_join <- function(seed = 3, n = 8, noise = 0.3) {
  set.seed(seed)
  raw <- data.frame(
    ROW_NUMBER = seq_len(n),
    A = rnorm(n),
    B = rnorm(n),
    C = rnorm(n)
  )
  anon <- raw
  for (v in c("A", "B", "C")) anon[[v]] <- raw[[v]] + rnorm(n, sd = noise)
  join_raw_anon_data(raw, anon)
}

## Redundant A/B (two views of one latent quantity) plus an independent C, with
## the anonymiser perturbing the *latent* quantity so A and B move together.
## A weighted sum counts the redundant pair as two votes against C's one.
redundant_join <- function(n = 120, noise = 0.5, seed = 1) {
  set.seed(seed)
  latent <- rnorm(n)
  raw <- data.frame(
    ROW_NUMBER = seq_len(n),
    A = latent + rnorm(n, sd = 0.05),
    B = 3 * latent + rnorm(n, sd = 0.15),
    C = rnorm(n)
  )
  shift <- rnorm(n, sd = noise)
  anon <- data.frame(
    ROW_NUMBER = seq_len(n),
    A = raw$A + shift,
    B = raw$B + 3 * shift,
    C = raw$C + rnorm(n, sd = noise)
  )
  join_raw_anon_data(raw, anon)
}

## ---------------------------------------------------------------------------
## normalize_scores()
## ---------------------------------------------------------------------------

test_that("range normalisation maps the scores onto [0, 1] and attains both ends", {
  s <- normalize_scores(score_num(simple_join(), "A"), method = "range")
  expect_true(all(s$SCORE >= 0 & s$SCORE <= 1))
  expect_equal(min(s$SCORE), 0)
  expect_equal(max(s$SCORE), 1)
})

test_that("zscore normalisation centres and scales the scores", {
  s <- normalize_scores(score_num(simple_join(), "A"), method = "zscore")
  expect_equal(mean(s$SCORE), 0)
  expect_equal(sd(s$SCORE), 1)
})

test_that("rank normalisation is the empirical CDF: [0, 1], mid-ranks for ties", {
  ## With no ties the endpoints are attained exactly. (A cross join of a table
  ## against itself always ties -- the whole diagonal scores 0 -- so the untied
  ## case is built as a score table directly.)
  untied <- new_reid_scores(
    raw_row_number = rep(1:2, each = 3),
    anon_row_number = rep(1:3, times = 2),
    score = c(0.4, 1.1, 2.5, 3.9, 4.2, 9.0)
  )
  s0 <- normalize_scores(untied, method = "rank")
  expect_equal(min(s0$SCORE), 0)
  expect_equal(max(s0$SCORE), 1)
  expect_equal(s0$SCORE, (0:5) / 5)

  ## With ties they are not, and that is the point: tied candidates share the
  ## average rank, so a heavily tied column spans less of [0, 1] and counts
  ## for less in the sum than a column that separates every candidate.
  raw <- data.frame(ROW_NUMBER = 1:4, V = c(0, 0, 1, 3))
  j <- join_raw_anon_data(raw, raw)
  d <- score_num(j, "V")$SCORE
  s <- normalize_scores(score_num(j, "V"), method = "rank")

  expect_true(all(s$SCORE >= 0 & s$SCORE <= 1))
  expect_gt(min(s$SCORE), 0)
  expect_lt(max(s$SCORE), 1)
  expect_equal(s$SCORE, (rank(d, ties.method = "average") - 1) / (length(d) - 1))

  ## equal distances get equal normalised values
  for (v in unique(d)) {
    expect_equal(length(unique(s$SCORE[d == v])), 1L)
  }
})

test_that("rank normalisation is invariant to any monotone rescaling of the metric", {
  ## This is the property that makes an edit distance and an unbounded L2
  ## distance comparable without picking units for either.
  s <- score_num(simple_join(), "A")
  stretched <- new_reid_scores(s$RAW_ROW_NUMBER, s$ANON_ROW_NUMBER, exp(3 * s$SCORE))

  expect_equal(
    normalize_scores(s, method = "rank")$SCORE,
    normalize_scores(stretched, method = "rank")$SCORE
  )
  ## range normalisation is not, which is why both are offered
  expect_false(isTRUE(all.equal(
    normalize_scores(s, method = "range")$SCORE,
    normalize_scores(stretched, method = "range")$SCORE
  )))
})

test_that("none leaves the scores untouched", {
  s <- score_num(simple_join(), "A")
  expect_equal(normalize_scores(s, method = "none")$SCORE, s$SCORE)
})

test_that("every normalisation leaves the ranking inside a score table unchanged", {
  ## Normalisation may only change how much a column counts for relative to
  ## the others; if it reordered a single column's own candidates it would be
  ## changing the attack, not the weighting.
  s <- score_num(simple_join(), "A")
  for (m in c("range", "zscore", "rank", "none")) {
    got <- normalize_scores(s, method = m)
    expect_equal(rank(got$SCORE, ties.method = "average"),
                 rank(s$SCORE, ties.method = "average"),
                 info = m)
  }
})

test_that("a column with no variation is given exactly zero under every normalisation", {
  raw <- data.frame(ROW_NUMBER = 1:5, CONST = rep(7, 5))
  s <- score_num(join_raw_anon_data(raw, raw), "CONST")
  expect_true(all(s$SCORE == 0))

  for (m in c("range", "zscore", "rank")) {
    expect_equal(normalize_scores(s, method = m)$SCORE, rep(0, nrow(s)), info = m)
  }
})

test_that("normalisation keeps the candidate pairs, their order and the score orientation", {
  s <- score_num(simple_join(), "A")
  sim <- new_reid_scores(s$RAW_ROW_NUMBER, s$ANON_ROW_NUMBER, s$SCORE,
                         score_type = "similarity")
  got <- normalize_scores(sim, method = "range")

  expect_equal(got$RAW_ROW_NUMBER, sim$RAW_ROW_NUMBER)
  expect_equal(got$ANON_ROW_NUMBER, sim$ANON_ROW_NUMBER)
  expect_equal(attr(got, "score_type"), "similarity")
  expect_s3_class(got, "reid_scores")
})

test_that("normalize_scores() takes a list and keeps its names", {
  j <- simple_join()
  got <- normalize_scores(list(a = score_num(j, "A"), b = score_num(j, "B")),
                          method = "range")
  expect_named(got, c("a", "b"))
  expect_true(all(vapply(got, function(x) max(x$SCORE) == 1, logical(1))))
})

test_that("normalize_scores() rejects malformed input", {
  j <- simple_join()
  s <- score_num(j, "A")

  expect_error(normalize_scores(1:5), regexp = "score table")
  expect_error(normalize_scores(s[, c("RAW_ROW_NUMBER", "SCORE")]),
               regexp = "missing score-layer column")

  bad <- s
  bad$SCORE[1] <- NA
  expect_error(normalize_scores(bad), regexp = "contains NA")
})

## ---------------------------------------------------------------------------
## score_mahalanobis()
## ---------------------------------------------------------------------------

test_that("score_mahalanobis() equals the explicit quadratic form", {
  j <- simple_join()
  raw_distinct <- j[!duplicated(j$RAW_ROW_NUMBER), c("RAW_A", "RAW_B")]

  cv <- stats::cov(as.matrix(raw_distinct))
  delta <- as.matrix(j[, c("RAW_A", "RAW_B")]) - as.matrix(j[, c("ANON_A", "ANON_B")])
  expected <- sqrt(rowSums((delta %*% solve(cv)) * delta))

  expect_equal(score_mahalanobis(j, c("A", "B"), ridge = 0)$SCORE, expected)
})

test_that("a single column reduces to the standardised absolute difference", {
  j <- simple_join()
  raw_distinct <- j$RAW_A[!duplicated(j$RAW_ROW_NUMBER)]
  expect_equal(
    score_mahalanobis(j, "A", ridge = 0)$SCORE,
    abs(j$RAW_A - j$ANON_A) / sd(raw_distinct)
  )
})

test_that("Mahalanobis distance is invariant under an invertible linear map of the attributes", {
  ## The defining property, and the reason the metric is worth having: the
  ## answer must not depend on whether spend is recorded in yen or in
  ## thousands of yen, nor on whether two attributes are stored as (x, y) or
  ## as (x + y, x - y).
  j <- simple_join()
  ## ridge = 0 on a correlated pair is ill-conditioned by construction, so the
  ## #59 warning fires throughout. This test is about the invariance.
  base <- suppressWarnings(score_mahalanobis(j, c("A", "B"), ridge = 0)$SCORE)

  transform_join <- function(m) {
    raw <- j[!duplicated(j$RAW_ROW_NUMBER), c("RAW_ROW_NUMBER", "RAW_A", "RAW_B")]
    anon <- j[!duplicated(j$ANON_ROW_NUMBER), c("ANON_ROW_NUMBER", "ANON_A", "ANON_B")]
    names(raw) <- c("ROW_NUMBER", "A", "B")
    names(anon) <- c("ROW_NUMBER", "A", "B")
    apply_map <- function(d) {
      v <- as.matrix(d[, c("A", "B")]) %*% m
      d$A <- v[, 1]
      d$B <- v[, 2]
      d
    }
    join_raw_anon_data(apply_map(raw), apply_map(anon))
  }

  ## pure rescaling of one coordinate
  scaled <- suppressWarnings(
    score_mahalanobis(transform_join(diag(c(100, 1))), c("A", "B"), ridge = 0))
  expect_equal(scaled$SCORE, base)

  ## a shear that mixes the two coordinates
  sheared <- suppressWarnings(score_mahalanobis(
    transform_join(matrix(c(1, 0.7, -0.4, 2), nrow = 2)), c("A", "B"), ridge = 0
  ))
  expect_equal(sheared$SCORE, base)
})

test_that("the covariance is estimated from distinct records, not from repeated candidate rows", {
  ## A record with many candidates must not count many times towards the
  ## covariance: that would let the shape of the candidate table, rather than
  ## the population, decide the metric.
  set.seed(11)
  raw <- data.frame(ROW_NUMBER = 1:3, A = c(0, 1, 5), B = c(0, 2, 3))
  anon <- data.frame(ROW_NUMBER = 1:3, A = c(0.1, 1.2, 4.7), B = c(0.2, 1.8, 3.4))
  full <- join_raw_anon_data(raw, anon)

  ## keep every candidate of RAW 1, but only one candidate of RAW 2 and 3
  lopsided <- full[full$RAW_ROW_NUMBER == 1 | full$RAW_ROW_NUMBER == full$ANON_ROW_NUMBER, ]
  expect_gt(sum(lopsided$RAW_ROW_NUMBER == 1), sum(lopsided$RAW_ROW_NUMBER == 2))

  cv <- stats::cov(as.matrix(raw[, c("A", "B")]))
  delta <- as.matrix(lopsided[, c("RAW_A", "RAW_B")]) -
    as.matrix(lopsided[, c("ANON_A", "ANON_B")])
  expected <- unname(sqrt(rowSums((delta %*% solve(cv)) * delta)))

  expect_equal(score_mahalanobis(lopsided, c("A", "B"), ridge = 0)$SCORE, expected)
})

test_that("cov_from selects which population defines the metric", {
  set.seed(5)
  n <- 10
  raw <- data.frame(ROW_NUMBER = 1:n, A = rnorm(n), B = rnorm(n))
  ## a deliberately differently-shaped ANON population
  anon <- data.frame(ROW_NUMBER = 1:n, A = raw$A * 10 + 1, B = raw$B / 10)
  j <- join_raw_anon_data(raw, anon)

  ## ridge = 0 leaves these ill-conditioned; the #59 warning is the correct
  ## reading of the fixture and is not what this test is about.
  from_raw <- suppressWarnings(
    score_mahalanobis(j, c("A", "B"), cov_from = "raw", ridge = 0)$SCORE)
  from_anon <- suppressWarnings(
    score_mahalanobis(j, c("A", "B"), cov_from = "anon", ridge = 0)$SCORE)
  pooled <- suppressWarnings(
    score_mahalanobis(j, c("A", "B"), cov_from = "pooled", ridge = 0)$SCORE)

  expect_false(isTRUE(all.equal(from_raw, from_anon)))
  expect_false(isTRUE(all.equal(from_raw, pooled)))
  expect_true(all(is.finite(pooled)))
})

test_that("identical records score exactly zero and squared = TRUE squares the distance", {
  j <- simple_join()
  identical_join <- join_raw_anon_data(
    data.frame(ROW_NUMBER = 1:4, A = c(1, 2, 3, 4), B = c(4, 1, 3, 2)),
    data.frame(ROW_NUMBER = 1:4, A = c(1, 2, 3, 4), B = c(4, 1, 3, 2))
  )
  s <- score_mahalanobis(identical_join, c("A", "B"), ridge = 0)
  same <- s$RAW_ROW_NUMBER == s$ANON_ROW_NUMBER
  expect_true(all(s$SCORE[same] == 0))
  expect_true(all(s$SCORE[!same] > 0))

  expect_equal(
    score_mahalanobis(j, c("A", "B"), ridge = 0, squared = TRUE)$SCORE,
    score_mahalanobis(j, c("A", "B"), ridge = 0)$SCORE^2
  )
})

test_that("a constant column is dropped with a warning rather than silently inverted", {
  raw <- data.frame(ROW_NUMBER = 1:5, A = c(1, 2, 3, 4, 5), CONST = rep(2, 5))
  j <- join_raw_anon_data(raw, raw)

  expect_warning(
    got <- score_mahalanobis(j, c("A", "CONST"), ridge = 0),
    regexp = "constant column"
  )
  expect_equal(got$SCORE, score_mahalanobis(j, "A", ridge = 0)$SCORE)
})

test_that("score_mahalanobis() refuses input it cannot honestly score", {
  raw <- data.frame(ROW_NUMBER = 1:5, A = c(1, 2, 3, 4, 5), CONST = rep(2, 5),
                    TXT = letters[1:5])
  j <- join_raw_anon_data(raw, raw)

  expect_error(score_mahalanobis(j, c("CONST")), regexp = "constant")
  expect_error(score_mahalanobis(j, "TXT"), regexp = "not numeric")
  expect_error(score_mahalanobis(j, c("A", "A")), regexp = "more than once")
  expect_error(score_mahalanobis(j, character(0)), regexp = "at least")
  expect_error(score_mahalanobis(j, "MISSING"), regexp = "MISSING")
  expect_error(score_mahalanobis(j, "A", ridge = -1), regexp = "non-negative")

  ## a single record gives no covariance to speak of
  one <- join_raw_anon_data(data.frame(ROW_NUMBER = 1, A = 1, B = 2),
                            data.frame(ROW_NUMBER = 1, A = 1, B = 2))
  expect_error(score_mahalanobis(one, c("A", "B")), regexp = "at least 2 distinct")

  ## an NA would silently drop out of the quadratic form and report a shorter
  ## distance -- i.e. a more confident match -- than the data supports
  j_na <- j
  j_na$ANON_A[1] <- NA
  expect_error(score_mahalanobis(j_na, "A"), regexp = "NA")
})

test_that("ridge keeps an exactly collinear pair invertible", {
  set.seed(9)
  n <- 12
  a <- rnorm(n)
  raw <- data.frame(ROW_NUMBER = 1:n, A = a, B = 2 * a)
  anon <- raw
  anon$A <- a + rnorm(n, sd = 0.1)
  anon$B <- 2 * anon$A
  j <- join_raw_anon_data(raw, anon)

  ## B = 2A exactly, so the #59 condition-number warning is the correct
  ## reading of this fixture; what is under test is that ridge keeps it usable.
  expect_error(suppressWarnings(score_mahalanobis(j, c("A", "B"), ridge = 0)))
  got <- suppressWarnings(score_mahalanobis(j, c("A", "B"), ridge = 1e-6))
  expect_true(all(is.finite(got$SCORE)))
  expect_true(all(got$SCORE >= 0))
})

## ---------------------------------------------------------------------------
## score_multi()
## ---------------------------------------------------------------------------

test_that("score_multi() with one target is that target's score, normalised", {
  j <- simple_join()
  expect_equal(score_multi(j, c(A = "num"), normalize = "none")$SCORE,
               score_num(j, "A")$SCORE)
  expect_equal(score_multi(j, c(A = "num"), normalize = "range")$SCORE,
               normalize_scores(score_num(j, "A"), "range")$SCORE)
})

test_that("an unnamed target vector is read as all-numeric", {
  j <- simple_join()
  expect_equal(score_multi(j, c("A", "B"))$SCORE,
               score_multi(j, c(A = "num", B = "num"))$SCORE)
})

test_that("score_multi() is the normalised weighted sum of its parts", {
  j <- simple_join()
  parts <- normalize_scores(list(score_num(j, "A"), score_char(j, "B")), "range")

  ## B ranks the true record 0.523 of the way down the candidate list on this
  ## n = 8 fixture -- no better than the 0.5 of chance -- so the #35 screen
  ## warns. True positive, asserted rather than suppressed (#43). The test is
  ## about the weighted-sum arithmetic, which is unaffected by the screen at
  ## the default screen = "warn".
  ##
  ## The assignment is inside expect_warning() because expect_warning()
  ## returns the condition, not the value of the expression.
  multi <- NULL
  expect_warning(
    multi <- score_multi(j, c(A = "num", B = "char"), weights = c(2, 5)),
    "show no signal"
  )
  expect_equal(multi$SCORE, combine_scores(parts, weights = c(2, 5))$SCORE)
})

test_that("score_multi() dispatches every declared score type", {
  d <- create_dummy_qi_data(people = 12, seed = 4)
  j <- join_raw_anon_data(d, d)
  spec <- c(AGE = "num", ZIP = "char", SPEND_DIST = "dist", VISIT_COUNT = "rank")

  parts <- normalize_scores(list(
    score_num(j, "AGE"), score_char(j, "ZIP"),
    score_dist(j, "SPEND_DIST"), score_num_rank(j, "VISIT_COUNT")
  ), "range")

  expect_equal(score_multi(j, spec)$SCORE, combine_scores(parts)$SCORE)
})

test_that("method = 'mahalanobis' handles the numeric block jointly and the rest separately", {
  d <- create_dummy_qi_data(people = 15, seed = 6)
  j <- join_raw_anon_data(d, d)
  spec <- c(AGE = "num", SPEND_MEAN = "num", ZIP = "char")

  parts <- normalize_scores(list(
    score_mahalanobis(j, c("AGE", "SPEND_MEAN")),
    score_char(j, "ZIP")
  ), "range")

  ## the numeric block inherits the combined weight of the columns it absorbed
  expect_equal(
    score_multi(j, spec, method = "mahalanobis", weights = c(2, 3, 5))$SCORE,
    combine_scores(parts, weights = c(5, 5))$SCORE
  )
})

test_that("method = 'mahalanobis' needs at least one numeric column", {
  d <- create_dummy_qi_data(people = 10, seed = 2)
  j <- join_raw_anon_data(d, d)
  expect_error(
    score_multi(j, c(ZIP = "char", SEX = "char"), method = "mahalanobis"),
    regexp = "at least one column of type"
  )
})

test_that("adding a column that separates nothing leaves the combined ranking alone", {
  raw <- data.frame(ROW_NUMBER = 1:6, A = 1:6, CONST = rep(7, 6))
  j <- join_raw_anon_data(raw, raw)

  ## CONST is constant by construction -- the whole point of this test -- so
  ## the #35 screen firing on it is the correct behaviour. Asserting the
  ## warning (#43) records that the constant column is intentional and keeps
  ## the screen under test: suppressWarnings() would hide a broken screen,
  ## expect_warning() fails if the screen stops detecting it.
  both <- NULL
  expect_warning(both <- score_multi(j, c(A = "num", CONST = "num")), "CONST")
  expect_equal(score_multi(j, c(A = "num"))$SCORE, both$SCORE)
})

test_that("score_multi() rejects malformed target specifications", {
  j <- simple_join()
  expect_error(score_multi(j, c(A = "bogus")), regexp = "unknown score type")
  expect_error(score_multi(j, c(A = "num", A = "num")), regexp = "more than once")
  expect_error(score_multi(j, c(A = "num", "B")), regexp = "mixes named and unnamed")
  expect_error(score_multi(j, character(0)), regexp = "at least one column")
  expect_error(score_multi(j, c(A = "num", B = "num"), weights = 1),
               regexp = "one entry per target column")
})

test_that("score_multi() output is a usable score table over the same candidate pairs", {
  j <- simple_join()
  s <- score_multi(j, c(A = "num", B = "num"))
  expect_s3_class(s, "reid_scores")
  expect_equal(attr(s, "score_type"), "distance")
  expect_equal(nrow(s), nrow(j))
  expect_equal(nrow(match_greedy(s)), length(unique(j$ANON_ROW_NUMBER)))
})

## ---------------------------------------------------------------------------
## the acceptance criteria on the issue
## ---------------------------------------------------------------------------

test_that("a multi-attribute attack beats every single-attribute attack", {
  j <- simple_join(seed = 21, n = 60, noise = 0.6)
  singles <- vapply(c("A", "B", "C"),
                    function(v) reid_evaluate(score_num(j, v), seeds = 1:5)$success_analytic,
                    numeric(1))
  multi <- reid_evaluate(score_multi(j, c("A", "B", "C")), seeds = 1:5)$success_analytic

  expect_gt(multi, max(singles))
})

test_that("normalisation is what makes columns on different scales combinable", {
  ## Three independent columns whose units are orders of magnitude apart, each
  ## perturbed by the same *relative* amount. Without normalisation the widest
  ## column decides on its own and the other two are wasted.
  set.seed(31)
  n <- 60
  raw <- data.frame(ROW_NUMBER = seq_len(n), SMALL = rnorm(n, sd = 1),
                    MEDIUM = rnorm(n, sd = 50), LARGE = rnorm(n, sd = 5000))
  anon <- raw
  for (v in c("SMALL", "MEDIUM", "LARGE")) {
    anon[[v]] <- raw[[v]] + rnorm(n, sd = 0.3 * sd(raw[[v]]))
  }
  j <- join_raw_anon_data(raw, anon)
  spec <- c(SMALL = "num", MEDIUM = "num", LARGE = "num")

  ## Without normalisation the widest column dominates, and #57 made
  ## combine_scores() say so instead of doing it silently.
  expect_warning(
    unnormalised <- score_multi(j, spec, normalize = "none"),
    regexp = "very different scales"
  )
  none <- reid_evaluate(unnormalised, seeds = 1:5)$success_analytic
  scaled <- vapply(
    c("range", "zscore", "rank"),
    function(m) reid_evaluate(score_multi(j, spec, normalize = m), seeds = 1:5)$success_analytic,
    numeric(1)
  )
  expect_true(all(scaled > none))

  ## and the unnormalised sum sits far closer to the widest column on its own
  ## than to any normalised combination: the other two columns are effectively
  ## thrown away
  largest <- reid_evaluate(score_num(j, "LARGE"), seeds = 1:5)$success_analytic
  expect_lt(none - largest, 0.25 * (min(scaled) - largest))
})

test_that("on correlated attributes Mahalanobis beats the plain weighted sum", {
  ## Measured across data seeds rather than on one draw: on the generator
  ## below, docs/investigation/multiattr-benchmark.R finds Mahalanobis ahead in
  ## 10 seeds out of 10 by +0.23 success rate on average.
  spec <- c(A = "num", B = "num", C = "num")
  deltas <- vapply(1:5, function(s) {
    j <- redundant_join(seed = s)
    w <- reid_evaluate(score_multi(j, spec), seeds = 1:5)$success_analytic
    ## The #59 condition-number warning fires on every seed here; the test
    ## below is about precisely that.
    m <- reid_evaluate(
      suppressWarnings(score_multi(j, spec, method = "mahalanobis")),
      seeds = 1:5)$success_analytic
    m - w
  }, numeric(1))

  expect_true(all(deltas > 0))
  expect_gt(mean(deltas), 0.1)
})

test_that("the #59 condition-number warning marks fragility, not a wrong answer", {
  ## The fixture above is ill-conditioned AND Mahalanobis wins on it. Reading
  ## the warning as "this number is too low" would be wrong here, so the
  ## message says the result is fragile and tells the caller to compare
  ## against method = "weighted" -- which is what the test above does.
  spec <- c(A = "num", B = "num", C = "num")
  j <- redundant_join(seed = 1)

  expect_warning(m <- score_multi(j, spec, method = "mahalanobis"),
                 regexp = "ill-conditioned")

  w_rate <- reid_evaluate(score_multi(j, spec), seeds = 1:5)$success_analytic
  m_rate <- reid_evaluate(m, seeds = 1:5)$success_analytic
  expect_gt(m_rate, w_rate)
})

test_that("Mahalanobis does not hurt when the attributes are in fact independent", {
  ## The control for the test above. With uncorrelated attributes perturbed
  ## isotropically, plain Euclidean distance is already the right rule, so the
  ## most that can be claimed is that whitening costs nothing much. Stating it
  ## as a test stops "Mahalanobis is better" from quietly becoming "always use
  ## Mahalanobis".
  spec <- c(A = "num", B = "num", C = "num")
  deltas <- vapply(1:5, function(s) {
    j <- simple_join(seed = 40 + s, n = 60, noise = 0.6)
    m <- reid_evaluate(score_multi(j, spec, method = "mahalanobis"),
                       seeds = 1:5)$success_analytic
    w <- reid_evaluate(score_multi(j, spec), seeds = 1:5)$success_analytic
    m - w
  }, numeric(1))

  expect_gt(mean(deltas), -0.05)
})

## ---------------------------------------------------------------------------
## score_by_knowledge() now goes through score_multi() (#13 -> #14)
## ---------------------------------------------------------------------------

test_that("score_by_knowledge() still produces exactly the #13 stopgap score by default", {
  d <- create_dummy_qi_data(people = 20, seed = 8)
  j <- join_raw_anon_data(d, d)
  k <- dummy_qi_knowledge("M")

  ## the pre-#14 formula, written out: per-column range rescaling, then sum
  stopgap <- lapply(seq_along(k$visible), function(i) {
    target <- names(k$visible)[i]
    s <- switch(
      unname(k$visible[i]),
      num = score_num(j, target),
      char = score_char(j, target),
      dist = score_dist(j, target),
      rank = score_num_rank(j, target)
    )
    rng <- range(s$SCORE)
    span <- rng[2] - rng[1]
    s$SCORE <- if (span > 0) (s$SCORE - rng[1]) / span else 0
    s
  })

  expect_equal(score_by_knowledge(j, k)$SCORE, combine_scores(stopgap)$SCORE)
})

test_that("score_by_knowledge() accepts the normalisations #14 added", {
  d <- create_dummy_qi_data(people = 20, seed = 8)
  j <- join_raw_anon_data(d, d)
  k <- dummy_qi_knowledge("M")

  for (m in c("range", "zscore", "rank", "none")) {
    ## normalize = "none" trips the #57 scale check by construction; this test
    ## is about the argument being accepted at all.
    s <- suppressWarnings(score_by_knowledge(j, k, normalize = m))
    expect_equal(nrow(s), nrow(j), info = m)
    expect_false(anyNA(s$SCORE), info = m)
  }
  expect_error(score_by_knowledge(j, k, normalize = "bogus"))
})

test_that("W < M < S stays strictly increasing under every normalisation", {
  ## The reason #14 exists at all: #13 could only make the knowledge levels
  ## comparable by bolting a range rescaling onto score_by_knowledge(). Now
  ## that the rescaling lives in score_multi(), the comparison must survive
  ## every choice it offers -- otherwise the conclusion was an artefact of the
  ## one normalisation that happened to be hard-coded.
  d <- create_dummy_qi_data(people = 60, seed = 7)
  anon <- d
  anon$AGE <- (anon$AGE %/% 10) * 10
  anon$ZIP <- substr(anon$ZIP, 1, 3)
  anon$VISIT_COUNT <- (anon$VISIT_COUNT %/% 5) * 5
  anon$SPEND_MEAN <- round(anon$SPEND_MEAN / 25) * 25
  anon$SPEND_DIST <- vapply(
    strsplit(anon$SPEND_DIST, ":", fixed = TRUE),
    function(v) paste(round(as.numeric(v) / 20) * 20, collapse = ":"),
    character(1)
  )
  j <- join_raw_anon_data(d, anon)

  qi_args <- list(
    quasi_identifiers = c(ZIP = "char", AGE = "num", SEX = "char"),
    behavior = c(VISIT_COUNT = "num", SPEND_MEAN = "num", SPEND_DIST = "dist"),
    identifiers = c(FINGERPRINT = "num"),
    weak_subset = "ZIP"
  )

  for (m in c("range", "zscore", "rank")) {
    ## FINGERPRINT alone identifies everyone, so at the S level the equal-weight
    ## combination is below its own best axis and the #59 check says so. That
    ## is a true positive about equal weighting; this test is about the
    ## ordering of the levels.
    curve <- suppressWarnings(
      do.call(reid_knowledge_curve,
              c(list(j, seeds = 1:5, normalize = m), qi_args)))
    expect_true(all(diff(curve$success_analytic) > 0), info = m)
  }

  ## and without any normalisation it is *not* strictly increasing -- the
  ## measurement that made #13 add the stopgap in the first place
  flat <- suppressWarnings(
    do.call(reid_knowledge_curve,
            c(list(j, seeds = 1:5, normalize = "none"), qi_args))
  )
  expect_false(all(diff(flat$success_analytic) > 0))
})

test_that("the knowledge model can be attacked with the Mahalanobis metric too", {
  d <- create_dummy_qi_data(people = 20, seed = 8)
  j <- join_raw_anon_data(d, d)
  k <- dummy_qi_knowledge("M")

  s <- score_by_knowledge(j, k, method = "mahalanobis")
  expect_s3_class(s, "reid_scores")
  expect_equal(nrow(s), nrow(j))
  expect_false(isTRUE(all.equal(s$SCORE, score_by_knowledge(j, k)$SCORE)))
})

## ---------------------------------------------------------------------------
## #59: whitening is fragile exactly where it is recommended
## ---------------------------------------------------------------------------

## Same shape as redundant_join(), but the release perturbs every column
## independently instead of along the latent direction. That one change flips
## Mahalanobis from the best metric on this data to the worst.
redundant_join_iso <- function(n = 120, noise = 0.5, seed = 1) {
  set.seed(seed)
  latent <- rnorm(n)
  raw <- data.frame(
    ROW_NUMBER = seq_len(n),
    A = latent + rnorm(n, sd = 0.05),
    B = 3 * latent + rnorm(n, sd = 0.15),
    C = rnorm(n)
  )
  anon <- data.frame(
    ROW_NUMBER = seq_len(n),
    A = raw$A + rnorm(n, sd = noise),
    B = raw$B + rnorm(n, sd = noise),
    C = raw$C + rnorm(n, sd = noise)
  )
  join_raw_anon_data(raw, anon)
}

test_that("score_mahalanobis() warns on an ill-conditioned covariance", {
  j <- redundant_join_iso()
  expect_warning(score_mahalanobis(j, c("A", "B", "C")),
                 regexp = "ill-conditioned")

  w <- tryCatch(score_mahalanobis(j, c("A", "B", "C")), warning = conditionMessage)
  expect_match(w, "condition number")
  ## the message must not tell the reader which way the error goes: it does
  ## not know, and claiming a direction is what makes #59 hard to spot
  expect_match(w, "WEAKER")
  expect_match(w, "STRONGER")
})

test_that("a well-conditioned covariance does not warn", {
  set.seed(59)
  n <- 80
  raw <- data.frame(ROW_NUMBER = seq_len(n), A = rnorm(n), B = rnorm(n),
                    C = rnorm(n))
  anon <- raw
  for (cn in c("A", "B", "C")) anon[[cn]] <- anon[[cn]] + rnorm(n, sd = 0.3)
  j <- join_raw_anon_data(raw, anon)

  expect_silent(score_mahalanobis(j, c("A", "B", "C")))
})

test_that("a single column is never called ill-conditioned", {
  ## kappa() of a 1x1 matrix is 1 by definition; the check must not invent a
  ## warning for a metric that is doing nothing but rescaling.
  set.seed(59)
  raw <- data.frame(ROW_NUMBER = 1:20, A = rnorm(20))
  j <- join_raw_anon_data(raw, raw)
  expect_silent(score_mahalanobis(j, "A"))
})

test_that("under an isotropic release, whitening loses to the weighted sum", {
  ## The measurement behind #59, on the repository's own generator with the
  ## perturbation made isotropic. The metric README recommends for correlated
  ## columns is the weaker attack here -- the direction a safety tool must not
  ## take quietly.
  spec <- c(A = "num", B = "num", C = "num")
  j <- redundant_join_iso()

  w <- reid_evaluate(score_multi(j, spec), seeds = 1:5)$success_analytic
  m <- reid_evaluate(suppressWarnings(score_multi(j, spec, method = "mahalanobis")),
                     seeds = 1:5)$success_analytic

  expect_lt(m, w)
  ## ... and the caller is told, rather than left with a plausible-looking
  ## smaller number. Both checks fire here, so the warnings are collected
  ## rather than matched one at a time.
  msgs <- character(0)
  withCallingHandlers(
    score_multi(j, spec, method = "mahalanobis"),
    warning = function(x) {
      msgs <<- c(msgs, conditionMessage(x))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("ill-conditioned", msgs)))
})

test_that("score_multi() warns when the combination measures less than one axis alone", {
  ## Single-axis screening cannot see this: every column is informative on its
  ## own, so #35's screen passes and the combined number still understates.
  spec <- c(A = "num", B = "num", C = "num")
  j <- redundant_join_iso()

  report <- axis_informativeness(list(
    A = score_num(j, "A"), B = score_num(j, "B"), C = score_num(j, "C")))
  expect_true(all(report$informative))

  msgs <- character(0)
  s <- withCallingHandlers(
    score_multi(j, spec, method = "mahalanobis"),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("measures LESS risk", msgs)))
  expect_true(any(grepl("ill-conditioned", msgs)))
})

test_that("a combination that is at least as good as every axis stays quiet", {
  spec <- c(A = "num", B = "num", C = "num")
  j <- redundant_join()
  ## the aligned-perturbation fixture: Mahalanobis wins, so only the
  ## conditioning warning is allowed to fire
  msgs <- character(0)
  withCallingHandlers(
    score_multi(j, spec, method = "mahalanobis"),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("measures LESS risk", msgs)))
})

test_that("the combination check keeps quiet when screening already spoke", {
  ## Two warnings about the same fixture in different words trains the reader
  ## to skip both, so the #59 check defers to #35's screen.
  set.seed(59)
  n <- 60
  raw <- data.frame(ROW_NUMBER = seq_len(n), SIGNAL = seq_len(n),
                    N1 = rnorm(n), N2 = rnorm(n), N3 = rnorm(n))
  anon <- raw
  anon$SIGNAL <- anon$SIGNAL + rnorm(n, sd = 3)
  ## redrawn, so the three noise columns carry nothing about identity
  for (cn in c("N1", "N2", "N3")) anon[[cn]] <- rnorm(n)
  j <- join_raw_anon_data(raw, anon)

  ## the combination really is worse than SIGNAL alone here (0.0333 vs 0.2167)
  spec <- c(SIGNAL = "num", N1 = "num", N2 = "num", N3 = "num")
  combined <- suppressWarnings(score_multi(j, spec, screen = "none"))
  expect_lt(reid_evaluate(combined, seeds = 1:5)$success_analytic,
            reid_evaluate(score_num(j, "SIGNAL"), seeds = 1:5)$success_analytic)

  ## ... and #35's screen is what reports it, exactly once
  msgs <- character(0)
  withCallingHandlers(
    score_multi(j, spec),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("no signal", msgs)))
  expect_false(any(grepl("measures LESS risk", msgs)))
})
