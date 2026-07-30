## Tests for rarity-weighted exact matching (#17).
##
## The acceptance criterion on the issue -- "beats the unweighted match score"
## -- is checked at the bottom on skewed categorical data, the same generator
## docs/investigation/idf-benchmark.R measures across seeds.

## Categorical columns with Zipf-like value frequencies: a few very common
## values and a long nearly-unique tail. The anonymiser replaces each cell
## independently with a fresh draw with probability `corrupt`.
zipf_join <- function(n = 150, p = 5, k = 15, corrupt = 0.4, s = 1.2, seed = 1) {
  set.seed(seed)
  probs <- seq_len(k)^(-s)
  probs <- probs / sum(probs)
  values <- sprintf("v%02d", seq_len(k))

  raw <- data.frame(ROW_NUMBER = seq_len(n), stringsAsFactors = FALSE)
  for (i in seq_len(p)) {
    raw[[paste0("X", i)]] <- sample(values, n, replace = TRUE, prob = probs)
  }
  anon <- raw
  for (i in seq_len(p)) {
    col <- paste0("X", i)
    hit <- runif(n) < corrupt
    anon[[col]][hit] <- sample(values, sum(hit), replace = TRUE, prob = probs)
  }
  join_raw_anon_data(raw, anon)
}

## Six records: "common" appears four times, "rare" and "unique" once each.
skewed_join <- function() {
  raw <- data.frame(
    ROW_NUMBER = 1:6,
    G = c("common", "common", "common", "common", "rare", "unique"),
    H = c("a", "a", "b", "b", "c", "d"),
    stringsAsFactors = FALSE
  )
  join_raw_anon_data(raw, raw)
}

## ---------------------------------------------------------------------------
## value_frequencies()
## ---------------------------------------------------------------------------

test_that("value_frequencies() counts records, not candidate pairs", {
  j <- skewed_join()
  f <- value_frequencies(j, "G")

  expect_equal(attr(f, "n_records"), 6L)
  expect_setequal(f$VALUE, c("common", "rare", "unique"))
  expect_equal(f$COUNT[f$VALUE == "common"], 4L)
  expect_equal(f$COUNT[f$VALUE == "rare"], 1L)
  expect_equal(sum(f$COUNT), 6L)
  expect_equal(sum(f$SHARE), 1)

  ## the candidate table has 36 rows; counting those would give 24 / 6 / 6
  expect_equal(nrow(j), 36L)
})

test_that("value_frequencies() counts each side independently", {
  raw <- data.frame(ROW_NUMBER = 1:4, G = c("a", "a", "a", "b"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:4, G = c("a", "b", "b", "b"),
                     stringsAsFactors = FALSE)
  j <- join_raw_anon_data(raw, anon)

  from_raw <- value_frequencies(j, "G", source = "raw")
  from_anon <- value_frequencies(j, "G", source = "anon")
  pooled <- value_frequencies(j, "G", source = "pooled")

  expect_equal(from_raw$COUNT[from_raw$VALUE == "a"], 3L)
  expect_equal(from_anon$COUNT[from_anon$VALUE == "a"], 1L)
  expect_equal(pooled$COUNT[pooled$VALUE == "a"], 4L)
  expect_equal(attr(pooled, "n_records"), 8L)
})

test_that("value_frequencies() is sorted by decreasing count and rejects NA", {
  f <- value_frequencies(skewed_join(), "G")
  expect_equal(f$COUNT, sort(f$COUNT, decreasing = TRUE))

  j <- skewed_join()
  j$ANON_G[1] <- NA
  expect_error(value_frequencies(j, "G"), regexp = "contains NA")
})

## ---------------------------------------------------------------------------
## idf_weight()
## ---------------------------------------------------------------------------

test_that("idf_weight() implements the documented formulas", {
  expect_equal(idf_weight(c(1, 2, 10), 100, "idf"), log(100 / c(1, 2, 10)))
  expect_equal(idf_weight(c(1, 2, 10), 100, "inv_log"), 1 / log(c(1, 2, 10) + 1))
  expect_equal(idf_weight(c(1, 2, 10), 100, "inv"), 1 / c(1, 2, 10))
  expect_equal(idf_weight(c(1, 2, 10), 100, "none"), c(1, 1, 1))
})

test_that("a value every record shares carries no information and gets weight 0", {
  expect_equal(idf_weight(50, 50, "idf"), 0)
  ## and a count that somehow exceeds n is clamped rather than going negative:
  ## a negative weight would turn a disagreement into evidence *for* a match
  expect_equal(idf_weight(60, 50, "idf"), 0)
})

test_that("the weight is non-increasing in the count for every scheme", {
  counts <- 1:50
  for (m in c("idf", "inv_log", "inv")) {
    w <- idf_weight(counts, 50, m)
    expect_true(all(diff(w) <= 0), info = m)
  }
})

test_that("the shifted inv_log stays finite on a singleton value", {
  ## Issue #17 names `w = 1 / log(freq)`. At freq = 1 that is 1 / log(1) =
  ## 1 / 0 = Inf, and freq = 1 is precisely the case rarity weighting exists
  ## for -- so the formula as written diverges exactly where it matters.
  ## The implementation shifts to 1 / log(freq + 1).
  expect_true(is.infinite(1 / log(1)))
  expect_true(all(is.finite(idf_weight(1:5, 100, "inv_log"))))
  expect_equal(idf_weight(1, 100, "inv_log"), 1 / log(2))
})

test_that("an unseen value is floored at singleton rarity, not given infinite weight", {
  expect_equal(idf_weight(0, 100, "idf"), idf_weight(1, 100, "idf"))
  expect_true(is.finite(idf_weight(0, 100, "inv")))
})

## ---------------------------------------------------------------------------
## score_idf()
## ---------------------------------------------------------------------------

test_that("score_idf() is 0 on agreement and the value's rarity weight otherwise", {
  j <- skewed_join()
  s <- score_idf(j, "G")

  agree <- as.character(j$RAW_G) == as.character(j$ANON_G)
  expect_true(all(s$SCORE[agree] == 0))
  expect_true(all(s$SCORE[!agree] > 0))

  ## the penalty is keyed on the ANON record's value
  counts <- c(common = 4, rare = 1, unique = 1)
  expected <- idf_weight(counts[as.character(j$ANON_G)], 6, "idf") * !agree
  expect_equal(s$SCORE, unname(expected))
})

test_that("disagreeing with a rare value costs more than disagreeing with a common one", {
  j <- skewed_join()
  s <- score_idf(j, "G")
  by_anon <- tapply(s$SCORE, as.character(j$ANON_G), max)

  expect_gt(by_anon[["unique"]], by_anon[["common"]])
  expect_equal(by_anon[["unique"]], by_anon[["rare"]])
})

test_that("weight = 'none' reduces to a plain 0/1 mismatch indicator", {
  j <- skewed_join()
  s <- score_idf(j, "G", weight = "none")
  expect_equal(s$SCORE, as.numeric(as.character(j$RAW_G) != as.character(j$ANON_G)))
})

test_that("the distance form ranks identically to the similarity form it is derived from", {
  ## sim  = sum over columns of w(v) * 1[agree]
  ## dist = sum over columns of w(v) * 1[disagree] = W(anon) - sim
  ## W(anon) does not depend on which RAW candidate is considered, so the two
  ## must pick the same winner for every ANON record. If they ever diverged,
  ## the package would be reporting a different attack from the one the method
  ## is written as.
  j <- zipf_join(n = 40, p = 4, seed = 5)
  targets <- paste0("X", 1:4)

  dist <- score_idf_match(j, targets)

  sim <- rowSums(vapply(targets, function(t) {
    f <- value_frequencies(j, t)
    a <- as.character(j[[paste0("ANON_", t)]])
    w <- idf_weight(f$COUNT[match(a, f$VALUE)], attr(f, "n_records"), "idf")
    w * (as.character(j[[paste0("RAW_", t)]]) == a)
  }, numeric(nrow(j))))
  sim_scores <- new_reid_scores(j$RAW_ROW_NUMBER, j$ANON_ROW_NUMBER, sim,
                                score_type = "similarity")

  by_dist <- match_greedy(dist, seed = 11)
  by_sim <- match_greedy(sim_scores, seed = 11)
  expect_equal(by_dist$RAW_ROW_NUMBER, by_sim$RAW_ROW_NUMBER)
  expect_equal(by_dist$CONFIDENCE, by_sim$CONFIDENCE)
})

test_that("score_idf() rejects NA rather than guessing what a missing value matches", {
  j <- skewed_join()
  j$RAW_G[1] <- NA
  expect_error(score_idf(j, "G"), regexp = "NA")
})

## ---------------------------------------------------------------------------
## score_idf_match()
## ---------------------------------------------------------------------------

test_that("score_idf_match() is the unnormalised sum of its columns", {
  j <- skewed_join()
  expect_equal(
    score_idf_match(j, c("G", "H"))$SCORE,
    score_idf(j, "G")$SCORE + score_idf(j, "H")$SCORE
  )
})

test_that("score_idf_match() keeps the columns at their own scale", {
  ## The point of the method is that a rare column outweighs a common one. If
  ## the block normalised each column first, every column would end up with
  ## the same influence and the weighting would be undone.
  j <- skewed_join()
  s <- score_idf_match(j, c("G", "H"))
  normalised <- combine_scores(
    normalize_scores(list(score_idf(j, "G"), score_idf(j, "H")), "range")
  )
  expect_false(isTRUE(all.equal(s$SCORE, normalised$SCORE)))
})

test_that("score_idf_match() rejects a repeated column", {
  expect_error(score_idf_match(skewed_join(), c("G", "G")),
               regexp = "more than once")
  expect_error(score_idf_match(skewed_join(), character(0)), regexp = "at least one")
})

## ---------------------------------------------------------------------------
## integration with #14
## ---------------------------------------------------------------------------

test_that("'idf' is a score type the knowledge model and score_multi() accept", {
  expect_true("idf" %in% reid_score_types())
  k <- attacker_knowledge("M", quasi_identifiers = c(G = "idf", H = "idf"))
  expect_equal(unname(k$visible[["G"]]), "idf")
})

test_that("score_multi() scores all 'idf' columns as one block, not one at a time", {
  j <- zipf_join(n = 60, p = 3, k = 8, seed = 4)
  targets <- paste0("X", 1:3)
  spec <- setNames(rep("idf", 3), targets)

  ## one block, normalised once, carrying the summed weight of its columns
  block <- combine_scores(
    list(normalize_scores(score_idf_match(j, targets), "range")),
    weights = 3
  )
  expect_equal(score_multi(j, spec)$SCORE, block$SCORE)

  ## normalising each column first would rescale every column to the same
  ## range and undo the rarity weighting across columns, which is why the
  ## block exists
  per_column <- combine_scores(
    normalize_scores(lapply(targets, function(t) score_idf(j, t)), "range")
  )
  expect_false(isTRUE(all.equal(score_multi(j, spec)$SCORE, per_column$SCORE)))
})

test_that("normalising the idf columns separately can reverse which candidate wins", {
  ## Not just different numbers -- a different answer. G holds a rare value,
  ## H does not, so G's weights span a wider range. Rescaling each column to
  ## [0, 1] hands H the same influence as G, which is exactly the cross-column
  ## weighting the method is supposed to supply.
  raw <- data.frame(
    ROW_NUMBER = 1:8,
    G = c("a", "a", "a", "a", "b", "b", "b", "c"),
    H = c("p", "q", "p", "q", "p", "q", "p", "q"),
    stringsAsFactors = FALSE
  )
  j <- join_raw_anon_data(raw, raw)

  block <- score_idf_match(j, c("G", "H"))
  per_column <- combine_scores(
    normalize_scores(list(score_idf(j, "G"), score_idf(j, "H")), "range")
  )

  ## ANON record 6 is (G = b, H = q). Candidate 5 is (b, p): it agrees on the
  ## rarer column. Candidate 2 is (a, q): it agrees on the commoner one.
  pick <- function(s, raw_row) {
    s$SCORE[s$ANON_ROW_NUMBER == 6 & s$RAW_ROW_NUMBER == raw_row]
  }
  expect_lt(pick(block, 5), pick(block, 2))
  expect_gt(pick(per_column, 5), pick(per_column, 2))
})

test_that("the idf block takes the summed weight of the columns it absorbed", {
  d <- create_dummy_qi_data(people = 20, seed = 4)
  j <- join_raw_anon_data(d, d)

  got <- score_multi(j, c(ZIP = "idf", SEX = "idf", AGE = "num"),
                     weights = c(2, 3, 4))
  expected <- combine_scores(
    normalize_scores(list(score_idf_match(j, c("ZIP", "SEX")), score_num(j, "AGE")),
                     "range"),
    weights = c(5, 4)
  )
  expect_equal(got$SCORE, expected$SCORE)
})

test_that("idf and mahalanobis blocks coexist and each absorbs its own columns", {
  d <- create_dummy_qi_data(people = 20, seed = 4)
  j <- join_raw_anon_data(d, d)

  got <- score_multi(j, c(ZIP = "idf", AGE = "num", SPEND_MEAN = "num", SEX = "char"),
                     method = "mahalanobis")
  expected <- combine_scores(
    normalize_scores(list(
      score_mahalanobis(j, c("AGE", "SPEND_MEAN")),
      score_idf_match(j, "ZIP"),
      score_char(j, "SEX")
    ), "range"),
    ## the Mahalanobis block absorbed two columns, the idf block one
    weights = c(2, 1, 1)
  )
  expect_equal(got$SCORE, expected$SCORE)
})

test_that("score_by_knowledge() passes the idf options through", {
  d <- create_dummy_qi_data(people = 20, seed = 4)
  j <- join_raw_anon_data(d, d)
  k <- attacker_knowledge("M", quasi_identifiers = c(ZIP = "idf", SEX = "idf"))

  expect_equal(
    score_by_knowledge(j, k)$SCORE,
    2 * normalize_scores(score_idf_match(j, c("ZIP", "SEX")), "range")$SCORE
  )
  expect_false(isTRUE(all.equal(
    score_by_knowledge(j, k)$SCORE,
    score_by_knowledge(j, k, weight = "none")$SCORE
  )))
})

## ---------------------------------------------------------------------------
## the acceptance criterion on the issue
## ---------------------------------------------------------------------------

test_that("rarity weighting beats unweighted exact matching on skewed data", {
  targets <- paste0("X", 1:5)
  deltas <- vapply(1:5, function(s) {
    j <- zipf_join(seed = s)
    weighted <- reid_evaluate(score_idf_match(j, targets), seeds = 1:5)$success_analytic
    plain <- reid_evaluate(score_idf_match(j, targets, weight = "none"),
                           seeds = 1:5)$success_analytic
    weighted - plain
  }, numeric(1))

  expect_true(all(deltas > 0))
  expect_gt(mean(deltas), 0.02)
})

test_that("with exactly equal value frequencies the weighting cannot change anything", {
  ## Every value shares one count, so every weight is the same constant and
  ## the score is the unweighted mismatch count times that constant -- a
  ## positive rescaling, which cannot reorder any candidate. This is the clean
  ## statement of "rarity weighting needs rarity to differ"; note that it is
  ## about *realised* counts, not about the values being drawn uniformly (see
  ## the uniform-draw result in docs/investigation/idf-benchmark-log.txt).
  raw <- data.frame(
    ROW_NUMBER = 1:6,
    G = c("a", "a", "b", "b", "c", "c"),
    H = c("p", "q", "p", "q", "p", "q"),
    stringsAsFactors = FALSE
  )
  j <- join_raw_anon_data(raw, raw)

  weighted <- score_idf_match(j, c("G", "H"))
  plain <- score_idf_match(j, c("G", "H"), weight = "none")

  expect_equal(
    reid_evaluate(weighted, seeds = 1:5)$success_analytic,
    reid_evaluate(plain, seeds = 1:5)$success_analytic
  )
  expect_equal(match_greedy(weighted, seed = 3)$RAW_ROW_NUMBER,
               match_greedy(plain, seed = 3)$RAW_ROW_NUMBER)
})

test_that("counting frequencies off the released ANON table costs nothing", {
  ## The argument for this method being cheap is that the attacker needs no
  ## extra knowledge: the frequencies are visible in the published data. That
  ## only holds if using the ANON side is about as good as using the RAW side.
  targets <- paste0("X", 1:5)
  from <- vapply(c("anon", "raw", "pooled"), function(src) {
    mean(vapply(1:3, function(s) {
      reid_evaluate(score_idf_match(zipf_join(seed = s), targets, source = src),
                    seeds = 1:5)$success_analytic
    }, numeric(1)))
  }, numeric(1))

  expect_lt(abs(from[["anon"]] - from[["raw"]]), 0.02)
})

test_that("an idf block added to a numeric attack raises the success rate", {
  set.seed(77)
  n <- 150
  k <- 15
  probs <- seq_len(k)^(-1.2)
  probs <- probs / sum(probs)
  values <- sprintf("v%02d", seq_len(k))
  raw <- data.frame(
    ROW_NUMBER = seq_len(n),
    A = sample(values, n, replace = TRUE, prob = probs),
    B = sample(values, n, replace = TRUE, prob = probs),
    AGE = sample(20:79, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  anon <- raw
  for (col in c("A", "B")) {
    hit <- runif(n) < 0.4
    anon[[col]][hit] <- sample(values, sum(hit), replace = TRUE, prob = probs)
  }
  anon$AGE <- (anon$AGE %/% 10) * 10
  j <- join_raw_anon_data(raw, anon)

  age_only <- reid_evaluate(score_multi(j, c(AGE = "num")), seeds = 1:5)$success_analytic
  idf_only <- reid_evaluate(score_multi(j, c(A = "idf", B = "idf")),
                            seeds = 1:5)$success_analytic
  both <- reid_evaluate(score_multi(j, c(A = "idf", B = "idf", AGE = "num")),
                        seeds = 1:5)$success_analytic

  expect_gt(both, age_only)
  expect_gt(both, idf_only)
})
