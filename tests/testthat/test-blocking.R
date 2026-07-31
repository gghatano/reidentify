## ---------------------------------------------------------------------------
## Candidate blocking (Issue #36)
##
## The property that matters here is not "fewer pairs" -- that is trivially
## true -- but "fewer pairs, and we know exactly how many true pairs went with
## them". A blocking step that loses true pairs lowers the reported
## reidentification rate, which is the failure direction nobody questions
## (docs/lessons-learned.md section 2). So most of what follows checks the
## bookkeeping, not the reduction.
## ---------------------------------------------------------------------------

qi_fixture <- function(people = 60, seed = 3, jitter_age = TRUE) {
  raw <- create_dummy_qi_data(people = people, seed = seed)
  anon <- create_dummy_qi_data(people = people, seed = seed)
  if (jitter_age) {
    anon$AGE <- with_local_seed(99, anon$AGE +
                                  sample(c(-2, 0, 2), nrow(anon), replace = TRUE))
  }
  list(raw = raw, anon = anon)
}

## ---------------------------------------------------------------------------
## block_candidates
## ---------------------------------------------------------------------------

test_that("block_candidates returns a deduplicated subset of the full cross join", {
  f <- qi_fixture()
  cand <- block_candidates(f$raw, f$anon, keys = "ZIP")
  full <- join_raw_anon_data(f$raw, f$anon)

  expect_true(all(c("RAW_ROW_NUMBER", "ANON_ROW_NUMBER", "RAW_ZIP", "ANON_ZIP")
                  %in% names(cand)))
  expect_lt(nrow(cand), nrow(full))

  k <- function(x) paste(x$RAW_ROW_NUMBER, x$ANON_ROW_NUMBER, sep = "\r")
  expect_true(all(k(cand) %in% k(full)))
  expect_equal(anyDuplicated(k(cand)), 0L)

  ## every kept pair really does agree on the key
  expect_true(all(cand$RAW_ZIP == cand$ANON_ZIP))
})

test_that("an exact block key that the anonymisation left alone has recall 1", {
  f <- qi_fixture()
  info <- attr(block_candidates(f$raw, f$anon, keys = "ZIP"), "blocking")

  expect_s3_class(info, "reid_blocking")
  expect_equal(info$recall, 1)
  expect_equal(info$n_true_pairs, nrow(f$anon))
  expect_equal(info$n_anon_without_candidate, 0)
  expect_equal(info$n_pairs_full, nrow(f$raw) * nrow(f$anon))
  expect_equal(info$reduction, 1 - info$kept_fraction)
})

test_that("recall-1 blocking on a scored key reproduces the full-join success rate", {
  ## The point of the whole issue: blocking must be a speed-up, not a different
  ## measurement. It only is when the block key is part of what the attacker
  ## scores on -- see the next test for what happens otherwise.
  f <- qi_fixture(people = 80, seed = 5)
  targets <- c(AGE = "num", ZIP = "char", SEX = "char")

  full <- join_raw_anon_data(f$raw, f$anon)
  cand <- block_candidates(f$raw, f$anon, keys = "ZIP")
  expect_equal(attr(cand, "blocking")$recall, 1)

  e_full <- reid_evaluate(score_multi(full, targets, screen = "none"), seeds = 1:5)
  e_blk <- reid_evaluate(score_multi(cand, targets, screen = "none"), seeds = 1:5)

  expect_equal(e_blk$success_analytic, e_full$success_analytic)
  expect_equal(e_blk$max_risk, e_full$max_risk)
})

test_that("blocking on a key the score ignores changes the measurement upwards", {
  ## Documented, not a bug: restricting candidates to the same ZIP *is* an
  ## attacker who uses ZIP. It is recorded here because the number moves a long
  ## way and a reader who assumed blocking is invisible would misread it.
  f <- qi_fixture(people = 80, seed = 5)
  full <- join_raw_anon_data(f$raw, f$anon)
  cand <- block_candidates(f$raw, f$anon, keys = "ZIP")

  e_full <- reid_evaluate(score_num(full, "AGE"), seeds = 1:5)
  e_blk <- reid_evaluate(score_num(cand, "AGE"), seeds = 1:5)

  expect_gt(e_blk$success_analytic, e_full$success_analytic)
  ## and the baseline moves with it, so `lift` still tells the truth
  expect_gt(e_blk$baseline$rate[1], e_full$baseline$rate[1])
})

test_that("a block key the anonymisation perturbed loses true pairs, and says so", {
  f <- qi_fixture()
  expect_warning(
    cand <- block_candidates(f$raw, f$anon, keys = "AGE"),
    "LOWER bound"
  )
  info <- attr(cand, "blocking")
  expect_lt(info$recall, 1)
  expect_lt(info$n_true_pairs_kept, info$n_true_pairs)
  expect_gt(info$n_anon_without_candidate, 0)
})

test_that("union passes recover recall that a single pass loses", {
  f <- qi_fixture()
  one <- suppressWarnings(block_candidates(f$raw, f$anon, keys = "AGE"))
  two <- block_candidates(f$raw, f$anon, keys = list("AGE", "ZIP"))

  expect_gt(attr(two, "blocking")$recall, attr(one, "blocking")$recall)
  expect_equal(attr(two, "blocking")$recall, 1)
  expect_gt(nrow(two), nrow(one))

  ## the union must still contain each pair exactly once
  k <- function(x) paste(x$RAW_ROW_NUMBER, x$ANON_ROW_NUMBER, sep = "\r")
  expect_equal(anyDuplicated(k(two)), 0L)
})

test_that("transform blocks on a coarsened value", {
  f <- qi_fixture()
  exact <- suppressWarnings(block_candidates(f$raw, f$anon, keys = "AGE"))
  decade <- suppressWarnings(block_candidates(
    f$raw, f$anon, keys = "AGE", transform = list(AGE = function(x) x %/% 10)
  ))

  expect_gt(nrow(decade), nrow(exact))
  expect_gte(attr(decade, "blocking")$recall, attr(exact, "blocking")$recall)
  ## the kept pairs share a decade but need not share an age
  expect_true(all(decade$RAW_AGE %/% 10 == decade$ANON_AGE %/% 10))
})

test_that("block_candidates refuses to build a table above max_pairs", {
  f <- qi_fixture()
  expect_error(
    block_candidates(f$raw, f$anon, keys = "SEX", max_pairs = 100),
    "too coarse"
  )
})

test_that("block_candidates validates its arguments", {
  f <- qi_fixture(people = 10)
  expect_error(block_candidates("nope", f$anon, keys = "ZIP"), "data frames")
  expect_error(block_candidates(f$raw, f$anon, keys = list()), "one per blocking pass")
  expect_error(block_candidates(f$raw, f$anon, keys = 1), "one per blocking pass")
  expect_error(block_candidates(f$raw, f$anon, keys = "NOPE"), "not found")
  expect_error(block_candidates(f$raw, f$anon, keys = "ZIP", row_number = "NOPE"),
               "not found")
  expect_error(
    block_candidates(f$raw, f$anon, keys = "ZIP", transform = list(ZIP = 1)),
    "not a function"
  )
  expect_error(
    block_candidates(f$raw, f$anon, keys = "ZIP", max_pairs = 0),
    "positive number"
  )
})

test_that("block_candidates on wholly disjoint keys keeps nothing", {
  raw <- data.frame(ROW_NUMBER = 1:3, K = c("a", "b", "c"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:3, K = c("x", "y", "z"),
                     stringsAsFactors = FALSE)
  cand <- suppressWarnings(block_candidates(raw, anon, keys = "K"))

  expect_equal(nrow(cand), 0L)
  info <- attr(cand, "blocking")
  expect_equal(info$n_pairs_kept, 0)
  expect_equal(info$recall, 0)
  expect_equal(info$n_anon_without_candidate, 3)
})

test_that("recall is NA, not 1, when there is no ground truth to measure", {
  raw <- data.frame(ROW_NUMBER = 1:3, K = c("a", "b", "c"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 11:13, K = c("a", "b", "c"),
                     stringsAsFactors = FALSE)
  info <- attr(block_candidates(raw, anon, keys = "K"), "blocking")

  expect_equal(info$n_true_pairs, 0)
  expect_true(is.na(info$recall))
  ## and no warning: there was nothing to lose
  expect_silent(block_candidates(raw, anon, keys = "K"))
})

## ---------------------------------------------------------------------------
## top_k_candidates
## ---------------------------------------------------------------------------

test_that("top_k_candidates keeps every candidate tied with the k-th", {
  ## Two RAW records tie for best against every ANON record, so k = 1 must
  ## still return both: cutting on row order would drop a true pair for no
  ## reason and lower the reported rate.
  raw <- data.frame(ROW_NUMBER = 1:4, V = c(1, 1, 5, 5))
  s <- score_num(join_raw_anon_data(raw, raw), "V")
  pruned <- top_k_candidates(s, k = 1)

  expect_equal(nrow(pruned), 8L)
  expect_equal(attr(pruned, "blocking")$recall, 1)
  expect_s3_class(pruned, "reid_scores")
  expect_equal(attr(pruned, "score_type"), "distance")
})

test_that("top_k_candidates with ties = random caps hard at k", {
  raw <- data.frame(ROW_NUMBER = 1:4, V = c(1, 1, 5, 5))
  s <- score_num(join_raw_anon_data(raw, raw), "V")
  pruned <- suppressWarnings(top_k_candidates(s, k = 1, ties = "random", seed = 1))

  expect_equal(nrow(pruned), 4L)
  expect_equal(length(unique(pruned$ANON_ROW_NUMBER)), 4L)
})

test_that("top_k_candidates is seed-reproducible", {
  raw <- data.frame(ROW_NUMBER = 1:8, V = c(1, 1, 1, 1, 5, 5, 5, 5))
  s <- score_num(join_raw_anon_data(raw, raw), "V")
  a <- suppressWarnings(top_k_candidates(s, k = 2, ties = "random", seed = 7))
  b <- suppressWarnings(top_k_candidates(s, k = 2, ties = "random", seed = 7))
  expect_equal(a, b)
})

test_that("top_k_candidates reports the recall it cost", {
  f <- qi_fixture(people = 60, seed = 4)
  s <- score_num(join_raw_anon_data(f$raw, f$anon), "AGE")

  expect_warning(pruned <- top_k_candidates(s, k = 1, ties = "random", seed = 2),
                 "LOWER bound")
  info <- attr(pruned, "blocking")
  expect_equal(info$method, "top-k")
  expect_lt(info$recall, 1)
  expect_equal(info$n_pairs_full, nrow(s))
  expect_equal(info$n_anon_without_candidate, 0)
})

test_that("top_k_candidates leaves the single-guess success rate alone", {
  ## k = 1 keeps exactly the argmin candidates, so the attack that guesses once
  ## is unaffected -- what it destroys is the top-k hit rate for k > 1.
  f <- qi_fixture(people = 60, seed = 4)
  s <- score_num(join_raw_anon_data(f$raw, f$anon), "AGE")
  pruned <- suppressWarnings(top_k_candidates(s, k = 1))

  e_full <- reid_evaluate(s, seeds = 1:5, top_k = c(1, 5))
  e_pruned <- reid_evaluate(pruned, seeds = 1:5, top_k = c(1, 5))

  expect_equal(e_pruned$success_analytic, e_full$success_analytic)
  expect_lt(max(e_pruned$top_k$hit_rate), max(e_full$top_k$hit_rate))
})

test_that("top_k_candidates handles similarity tables and validates arguments", {
  raw <- data.frame(ROW_NUMBER = 1:4, V = c(1, 2, 3, 4))
  d <- join_raw_anon_data(raw, raw)
  s <- score_num(d, "V")
  sim <- new_reid_scores(s$RAW_ROW_NUMBER, s$ANON_ROW_NUMBER, -s$SCORE,
                         score_type = "similarity")

  pruned <- suppressWarnings(top_k_candidates(sim, k = 1))
  expect_equal(attr(pruned, "score_type"), "similarity")
  expect_equal(attr(pruned, "blocking")$recall, 1)

  expect_error(top_k_candidates(s, k = 0), "positive number")
  expect_error(top_k_candidates(data.frame(a = 1)), "missing score-layer")

  s_na <- s
  s_na$SCORE[1] <- NA
  expect_error(top_k_candidates(s_na, k = 1), "contains NA")
})

## ---------------------------------------------------------------------------
## blocking_recall
## ---------------------------------------------------------------------------

test_that("blocking_recall measures a hand-built candidate set", {
  f <- qi_fixture(people = 40, seed = 6)
  full <- join_raw_anon_data(f$raw, f$anon)
  ## a deliberately lossy hand-made filter: drop the true pair of ANON 1
  hand <- full[!(full$RAW_ROW_NUMBER == 1 & full$ANON_ROW_NUMBER == 1), ]

  info <- blocking_recall(hand, f$raw, f$anon)
  expect_s3_class(info, "reid_blocking")
  expect_equal(info$n_true_pairs, 40)
  expect_equal(info$n_true_pairs_kept, 39)
  expect_equal(info$recall, 39 / 40)
  expect_equal(info$n_pairs_full, 1600)
})

test_that("blocking_recall accepts a score table and infers totals when it must", {
  f <- qi_fixture(people = 30, seed = 8)
  cand <- block_candidates(f$raw, f$anon, keys = "ZIP")
  s <- score_num(cand, "AGE")

  info <- blocking_recall(s)
  expect_equal(info$n_true_pairs_kept, 30)
  expect_equal(info$method, "measured (totals inferred)")
})

test_that("blocking_recall validates its input", {
  expect_error(blocking_recall("nope"), "data frame")
  expect_error(blocking_recall(data.frame(a = 1)), "neither")
  f <- qi_fixture(people = 10)
  cand <- block_candidates(f$raw, f$anon, keys = "ZIP")
  expect_error(blocking_recall(cand, f$raw[, "ZIP", drop = FALSE], f$anon),
               "not found in `raw`")
})

## ---------------------------------------------------------------------------
## the blocking record itself
## ---------------------------------------------------------------------------

test_that("printing a blocking record names the loss", {
  f <- qi_fixture()
  lossy <- suppressWarnings(block_candidates(f$raw, f$anon, keys = "AGE"))
  out <- paste(utils::capture.output(print(attr(lossy, "blocking"))), collapse = "\n")

  expect_match(out, "blocking \\(deterministic\\)")
  expect_match(out, "recall")
  expect_match(out, "LOWER bound")
  expect_match(out, "keys = AGE")

  clean <- block_candidates(f$raw, f$anon, keys = "ZIP")
  out2 <- paste(utils::capture.output(print(attr(clean, "blocking"))), collapse = "\n")
  expect_false(grepl("LOWER bound", out2))
})

test_that("lsh_candidates now reports recall alongside the reduction", {
  set.seed(11)
  items <- lapply(seq_len(40), function(i) sort(sample.int(300, 12)))
  raw <- data.frame(
    ROW_NUMBER = seq_len(40),
    S = vapply(items, paste, character(1), collapse = ":"),
    stringsAsFactors = FALSE
  )
  anon <- data.frame(
    ROW_NUMBER = seq_len(40),
    S = vapply(items, function(v) paste(sort(sample(v, 6)), collapse = ":"),
               character(1)),
    stringsAsFactors = FALSE
  )

  strict <- suppressWarnings(lsh_candidates(raw, anon, "S", n_hash = 64, bands = 8))
  loose <- suppressWarnings(lsh_candidates(raw, anon, "S", n_hash = 64, bands = 64))

  expect_s3_class(attr(strict, "blocking"), "reid_blocking")
  expect_lte(attr(strict, "blocking")$recall, attr(loose, "blocking")$recall)
  expect_equal(attr(loose, "blocking")$n_hash, 64L)
  ## identical sides: every record must find itself, so recall is exactly 1
  expect_equal(attr(lsh_candidates(raw, raw, "S", n_hash = 64, bands = 32),
                    "blocking")$recall, 1)
})

## ---------------------------------------------------------------------------
## reid_evaluate must notice on its own
## ---------------------------------------------------------------------------

test_that("reid_evaluate detects a candidate set that is not the full join", {
  f <- qi_fixture(people = 60, seed = 9)
  full <- join_raw_anon_data(f$raw, f$anon)
  cand <- block_candidates(f$raw, f$anon, keys = "ZIP")

  e_full <- reid_evaluate(score_num(full, "AGE"), seeds = 1:3)
  e_blk <- reid_evaluate(score_num(cand, "AGE"), seeds = 1:3)

  expect_false(e_full$blocked)
  expect_equal(e_full$candidate_coverage, 1)
  expect_true(e_blk$blocked)
  expect_lt(e_blk$candidate_coverage, 1)
  expect_equal(e_blk$n_pairs_full, e_blk$n_anon * e_blk$n_raw)
})

test_that("the printed evaluation says so, without being asked", {
  f <- qi_fixture(people = 60, seed = 9)
  lossy <- suppressWarnings(block_candidates(f$raw, f$anon, keys = "AGE"))
  e <- reid_evaluate(score_num(lossy, "VISIT_COUNT"), seeds = 1:3)
  out <- paste(utils::capture.output(print(e)), collapse = "\n")

  expect_match(out, "BLOCKED")
  expect_match(out, "LOWER bound")
  expect_gt(e$n_true_missing, 0)
})

test_that("a full join prints no blocking line at all", {
  raw <- data.frame(ROW_NUMBER = 1:6, V = c(1, 2, 3, 4, 5, 6))
  e <- reid_evaluate(score_num(join_raw_anon_data(raw, raw), "V"), seeds = 1:3)
  out <- paste(utils::capture.output(print(e)), collapse = "\n")

  expect_false(grepl("BLOCKED", out))
  expect_false(e$blocked)
})
