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

## A candidate set whose ANON records have DIFFERENT numbers of candidates --
## the shape top_k_candidates() is actually used on, and the one a cross join
## can never produce (Issue #64). `sizes` gives the block sizes; each block
## contributes one ANON record, matched against `sizes[i]` RAW records whose
## distance to it is 0, 1, 2, ... in order.
##
## Every per-block quantity here has to differ from every other, because the
## bug this fixture exists to catch -- indexing the k-th best score with the
## wrong per-group offset -- is invisible the moment two groups agree.
uneven_fixture <- function(sizes = c(5, 3, 2)) {
  zip <- rep(LETTERS[seq_along(sizes)], times = sizes)
  age <- unlist(lapply(sizes, function(n) seq_len(n) - 1L), use.names = FALSE)
  raw <- data.frame(ROW_NUMBER = seq_along(zip), ZIP = zip, AGE = age,
                    stringsAsFactors = FALSE)
  ## one ANON record per block: the first RAW record of each, so the true pair
  ## is always present and always scores 0
  anon <- raw[cumsum(c(0, utils::head(sizes, -1))) + 1, , drop = FALSE]
  list(raw = raw, anon = anon, sizes = sizes)
}

## What top_k_candidates(ties = "keep") must return, worked out per ANON record
## with no shared code: keep every candidate whose score is at most the k-th
## smallest score of that record.
top_k_reference <- function(scores, k) {
  keep <- unsplit(
    lapply(split(scores$SCORE, scores$ANON_ROW_NUMBER), function(v) {
      v <= sort(v)[min(k, length(v))]
    }),
    scores$ANON_ROW_NUMBER
  )
  scores[keep, c("RAW_ROW_NUMBER", "ANON_ROW_NUMBER"), drop = FALSE]
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

test_that("max_pairs is a ceiling the key is allowed to reach exactly", {
  ## The boundary, in both directions: a budget of exactly the number of pairs
  ## the key produces is enough, one less is not. A `>=` here would reject a
  ## key the user sized correctly.
  raw <- data.frame(ROW_NUMBER = 1:3, K = c("a", "a", "a"),
                    stringsAsFactors = FALSE)
  expect_equal(nrow(block_candidates(raw, raw, keys = "K", max_pairs = 9)), 9L)
  expect_error(block_candidates(raw, raw, keys = "K", max_pairs = 8),
               "too coarse")
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
  ## An unnamed transform cannot be matched to a key, so it would be applied to
  ## nothing at all: blocking would silently run on the untransformed key, keep
  ## fewer pairs than the caller asked for, and lower the recall without saying
  ## so. Refuse it rather than ignore it.
  expect_error(
    block_candidates(f$raw, f$anon, keys = "ZIP",
                     transform = list(function(x) substr(x, 1, 1))),
    "named list of functions"
  )
  expect_error(
    block_candidates(f$raw, f$anon, keys = "ZIP", transform = "toupper"),
    "named list of functions"
  )
  expect_error(
    block_candidates(f$raw, f$anon, keys = "ZIP", max_pairs = 0),
    "positive number"
  )
  ## every clause of the max_pairs check, not just the sign: a vector, an NA
  ## and a string all have to be refused *here*, with the message that names
  ## the argument, rather than surfacing later as "the key is too coarse".
  expect_error(
    block_candidates(f$raw, f$anon, keys = "ZIP", max_pairs = c(10, 20)),
    "positive number"
  )
  expect_error(
    block_candidates(f$raw, f$anon, keys = "ZIP", max_pairs = NA_real_),
    "positive number"
  )
  expect_error(
    block_candidates(f$raw, f$anon, keys = "ZIP", max_pairs = "10"),
    "positive number"
  )
  ## a pass has to name at least one existing column; an empty pass or a
  ## numeric "column index" would otherwise block on whatever column happened
  ## to sit in that position
  expect_error(block_candidates(f$raw, f$anon, keys = list(character(0))),
               "one per blocking pass")
  expect_error(block_candidates(f$raw, f$anon, keys = list(1)),
               "one per blocking pass")
  expect_error(block_candidates(f$raw, f$anon, keys = list("ZIP", 2)),
               "one per blocking pass")
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
  ## NA_real_ exactly. 0/0 is NaN, and is.na(NaN) is TRUE, so `is.na()` alone
  ## cannot tell "we refused to divide" from "we divided by zero anyway" --
  ## and NaN would go on to print and compare as a number-shaped nothing.
  expect_identical(info$recall, NA_real_)
  ## and no warning: there was nothing to lose
  expect_silent(block_candidates(raw, anon, keys = "K"))
})

test_that("an empty side gives NA ratios, not a 100% reduction", {
  ## Zero pairs in the full join is the other divide-by-zero. Reporting
  ## kept_fraction 1 / reduction 0 / recall 1 here would be three reassuring
  ## numbers about a measurement that did not happen -- exactly what
  ## docs/lessons-learned.md section 2 says a safety tool must not do.
  empty <- data.frame(ROW_NUMBER = integer(0), K = character(0),
                      stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:3, K = c("a", "b", "c"),
                     stringsAsFactors = FALSE)

  info <- attr(block_candidates(empty, anon, keys = "K"), "blocking")
  expect_equal(info$n_pairs_full, 0)
  expect_identical(info$kept_fraction, NA_real_)
  expect_identical(info$reduction, NA_real_)
  expect_identical(info$recall, NA_real_)
  expect_equal(info$n_anon_without_candidate, 3)

  ## the same on the other side, and through blocking_recall()
  info2 <- blocking_recall(
    data.frame(RAW_ROW_NUMBER = integer(0), ANON_ROW_NUMBER = integer(0)),
    empty, empty
  )
  expect_identical(info2$kept_fraction, NA_real_)
  expect_identical(info2$reduction, NA_real_)
  expect_identical(info2$recall, NA_real_)

  ## and it prints without inventing a percentage
  out <- paste(utils::capture.output(print(info)), collapse = "\n")
  expect_match(out, "not measurable")
})

test_that("no warning and no loss line when blocking kept every true pair", {
  ## The warning must be strictly one-sided. If it fired at recall == 1 too it
  ## would appear on every correct run, and a warning that always fires is a
  ## warning nobody reads -- which would hide the case it exists for.
  f <- qi_fixture()
  expect_silent(block_candidates(f$raw, f$anon, keys = "ZIP"))
  clean <- block_candidates(f$raw, f$anon, keys = "ZIP")
  expect_equal(attr(clean, "blocking")$recall, 1)
  expect_silent(warn_blocking_loss(attr(clean, "blocking"), "test"))
  expect_silent(block_candidates(f$raw, f$anon, keys = list("AGE", "ZIP")))
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

test_that("top_k_candidates cuts each ANON record at its OWN k-th best score", {
  ## Issue #64. Every previous test fed this function a full cross join, where
  ## every ANON record has the same number of candidates -- and with equal
  ## group sizes an off-by-one in the per-group offset is invisible, because
  ## every wrong offset happens to land on an equally-sized group. The shape
  ## README recommends (block first, prune second) never has that property.
  f <- uneven_fixture(c(5, 3, 2))
  cand <- block_candidates(f$raw, f$anon, keys = "ZIP")
  s <- score_num(cand, "AGE")

  ## the premise: candidate counts differ between ANON records
  expect_equal(as.integer(table(s$ANON_ROW_NUMBER)), c(5L, 3L, 2L))

  pruned <- top_k_candidates(s, k = 2)

  ## Worked through by hand: each ANON record's two best distances are 0 and 1,
  ## so exactly two candidates survive per record.
  expect_equal(nrow(pruned), 6L)
  expect_equal(as.integer(table(pruned$ANON_ROW_NUMBER)), c(2L, 2L, 2L))
  expect_true(all(pruned$SCORE <= 1))
  expect_equal(attr(pruned, "blocking")$recall, 1)
})

test_that("top_k_candidates agrees with a per-record reference on uneven blocks", {
  ## The same property stated generally, over several block shapes and several
  ## k, so it keeps holding when the implementation changes.
  for (sizes in list(c(5, 3, 2), c(2, 7, 4, 1), c(9, 1, 6, 3, 8))) {
    f <- uneven_fixture(sizes)
    s <- score_num(block_candidates(f$raw, f$anon, keys = "ZIP"), "AGE")
    for (k in c(1, 2, 3)) {
      pruned <- suppressWarnings(top_k_candidates(s, k = k))
      ref <- top_k_reference(s, k)
      expect_equal(
        pruned[, c("RAW_ROW_NUMBER", "ANON_ROW_NUMBER")],
        `rownames<-`(ref, NULL),
        info = paste0("sizes = ", paste(sizes, collapse = ","), ", k = ", k)
      )
    }
  }
})

test_that("top_k_candidates keeps the k-th tie on uneven blocks too", {
  ## Ties and unequal group sizes at once: the group with the flat score must
  ## keep more than k, and that must not shift the cut of any other group.
  raw <- data.frame(
    ROW_NUMBER = 1:9,
    ZIP = c("A", "A", "A", "A", "B", "B", "B", "C", "C"),
    AGE = c(0, 0, 0, 7, 0, 1, 2, 0, 5),
    stringsAsFactors = FALSE
  )
  anon <- raw[c(1, 5, 8), , drop = FALSE]
  s <- score_num(block_candidates(raw, anon, keys = "ZIP"), "AGE")

  pruned <- top_k_candidates(s, k = 2)
  ## ANON 1: three candidates tie at distance 0, so all three stay.
  ## ANON 5: distances 0, 1, 2 -> the two best.
  ## ANON 8: distances 0, 5 -> the two best (both).
  expect_equal(as.integer(table(pruned$ANON_ROW_NUMBER)), c(3L, 2L, 2L))
  expect_equal(nrow(pruned), 7L)
})

test_that("top_k_candidates with ties = random caps every uneven block at k", {
  f <- uneven_fixture(c(6, 4, 2))
  s <- score_num(block_candidates(f$raw, f$anon, keys = "ZIP"), "AGE")
  pruned <- suppressWarnings(top_k_candidates(s, k = 3, ties = "random", seed = 4))

  ## the last block only has 2 candidates, so it cannot reach k
  expect_equal(as.integer(table(pruned$ANON_ROW_NUMBER)), c(3L, 3L, 2L))
})

test_that("top_k_candidates counts records, not rows, in its blocking record", {
  f <- uneven_fixture(c(5, 3, 2))
  s <- score_num(block_candidates(f$raw, f$anon, keys = "ZIP"), "AGE")
  pruned <- top_k_candidates(s, k = 2)
  info <- attr(pruned, "blocking")

  ## 10 distinct RAW records appear as candidates, 3 distinct ANON records --
  ## not the 10 candidate rows.
  expect_equal(info$n_raw, 10)
  expect_equal(info$n_anon, 3)
  expect_equal(info$n_pairs_full, 10)
  expect_equal(info$n_pairs_kept, 6)
  expect_equal(info$n_anon_without_candidate, 0)
  expect_equal(info$kept_fraction, 0.6)

  ## the class is set, not accumulated: the input is already a reid_scores
  expect_identical(class(pruned), c("reid_scores", "data.frame"))

  ## and where a RAW record IS offered to several ANON records, n_raw still
  ## counts it once -- rows would give 16 for four people
  wide <- data.frame(ROW_NUMBER = 1:4, V = c(1, 2, 3, 4))
  ws <- score_num(join_raw_anon_data(wide, wide), "V")
  winfo <- attr(suppressWarnings(top_k_candidates(ws, k = 2)), "blocking")
  expect_equal(winfo$n_raw, 4)
  expect_equal(winfo$n_anon, 4)
  expect_equal(winfo$n_pairs_full, 16)
})

test_that("top_k_candidates says nothing when it lost nothing", {
  ## The warning has to be one-sided. Firing on recall == 1 as well would
  ## teach a reader to ignore it, which is the same as not having it.
  f <- uneven_fixture(c(5, 3, 2))
  s <- score_num(block_candidates(f$raw, f$anon, keys = "ZIP"), "AGE")
  expect_equal(attr(top_k_candidates(s, k = 2), "blocking")$recall, 1)
  expect_silent(top_k_candidates(s, k = 2))
  expect_silent(top_k_candidates(s, k = 2, ties = "random", seed = 1))
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

  ## The inferred totals count *records*, not candidate rows: every record
  ## appears once per candidate, so counting rows would report an n_raw of
  ## several hundred for 30 people and a full-join size in the millions --
  ## which would then be divided into, making the reduction look enormous.
  expect_equal(info$n_raw, 30)
  expect_equal(info$n_anon, 30)
  expect_equal(info$n_pairs_full, 900)
  expect_equal(info$n_true_pairs, 30)
  expect_equal(info$n_pairs_kept, nrow(s))
  expect_lt(info$kept_fraction, 1)
})

test_that("blocking_recall validates its input", {
  expect_error(blocking_recall("nope"), "data frame")
  expect_error(blocking_recall(data.frame(a = 1)), "neither")
  f <- qi_fixture(people = 10)
  cand <- block_candidates(f$raw, f$anon, keys = "ZIP")
  expect_error(blocking_recall(cand, f$raw[, "ZIP", drop = FALSE], f$anon),
               "not found in `raw`")
  ## the same on the ANON side. `anon` sets n_anon and n_true_pairs -- the
  ## denominators recall is measured against -- so falling back to "infer the
  ## totals from the candidate table" here would count only the records that
  ## survived blocking and report a recall that cannot see what was lost.
  expect_error(blocking_recall(cand, f$raw, f$anon[, "ZIP", drop = FALSE]),
               "not found in `anon`")

  ## half a pair of columns is not a pair: RAW_ID without ANON_ID must be
  ## refused, not silently completed from whatever else is lying around
  expect_error(
    blocking_recall(data.frame(RAW_ID = 1:2, SOMETHING = 1:2), row_number = "ID"),
    "neither"
  )
  ## and half a score-layer pair is not a fallback either. The message has to
  ## name the columns the caller asked for -- reporting the score-layer names
  ## instead sends them looking for a column they never mentioned.
  expect_error(
    blocking_recall(data.frame(RAW_ROW_NUMBER = 1:2, RAW_ID = 1:2),
                    row_number = "ID"),
    '"RAW_ID"/"ANON_ID"'
  )
})

test_that("blocking_recall falls back to the score-layer columns as a pair", {
  ## Both prefixed columns have to be present for the requested row_number to
  ## be used. With only one of them the score-layer names are the right
  ## reading -- taking the half-present pair instead makes the call fail on a
  ## table that does carry the ground truth.
  cand <- data.frame(
    RAW_ROW_NUMBER = c(1L, 2L, 2L),
    ANON_ROW_NUMBER = c(1L, 1L, 2L),
    RAW_ID = c(1L, 2L, 2L)
  )
  info <- blocking_recall(cand, row_number = "ID")
  expect_equal(info$n_true_pairs_kept, 2)
  expect_equal(info$n_raw, 2)
  expect_equal(info$n_anon, 2)
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

test_that("reid_evaluate keeps every k some ANON record could reach", {
  ## The same shape blind spot as top_k_candidates(), one file over
  ## (R/evaluate.R): the top-k table drops any k above the largest candidate
  ## set, and on a cross join the largest and the smallest are the same
  ## number, so a bound taken from the *smallest* is indistinguishable. On a
  ## blocked candidate set it silently deletes rows from the reported table --
  ## and a missing row reads as "that k was not measured", not as a bug.
  f <- uneven_fixture(c(6, 3, 2))
  s <- score_num(block_candidates(f$raw, f$anon, keys = "ZIP"), "AGE")
  expect_equal(range(as.integer(table(s$ANON_ROW_NUMBER))), c(2L, 6L))

  e <- reid_evaluate(s, seeds = 1:3, top_k = c(1, 3, 6, 7))
  ## 6 is reachable (one record has six candidates); 7 is not reachable by any
  expect_equal(e$top_k$k, c(1, 3, 6))
  expect_true(all(diff(e$top_k$hit_rate) >= 0))
})

test_that("a full join prints no blocking line at all", {
  raw <- data.frame(ROW_NUMBER = 1:6, V = c(1, 2, 3, 4, 5, 6))
  e <- reid_evaluate(score_num(join_raw_anon_data(raw, raw), "V"), seeds = 1:3)
  out <- paste(utils::capture.output(print(e)), collapse = "\n")

  expect_false(grepl("BLOCKED", out))
  expect_false(e$blocked)
})
