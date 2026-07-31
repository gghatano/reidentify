## Tests for the set-similarity scores (Issue #18): score_jaccard(),
## score_minhash() and lsh_candidates().

make_set_fixture <- function() {
  raw <- data.frame(
    ROW_NUMBER = 1:3,
    ITEMS = c("a:b:c", "c:d", "x:y:z"),
    stringsAsFactors = FALSE
  )
  anon <- data.frame(
    ROW_NUMBER = 1:3,
    ITEMS = c("b:c", "d:c", "z:y"),
    stringsAsFactors = FALSE
  )
  join_raw_anon_data(raw, anon)
}

sim_of <- function(scores, raw, anon) {
  1 - scores$SCORE[scores$RAW_ROW_NUMBER == raw & scores$ANON_ROW_NUMBER == anon]
}

## ---------------------------------------------------------------------------
## known values
## ---------------------------------------------------------------------------

test_that("score_jaccard reproduces hand-computed Jaccard coefficients", {
  s <- score_jaccard(make_set_fixture(), "ITEMS")

  # {a,b,c} vs {b,c}: |inter| = 2, |union| = 3
  expect_equal(sim_of(s, 1, 1), 2 / 3)
  # {c,d} vs {b,c}: |inter| = 1, |union| = 3
  expect_equal(sim_of(s, 2, 1), 1 / 3)
  # {x,y,z} vs {b,c}: disjoint
  expect_equal(sim_of(s, 3, 1), 0)
  # {c,d} vs {d,c}: identical as sets
  expect_equal(sim_of(s, 2, 2), 1)
  # {x,y,z} vs {z,y}
  expect_equal(sim_of(s, 3, 3), 2 / 3)
})

test_that("dice, overlap and tversky reproduce their definitions", {
  d <- make_set_fixture()

  dice <- score_jaccard(d, "ITEMS", method = "dice")
  # {a,b,c} vs {b,c}: 2*2 / (3 + 2)
  expect_equal(sim_of(dice, 1, 1), 4 / 5)

  ov <- score_jaccard(d, "ITEMS", method = "overlap")
  # {a,b,c} vs {b,c}: 2 / min(3, 2)
  expect_equal(sim_of(ov, 1, 1), 1)

  tv11 <- score_jaccard(d, "ITEMS", method = "tversky", alpha = 1, beta = 1)
  expect_equal(tv11$SCORE, score_jaccard(d, "ITEMS")$SCORE)

  tv55 <- score_jaccard(d, "ITEMS", method = "tversky", alpha = 0.5, beta = 0.5)
  expect_equal(tv55$SCORE, dice$SCORE)

  # asymmetry: alpha prices RAW-only elements, beta ANON-only ones
  asym <- score_jaccard(d, "ITEMS", method = "tversky", alpha = 0.1, beta = 10)
  # {a,b,c} vs {b,c}: only_raw = 1, only_anon = 0 -> barely penalised
  expect_gt(sim_of(asym, 1, 1), sim_of(score_jaccard(d, "ITEMS"), 1, 1))
})

test_that("all set similarities stay inside [0, 1] and are distances", {
  d <- make_set_fixture()
  for (m in c("jaccard", "dice", "overlap", "tversky")) {
    s <- score_jaccard(d, "ITEMS", method = m, alpha = 0.3, beta = 0.2)
    expect_true(all(s$SCORE >= 0 & s$SCORE <= 1), info = m)
    expect_equal(attr(s, "score_type"), "distance", info = m)
  }
})

## ---------------------------------------------------------------------------
## set semantics: order and multiplicity
## ---------------------------------------------------------------------------

test_that("element order carries no information", {
  raw <- data.frame(ROW_NUMBER = 1:2, S = c("a:b:c", "p:q:r"),
                    stringsAsFactors = FALSE)
  shuffled <- data.frame(ROW_NUMBER = 1:2, S = c("c:a:b", "r:q:p"),
                         stringsAsFactors = FALSE)

  s <- score_jaccard(join_raw_anon_data(raw, shuffled), "S")
  expect_equal(sim_of(s, 1, 1), 1)
  expect_equal(sim_of(s, 2, 2), 1)
  expect_equal(sim_of(s, 1, 2), 0)
})

test_that("multiset = FALSE ignores repeats, multiset = TRUE counts them", {
  raw <- data.frame(ROW_NUMBER = 1L, S = "a:a:a:b", stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1L, S = "a:b", stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  expect_equal(1 - score_jaccard(d, "S")$SCORE, 1)

  # multiset: inter = min(3,1) + min(1,1) = 2, union = 4 + 2 - 2 = 4
  expect_equal(1 - score_jaccard(d, "S", multiset = TRUE)$SCORE, 2 / 4)
})

test_that("empty sets follow the documented convention", {
  raw <- data.frame(ROW_NUMBER = 1:2, S = c("", ""), stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:2, S = c("", "a"), stringsAsFactors = FALSE)
  s <- score_jaccard(join_raw_anon_data(raw, anon), "S")

  # both empty -> similarity 1 (distance 0)
  expect_equal(s$SCORE[s$ANON_ROW_NUMBER == 1], c(0, 0))
  # one empty -> similarity 0 (distance 1)
  expect_equal(s$SCORE[s$ANON_ROW_NUMBER == 2], c(1, 1))
})

## ---------------------------------------------------------------------------
## separators are literal (Issue #32 convention)
## ---------------------------------------------------------------------------

test_that("split is a literal string, not a regular expression", {
  for (sep in c("|", ".", "(", "+", "*")) {
    raw <- data.frame(
      ROW_NUMBER = 1:2,
      S = c(paste("aa", "bb", "cc", sep = sep), paste("xx", "yy", sep = sep)),
      stringsAsFactors = FALSE
    )
    anon <- data.frame(
      ROW_NUMBER = 1:2,
      S = c(paste("bb", "aa", sep = sep), paste("yy", "zz", sep = sep)),
      stringsAsFactors = FALSE
    )
    s <- score_jaccard(join_raw_anon_data(raw, anon), "S", split = sep)
    # {aa,bb,cc} vs {bb,aa} -> 2/3
    expect_equal(sim_of(s, 1, 1), 2 / 3, info = sep)
    # {xx,yy} vs {yy,zz} -> 1/3
    expect_equal(sim_of(s, 2, 2), 1 / 3, info = sep)
  }
})

test_that("score_jaccard rejects an unusable split", {
  d <- make_set_fixture()
  expect_error(score_jaccard(d, "ITEMS", split = ""), "empty string")
  expect_error(score_jaccard(d, "ITEMS", split = c(":", ",")), "single non-NA")
  expect_error(score_jaccard(d, "ITEMS", split = NA_character_), "single non-NA")
})

## ---------------------------------------------------------------------------
## argument validation
## ---------------------------------------------------------------------------

test_that("score_jaccard reports a missing column by name", {
  d <- make_set_fixture()
  expect_error(score_jaccard(d, "NOPE"), "RAW_NOPE")
  expect_error(score_jaccard(d, "ITEMS", row_number = "NOPE"), "RAW_NOPE")
})

test_that("negative Tversky weights are rejected", {
  d <- make_set_fixture()
  expect_error(score_jaccard(d, "ITEMS", method = "tversky", alpha = -1),
               "non-negative")
  expect_error(score_jaccard(d, "ITEMS", method = "tversky", beta = -1),
               "non-negative")
})

test_that("NA in the set column is an error, not a silent empty set", {
  raw <- data.frame(ROW_NUMBER = 1:2, S = c("a:b", NA), stringsAsFactors = FALSE)
  expect_error(score_jaccard(join_raw_anon_data(raw, raw), "S"), "contains NA")
})

## ---------------------------------------------------------------------------
## the score table lines up with the input rows
## ---------------------------------------------------------------------------

test_that("scores follow the rows of dat_raw_anon, whatever their order", {
  raw <- data.frame(ROW_NUMBER = c(70L, 5L, 42L),
                    S = c("a:b:c", "c:d", "x:y:z"), stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = c(42L, 70L, 5L),
                     S = c("z:y", "b:c", "d:c"), stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)
  d <- d[sample.int(nrow(d)), , drop = FALSE]

  s <- score_jaccard(d, "S")
  expect_equal(s$RAW_ROW_NUMBER, d$RAW_ROW_NUMBER)
  expect_equal(s$ANON_ROW_NUMBER, d$ANON_ROW_NUMBER)

  # recompute one pair by hand from the row it came from
  i <- which(d$RAW_ROW_NUMBER == 70L & d$ANON_ROW_NUMBER == 70L)
  expect_equal(s$SCORE[i], 1 - 2 / 3)

  # and the result is invariant to that shuffling
  s2 <- score_jaccard(join_raw_anon_data(raw, anon), "S")
  key <- function(x) paste(x$RAW_ROW_NUMBER, x$ANON_ROW_NUMBER)
  expect_equal(s$SCORE[order(key(s))], s2$SCORE[order(key(s2))])
})

test_that("identical RAW and ANON sets are matched perfectly", {
  set.seed(11)
  items <- vapply(seq_len(25), function(i) {
    paste(sample(letters, 8), collapse = ":")
  }, character(1))
  raw <- data.frame(ROW_NUMBER = seq_along(items), S = items,
                    stringsAsFactors = FALSE)
  sc <- score_jaccard(join_raw_anon_data(raw, raw), "S")
  m <- match_greedy(sc)
  expect_true(all(m$RESULT))
  ## "every best candidate is unique" is what CONFIDENCE == 1 meant here; the
  ## default measure became "margin" in #44, so say which one is being read.
  expect_true(all(match_greedy(sc, confidence = "tie")$CONFIDENCE == 1))
  ## and under the default every winner is clear of its runner-up
  expect_true(all(m$CONFIDENCE > 0))
})

## ---------------------------------------------------------------------------
## Issue #18's acceptance criterion
## ---------------------------------------------------------------------------

test_that("score_jaccard beats score_dist on a set-valued numeric column", {
  set.seed(4242)
  people <- 60
  raw_items <- lapply(seq_len(people), function(i) sort(sample.int(200, 8)))
  anon_items <- lapply(raw_items, function(v) sort(sample(v, 4)))

  raw <- data.frame(
    ROW_NUMBER = seq_len(people),
    ITEMS = vapply(raw_items, paste, character(1), collapse = ":"),
    stringsAsFactors = FALSE
  )
  anon <- data.frame(
    ROW_NUMBER = seq_len(people),
    ITEMS = vapply(anon_items, paste, character(1), collapse = ":"),
    stringsAsFactors = FALSE
  )
  d <- join_raw_anon_data(raw, anon)

  jac <- reid_evaluate(score_jaccard(d, "ITEMS"), seeds = 1:5, top_k = 1)
  dst <- reid_evaluate(score_dist(d, "ITEMS"), seeds = 1:5, top_k = 1)

  expect_gt(jac$success_analytic, dst$success_analytic)
  # not a marginal win: the set reading finds most people, the quantile
  # reading finds almost none
  expect_gt(jac$success_analytic, 0.5)
  expect_gt(jac$success_analytic, 5 * dst$success_analytic)
})

test_that("score_jaccard reads a categorical set column that score_dist cannot", {
  raw <- data.frame(
    ROW_NUMBER = 1:3,
    SHOPS = c("ginza:shibuya", "ueno:ikebukuro", "kyoto:osaka"),
    stringsAsFactors = FALSE
  )
  d <- join_raw_anon_data(raw, raw)

  expect_error(score_dist(d, "SHOPS"), "could not convert")
  expect_true(all(match_greedy(score_jaccard(d, "SHOPS"))$RESULT))
})

## ---------------------------------------------------------------------------
## min-hash
## ---------------------------------------------------------------------------

test_that("minhash_signatures is deterministic given a seed and shared universe", {
  sets <- list(c("a", "b", "c"), c("b", "c", "d"))
  s1 <- minhash_signatures(sets, n_hash = 16, seed = 3)
  s2 <- minhash_signatures(sets, n_hash = 16, seed = 3)
  expect_identical(s1, s2)
  expect_equal(dim(s1), c(16L, 2L))

  s3 <- minhash_signatures(sets, n_hash = 16, seed = 4)
  expect_false(identical(s1, s3))
})

test_that("minhash_signatures gives an empty set an NA column", {
  sig <- minhash_signatures(list(c("a", "b"), character(0)), n_hash = 8, seed = 1)
  expect_false(anyNA(sig[, 1]))
  expect_true(all(is.na(sig[, 2])))
})

test_that("score_minhash estimates exact Jaccard", {
  set.seed(7)
  people <- 40
  items <- lapply(seq_len(people), function(i) sort(sample.int(120, 15)))
  raw <- data.frame(
    ROW_NUMBER = seq_len(people),
    S = vapply(items, paste, character(1), collapse = ":"),
    stringsAsFactors = FALSE
  )
  anon <- data.frame(
    ROW_NUMBER = seq_len(people),
    S = vapply(items, function(v) paste(sort(sample(v, 10)), collapse = ":"),
               character(1)),
    stringsAsFactors = FALSE
  )
  d <- join_raw_anon_data(raw, anon)

  exact <- score_jaccard(d, "S")$SCORE
  approx <- score_minhash(d, "S", n_hash = 512, seed = 1)$SCORE

  expect_lt(mean(abs(exact - approx)), 0.02)
  expect_gt(stats::cor(exact, approx), 0.95)

  # more components -> a better estimate
  coarse <- score_minhash(d, "S", n_hash = 32, seed = 1)$SCORE
  expect_gt(mean(abs(exact - coarse)), mean(abs(exact - approx)))
})

test_that("score_minhash is exact for identical and for disjoint sets", {
  raw <- data.frame(ROW_NUMBER = 1:2, S = c("a:b:c:d", "p:q:r:s"),
                    stringsAsFactors = FALSE)
  s <- score_minhash(join_raw_anon_data(raw, raw), "S", n_hash = 32)

  expect_equal(s$SCORE[s$RAW_ROW_NUMBER == s$ANON_ROW_NUMBER], c(0, 0))
  expect_equal(s$SCORE[s$RAW_ROW_NUMBER != s$ANON_ROW_NUMBER], c(1, 1))
})

test_that("score_minhash matches the empty-set convention of score_jaccard", {
  raw <- data.frame(ROW_NUMBER = 1:2, S = c("", ""), stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:2, S = c("", "a"), stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  expect_equal(score_minhash(d, "S", n_hash = 8)$SCORE,
               score_jaccard(d, "S")$SCORE)
})

test_that("score_minhash validates n_hash and split", {
  d <- make_set_fixture()
  expect_error(score_minhash(d, "ITEMS", n_hash = 0), "positive")
  expect_error(score_minhash(d, "ITEMS", split = ""), "empty string")
  expect_error(score_minhash(d, "NOPE"), "RAW_NOPE")
})

## ---------------------------------------------------------------------------
## LSH blocking
## ---------------------------------------------------------------------------

test_that("lsh_candidates returns a usable raw_anon table that is a subset of the full join", {
  set.seed(21)
  people <- 30
  items <- lapply(seq_len(people), function(i) sort(sample.int(150, 10)))
  raw <- data.frame(
    ROW_NUMBER = seq_len(people),
    S = vapply(items, paste, character(1), collapse = ":"),
    stringsAsFactors = FALSE
  )

  blocked <- lsh_candidates(raw, raw, "S", n_hash = 64, bands = 32)

  expect_true(all(c("RAW_ROW_NUMBER", "ANON_ROW_NUMBER", "RAW_S", "ANON_S")
                  %in% names(blocked)))
  expect_lt(nrow(blocked), people * people)

  # every kept pair also exists in the full cross join
  full <- join_raw_anon_data(raw, raw)
  k <- function(x) paste(x$RAW_ROW_NUMBER, x$ANON_ROW_NUMBER, sep = "\r")
  expect_true(all(k(blocked) %in% k(full)))

  # and each pair appears exactly once, even when it collides in several bands
  expect_equal(anyDuplicated(k(blocked)), 0L)

  # identical RAW/ANON: every record must still find itself
  expect_equal(sum(blocked$RAW_ROW_NUMBER == blocked$ANON_ROW_NUMBER), people)

  # the reduced table feeds the score layer unchanged
  m <- match_greedy(score_jaccard(blocked, "S"))
  expect_true(all(m$RESULT))
})

test_that("lsh_candidates records what it discarded", {
  raw <- data.frame(ROW_NUMBER = 1:6,
                    S = c("a:b:c", "a:b:d", "e:f:g", "h:i:j", "k:l:m", "n:o:p"),
                    stringsAsFactors = FALSE)
  blocked <- lsh_candidates(raw, raw, "S", n_hash = 32, bands = 16)
  info <- attr(blocked, "blocking")

  expect_equal(info$n_pairs_full, 36)
  expect_equal(info$n_pairs_kept, nrow(blocked))
  expect_equal(info$kept_fraction, nrow(blocked) / 36)
  expect_true(info$n_anon_without_candidate >= 0)
  expect_equal(info$n_hash, 32L)
  expect_equal(info$bands, 16L)
})

test_that("more bands keep more pairs", {
  set.seed(5)
  items <- lapply(seq_len(40), function(i) sort(sample.int(300, 12)))
  raw <- data.frame(
    ROW_NUMBER = seq_len(40),
    S = vapply(items, paste, character(1), collapse = ":"),
    stringsAsFactors = FALSE
  )
  anon <- data.frame(
    ROW_NUMBER = seq_len(40),
    S = vapply(items, function(v) paste(sort(sample(v, 8)), collapse = ":"),
               character(1)),
    stringsAsFactors = FALSE
  )

  ## Fewer bands means a stricter filter, so the recall-loss warning added in
  ## Issue #36 fires for the low-band settings; that is the behaviour under
  ## test two lines down, not a surprise.
  info <- lapply(c(8, 16, 32, 64), function(b) {
    suppressWarnings(attr(lsh_candidates(raw, anon, "S", n_hash = 64, bands = b),
                          "blocking"))
  })
  kept <- vapply(info, function(i) i$n_pairs_kept, numeric(1))
  expect_true(all(diff(kept) >= 0))

  ## and keeping more pairs can only keep more true pairs (Issue #36)
  recall <- vapply(info, function(i) i$recall, numeric(1))
  expect_true(all(diff(recall) >= 0))
})

test_that("lsh_candidates validates its arguments", {
  raw <- data.frame(ROW_NUMBER = 1:2, S = c("a:b", "c:d"),
                    stringsAsFactors = FALSE)
  expect_error(lsh_candidates(raw, raw, "S", n_hash = 64, bands = 7),
               "divide")
  expect_error(lsh_candidates(raw, raw, "NOPE"), "not found")
  expect_error(lsh_candidates(raw, "not a data frame", "S"), "data frames")
  expect_error(lsh_candidates(raw, raw, "S", split = ""), "empty string")
})

test_that("lsh_candidates on wholly disjoint data keeps nothing and says so", {
  raw <- data.frame(ROW_NUMBER = 1:3, S = c("a:b", "c:d", "e:f"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:3, S = c("u:v", "w:x", "y:z"),
                     stringsAsFactors = FALSE)
  ## Losing every true pair is the loudest case there is, so it warns
  ## (Issue #36): a candidate set with nothing in it reports zero
  ## reidentifications, which is indistinguishable from a safe release.
  expect_warning(
    blocked <- lsh_candidates(raw, anon, "S", n_hash = 32, bands = 8),
    "LOWER bound"
  )

  expect_equal(nrow(blocked), 0L)
  expect_equal(attr(blocked, "blocking")$n_anon_without_candidate, 3)
  expect_equal(attr(blocked, "blocking")$recall, 0)
})
