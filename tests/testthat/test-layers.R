## Tests for the score / integration / assignment layers introduced by #11.
##
## The point of the split is that a distance definition and an assignment rule
## can now be combined independently, so each layer's contract has to be
## pinned down on its own: schema, orientation, validation.
##
## This file used to end with an equivalence test between the four legacy
## reid_by_*() wrappers and score_*() + match_greedy(). The wrappers were
## removed in 3.0.0, so there is no second implementation left to disagree
## with; what that test guarded -- that each score's SCORE is the distance it
## claims to be, and that the assignment picks the argmin of it -- is asserted
## directly below, against the score tables themselves.

## ---------------------------------------------------------------------------
## fixtures
## ---------------------------------------------------------------------------

## 5 records, V unique => every ANON record has a unique nearest RAW record.
make_unique_join <- function() {
  raw <- data.frame(ROW_NUMBER = 1:5, V = c(10, 20, 30, 40, 50), W = c(1, 3, 5, 7, 9))
  join_raw_anon_data(raw, raw)
}

## 6 records, V has 3 tie groups of size 2 => each ANON record ties with
## exactly one other RAW record at distance 0.
make_tied_join <- function() {
  raw <- data.frame(ROW_NUMBER = 1:6, V = c(1, 1, 2, 2, 3, 3))
  join_raw_anon_data(raw, raw)
}

make_master_join <- function(people = 15, size = 3, seed = 71) {
  set.seed(seed)
  dat <- create_dummy_transaction_data(people = people, size = size)
  dat$CHAR_STATIC <- paste("CHAR", dat$ID, sep = "")
  m <- transform_transaction_to_master(
    dat,
    ROW_NUMBER = "ROW_NUMBER",
    STATIC_NUM = "NUM_STATIC",
    STATIC_CHAR = "CHAR_STATIC",
    DYNAMIC_NUM = c("BIN", "NUM_DYNAMIC"),
    DYNAMIC_CHAR = "CHAR"
  )
  join_raw_anon_data(m, m)
}

## ---------------------------------------------------------------------------
## score layer: schema
## ---------------------------------------------------------------------------

test_that("all 4 score_*() functions return exactly (RAW_ROW_NUMBER, ANON_ROW_NUMBER, SCORE), one row per candidate pair", {
  d <- make_master_join()

  scores <- list(
    num = score_num(d, "NUM_DYNAMIC_MEAN"),
    char = score_char(d, "CHAR_STATIC"),
    dist = score_dist(d, "NUM_DYNAMIC_DIST"),
    rank = score_num_rank(d, "NUM_DYNAMIC_MEAN")
  )

  for (nm in names(scores)) {
    s <- scores[[nm]]
    ## the O-7 unification: identical column set and order for all 4,
    ## independent of what the input happened to contain
    expect_identical(
      names(s), c("RAW_ROW_NUMBER", "ANON_ROW_NUMBER", "SCORE"),
      info = nm
    )
    expect_s3_class(s, "reid_scores")
    expect_identical(attr(s, "score_type"), "distance")
    expect_true(is.numeric(s$SCORE), info = nm)
    expect_false(anyNA(s$SCORE), info = nm)
    ## one row per (RAW, ANON) candidate pair, i.e. per input row
    expect_equal(nrow(s), nrow(d), info = nm)
    expect_false(
      anyDuplicated(paste(s$ANON_ROW_NUMBER, s$RAW_ROW_NUMBER)) > 0,
      info = nm
    )
    ## SCORE is unnamed (a named vector used to leak out of mapply())
    expect_null(names(s$SCORE), info = nm)
  }
})

test_that("score_*() shape does not depend on the input's column set (reid_by_char's O-7 defect)", {
  d <- make_master_join()
  d2 <- d
  d2$EXTRA_ONE <- 1
  d2$EXTRA_TWO <- "x"

  expect_identical(names(score_char(d, "CHAR_STATIC")), names(score_char(d2, "CHAR_STATIC")))
  expect_equal(score_char(d, "CHAR_STATIC")$SCORE, score_char(d2, "CHAR_STATIC")$SCORE)
})

test_that("score_*() error messages name the function the user called", {
  d <- make_unique_join()

  expect_error(score_num(d, "NOPE"), regexp = "score_num\\(\\)")
  expect_error(score_char(d, "NOPE"), regexp = "score_char\\(\\)")
  expect_error(score_dist(d, "NOPE"), regexp = "score_dist\\(\\)")
  expect_error(score_num_rank(d, "NOPE"), regexp = "score_num_rank\\(\\)")

  ## a wrapper naming itself via .fn_name keeps pointing at the function the
  ## user actually called, not at the score layer underneath it
  expect_error(score_num(d, "NOPE", .fn_name = "my_wrapper"),
               regexp = "my_wrapper\\(\\)")
})

## ---------------------------------------------------------------------------
## score layer: known values
## ---------------------------------------------------------------------------

test_that("score_num() is the absolute difference (hand-computed)", {
  raw <- data.frame(ROW_NUMBER = 1:2, V = c(10, 20))
  anon <- data.frame(ROW_NUMBER = 1:2, V = c(12, 25))
  d <- join_raw_anon_data(raw, anon)

  s <- score_num(d, "V")
  got <- s$SCORE[order(s$ANON_ROW_NUMBER, s$RAW_ROW_NUMBER)]
  ## ANON1(12) vs RAW1(10)=2, RAW2(20)=8 ; ANON2(25) vs RAW1(10)=15, RAW2(20)=5
  expect_equal(got, c(2, 8, 15, 5))
})

test_that("score_char() is the Levenshtein edit distance (hand-computed)", {
  raw <- data.frame(ROW_NUMBER = 1:2, V = c("abc", "xyz"), stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:2, V = c("abd", "xy"), stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  s <- score_char(d, "V")
  got <- s$SCORE[order(s$ANON_ROW_NUMBER, s$RAW_ROW_NUMBER)]
  ## "abd" vs "abc" = 1, vs "xyz" = 3 ; "xy" vs "abc" = 3, vs "xyz" = 1
  expect_equal(got, c(1, 3, 3, 1))
})

test_that("score_num_rank() is the gap between within-side ranks, with ties.method = 'min'", {
  raw <- data.frame(ROW_NUMBER = 1:4, V = c(5, 5, 7, 9))
  d <- join_raw_anon_data(raw, raw)

  s <- score_num_rank(d, "V")
  ## ranks with ties.method = "min": 5->1, 5->1, 7->3, 9->4 on both sides
  expected_rank <- c(1L, 1L, 3L, 4L)
  key <- order(s$ANON_ROW_NUMBER, s$RAW_ROW_NUMBER)
  got <- s$SCORE[key]
  want <- as.vector(t(outer(expected_rank, expected_rank, function(a, b) abs(a - b))))
  expect_equal(got, want)
})

test_that("score_dist() agrees with distribution_distance() pair by pair", {
  raw <- data.frame(
    ROW_NUMBER = 1:3, D = c("1:2:3", "4:5:6", "1:1:1"),
    stringsAsFactors = FALSE
  )
  d <- join_raw_anon_data(raw, raw)

  s <- score_dist(d, "D")
  want <- mapply(
    function(x, y) distribution_distance(x, y),
    d$RAW_D, d$ANON_D
  )
  expect_equal(s$SCORE, unname(want))
})

## ---------------------------------------------------------------------------
## assignment layer
## ---------------------------------------------------------------------------

test_that("match_greedy() returns exactly (ANON_ROW_NUMBER, RAW_ROW_NUMBER, CONFIDENCE, RESULT), one row per ANON record, ANON-ordered", {
  d <- make_master_join()
  m <- match_greedy(score_num(d, "NUM_DYNAMIC_MEAN"))

  expect_identical(
    names(m),
    c("ANON_ROW_NUMBER", "RAW_ROW_NUMBER", "CONFIDENCE", "RESULT")
  )
  expect_type(m$RESULT, "logical")
  expect_true(is.numeric(m$CONFIDENCE))
  expect_equal(nrow(m), length(unique(d$ANON_ROW_NUMBER)))
  expect_false(anyDuplicated(m$ANON_ROW_NUMBER) > 0)
  expect_false(is.unsorted(m$ANON_ROW_NUMBER))
  expect_true(all(m$CONFIDENCE > 0 & m$CONFIDENCE <= 1))
})

test_that("match_greedy() picks the unique argmin, with CONFIDENCE 1 when there is no tie", {
  d <- make_unique_join()
  ## confidence = "tie" is passed explicitly because this test is about the
  ## 1/k measure. Since #44 the default is "margin", under which these five
  ## records score 0.63 / 0.88 / 1.20 / 0.88 / 0.63 instead of a flat 1 --
  ## the resolution #44 wanted, and the reason the literal below needs the
  ## argument to keep meaning what it was written to mean.
  m <- match_greedy(score_num(d, "V"), confidence = "tie")

  ## ANON is an exact copy of RAW and V is unique => everyone is found
  expect_true(all(m$RESULT))
  expect_equal(m$RAW_ROW_NUMBER, 1:5)
  expect_equal(m$CONFIDENCE, rep(1, 5))
})

test_that("match_greedy() reports CONFIDENCE = 1/k for a tie group of size k", {
  d <- make_tied_join()
  m <- match_greedy(score_num(d, "V"), confidence = "tie")

  ## V = c(1,1,2,2,3,3): every ANON record ties with exactly one other RAW
  ## record at distance 0, so k = 2 everywhere.
  expect_equal(m$CONFIDENCE, rep(0.5, 6))

  ## Under the "margin" default the same records report eccentricity 0: the
  ## two best candidates are tied, so there is no gap to be confident about.
  ## Both statements are true of the same coin flip; they are different
  ## summaries of it, which is exactly what #44 changed the default over.
  expect_equal(match_greedy(score_num(d, "V"))$CONFIDENCE, rep(0, 6))

  ## and over many seeds the success rate converges on that same 1/2
  rate <- mean(vapply(1:200, function(s) mean(match_greedy(score_num(d, "V"), seed = s)$RESULT), numeric(1)))
  expect_equal(rate, 0.5, tolerance = 0.05)
})

test_that("match_greedy() is reproducible for a fixed seed and varies across seeds", {
  d <- make_tied_join()

  expect_identical(match_greedy(score_num(d, "V"), seed = 3), match_greedy(score_num(d, "V"), seed = 3))

  picks <- vapply(1:20, function(s) paste(match_greedy(score_num(d, "V"), seed = s)$RAW_ROW_NUMBER, collapse = "-"), character(1))
  expect_gt(length(unique(picks)), 1)
})

test_that("match_greedy() maximises a similarity score instead of minimising it", {
  scores <- data.frame(
    RAW_ROW_NUMBER = c(1, 2, 1, 2),
    ANON_ROW_NUMBER = c(1, 1, 2, 2),
    SCORE = c(0.9, 0.1, 0.2, 0.8)
  )

  as_distance <- scores
  attr(as_distance, "score_type") <- "distance"
  expect_equal(match_greedy(as_distance)$RAW_ROW_NUMBER, c(2, 1))

  as_similarity <- scores
  attr(as_similarity, "score_type") <- "similarity"
  expect_equal(match_greedy(as_similarity)$RAW_ROW_NUMBER, c(1, 2))
})

test_that("match_greedy() rejects input that is not a score table", {
  expect_error(match_greedy(data.frame(A = 1)), regexp = "missing score-layer column")
  expect_error(match_greedy("nope"), regexp = "must be a data frame")

  bad <- data.frame(RAW_ROW_NUMBER = 1, ANON_ROW_NUMBER = 1, SCORE = 1)
  attr(bad, "score_type") <- "whatever"
  expect_error(match_greedy(bad), regexp = "unknown score_type")
})

test_that("match_greedy() leaves the caller's RNG stream alone", {
  d <- make_tied_join()

  set.seed(99)
  before <- runif(3)
  set.seed(99)
  invisible(match_greedy(score_num(d, "V"), seed = 12345))
  after <- runif(3)

  expect_identical(before, after)
})

## ---------------------------------------------------------------------------
## integration layer
## ---------------------------------------------------------------------------

test_that("combine_scores() of a single table reproduces it unchanged", {
  d <- make_unique_join()
  s <- score_num(d, "V")
  c1 <- combine_scores(list(s))

  expect_equal(c1$SCORE, s$SCORE)
  expect_equal(c1$RAW_ROW_NUMBER, s$RAW_ROW_NUMBER)
  expect_equal(c1$ANON_ROW_NUMBER, s$ANON_ROW_NUMBER)
  expect_identical(attr(c1, "score_type"), "distance")
})

test_that("combine_scores() computes the weighted sum, matching a hand calculation", {
  d <- make_unique_join()
  sv <- score_num(d, "V")
  sw <- score_num(d, "W")

  got <- combine_scores(list(sv, sw), weights = c(2, 3))
  expect_equal(got$SCORE, 2 * sv$SCORE + 3 * sw$SCORE)

  ## default weights are all 1
  expect_equal(combine_scores(list(sv, sw))$SCORE, sv$SCORE + sw$SCORE)
})

test_that("combine_scores() aligns on the (ANON, RAW) pair, not on row order", {
  d <- make_unique_join()
  sv <- score_num(d, "V")
  sw <- score_num(d, "W")
  sw_shuffled <- sw[c(5:nrow(sw), 1:4), ]

  expect_equal(
    combine_scores(list(sv, sw_shuffled))$SCORE,
    combine_scores(list(sv, sw))$SCORE
  )
})

test_that("combine_scores() refuses to silently drop candidate pairs", {
  d <- make_unique_join()
  sv <- score_num(d, "V")
  sw <- score_num(d, "W")

  expect_error(
    combine_scores(list(sv, sw[-1, ])),
    regexp = "same \\(ANON_ROW_NUMBER, RAW_ROW_NUMBER\\) candidate pairs"
  )

  ## same row count, but a pair that does not exist in the first table
  sw_wrong <- sw
  sw_wrong$RAW_ROW_NUMBER[1] <- 999
  expect_error(combine_scores(list(sv, sw_wrong)), regexp = "unmatched")
})

test_that("combine_scores() validates its arguments", {
  d <- make_unique_join()
  s <- score_num(d, "V")

  expect_error(combine_scores(s), regexp = "must be a \\*list\\*")
  expect_error(combine_scores(list()), regexp = "at least one")
  expect_error(combine_scores(list(s), weights = c(1, 2)), regexp = "one entry per score table")
  expect_error(combine_scores(list(s), weights = -1), regexp = "non-negative")
  expect_error(combine_scores(list(s), weights = 0), regexp = "all zero")

  sim <- s
  attr(sim, "score_type") <- "similarity"
  expect_error(combine_scores(list(s, sim)), regexp = "same score_type")
})

## ---------------------------------------------------------------------------
## #57: combine_scores() does not normalise, and says so when it matters
## ---------------------------------------------------------------------------

## V spans 10..50 and W spans 1..2, so the weighted spreads differ by ~55x.
make_lopsided_join <- function() {
  raw <- data.frame(ROW_NUMBER = 1:5, V = c(10, 20, 30, 40, 50),
                    W = c(1, 1, 2, 2, 2))
  join_raw_anon_data(raw, raw)
}

test_that("combine_scores() warns when one component's weighted spread dominates", {
  d <- make_lopsided_join()
  big   <- score_num(d, "V")
  small <- score_num(d, "W")

  expect_warning(combine_scores(list(big, small)),
                 regexp = "very different scales")
  ## the message must name the offender and the direction of the error
  w <- tryCatch(combine_scores(list(big, small)), warning = conditionMessage)
  expect_match(w, "scores\\[\\[1\\]\\]")
  expect_match(w, "LOWERS the measured reidentification rate")

  ## names of the list are used when present, so the warning is readable
  named <- tryCatch(combine_scores(list(BIG = big, SMALL = small)),
                    warning = conditionMessage)
  expect_match(named, "`BIG`")
  expect_match(named, "`SMALL`")
})

test_that("combine_scores() stays quiet when the scales are comparable", {
  d <- make_unique_join()
  ## V sd ~12.2, W sd ~2.4: a 5x gap, below the 10x threshold
  expect_silent(combine_scores(list(score_num(d, "V"), score_num(d, "W"))))

  ## normalising first is the documented fix, and it must silence the warning
  d2 <- make_lopsided_join()
  parts <- normalize_scores(list(score_num(d2, "V"), score_num(d2, "W")), "range")
  expect_silent(combine_scores(parts))
})

test_that("weights that compensate for the scale gap silence the warning", {
  ## The check looks at the spread each component contributes *after* its
  ## weight: a caller who has already corrected the gap must not be nagged.
  d <- make_lopsided_join()
  big   <- score_num(d, "V")
  small <- score_num(d, "W")
  w <- stats::sd(big$SCORE) / stats::sd(small$SCORE)

  expect_silent(combine_scores(list(big, small), weights = c(1, w)))
  ## ... and a weight that *creates* a gap is flagged just the same
  expect_warning(combine_scores(list(big, small), weights = c(1, w * 1000)),
                 regexp = "very different scales")
})

test_that("scale_check = \"none\" turns the check off", {
  d <- make_lopsided_join()
  expect_silent(
    combine_scores(list(score_num(d, "V"), score_num(d, "W")),
                   scale_check = "none")
  )
  expect_error(
    combine_scores(list(score_num(d, "V")), scale_check = "bogus"),
    regexp = "should be one of"
  )
})

test_that("components that cannot reorder anything do not trip the check", {
  ## A zero-weight component, and a component whose score is constant, add a
  ## constant to every candidate. Counting them would make the ratio infinite
  ## and fire on something that provably changes no ranking.
  d <- make_lopsided_join()
  big   <- score_num(d, "V")
  small <- score_num(d, "W")

  expect_silent(combine_scores(list(big, small), weights = c(1, 0)))

  flat <- big
  flat$SCORE <- rep(1, nrow(flat))
  attr(flat, "score_type") <- attr(big, "score_type")
  expect_silent(combine_scores(list(big, flat)))
})

test_that("a dominated informative axis is exactly the failure the warning describes", {
  ## Two columns: SIGNAL identifies every record, NOISE is pure noise on a
  ## scale 1000x wider. Summed without normalisation the attack collapses to
  ## the noise column; normalised, it recovers. The warning fires on the case
  ## that loses, which is the whole point of #57.
  set.seed(57)
  n <- 40
  raw <- data.frame(ROW_NUMBER = seq_len(n),
                    SIGNAL = seq_len(n),
                    NOISE = rnorm(n, sd = 1000))
  anon <- raw
  anon$NOISE <- rnorm(n, sd = 1000)   # carries no information at all
  j <- join_raw_anon_data(raw, anon)

  parts <- list(SIGNAL = score_num(j, "SIGNAL"), NOISE = score_num(j, "NOISE"))

  expect_warning(dominated <- combine_scores(parts), regexp = "very different scales")
  fixed <- combine_scores(normalize_scores(parts, "range"))

  rate <- function(s) mean(reid_evaluate(s, seeds = 1:5)$per_record$RISK)
  expect_lt(rate(dominated), rate(fixed))
  ## and the dominated sum is worse than SIGNAL on its own: more knowledge,
  ## a lower reported risk
  expect_lt(rate(dominated), rate(parts$SIGNAL))
})

test_that("combining two attributes finds records that neither attribute finds alone", {
  ## V alone and W alone each leave every ANON record in a tie of 2, but the
  ## pair (V, W) is unique for every record.
  raw <- data.frame(
    ROW_NUMBER = 1:4,
    V = c(1, 1, 2, 2),
    W = c(1, 2, 1, 2)
  )
  d <- join_raw_anon_data(raw, raw)

  ## confidence = "tie" makes the tie sizes the assertions talk about visible;
  ## the default has been "margin" since #44.
  m_v <- match_greedy(score_num(d, "V"), seed = 1, confidence = "tie")
  m_w <- match_greedy(score_num(d, "W"), seed = 1, confidence = "tie")
  m_both <- match_greedy(combine_scores(list(score_num(d, "V"), score_num(d, "W"))),
                         seed = 1, confidence = "tie")

  expect_equal(m_v$CONFIDENCE, rep(0.5, 4))
  expect_equal(m_w$CONFIDENCE, rep(0.5, 4))
  expect_equal(m_both$CONFIDENCE, rep(1, 4))
  expect_true(all(m_both$RESULT))
})

## ---------------------------------------------------------------------------
## the assignment really is the argmin of the score it was handed
##
## The successor of the reid_by_*()-vs-layers equivalence test: with only one
## implementation left, the property to hold on to is that match_greedy() picks
## a minimal-SCORE candidate of the score table it was given -- for every one
## of the four scores, and for every seed. If the two ever came apart, the
## reported rate would be the rate of a different attack than the one the
## caller described.
## ---------------------------------------------------------------------------

test_that("match_greedy() picks a minimal-SCORE candidate of the score table, for all 4 scores and every seed", {
  d <- make_master_join()

  cases <- list(
    num = function() score_num(d, "NUM_DYNAMIC_MEAN"),
    char = function() score_char(d, "CHAR_STATIC"),
    dist = function() score_dist(d, "NUM_DYNAMIC_DIST"),
    rank = function() score_num_rank(d, "NUM_DYNAMIC_MEAN"),
    ## tie-heavy columns: the seed actually matters here
    num_tied = function() score_num(d, "BIN_MEAN"),
    rank_tied = function() score_num_rank(d, "BIN_MEAN")
  )

  for (nm in names(cases)) {
    sc <- cases[[nm]]()
    best <- tapply(sc$SCORE, sc$ANON_ROW_NUMBER, min)

    for (s in c(0L, 1L, 7L, 42L)) {
      lab <- paste(nm, "seed", s)
      m <- match_greedy(sc, seed = s)

      ## exactly one row per ANON record, ANON-ordered
      expect_equal(nrow(m), length(unique(sc$ANON_ROW_NUMBER)), info = lab)
      expect_false(is.unsorted(m$ANON_ROW_NUMBER), info = lab)

      ## the pair chosen for each ANON record exists in the score table, and
      ## its SCORE is that record's minimum
      idx <- match(
        paste(m$ANON_ROW_NUMBER, m$RAW_ROW_NUMBER),
        paste(sc$ANON_ROW_NUMBER, sc$RAW_ROW_NUMBER)
      )
      expect_false(anyNA(idx), info = lab)
      expect_equal(
        as.numeric(sc$SCORE[idx]),
        as.numeric(best[as.character(m$ANON_ROW_NUMBER)]),
        info = lab
      )

      ## RESULT says exactly whether the guess was the true record
      expect_equal(m$RESULT, m$ANON_ROW_NUMBER == m$RAW_ROW_NUMBER, info = lab)
    }
  }
})

test_that("score_dist() honours the `split` argument (it used to be accepted and ignored)", {
  ## Same numbers, written with two different separators. Parsed correctly,
  ## both give exactly the same distances.
  raw_colon <- data.frame(
    ROW_NUMBER = 1:4, D = c("1:2:3", "4:5:6", "7:8:9", "1:5:9"),
    stringsAsFactors = FALSE
  )
  ## NB: `split` is handed to strsplit(fixed = TRUE) and is therefore a
  ## *literal* string; regex metacharacters such as "|" or "." are valid
  ## separators (Issue #32). See test-split-literal.R.
  raw_semi <- raw_colon
  raw_semi$D <- gsub(":", ";", raw_semi$D, fixed = TRUE)

  d_colon <- join_raw_anon_data(raw_colon, raw_colon)
  d_semi <- join_raw_anon_data(raw_semi, raw_semi)

  s_colon <- score_dist(d_colon, "D")
  s_semi <- score_dist(d_semi, "D", split = ";")

  expect_equal(s_colon$SCORE, s_semi$SCORE)
  expect_equal(
    match_greedy(s_colon, seed = 1)$RESULT,
    match_greedy(s_semi, seed = 1)$RESULT
  )

  ## and without split= the semicolon-separated data is not parseable as
  ## numbers, rather than being silently mis-scored
  expect_error(score_dist(d_semi, "D"), regexp = "numeric")
})

test_that("print methods for the new objects work and return invisibly", {
  d <- make_unique_join()
  s <- score_num(d, "V")

  expect_output(print(s), "reid scores \\(distance\\)")
  expect_identical(
    withVisible(capture.output(res <- print(s)))$visible,
    TRUE
  )
  expect_identical(res, s)
})
