## Tests for the activity profile scores (#22): record count, profile shape and
## activity span.
##
## The acceptance criteria on the issue are checked at the bottom on a fixture
## that goes through the real transform_transaction_to_master() pipeline --
## which is where ROWCOUNT and the collapsed <col>_DIST columns come from, and
## the reason the issue exists.

DOW7 <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

count_join <- function(raw_counts, anon_counts) {
  join_raw_anon_data(
    data.frame(ROW_NUMBER = seq_along(raw_counts), ROWCOUNT = raw_counts),
    data.frame(ROW_NUMBER = seq_along(anon_counts), ROWCOUNT = anon_counts)
  )
}

profile_join <- function(raw, anon = raw) {
  join_raw_anon_data(
    data.frame(ROW_NUMBER = seq_along(raw), P = raw, stringsAsFactors = FALSE),
    data.frame(ROW_NUMBER = seq_along(anon), P = anon, stringsAsFactors = FALSE)
  )
}

## The transaction -> master pipeline, with two anonymisers that treat the
## record count completely differently. See docs/investigation/activity-
## benchmark.R for the same generator measured across more seeds.
activity_master <- function(n = 60, mode = c("jitter", "subsample"),
                            keep = 0.6, seed = 1) {
  mode <- match.arg(mode)
  set.seed(seed)

  rate <- rpois(n, lambda = 12) + 3
  favourite <- sample.int(7L, n, replace = TRUE)
  start <- sample.int(300L, n, replace = TRUE)
  window <- sample(c(10L, 30L, 90L, 300L), n, replace = TRUE)

  make_master <- function(counts) {
    rows <- lapply(seq_len(n), function(i) {
      p <- rep(1, 7)
      p[favourite[i]] <- 4
      data.frame(
        ID = i,
        DOW = DOW7[sample.int(7L, counts[i], replace = TRUE, prob = p)],
        DAY = start[i] + sample.int(window[i], counts[i], replace = TRUE),
        stringsAsFactors = FALSE
      )
    })
    tran <- do.call(rbind, rows)
    tran$ROW_NUMBER <- seq_len(nrow(tran))
    m <- transform_transaction_to_master(
      tran, ROW_NUMBER = "ROW_NUMBER", ID = "ID",
      DYNAMIC_NUM = "DAY", DYNAMIC_CHAR = "DOW"
    )
    m$ROW_NUMBER <- m$ID
    as.data.frame(m)
  }

  anon_counts <- if (identical(mode, "jitter")) {
    pmax(1L, rate + sample(-1:1, n, replace = TRUE))
  } else {
    pmax(1L, rbinom(n, rate, keep))
  }
  join_raw_anon_data(make_master(rate), make_master(anon_counts))
}

random_baseline <- function(scores, seeds = 1:5) {
  e <- reid_evaluate(scores, seeds = seeds)
  e$baseline$rate[e$baseline$method == "random"]
}

## ---------------------------------------------------------------------------
## score_count()
## ---------------------------------------------------------------------------

test_that("score_count() defaults to the ROWCOUNT column the master format makes", {
  j <- count_join(c(1, 5, 20), c(1, 5, 20))
  s <- score_count(j)
  same <- j$RAW_ROW_NUMBER == j$ANON_ROW_NUMBER
  expect_true(all(s$SCORE[same] == 0))
  expect_true(all(s$SCORE[!same] > 0))
})

test_that("score_count() implements each documented comparison", {
  j <- count_join(c(0, 3, 10), c(0, 3, 10))
  r <- j$RAW_ROWCOUNT
  a <- j$ANON_ROWCOUNT

  expect_equal(score_count(j)$SCORE, abs(log1p(r) - log1p(a)))
  expect_equal(score_count(j, method = "absolute")$SCORE, abs(r - a))
  expect_equal(score_count(j, method = "relative")$SCORE,
               abs(r - a) / pmax(r, a, 1))
  expect_true(all(score_count(j, method = "relative")$SCORE <= 1))
})

test_that("a count of zero is a value, not an infinity", {
  ## log(), rather than log1p(), would put every zero-activity record
  ## infinitely far from everything and make it unmatchable.
  j <- count_join(c(0, 0, 4), c(0, 1, 4))
  expect_true(all(is.finite(score_count(j)$SCORE)))
})

test_that("the log-ratio comparison weights a gap by how large the counts are", {
  ## 2 vs 7 is strong evidence of two different people; 200 vs 205 is nothing.
  ## An absolute difference cannot tell those apart.
  small <- count_join(2, 7)
  large <- count_join(200, 205)

  expect_equal(score_count(small, method = "absolute")$SCORE,
               score_count(large, method = "absolute")$SCORE)
  expect_gt(score_count(small)$SCORE, 10 * score_count(large)$SCORE)
})

test_that("score_count() rejects anything that is not a count", {
  j <- count_join(c(1, 2), c(1, 2))
  j$RAW_ROWCOUNT[1] <- -1
  expect_error(score_count(j), regexp = "negative")

  j2 <- count_join(c(1, 2), c(1, 2))
  j2$ANON_ROWCOUNT[1] <- NA
  expect_error(score_count(j2), regexp = "NA")

  j3 <- join_raw_anon_data(
    data.frame(ROW_NUMBER = 1:2, ROWCOUNT = c("a", "b"), stringsAsFactors = FALSE),
    data.frame(ROW_NUMBER = 1:2, ROWCOUNT = c("a", "b"), stringsAsFactors = FALSE)
  )
  expect_error(score_count(j3), regexp = "not numeric")

  expect_error(score_count(count_join(1:2, 1:2), target = "NOPE"), regexp = "NOPE")
})

## ---------------------------------------------------------------------------
## score_profile()
## ---------------------------------------------------------------------------

test_that("score_profile() compares normalised histograms over the declared bins", {
  j <- profile_join(c("Mon:Mon:Tue", "Mon:Tue:Tue"))
  s <- score_profile(j, "P", bins = DOW7)

  ## (2/3, 1/3, 0, ...) against (1/3, 2/3, 0, ...): L1 distance 2/3
  cross <- s$SCORE[j$RAW_ROW_NUMBER == 1 & j$ANON_ROW_NUMBER == 2]
  expect_equal(cross, 2 / 3)
  expect_equal(s$SCORE[j$RAW_ROW_NUMBER == j$ANON_ROW_NUMBER], c(0, 0))
})

test_that("l2 squares the per-bin gaps instead of adding their absolute values", {
  j <- profile_join(c("Mon:Mon:Tue", "Mon:Tue:Tue"))
  l2 <- score_profile(j, "P", bins = DOW7, metric = "l2")
  cross <- l2$SCORE[j$RAW_ROW_NUMBER == 1 & j$ANON_ROW_NUMBER == 2]
  expect_equal(cross, (1 / 3)^2 + (1 / 3)^2)
})

test_that("the shape score does not move when every event is duplicated", {
  ## The property that keeps this axis separate from score_count(). If volume
  ## leaked in here as well, adding the two together in score_multi() would
  ## count the same evidence twice -- and Issue #5 removed the count from
  ## distribution_distance() precisely to stop that.
  raw <- c("Mon:Mon:Tue", "Sat:Sun", "Wed:Wed:Wed:Thu", "Mon:Fri")
  doubled <- vapply(strsplit(raw, ":", fixed = TRUE),
                    function(v) paste(rep(v, 2), collapse = ":"), character(1))
  tripled <- vapply(strsplit(raw, ":", fixed = TRUE),
                    function(v) paste(rep(v, 3), collapse = ":"), character(1))

  base <- score_profile(profile_join(raw, raw), "P", bins = DOW7)$SCORE
  expect_equal(score_profile(profile_join(raw, doubled), "P", bins = DOW7)$SCORE, base)
  expect_equal(score_profile(profile_join(tripled, doubled), "P", bins = DOW7)$SCORE, base)
})

test_that("shape_only = FALSE deliberately does let the volume back in", {
  raw <- c("Mon:Mon:Tue", "Sat:Sun")
  doubled <- vapply(strsplit(raw, ":", fixed = TRUE),
                    function(v) paste(rep(v, 2), collapse = ":"), character(1))
  base <- score_profile(profile_join(raw, raw), "P", bins = DOW7, shape_only = FALSE)
  scaled <- score_profile(profile_join(raw, doubled), "P", bins = DOW7,
                          shape_only = FALSE)
  expect_false(isTRUE(all.equal(base$SCORE, scaled$SCORE)))
})

test_that("bins are labels, not numbers", {
  ## An hour-of-day profile must not be run through anything that thinks 23
  ## and 0 are 23 apart while 0 and 1 are 1 apart.
  j <- profile_join(c("23", "0", "1"))
  s <- score_profile(j, "P", bins = as.character(0:23))
  pair <- function(r, a) s$SCORE[s$RAW_ROW_NUMBER == r & s$ANON_ROW_NUMBER == a]
  expect_equal(pair(1, 2), pair(2, 3))
  expect_equal(pair(1, 3), pair(1, 2))
})

test_that("bins = NULL uses every value seen on either side", {
  j <- profile_join(c("a:b", "c"), c("a", "d:d"))
  auto <- score_profile(j, "P")
  explicit <- score_profile(j, "P", bins = c("a", "b", "c", "d"))
  expect_equal(auto$SCORE, explicit$SCORE)

  ## a bin nobody uses simply adds zero
  padded <- score_profile(j, "P", bins = c("a", "b", "c", "d", "z"))
  expect_equal(padded$SCORE, explicit$SCORE)
})

test_that("a value outside the declared bins is dropped from the histogram", {
  ## Restricting `bins` is how a caller says what the support is; a value
  ## outside it contributes to neither side.
  j <- profile_join(c("Mon:Zzz", "Mon"))
  s <- score_profile(j, "P", bins = DOW7)
  expect_equal(s$SCORE, rep(0, nrow(j)))
})

test_that("score_profile() rejects NA and a malformed bin set", {
  j <- profile_join(c("Mon", "Tue"))
  j$ANON_P[1] <- NA
  expect_error(score_profile(j, "P", bins = DOW7), regexp = "NA")

  ok <- profile_join(c("Mon", "Tue"))
  expect_error(score_profile(ok, "P", bins = c("Mon", "Mon")), regexp = "distinct")
  expect_error(score_profile(ok, "P", bins = character(0)), regexp = "distinct")
})

test_that("the separator is taken literally, not as a regular expression", {
  ## "." as a regex matches every character and would split "1.2" into empty
  ## strings. The collapsed columns are written with paste(collapse = ), which
  ## is literal, so they are read back literally.
  j <- profile_join(c("a.b", "b.b"))
  s <- score_profile(j, "P", split = ".", bins = c("a", "b"))
  cross <- s$SCORE[j$RAW_ROW_NUMBER == 1 & j$ANON_ROW_NUMBER == 2]
  expect_equal(cross, 1)
})

## ---------------------------------------------------------------------------
## score_span()
## ---------------------------------------------------------------------------

test_that("score_span() compares max - min of the collapsed values", {
  j <- profile_join(c("1:2:3", "1:40", "5:6:7:80"))
  spans <- c(2, 39, 75)
  s <- score_span(j, "P")
  expect_equal(
    s$SCORE,
    abs(log1p(spans[j$RAW_ROW_NUMBER]) - log1p(spans[j$ANON_ROW_NUMBER]))
  )
})

test_that("a single-event record has a span of zero, and order does not matter", {
  j <- profile_join(c("7", "3:9:1", "9:1:3"))
  s <- score_span(j, "P", method = "absolute")
  ## records 2 and 3 hold the same values in a different order
  expect_equal(s$SCORE[s$RAW_ROW_NUMBER == 2 & s$ANON_ROW_NUMBER == 3], 0)
  ## record 1 has one event, so span 0, and is 8 away from the others' span 8
  expect_equal(s$SCORE[s$RAW_ROW_NUMBER == 1 & s$ANON_ROW_NUMBER == 2], 8)
})

test_that("score_span() needs a numeric collapsed column", {
  expect_error(score_span(profile_join(c("Mon:Tue", "Wed")), "P"),
               regexp = "numeric")
})

## ---------------------------------------------------------------------------
## integration with #14
## ---------------------------------------------------------------------------

test_that("the new score types are registered and dispatch correctly", {
  expect_true(all(c("count", "profile", "span") %in% reid_score_types()))

  j <- profile_join(c("1:2:3", "1:40", "5:6:7:80"))
  expect_equal(score_multi(j, c(P = "span"), normalize = "none")$SCORE,
               score_span(j, "P")$SCORE)
  expect_equal(score_multi(j, c(P = "profile"), normalize = "none")$SCORE,
               score_profile(j, "P")$SCORE)

  jc <- count_join(c(1, 5, 20), c(1, 5, 20))
  expect_equal(score_multi(jc, c(ROWCOUNT = "count"), normalize = "none")$SCORE,
               score_count(jc)$SCORE)
})

test_that("score_multi() passes the separator to the profile and span types", {
  j <- profile_join(c("1|2|3", "1|40"))
  expect_equal(
    score_multi(j, c(P = "span"), split = "|", normalize = "none")$SCORE,
    score_span(j, "P", split = "|")$SCORE
  )
  expect_error(score_multi(j, c(P = "span")), regexp = "numeric")
})

test_that("the knowledge model accepts activity columns", {
  k <- attacker_knowledge(
    "M",
    quasi_identifiers = c(AGE = "num"),
    behavior = c(ROWCOUNT = "count", DOW_DIST = "profile", DAY_DIST = "span")
  )
  expect_setequal(names(k$visible),
                  c("AGE", "ROWCOUNT", "DOW_DIST", "DAY_DIST"))
})

## ---------------------------------------------------------------------------
## the acceptance criteria on the issue
## ---------------------------------------------------------------------------

test_that("each activity axis alone beats the random-assignment baseline", {
  for (s in 1:3) {
    j <- activity_master(mode = "jitter", seed = s)
    base <- random_baseline(score_count(j))

    expect_gt(reid_evaluate(score_count(j), seeds = 1:5)$success_analytic, base)
    expect_gt(reid_evaluate(score_profile(j, "DOW_DIST", bins = DOW7),
                            seeds = 1:5)$success_analytic, base)
    expect_gt(reid_evaluate(score_span(j, "DAY_DIST"), seeds = 1:5)$success_analytic,
              base)
  }
})

test_that("adding the activity block to a static attack raises the success rate", {
  for (s in 1:3) {
    j <- activity_master(mode = "jitter", seed = s)
    static <- reid_evaluate(score_multi(j, c(DAY_MEAN = "num")),
                            seeds = 1:5)$success_analytic
    both <- reid_evaluate(
      score_multi(j, c(DAY_MEAN = "num", ROWCOUNT = "count",
                       DOW_DIST = "profile", DAY_DIST = "span")),
      seeds = 1:5
    )$success_analytic
    expect_gt(both, static)
  }
})

test_that("the three activity axes carry different evidence", {
  ## The justification for splitting them (and for Issue #5 having taken the
  ## count out of the distribution distance) is that each sees something the
  ## others cannot. If they were near-duplicates, summing them in score_multi()
  ## would triple-count one fact.
  j <- activity_master(mode = "jitter", seed = 2)
  cnt <- score_count(j)$SCORE
  shp <- score_profile(j, "DOW_DIST", bins = DOW7)$SCORE
  spn <- score_span(j, "DAY_DIST")$SCORE

  expect_lt(abs(cor(cnt, shp)), 0.3)
  expect_lt(abs(cor(cnt, spn)), 0.3)
  expect_lt(abs(cor(shp, spn)), 0.3)
})

test_that("a subsampling anonymiser destroys the count axis but not the shape axis", {
  ## Measured, and worth keeping visible: when the release keeps only a
  ## fraction of each person's events, every count is scaled by roughly the
  ## same factor, so the nearest count is no longer the right person. The
  ## profile score is scale-free by construction and survives.
  j <- activity_master(mode = "subsample", seed = 1)
  base <- random_baseline(score_count(j))

  count_rate <- reid_evaluate(score_count(j), seeds = 1:5)$success_analytic
  shape_rate <- reid_evaluate(score_profile(j, "DOW_DIST", bins = DOW7),
                              seeds = 1:5)$success_analytic

  expect_lt(count_rate, 2 * base)
  expect_gt(shape_rate, 2 * base)
})
