## Regression tests for Issue #3: tie-breaking must not depend on input row
## order, must be reproducible from a seed, and the run-to-run spread must be
## reportable.
##
## Before the fix, ties were resolved by keeping RAW_ROW_NUMBER[1] -- the
## first tied candidate in input order. On a 50-person fixture with a
## 3-valued column that made the reported success rate range over
## [0.02, 0.14] purely as a function of how the input rows happened to be
## ordered, and it concentrated every possible success onto the 3 records
## that led their tie group.
##
## These used to run through reid_by_num(); the wrappers were removed in
## 3.0.0 and the tie-break itself never lived there -- it is
## resolve_min_distance_ties(), reached from match_greedy(), which is the
## single assignment entry point every score feeds into. The tests were moved
## onto that path rather than deleted with the wrappers.

make_tie_heavy <- function(people = 50, seed = 71) {
  set.seed(seed)
  raw <- suppressWarnings(create_dummy_master_data(people = people))
  ## ANON is an exact copy => the true match always exists, and BIN has only
  ## 3 distinct values so every ANON record has a large tie group.
  join_raw_anon_data(raw, raw)
}

## the attack these tests measure the spread of: a score plus an assignment
## rule. reid_stability() takes a whole attack, so it is written once here.
attack_num <- function(dat, target, seed) {
  match_greedy(score_num(dat, target), seed = seed)
}

test_that("the same seed gives an identical result", {
  d <- make_tie_heavy()

  a <- match_greedy(score_num(d, "BIN"), seed = 42)
  b <- match_greedy(score_num(d, "BIN"), seed = 42)

  expect_identical(a, b)
})

test_that("different seeds actually change which tied candidate is chosen", {
  d <- make_tie_heavy()
  s <- score_num(d, "BIN")

  rates <- vapply(1:20, function(sd) mean(match_greedy(s, seed = sd)$RESULT), numeric(1))

  expect_gt(length(unique(rates)), 1)
})

test_that("input row order no longer changes the result for a fixed seed", {
  d <- make_tie_heavy()

  baseline <- match_greedy(score_num(d, "BIN"), seed = 7)

  set.seed(1)
  for (i in 1:10) {
    shuffled <- d[sample(nrow(d)), ]
    got <- match_greedy(score_num(shuffled, "BIN"), seed = 7)
    ## row.names differ after subsetting; compare the payload only
    expect_equal(got$ANON_ROW_NUMBER, baseline$ANON_ROW_NUMBER)
    expect_equal(got$RAW_ROW_NUMBER, baseline$RAW_ROW_NUMBER)
    expect_equal(got$RESULT, baseline$RESULT)
  }
})

test_that("output is ordered by ANON_ROW_NUMBER regardless of input order", {
  d <- make_tie_heavy()

  r <- match_greedy(score_num(d, "BIN"), seed = 5)
  expect_false(is.unsorted(r$ANON_ROW_NUMBER))

  r2 <- match_greedy(score_num(d[sample(nrow(d)), ], "BIN"), seed = 5)
  expect_identical(r$ANON_ROW_NUMBER, r2$ANON_ROW_NUMBER)
})

test_that("passing seed= does not disturb the caller's RNG stream", {
  d <- make_tie_heavy(people = 20)
  s <- score_num(d, "BIN")

  set.seed(99)
  before <- runif(3)

  set.seed(99)
  invisible(match_greedy(s, seed = 12345))
  after <- runif(3)

  expect_identical(before, after)
})

test_that("all 4 score functions accept a seeded assignment and stay reproducible", {
  d <- make_tie_heavy(people = 20)

  expect_identical(
    match_greedy(score_num(d, "BIN"), seed = 3),
    match_greedy(score_num(d, "BIN"), seed = 3)
  )
  expect_identical(
    match_greedy(score_char(d, "CHAR"), seed = 3),
    match_greedy(score_char(d, "CHAR"), seed = 3)
  )
  expect_identical(
    match_greedy(score_num_rank(d, "BIN"), seed = 3),
    match_greedy(score_num_rank(d, "BIN"), seed = 3)
  )

  raw <- data.frame(
    ROW_NUMBER = 1:10, ID = 1:10,
    D = rep(c("1:2:3", "4:5:6"), length.out = 10), stringsAsFactors = FALSE
  )
  dd <- join_raw_anon_data(raw, raw)
  expect_identical(
    match_greedy(score_dist(dd, "D"), seed = 3),
    match_greedy(score_dist(dd, "D"), seed = 3)
  )
})

test_that("random tie-breaking is unbiased: the mean rate matches mean(1 / tie_size)", {
  d <- make_tie_heavy()

  tie_sizes <- tapply(
    abs(d$RAW_BIN - d$ANON_BIN), d$ANON_ROW_NUMBER,
    function(v) sum(v == min(v))
  )
  theoretical <- mean(1 / tie_sizes)

  st <- reid_stability(attack_num, d, "BIN", seeds = 1:200)

  ## sampling error over 200 seeds is small; allow a generous tolerance
  expect_equal(st$mean, theoretical, tolerance = 0.02)

  ## reid_evaluate() computes the same expectation analytically rather than by
  ## simulation, from the same tie structure. The two routes agreeing is the
  ## "would I notice if this broke" check docs/lessons-learned.md section 2
  ## asks for, so it is asserted here as well as inside test-evaluate.R.
  e <- reid_evaluate(score_num(d, "BIN"), seeds = 1:20, top_k = 1)
  expect_equal(e$success_analytic, theoretical, tolerance = 1e-8)
})

test_that("every record becomes reachable, not just the head of each tie group", {
  d <- make_tie_heavy()
  s <- score_num(d, "BIN")

  ever_hit <- rowSums(vapply(
    1:100,
    function(sd) as.integer(match_greedy(s, seed = sd)$RESULT),
    numeric(50)
  ))

  ## The deterministic RAW_ROW_NUMBER[1] rule could only ever succeed on the
  ## 3 records leading the 3 BIN tie groups; random tie-breaking must spread
  ## the chance across essentially all records.
  expect_gt(sum(ever_hit > 0), 40)
})

test_that("reid_stability reports mean, sd and range over seeds", {
  d <- make_tie_heavy()

  st <- reid_stability(attack_num, d, "BIN", seeds = 1:30)

  expect_s3_class(st, "reid_stability")
  expect_setequal(
    names(st),
    c("per_seed", "mean", "sd", "min", "max", "trial", "n_seeds")
  )
  expect_equal(nrow(st$per_seed), 30)
  expect_setequal(names(st$per_seed), c("seed", "success", "trial", "rate"))
  expect_equal(st$n_seeds, 30)
  expect_equal(st$trial, 50)

  ## a tie-heavy column must show real spread
  expect_gt(st$sd, 0)
  expect_true(st$min <= st$mean && st$mean <= st$max)

  ## printing works and returns invisibly
  expect_output(print(st), "reid stability over 30 tie-break seeds")
})

test_that("reid_stability reports sd 0 for a collision-free column", {
  d <- make_tie_heavy()

  ## NUM is continuous, so ANON == RAW matches uniquely: no ties, no spread
  st <- reid_stability(attack_num, d, "NUM", seeds = 1:10)

  expect_equal(st$mean, 1)
  expect_equal(st$sd, 0)
})

test_that("reid_stability validates its seeds argument", {
  d <- make_tie_heavy(people = 10)

  expect_error(reid_stability(attack_num, d, "BIN", seeds = 1), regexp = "at least 2")
  expect_error(reid_stability(attack_num, d, "BIN", seeds = c(1, 1, 2)), regexp = "duplicate")
})

test_that("reid_stability accepts a function given by name, and passes ... through", {
  d <- make_tie_heavy(people = 20)

  ## match.fun(): a character name resolves to the function of that name
  assign("attack_by_name", attack_num, envir = globalenv())
  on.exit(rm("attack_by_name", envir = globalenv()), add = TRUE)
  st <- reid_stability("attack_by_name", d, "BIN", seeds = 1:5)
  expect_s3_class(st, "reid_stability")

  ## extra arguments reach the attack. Fixture: V = c(10, 20, 30, 30), so
  ## records 1-2 have a unique best candidate (tie confidence 1) and records
  ## 3-4 are indistinguishable from each other (tie confidence 1/2). Attacking
  ## everything gives mean rate (1 + 1 + 1/2 + 1/2) / 4 = 0.75; declining below
  ## confidence 0.9 drops records 3-4 -- they keep their row, so the trial
  ## count is still 4 and the rate is exactly 0.5.
  raw <- data.frame(ROW_NUMBER = 1:4, V = c(10, 20, 30, 30))
  d4 <- join_raw_anon_data(raw, raw)

  cautious <- function(dat, target, seed, min_confidence = 0) {
    match_greedy(score_num(dat, target), seed = seed,
                 confidence = "tie", min_confidence = min_confidence)
  }

  st_all <- reid_stability(cautious, d4, "V", seeds = 1:200)
  expect_equal(st_all$trial, 4)
  expect_equal(st_all$mean, 0.75, tolerance = 0.05)

  st_cautious <- reid_stability(cautious, d4, "V", seeds = 1:5,
                                min_confidence = 0.9)
  expect_equal(st_cautious$trial, 4)
  expect_equal(st_cautious$mean, 0.5)
  expect_equal(st_cautious$sd, 0)
})
