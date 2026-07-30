## Tests for the W / M / S attacker knowledge model (#13).
##
## The acceptance criterion on the issue is that the same data yields a
## monotonically increasing success rate as the attacker is given more to work
## with. That is checked here on a fixture where ANON really has been
## generalised, because on an identity join (ANON an exact copy of RAW) level M
## already reaches 1.0 and level S has nowhere left to go -- see the note on
## the strict-monotonicity test below.

## An anonymiser: coarsen the quasi-identifiers and the behavioural summaries,
## but leave FINGERPRINT exact. FINGERPRINT standing in for "the RAW record
## itself" is what makes level S different from level M.
generalize_qi <- function(x) {
  x$AGE <- (x$AGE %/% 10) * 10
  x$ZIP <- substr(x$ZIP, 1, 3)
  x$VISIT_COUNT <- (x$VISIT_COUNT %/% 5) * 5
  x$SPEND_MEAN <- round(x$SPEND_MEAN / 25) * 25
  x$SPEND_DIST <- vapply(
    strsplit(x$SPEND_DIST, ":", fixed = TRUE),
    function(v) paste(round(as.numeric(v) / 20) * 20, collapse = ":"),
    character(1)
  )
  x
}

make_generalized_join <- function(people = 60, seed = 7) {
  raw <- create_dummy_qi_data(people = people, seed = seed)
  join_raw_anon_data(raw, generalize_qi(raw))
}

make_identity_qi_join <- function(people = 30, seed = 3) {
  raw <- create_dummy_qi_data(people = people, seed = seed)
  join_raw_anon_data(raw, raw)
}

qi_args <- list(
  quasi_identifiers = c(ZIP = "char", AGE = "num", SEX = "char"),
  behavior = c(VISIT_COUNT = "num", SPEND_MEAN = "num", SPEND_DIST = "dist"),
  identifiers = c(FINGERPRINT = "num"),
  weak_subset = "ZIP"
)

## ---------------------------------------------------------------------------
## the knowledge object
## ---------------------------------------------------------------------------

test_that("each level exposes exactly the columns its definition allows", {
  w <- dummy_qi_knowledge("W")
  m <- dummy_qi_knowledge("M")
  s <- dummy_qi_knowledge("S")

  expect_equal(names(w$visible), "ZIP")
  expect_setequal(names(m$visible), c("ZIP", "AGE", "SEX", "VISIT_COUNT", "SPEND_MEAN", "SPEND_DIST"))
  expect_setequal(names(s$visible), c(names(m$visible), "FINGERPRINT"))

  ## the identifying column is the thing that separates S from M
  expect_false("FINGERPRINT" %in% names(m$visible))
  expect_true("FINGERPRINT" %in% names(s$visible))
})

test_that("knowledge is nested: W is a subset of M is a subset of S", {
  w <- dummy_qi_knowledge("W")
  m <- dummy_qi_knowledge("M")
  s <- dummy_qi_knowledge("S")

  expect_true(all(names(w$visible) %in% names(m$visible)))
  expect_true(all(names(m$visible) %in% names(s$visible)))
  expect_lt(length(w$visible), length(m$visible))
  expect_lt(length(m$visible), length(s$visible))
})

test_that("weak_subset defaults to part of the quasi-identifiers and can be set explicitly", {
  k <- attacker_knowledge("W", quasi_identifiers = c(A = "num", B = "num", C = "num", D = "num"))
  expect_equal(names(k$visible), c("A", "B"))

  k2 <- attacker_knowledge("W", quasi_identifiers = c(A = "num", B = "num", C = "num"))
  expect_equal(names(k2$visible), "A")

  k3 <- attacker_knowledge("W", quasi_identifiers = c(A = "num", B = "num", C = "num"), weak_subset = c("B", "C"))
  expect_equal(names(k3$visible), c("B", "C"))
})

test_that("attacker_knowledge() validates its specification", {
  expect_error(attacker_knowledge("W", quasi_identifiers = c("num", "char")), regexp = "named")
  expect_error(attacker_knowledge("W", quasi_identifiers = c(A = "nope")), regexp = "unknown score type")
  expect_error(attacker_knowledge("W", quasi_identifiers = character(0)), regexp = "at least one column")
  expect_error(
    attacker_knowledge("W", quasi_identifiers = c(A = "num"), weak_subset = "Z"),
    regexp = "not quasi-identifiers"
  )
  expect_error(attacker_knowledge("X", quasi_identifiers = c(A = "num")))
})

test_that("print.attacker_knowledge() names the level and both the visible and the withheld columns", {
  out <- capture.output(res <- print(dummy_qi_knowledge("M")))
  expect_true(any(grepl("level M \\(medium\\)", out)))
  expect_true(any(grepl("visible columns \\(6\\)", out)))
  expect_true(any(grepl("FINGERPRINT", out)))
  expect_s3_class(res, "attacker_knowledge")
})

## ---------------------------------------------------------------------------
## scoring under restricted knowledge
## ---------------------------------------------------------------------------

test_that("score_by_knowledge() returns a score table over the same candidate pairs", {
  j <- make_identity_qi_join()
  s <- score_by_knowledge(j, dummy_qi_knowledge("M"))

  expect_identical(names(s), c("RAW_ROW_NUMBER", "ANON_ROW_NUMBER", "SCORE"))
  expect_s3_class(s, "reid_scores")
  expect_equal(nrow(s), nrow(j))
  expect_false(anyNA(s$SCORE))
})

test_that("score_by_knowledge() uses only the visible columns", {
  j <- make_identity_qi_join()

  ## corrupting a withheld column must not move a level-W score at all
  j2 <- j
  j2$ANON_FINGERPRINT <- rev(j2$ANON_FINGERPRINT)
  j2$ANON_AGE <- rev(j2$ANON_AGE)

  expect_equal(
    score_by_knowledge(j, dummy_qi_knowledge("W"))$SCORE,
    score_by_knowledge(j2, dummy_qi_knowledge("W"))$SCORE
  )

  ## but it does move a level-S score, which can see them
  ##
  ## Reversing ANON_AGE and ANON_FINGERPRINT is exactly what makes those two
  ## axes worthless, so scoring j2 at level S trips the #35 screen while
  ## scoring the intact j does not. That asymmetry is the corruption this
  ## test is performing, seen from the screen's side, and asserting it (#43)
  ## rather than suppressing it keeps the screen under test: if it stopped
  ## detecting a deliberately scrambled axis, this expectation would fail.
  ##
  ## The assignment is inside expect_warning() because expect_warning()
  ## returns the condition, not the value of the expression.
  s_j <- score_by_knowledge(j, dummy_qi_knowledge("S"))
  s_j2 <- NULL
  expect_warning(
    s_j2 <- score_by_knowledge(j2, dummy_qi_knowledge("S")),
    "show no signal"
  )
  expect_false(isTRUE(all.equal(s_j$SCORE, s_j2$SCORE)))
})

test_that("score_by_knowledge() equals the single score_*() call when only one column is visible", {
  j <- make_identity_qi_join()
  k <- attacker_knowledge("W", quasi_identifiers = c(AGE = "num"))

  raw <- score_num(j, "AGE")
  got <- score_by_knowledge(j, k, normalize = "none")
  expect_equal(got$SCORE, raw$SCORE)

  ## with normalisation it is the same score rescaled to [0, 1]
  scaled <- score_by_knowledge(j, k, normalize = "range")
  expect_equal(scaled$SCORE, (raw$SCORE - min(raw$SCORE)) / diff(range(raw$SCORE)))
  expect_true(all(scaled$SCORE >= 0 & scaled$SCORE <= 1))
})

test_that("normalisation gives a column with no variation exactly zero weight", {
  raw <- data.frame(
    ROW_NUMBER = 1:6,
    A = c(1, 2, 3, 4, 5, 6),
    CONST = rep(7, 6)
  )
  j <- join_raw_anon_data(raw, raw)

  k_a <- attacker_knowledge("M", quasi_identifiers = c(A = "num"))
  k_both <- attacker_knowledge("M", quasi_identifiers = c(A = "num", CONST = "num"))

  ## CONST separates nothing, so adding it must not change the ranking at all
  ##
  ## ... and the #35 screen says so out loud. Asserting the warning (#43)
  ## makes the intent explicit: a constant column is deliberately in this
  ## fixture, and being told about it is the correct behaviour, not noise.
  both <- NULL
  expect_warning(both <- score_by_knowledge(j, k_both), "CONST")
  expect_equal(score_by_knowledge(j, k_a)$SCORE, both$SCORE)
})

test_that("score_by_knowledge() rejects a non-knowledge object", {
  j <- make_identity_qi_join()
  expect_error(score_by_knowledge(j, "M"), regexp = "attacker_knowledge object")
})

## ---------------------------------------------------------------------------
## the acceptance criterion: W < M < S
## ---------------------------------------------------------------------------

test_that("success rate increases strictly with knowledge on generalised data (W < M < S)", {
  j <- make_generalized_join()
  curve <- do.call(reid_knowledge_curve, c(list(j, seeds = 1:20), qi_args))

  expect_equal(curve$level, c("W", "M", "S"))
  expect_lt(curve$success_analytic[1], curve$success_analytic[2])
  expect_lt(curve$success_analytic[2], curve$success_analytic[3])

  ## the simulated rates order the same way
  expect_lt(curve$success_mean[1], curve$success_mean[2])
  expect_lt(curve$success_mean[2], curve$success_mean[3])

  ## a weak attacker is barely better than guessing; a strong one is not
  expect_lt(curve$lift[1], 5)
  expect_gt(curve$lift[3], 20)
})

test_that("success rate is non-decreasing with knowledge on an identity join too (it saturates rather than dropping)", {
  ## On an exact copy, level M already identifies everybody, so S cannot be
  ## strictly higher. What must never happen is the rate going *down* when the
  ## attacker is told more.
  j <- make_identity_qi_join()
  curve <- do.call(reid_knowledge_curve, c(list(j, seeds = 1:5), qi_args))

  expect_true(all(diff(curve$success_analytic) >= 0))
  expect_equal(curve$success_analytic[2], 1)
  expect_equal(curve$success_analytic[3], 1)
})

test_that("reid_knowledge_curve() reports the baseline and the spread alongside each level", {
  j <- make_generalized_join(people = 30, seed = 5)

  ## Generalisation collapses ZIP to a single value on this fixture, so the
  ## axis carries no information and the #35 screen warns -- once per
  ## knowledge level, i.e. three times, because the curve scores W, M and S
  ## separately. All three are true positives and are asserted rather than
  ## suppressed (#43); the count is part of the assertion, so losing the
  ## screen at any one level fails this test.
  curve <- NULL
  expect_warning(
    expect_warning(
      expect_warning(
        curve <- do.call(reid_knowledge_curve, c(list(j, seeds = 1:10), qi_args)),
        "ZIP"
      ),
      "ZIP"
    ),
    "ZIP"
  )

  expect_setequal(
    names(curve),
    c("level", "n_visible", "success_analytic", "success_mean", "success_sd",
      "baseline_random", "lift", "max_risk")
  )
  expect_equal(nrow(curve), 3)
  expect_equal(curve$n_visible, c(1, 6, 7))
  ## every level is evaluated on the same data, so the baseline is the same
  expect_equal(length(unique(curve$baseline_random)), 1)
  expect_equal(curve$baseline_random[1], 1 / 30)
})

test_that("reid_knowledge_curve() can be restricted to a subset of levels", {
  j <- make_identity_qi_join(people = 15, seed = 2)
  curve <- do.call(
    reid_knowledge_curve,
    c(list(j, levels = c("W", "S"), seeds = 1:5), qi_args)
  )
  expect_equal(curve$level, c("W", "S"))
  expect_equal(nrow(curve), 2)
})

## ---------------------------------------------------------------------------
## the dummy generator
## ---------------------------------------------------------------------------

test_that("create_dummy_qi_data() has the documented schema", {
  d <- create_dummy_qi_data(people = 20, seed = 1)

  expect_true(tibble::is_tibble(d))
  expect_equal(nrow(d), 20)
  expect_identical(
    names(d),
    c("ROW_NUMBER", "ID", "AGE", "ZIP", "SEX", "VISIT_COUNT",
      "SPEND_MEAN", "SPEND_DIST", "FINGERPRINT")
  )
  expect_type(d$ZIP, "character")
  expect_type(d$SEX, "character")
  expect_type(d$SPEND_DIST, "character")
  expect_type(d$FINGERPRINT, "double")
})

test_that("create_dummy_qi_data() columns have the intended discriminating power", {
  d <- create_dummy_qi_data(people = 100, seed = 1)

  ## quasi-identifiers collide heavily; the fingerprint does not collide at all
  expect_lt(length(unique(d$ZIP)), 40)
  expect_equal(length(unique(d$SEX)), 2)
  expect_equal(length(unique(d$FINGERPRINT)), 100)

  ## SPEND_DIST is a colon-joined distribution with VISIT_COUNT elements
  n_elements <- lengths(strsplit(d$SPEND_DIST, ":", fixed = TRUE))
  expect_equal(n_elements, d$VISIT_COUNT)
})

test_that("create_dummy_qi_data() is reproducible from its seed and leaves the RNG stream alone", {
  expect_identical(create_dummy_qi_data(10, seed = 5), create_dummy_qi_data(10, seed = 5))
  expect_false(identical(create_dummy_qi_data(10, seed = 5), create_dummy_qi_data(10, seed = 6)))

  set.seed(99)
  before <- runif(3)
  set.seed(99)
  invisible(create_dummy_qi_data(10, seed = 12345))
  after <- runif(3)
  expect_identical(before, after)
})

test_that("create_dummy_qi_data() validates people", {
  expect_error(create_dummy_qi_data(0), regexp = "people")
  expect_error(create_dummy_qi_data(-1), regexp = "people")
  expect_error(create_dummy_qi_data("a"), regexp = "people")
})

test_that("existing dummy generators are untouched by the quasi-identifier addition", {
  set.seed(1)
  m <- create_dummy_master_data(5)
  expect_identical(names(m), c("ROW_NUMBER", "ID", "NUM", "BIN", "CHAR"))
})
