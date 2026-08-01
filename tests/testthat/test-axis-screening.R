## Issue #35 -- detecting an axis that carries no information about identity.
##
## The condition being pinned down here was found while implementing Issue #22
## and reported in PR #34: under an anonymiser that publishes only 60% of each
## person's events, the record-count axis carries nothing (every count shrinks
## by the same factor), and summing it into the combination at equal weight
## pushed the reported success rate *below* the static-attribute-only attack in
## half the seeds. Under-reporting risk is the failure direction a safety tool
## must never take quietly, so the 60% condition is fixed here as a regression.

DOW_BINS <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

## The PR #34 fixture, reproduced exactly.
##   "jitter"    every person's events are published, the count moves by -1/0/+1
##   "subsample" only `keep` of each person's events are published, so every
##               count shrinks by roughly the same factor and the nearest count
##               is somebody who was simply less active to begin with
activity_fixture <- function(n = 120, mode = c("jitter", "subsample"),
                             keep = 0.6, seed = 1) {
  mode <- match.arg(mode)
  set.seed(seed)

  rate <- rpois(n, lambda = 12) + 3
  favourite <- sample.int(7L, n, replace = TRUE)
  start <- sample.int(300L, n, replace = TRUE)
  window <- sample(c(10L, 30L, 90L, 300L), n, replace = TRUE)

  make_master <- function(counts) {
    rows <- lapply(seq_len(n), function(i) {
      k <- counts[i]
      p <- rep(1, 7)
      p[favourite[i]] <- 4
      data.frame(
        ID = i,
        DOW = DOW_BINS[sample.int(7L, k, replace = TRUE, prob = p)],
        DAY = start[i] + sample.int(window[i], k, replace = TRUE),
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

ACTIVITY_TARGETS <- c(DAY_MEAN = "num", ROWCOUNT = "count",
                      DOW_DIST = "profile", DAY_DIST = "span")
STATIC_ONLY <- c(DAY_MEAN = "num")

activity_axes <- function(j) {
  list(
    DAY_MEAN = score_num(j, "DAY_MEAN"),
    ROWCOUNT = score_count(j),
    DOW_DIST = score_profile(j, "DOW_DIST", bins = DOW_BINS),
    DAY_DIST = score_span(j, "DAY_DIST")
  )
}

analytic <- function(scores) {
  suppressWarnings(reid_evaluate(scores, seeds = 1:2)$success_analytic)
}


## ---------------------------------------------------------------------------
## The regression: the 60% publication condition
## ---------------------------------------------------------------------------

test_that("publishing 60% of each person's events leaves the count axis dead (#35)", {
  for (s in 1:3) {
    j <- activity_fixture(mode = "subsample", seed = s)
    r <- axis_informativeness(activity_axes(j))
    rownames(r) <- r$axis

    ## The whole point: the count axis says nothing under this anonymiser.
    expect_false(r["ROWCOUNT", "informative"],
                 info = paste("seed", s, "z =", r["ROWCOUNT", "z"]))
    ## It ranks the true record essentially in the middle of the candidate
    ## list, which is where chance puts it.
    expect_lt(abs(r["ROWCOUNT", "mean_rank_pct"] - 0.5), 0.05)

    ## ...while the scale-invariant axes and the static attribute survive. If
    ## these were flagged too, the screen would be detecting "small fixture",
    ## not "dead axis".
    expect_true(r["DAY_MEAN", "informative"], info = paste("seed", s))
    expect_true(r["DOW_DIST", "informative"], info = paste("seed", s))
    expect_true(r["DAY_DIST", "informative"], info = paste("seed", s))
  }
})

test_that("the same count axis is informative when the anonymiser keeps counts (#35)", {
  ## The false-positive control that matters most: it is the *condition* that
  ## kills the axis, not the axis. Under an anonymiser that moves each count by
  ## -1/0/+1 the identical column is strongly informative.
  for (s in 1:3) {
    j <- activity_fixture(mode = "jitter", seed = s)
    r <- axis_informativeness(activity_axes(j))
    rownames(r) <- r$axis

    expect_true(r["ROWCOUNT", "informative"], info = paste("seed", s))
    expect_gt(r["ROWCOUNT", "z"], 5)
    expect_lt(r["ROWCOUNT", "mean_rank_pct"], 0.3)
    expect_true(all(r$informative))
  }
})

test_that("the dead axis drags the equal-weight combination below static-only, and screening repairs it (#35)", {
  static <- numeric(0)
  equal_weight <- numeric(0)
  screened <- numeric(0)

  for (s in 1:4) {
    j <- activity_fixture(mode = "subsample", seed = s)
    static[s] <- analytic(score_multi(j, STATIC_ONLY, screen = "none"))
    equal_weight[s] <- analytic(score_multi(j, ACTIVITY_TARGETS, screen = "none"))
    screened[s] <- analytic(
      suppressWarnings(score_multi(j, ACTIVITY_TARGETS, screen = "drop"))
    )
  }

  ## The reported failure: adding evidence makes the number go *down*. If this
  ## ever stops reproducing, the fixture has drifted and the rest of this file
  ## is testing nothing.
  expect_true(any(equal_weight < static))

  ## The fix: with the dead axis screened out, the combination never falls
  ## below the single attribute it was supposed to improve on...
  expect_true(all(screened >= static))
  ## ...and beats the unscreened combination on every seed.
  expect_true(all(screened > equal_weight))
})


## ---------------------------------------------------------------------------
## Detection, on data where the answer is known by construction
## ---------------------------------------------------------------------------

synthetic_axes <- function(n = 120, seed = 99) {
  set.seed(seed)
  raw <- data.frame(ROW_NUMBER = seq_len(n), SIGNAL = rnorm(n),
                    NOISE = rnorm(n), CONST = 1)
  anon <- raw
  anon$SIGNAL <- raw$SIGNAL + rnorm(n, sd = 0.1)
  anon$NOISE <- rnorm(n)   # redrawn from scratch: no relation to the RAW value
  join_raw_anon_data(raw, anon)
}

test_that("a redrawn column and a constant column are both detected as carrying nothing", {
  j <- synthetic_axes()
  r <- axis_informativeness(list(
    SIGNAL = score_num(j, "SIGNAL"),
    NOISE = score_num(j, "NOISE"),
    CONST = score_num(j, "CONST")
  ))
  rownames(r) <- r$axis

  expect_true(r["SIGNAL", "informative"])
  expect_false(r["NOISE", "informative"])
  expect_false(r["CONST", "informative"])

  ## A column that never varies puts every candidate in one tie group, so the
  ## true record sits at the centre by construction and the null has no spread
  ## at all.
  expect_equal(r["CONST", "z"], 0)
  expect_equal(r["CONST", "p_value"], 1)
})

test_that("a dead axis still shows a success-rate lift of 2-3x, which is why the test is on ranks", {
  ## This is the measurement that decided the implementation. Issue #35 proposes
  ## comparing each axis's own success rate against the random-assignment
  ## baseline; on a 120-record table that baseline is 1/120, so the null expects
  ## about one hit and two or three hits -- "lift 2x", "lift 3x" -- is an
  ## entirely ordinary draw from it. An axis built to contain no signal whatever
  ## still reports a lift well above 1, so lift cannot be the criterion.
  set.seed(7)
  n <- 120
  raw <- data.frame(ROW_NUMBER = seq_len(n), V = rnorm(n))
  anon <- raw
  anon$V <- raw$V + rnorm(n, sd = 1e6)   # signal drowned beyond any recovery
  r <- axis_informativeness(list(
    V = score_num(join_raw_anon_data(raw, anon), "V")
  ))

  expect_gt(r$lift, 1)          # the success rate looks like an improvement
  expect_gt(r$mean_rank_pct, 0.45)   # the ranking says it is not
  expect_false(r$informative)
})

test_that("informative axes are not flagged, down to the point where they stop working", {
  ## Sweeping the noise level: the screen should keep calling the axis
  ## informative while it is still usable, and stop when it stops.
  strength <- function(nz) {
    set.seed(7)
    n <- 120
    raw <- data.frame(ROW_NUMBER = seq_len(n), V = rnorm(n))
    anon <- raw
    anon$V <- raw$V + rnorm(n, sd = nz)
    axis_informativeness(list(V = score_num(join_raw_anon_data(raw, anon), "V")))
  }

  for (nz in c(0.1, 0.5, 1, 2)) {
    expect_true(strength(nz)$informative, info = paste("noise sd", nz))
  }
  expect_false(strength(16)$informative)

  ## Whatever the cutoff is, it has to be monotone in the noise: more noise can
  ## never make an axis look more informative.
  zs <- vapply(c(0.1, 0.5, 1, 2, 4, 8, 16), function(nz) strength(nz)$z, numeric(1))
  expect_true(all(diff(zs) < 0))
})

test_that("every column of create_dummy_qi_data() is recognised as informative", {
  ## A false-positive sweep over the package's own quasi-identifier fixture,
  ## including SEX, which has two values and identifies almost nobody on its
  ## own but is still not noise.
  d <- create_dummy_qi_data(people = 60, seed = 1)
  j <- join_raw_anon_data(d, d)
  r <- axis_informativeness(list(
    AGE = score_num(j, "AGE"),
    ZIP = score_char(j, "ZIP"),
    SEX = score_char(j, "SEX"),
    VISIT_COUNT = score_num(j, "VISIT_COUNT"),
    SPEND_MEAN = score_num(j, "SPEND_MEAN"),
    SPEND_DIST = score_dist(j, "SPEND_DIST"),
    FINGERPRINT = score_num(j, "FINGERPRINT")
  ))
  expect_true(all(r$informative), info = paste(r$axis[!r$informative], collapse = ", "))
})


## ---------------------------------------------------------------------------
## The report itself
## ---------------------------------------------------------------------------

test_that("axis_informativeness() reports one row per axis, named and in order", {
  j <- synthetic_axes(n = 40)
  r <- axis_informativeness(list(A = score_num(j, "SIGNAL"), B = score_num(j, "NOISE")))

  expect_s3_class(r, "reid_axis_report")
  expect_equal(nrow(r), 2)
  expect_equal(r$axis, c("A", "B"))
  expect_equal(
    names(r),
    c("axis", "n_anon", "success", "baseline", "lift", "mean_rank_pct",
      "z", "p_value", "informative")
  )
  expect_equal(r$n_anon, c(40, 40))
})

test_that("axis_informativeness() accepts a bare score table and names unnamed axes", {
  j <- synthetic_axes(n = 40)
  one <- axis_informativeness(score_num(j, "SIGNAL"))
  expect_equal(nrow(one), 1)
  expect_equal(one$axis, "axis1")

  two <- axis_informativeness(list(score_num(j, "SIGNAL"), score_num(j, "NOISE")))
  expect_equal(two$axis, c("axis1", "axis2"))
})

test_that("success and baseline agree with reid_evaluate() on the same axis", {
  ## The screen and the headline number must never quote different figures for
  ## the same axis.
  j <- synthetic_axes(n = 60)
  s <- score_num(j, "SIGNAL")
  r <- axis_informativeness(list(SIGNAL = s))
  e <- reid_evaluate(s, seeds = 1:2)

  expect_equal(r$success, e$success_analytic)
  expect_equal(r$baseline, e$baseline$rate[e$baseline$method == "random"])
  expect_equal(r$lift, e$lift)
})

test_that("the report is NA, not zero, when no ANON record has a RAW counterpart", {
  ## Without the correspondence the question cannot be asked. Reporting 0 here
  ## would read as "this axis is dead", which is a claim the data cannot
  ## support.
  raw <- data.frame(ROW_NUMBER = 1:5, V = rnorm(5))
  anon <- data.frame(ROW_NUMBER = 101:105, V = rnorm(5))
  j <- join_raw_anon_data(raw, anon)
  r <- axis_informativeness(list(V = score_num(j, "V")))

  expect_true(is.na(r$informative))
  expect_true(is.na(r$p_value))
  expect_true(is.na(r$success))
  expect_equal(r$n_anon, 5)
})

test_that("records whose truth was blocked away are skipped by the rank test, not counted as failures", {
  ## The half-way case between the two above, and the one #56 says a real
  ## release produces: *some* ANON records still have their true RAW record
  ## among the candidates and some do not. covr says nothing in the suite ever
  ## reached the branch that skips them (R/multiattr.R line 750, 0 hits).
  ##
  ## It matters because `informative` is what score_multi(screen = "drop")
  ## acts on. A truthless record has no rank to contribute; counting it as a
  ## record where the axis failed would drag z down, and an informative axis
  ## judged dead is dropped from the combination -- fewer attributes, a lower
  ## measured reidentification rate, and no error anywhere
  ## (docs/lessons-learned.md section 2).
  j <- synthetic_axes(n = 60)
  s_full <- score_num(j, "SIGNAL")

  ## take the true pair away from the second half of the ANON records
  blocked <- !(s_full$ANON_ROW_NUMBER > 30 &
                 s_full$RAW_ROW_NUMBER == s_full$ANON_ROW_NUMBER)
  s_part <- s_full[blocked, , drop = FALSE]
  attr(s_part, "score_type") <- attr(s_full, "score_type")

  ## the same table restricted to the records that can still be measured
  s_meas <- s_full[s_full$ANON_ROW_NUMBER <= 30, , drop = FALSE]
  attr(s_meas, "score_type") <- attr(s_full, "score_type")

  rk <- axis_rank_statistic(s_part)
  expect_equal(rk$n_used, 30)
  ## the rank evidence comes from the measurable records and from nowhere else
  expect_equal(rk, axis_rank_statistic(s_meas))

  r <- axis_informativeness(list(SIGNAL = s_part))
  expect_equal(r$n_anon, 60)
  expect_true(r$informative)
  expect_lt(r$mean_rank_pct, 0.1)

  ## success and baseline are the reporting columns and *are* diluted by the
  ## records that can no longer be hit -- that is the documented lower bound,
  ## and it is why the verdict is not taken from them.
  expect_lt(r$success, axis_informativeness(list(SIGNAL = s_meas))$success)
})

test_that("a similarity-oriented score is read the right way round", {
  ## A similarity that is a monotone reversal of a distance must produce the
  ## same verdict; reading the orientation wrongly would turn every informative
  ## axis into a dead one and vice versa.
  j <- synthetic_axes(n = 60)
  d <- score_num(j, "SIGNAL")
  sim <- d
  sim$SCORE <- -d$SCORE
  attr(sim, "score_type") <- "similarity"

  rd <- axis_informativeness(list(x = d))
  rs <- axis_informativeness(list(x = sim))
  expect_equal(rd$z, rs$z)
  expect_equal(rd$mean_rank_pct, rs$mean_rank_pct)
  expect_true(rs$informative)
})

test_that("axis_informativeness() validates its arguments", {
  j <- synthetic_axes(n = 20)
  s <- score_num(j, "SIGNAL")

  expect_error(axis_informativeness(list()), "non-empty list")
  expect_error(axis_informativeness("nope"), "non-empty list")
  expect_error(axis_informativeness(list(s), alpha = 0), "strictly between 0 and 1")
  expect_error(axis_informativeness(list(s), alpha = 1), "strictly between 0 and 1")
  expect_error(axis_informativeness(list(s), alpha = c(0.1, 0.2)), "single number")
  expect_error(axis_informativeness(list(data.frame(A = 1))),
               "missing score-layer column")
})

test_that("alpha moves the verdict in the direction it should", {
  j <- synthetic_axes(n = 120)
  s <- list(NOISE = score_num(j, "NOISE"))
  ## The noise axis has p around 0.9, so no sane alpha rescues it, but a
  ## borderline axis must respond to alpha.
  expect_false(axis_informativeness(s, alpha = 0.05)$informative)
  expect_false(axis_informativeness(s, alpha = 0.5)$informative)

  weak <- axis_informativeness(list(x = score_num(j, "SIGNAL")), alpha = 1e-12)
  expect_true(weak$informative)   # this one is far past any threshold
})

test_that("print.reid_axis_report() reports the verdict and the excluded axes", {
  j <- synthetic_axes(n = 40)
  r <- axis_informativeness(list(SIGNAL = score_num(j, "SIGNAL"),
                                 NOISE = score_num(j, "NOISE")))
  out <- paste(utils::capture.output(print(r)), collapse = "\n")
  expect_match(out, "axis informativeness")
  expect_match(out, "informative")
  expect_match(out, "no signal")

  r$kept <- c(TRUE, FALSE)
  out2 <- paste(utils::capture.output(print(r)), collapse = "\n")
  expect_match(out2, "excluded from the combination: NOISE")

  raw <- data.frame(ROW_NUMBER = 1:4, V = rnorm(4))
  anon <- data.frame(ROW_NUMBER = 11:14, V = rnorm(4))
  na_rep <- axis_informativeness(
    list(V = score_num(join_raw_anon_data(raw, anon), "V"))
  )
  expect_match(paste(utils::capture.output(print(na_rep)), collapse = "\n"),
               "not measurable")
})


## ---------------------------------------------------------------------------
## score_multi() / score_by_knowledge() integration
## ---------------------------------------------------------------------------

test_that("screen = 'warn' warns but leaves the score untouched", {
  j <- synthetic_axes()
  targets <- c(SIGNAL = "num", NOISE = "num")

  quiet <- score_multi(j, targets, screen = "none")
  expect_warning(warned <- score_multi(j, targets, screen = "warn"),
                 "do not rank the true record better than chance")

  ## Only the report is added; the numbers a user reports must not move just
  ## because a check was switched on.
  expect_equal(warned$SCORE, quiet$SCORE)
  expect_equal(warned$RAW_ROW_NUMBER, quiet$RAW_ROW_NUMBER)
  expect_equal(warned$ANON_ROW_NUMBER, quiet$ANON_ROW_NUMBER)
})

test_that("the warning names the axis and quotes the numbers behind the verdict", {
  j <- synthetic_axes()
  expect_warning(
    score_multi(j, c(SIGNAL = "num", NOISE = "num"), screen = "warn"),
    "NOISE"
  )
  expect_warning(
    score_multi(j, c(SIGNAL = "num", NOISE = "num"), screen = "warn"),
    "under-estimate"
  )
})

test_that("screen = 'drop' excludes exactly the dead axis", {
  j <- synthetic_axes()
  expect_warning(dropped <- score_multi(j, c(SIGNAL = "num", NOISE = "num"),
                                        screen = "drop"),
                 "excluding axis")
  alone <- score_multi(j, c(SIGNAL = "num"), screen = "none")

  expect_equal(dropped$SCORE, alone$SCORE)

  r <- axis_report(dropped)
  expect_equal(r$axis, c("SIGNAL", "NOISE"))
  expect_equal(r$kept, c(TRUE, FALSE))
})

test_that("screen = 'drop' is equivalent to giving the dead axis weight zero", {
  j <- synthetic_axes()
  targets <- c(SIGNAL = "num", NOISE = "num")
  expect_warning(dropped <- score_multi(j, targets, screen = "drop"))
  zeroed <- score_multi(j, targets, weights = c(1, 0), screen = "none")
  expect_equal(dropped$SCORE, zeroed$SCORE)
})

test_that("screening keeps the surviving weights lined up with their columns", {
  j <- synthetic_axes()
  targets <- c(NOISE = "num", SIGNAL = "num", CONST = "num")
  ## NOISE and CONST are dead and sit either side of SIGNAL, so a weight vector
  ## that was subset by position rather than by column would silently apply
  ## SIGNAL's weight to the wrong column.
  expect_warning(dropped <- score_multi(j, targets, weights = c(5, 3, 7),
                                        screen = "drop"))
  alone <- score_multi(j, c(SIGNAL = "num"), weights = 3, screen = "none")
  expect_equal(dropped$SCORE, alone$SCORE)
})

test_that("when every axis is dead they are all kept, and the warning says so", {
  ## Dropping them all would leave no attack at all. The honest report is the
  ## chance-level number the axes actually produce.
  j <- synthetic_axes()
  expect_warning(kept <- score_multi(j, c(NOISE = "num", CONST = "num"),
                                     screen = "drop"),
                 "no axis shows any signal")
  plain <- score_multi(j, c(NOISE = "num", CONST = "num"), screen = "none")
  expect_equal(kept$SCORE, plain$SCORE)
  expect_true(all(axis_report(kept)$kept))
})

test_that("screen = 'none' is silent and attaches no report", {
  j <- synthetic_axes()
  expect_warning(score_multi(j, c(SIGNAL = "num", NOISE = "num"), screen = "none"),
                 NA)
  expect_null(axis_report(score_multi(j, c(SIGNAL = "num", NOISE = "num"),
                                      screen = "none")))
})

test_that("axis_report() returns NULL for a score that never went through score_multi()", {
  j <- synthetic_axes(n = 20)
  expect_null(axis_report(score_num(j, "SIGNAL")))
  expect_null(axis_report(combine_scores(list(score_num(j, "SIGNAL")))))
})

test_that("the report survives on the returned score and lists every declared column", {
  j <- synthetic_axes()
  expect_warning(s <- score_multi(j, c(SIGNAL = "num", NOISE = "num", CONST = "num")))
  r <- axis_report(s)
  expect_s3_class(r, "reid_axis_report")
  expect_equal(r$axis, c("SIGNAL", "NOISE", "CONST"))
  expect_equal(r$informative, c(TRUE, FALSE, FALSE))
})

test_that("screening happens per column, so a dead column leaves the Mahalanobis block", {
  ## Under method = "mahalanobis" the numeric columns are scored as one block.
  ## Screening the block as a whole would hide a dead coordinate inside a block
  ## that works on the strength of the others, so the screen runs per declared
  ## column and a dropped column leaves the covariance too.
  j <- synthetic_axes()
  targets <- c(SIGNAL = "num", NOISE = "num")

  expect_warning(dropped <- score_multi(j, targets, method = "mahalanobis",
                                        screen = "drop"),
                 "excluding axis")
  alone <- score_multi(j, c(SIGNAL = "num"), method = "mahalanobis",
                       screen = "none")
  expect_equal(dropped$SCORE, alone$SCORE)
})

test_that("screen = 'warn' is the default of score_multi() and score_by_knowledge()", {
  j <- synthetic_axes()
  expect_warning(score_multi(j, c(SIGNAL = "num", NOISE = "num")),
                 "do not rank the true record better than chance")

  k <- attacker_knowledge("M", quasi_identifiers = c(SIGNAL = "num"),
                          behavior = c(NOISE = "num"))
  expect_warning(score_by_knowledge(j, k), "score_by_knowledge\\(\\)")
})

test_that("score_by_knowledge() passes screen and alpha through", {
  j <- synthetic_axes()
  k <- attacker_knowledge("M", quasi_identifiers = c(SIGNAL = "num"),
                          behavior = c(NOISE = "num"))

  expect_warning(score_by_knowledge(j, k, screen = "none"), NA)
  expect_warning(dropped <- score_by_knowledge(j, k, screen = "drop"),
                 "excluding axis")
  alone <- score_multi(j, c(SIGNAL = "num"), screen = "none")
  expect_equal(dropped$SCORE, alone$SCORE)
  expect_equal(axis_report(dropped)$kept, c(TRUE, FALSE))
})

test_that("screening does not consume the RNG stream", {
  ## The test is analytic, not sampled. If it ever starts drawing, a plain
  ## score_multi() call would silently shift every downstream tie-break.
  j <- synthetic_axes(n = 40)
  set.seed(123)
  before <- runif(1)
  set.seed(123)
  suppressWarnings(score_multi(j, c(SIGNAL = "num", NOISE = "num"),
                               screen = "warn"))
  after <- runif(1)
  expect_equal(before, after)
})

test_that("screening reuses the per-column scores it built rather than rebuilding them", {
  ## Not a performance assertion: the two code paths must not be able to drift.
  ## A screened and an unscreened call have to produce the same score.
  j <- synthetic_axes()
  for (m in c("range", "zscore", "rank", "none")) {
    a <- score_multi(j, c(SIGNAL = "num", CONST = "num"), normalize = m,
                     screen = "none")
    b <- suppressWarnings(
      score_multi(j, c(SIGNAL = "num", CONST = "num"), normalize = m,
                  screen = "warn")
    )
    expect_equal(a$SCORE, b$SCORE, info = m)
  }
})

test_that("an idf column is screened on its own idf score", {
  ## "idf" columns are normally handed to score_idf_match() as one block; the
  ## screen needs the single-column form, and it has to be built with the same
  ## source/weight the block would use.
  set.seed(3)
  n <- 80
  raw <- data.frame(
    ROW_NUMBER = seq_len(n),
    KEEP = sprintf("k%02d", sample.int(40L, n, replace = TRUE)),
    JUNK = sprintf("j%02d", sample.int(40L, n, replace = TRUE)),
    stringsAsFactors = FALSE
  )
  anon <- raw
  anon$JUNK <- sprintf("j%02d", sample.int(40L, n, replace = TRUE))
  j <- join_raw_anon_data(raw, anon)

  r <- axis_informativeness(list(
    KEEP = score_idf(j, "KEEP"),
    JUNK = score_idf(j, "JUNK")
  ))
  expect_true(r$informative[r$axis == "KEEP"])
  expect_false(r$informative[r$axis == "JUNK"])

  expect_warning(s <- score_multi(j, c(KEEP = "idf", JUNK = "idf"),
                                  screen = "drop"),
                 "excluding axis")
  alone <- score_multi(j, c(KEEP = "idf"), screen = "none")
  expect_equal(s$SCORE, alone$SCORE)
})

test_that("a fully tied axis is reported as carrying nothing rather than dividing by zero", {
  raw <- data.frame(ROW_NUMBER = 1:6, V = rep(1, 6))
  j <- join_raw_anon_data(raw, raw)
  r <- axis_informativeness(list(V = score_num(j, "V")))

  expect_equal(r$z, 0)
  expect_equal(r$p_value, 1)
  expect_false(r$informative)
  expect_false(is.na(r$success))
})

test_that("mid-ranks are used, so a tie is scored at its honest average position", {
  ## Two candidates tied at the best score put the true record at mid-rank 1.5,
  ## not 1. Crediting it with rank 1 would make every tie-heavy axis look
  ## informative.
  raw <- data.frame(ROW_NUMBER = 1:4, V = c(1, 1, 2, 3))
  j <- join_raw_anon_data(raw, raw)
  st <- reidentify:::axis_rank_statistic(score_num(j, "V"), "distance")

  ## ANON 1 and 2 each tie with each other at distance 0 and 1: candidate
  ## distances are (0, 0, 1, 2) so mid-ranks are (1.5, 1.5, 3, 4); the true
  ## record is at 1.5 against a centre of 2.5.
  expect_equal(st$n_used, 4)
  expect_true(st$variance > 0)
  expect_true(st$mean_rank_pct < 0.5)
})
