## ---------------------------------------------------------------------------
## Issue #22 -- are the activity profile columns worth attacking, and does
## adding them to a multi-attribute attack help?
##
## Run with:  Rscript docs/investigation/activity-benchmark.R
##
## The fixture goes through the real pipeline: a transaction table is reduced
## with transform_transaction_to_master(), which is where ROWCOUNT and the
## collapsed <col>_DIST columns come from in the first place. The anonymiser
## then drops a fraction of each person's events and jitters the rest, so the
## published profile resembles the original without matching it.
##
## Issue #5 removed the record-count difference from distribution_distance()
## and said it belonged here. The last section checks the consequence of that
## split: that the count axis and the shape axis really do carry different
## evidence, rather than one being a noisy copy of the other.
## ---------------------------------------------------------------------------

suppressMessages(pkgload::load_all(".", quiet = TRUE))

SEEDS <- 1:10
success <- function(scores) reid_evaluate(scores, seeds = SEEDS)$success_analytic
baseline <- function(scores) {
  e <- reid_evaluate(scores, seeds = SEEDS)
  e$baseline$rate[e$baseline$method == "random"]
}

DOW <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

## Each person has their own activity level, their own day-of-week habits and
## their own active window.
##
## Two anonymisers, because they treat the count completely differently:
##
##   "jitter"    the released table keeps every person's events, with the
##               values re-drawn from their own habits and the number of
##               events moved by a small symmetric amount. This is what
##               generalising a master table looks like: the transaction
##               count per person survives more or less intact.
##   "subsample" the released table keeps only a `keep` fraction of each
##               person's events. Every count is scaled by roughly the same
##               factor.
activity_master <- function(n = 120, mode = c("jitter", "subsample"),
                            keep = 0.6, seed = 1) {
  mode <- match.arg(mode)
  set.seed(seed)

  ## per-person habits
  rate <- rpois(n, lambda = 12) + 3
  favourite <- sample.int(7L, n, replace = TRUE)
  start <- sample.int(300L, n, replace = TRUE)
  window <- sample(c(10L, 30L, 90L, 300L), n, replace = TRUE)

  make_master <- function(counts) {
    rows <- lapply(seq_len(n), function(i) {
      k <- counts[i]
      ## a day-of-week preference: the favourite day is four times as likely
      p <- rep(1, 7)
      p[favourite[i]] <- 4
      data.frame(
        ID = i,
        DOW = DOW[sample.int(7L, k, replace = TRUE, prob = p)],
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

j <- activity_master(mode = "jitter")
cat("Issue #22 activity profile benchmark\n")
cat("120 people, master format via transform_transaction_to_master()\n")
cat("anonymiser 'jitter': every person's events kept, count moved by -1/0/+1\n")
cat(sprintf("random-assignment baseline: %.4f\n", baseline(score_count(j))))

cat("\n== each axis on its own ==\n")
axes <- list(
  "count (log_ratio)" = score_count(j),
  "count (absolute)"  = score_count(j, method = "absolute"),
  "count (relative)"  = score_count(j, method = "relative"),
  "profile DOW (l1)"  = score_profile(j, "DOW_DIST", bins = DOW),
  "profile DOW (l2)"  = score_profile(j, "DOW_DIST", bins = DOW, metric = "l2"),
  "profile DOW, volume kept" =
    score_profile(j, "DOW_DIST", bins = DOW, shape_only = FALSE),
  "span DAY"          = score_span(j, "DAY_DIST"),
  "-- for comparison: DAY_DIST via score_dist()" = score_dist(j, "DAY_DIST")
)
for (nm in names(axes)) cat(sprintf("  %-44s : %.4f\n", nm, success(axes[[nm]])))

cat("\n== combined through score_multi() (#14) ==\n")
combos <- list(
  "count only"                = c(ROWCOUNT = "count"),
  "profile only"              = c(DOW_DIST = "profile"),
  "span only"                 = c(DAY_DIST = "span"),
  "count + profile"           = c(ROWCOUNT = "count", DOW_DIST = "profile"),
  "count + profile + span"    = c(ROWCOUNT = "count", DOW_DIST = "profile",
                                  DAY_DIST = "span"),
  "DAY_MEAN only"             = c(DAY_MEAN = "num"),
  "DAY_MEAN + activity"       = c(DAY_MEAN = "num", ROWCOUNT = "count",
                                  DOW_DIST = "profile", DAY_DIST = "span")
)
for (nm in names(combos)) {
  cat(sprintf("  %-24s : %.4f\n", nm, success(score_multi(j, combos[[nm]]))))
}

across_seeds <- function(mode) {
  cat(sprintf("\n== across 8 data seeds, anonymiser = %s ==\n", mode))
  tab <- do.call(rbind, lapply(1:8, function(s) {
    jj <- activity_master(mode = mode, seed = s)
    data.frame(
      seed = s,
      random = baseline(score_count(jj)),
      count = success(score_multi(jj, c(ROWCOUNT = "count"))),
      profile = success(score_multi(jj, c(DOW_DIST = "profile"))),
      span = success(score_multi(jj, c(DAY_DIST = "span"))),
      static = success(score_multi(jj, c(DAY_MEAN = "num"))),
      activity = success(score_multi(jj, c(ROWCOUNT = "count", DOW_DIST = "profile",
                                           DAY_DIST = "span"))),
      both = success(score_multi(jj, c(DAY_MEAN = "num", ROWCOUNT = "count",
                                       DOW_DIST = "profile", DAY_DIST = "span")))
    )
  }))
  print(tab, row.names = FALSE, digits = 3)
  cat(sprintf(
    "\n  mean: random %.4f | count %.4f | profile %.4f | span %.4f | static %.4f | activity %.4f | both %.4f\n",
    mean(tab$random), mean(tab$count), mean(tab$profile), mean(tab$span),
    mean(tab$static), mean(tab$activity), mean(tab$both)))
  for (ax in c("count", "profile", "span")) {
    cat(sprintf("  %-8s beats the random baseline in %d/8 seeds\n",
                ax, sum(tab[[ax]] > tab$random)))
  }
  cat(sprintf("  both > static in %d/8, both > activity in %d/8\n",
              sum(tab$both > tab$static), sum(tab$both > tab$activity)))
  invisible(tab)
}

across_seeds("jitter")
across_seeds("subsample")

## ---------------------------------------------------------------------------
## The #5 split: count and shape must be different evidence, not two views of
## the same thing. If score_profile() still leaked the volume, adding it to
## score_count() would double count and the pair would behave like one axis.
## ---------------------------------------------------------------------------
cat("\n== is the shape score really independent of the volume? ==\n")
raw <- data.frame(
  ROW_NUMBER = 1:5,
  P = c("Mon:Mon:Tue", "Sat:Sun", "Wed:Wed:Wed:Thu", "Mon:Fri", "Tue:Tue:Wed")
)
doubled <- raw
doubled$P <- vapply(strsplit(raw$P, ":", fixed = TRUE),
                    function(v) paste(rep(v, 2), collapse = ":"), character(1))
plain <- score_profile(join_raw_anon_data(raw, raw), "P", bins = DOW)
scaled <- score_profile(join_raw_anon_data(raw, doubled), "P", bins = DOW)
cat(sprintf("  every event duplicated: max |shape score change| = %.3g\n",
            max(abs(plain$SCORE - scaled$SCORE))))
with_volume <- score_profile(join_raw_anon_data(raw, doubled), "P", bins = DOW,
                             shape_only = FALSE)
cat(sprintf("  same, with shape_only = FALSE : max change = %.3g\n",
            max(abs(score_profile(join_raw_anon_data(raw, raw), "P", bins = DOW,
                                  shape_only = FALSE)$SCORE - with_volume$SCORE))))

cat("\n== correlation between the count axis and the shape axis ==\n")
cnt <- score_count(j)$SCORE
shp <- score_profile(j, "DOW_DIST", bins = DOW)$SCORE
spn <- score_span(j, "DAY_DIST")$SCORE
cat(sprintf("  cor(count, shape) = %+.3f\n", cor(cnt, shp)))
cat(sprintf("  cor(count, span)  = %+.3f\n", cor(cnt, spn)))
cat(sprintf("  cor(shape, span)  = %+.3f\n", cor(shp, spn)))
