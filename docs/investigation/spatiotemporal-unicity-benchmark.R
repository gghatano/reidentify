## ---------------------------------------------------------------------------
## Issue #24 -- spatio-temporal unicity (k-point identification).
##
## Run with:  Rscript docs/investigation/spatiotemporal-unicity-benchmark.R
##
## de Montjoye et al., "Unique in the Crowd", reported that four approximate
## (place, time) points identified 95% of individuals in a fifteen-month
## mobility data set, and that coarsening the recording resolution bought far
## less protection than intuition suggests. This reproduces the shape of that
## result on the package's own generator and checks the property the issue
## asks for: coarsening either axis must lower the unicity, monotonically.
##
## It also carries the correction Issue #21 established by measurement --
## unicity is a *lower* bound on attack success, not an upper one -- by
## reporting the anonymity-set-aware rate next to it. The gap is the part a
## unicity-only report would hide.
## ---------------------------------------------------------------------------

suppressMessages(pkgload::load_all(".", quiet = TRUE))

tran <- create_dummy_transaction_data(
  people = 200, size = 40, spatiotemporal = TRUE, places = 50, days = 30,
  seed = 1
)
cat(sprintf(
  "fixture: %d events, %d people, %d locations, %d hours of observation\n",
  nrow(tran), length(unique(tran$ID)), length(unique(tran$PLACE)),
  max(tran$TIME) - min(tran$TIME) + 1
))

TIME_RES <- c(1, 6, 24, 24 * 7)
SPACE_RES <- c(1, 2, 5, 10)
TIME_LABEL <- c("hour", "6 hours", "day", "week")

res <- spatiotemporal_unicity(
  tran, k = 1:4, time_resolution = TIME_RES, space_resolution = SPACE_RES
)

## ---------------------------------------------------------------------------
cat("\n== 1. unicity by k, at full resolution ==\n")
## ---------------------------------------------------------------------------
full <- res[res$time_resolution == 1 & res$space_resolution == 1, ]
for (i in seq_len(nrow(full))) {
  cat(sprintf("  k = %d : unicity %.4f | attacker rate %.4f | mean anonymity set %.2f\n",
              full$k[i], full$unicity[i], full$expected_id_rate[i],
              full$mean_anonymity_set[i]))
}

## ---------------------------------------------------------------------------
cat("\n== 2. the resolution grid (unicity) ==\n")
## ---------------------------------------------------------------------------
for (kk in 1:4) {
  cat(sprintf("\n  k = %d\n", kk))
  cat(sprintf("  %-10s |%s\n", "time \\ space",
              paste(sprintf(" merge %2g ", SPACE_RES), collapse = "")))
  for (ti in seq_along(TIME_RES)) {
    vals <- vapply(SPACE_RES, function(sr) {
      res$unicity[res$k == kk & res$time_resolution == TIME_RES[ti] &
                    res$space_resolution == sr]
    }, numeric(1))
    cat(sprintf("  %-10s |%s\n", TIME_LABEL[ti],
                paste(sprintf("   %.3f  ", vals), collapse = "")))
  }
}

## ---------------------------------------------------------------------------
cat("\n== 3. monotonicity: coarser must never mean more unique ==\n")
## ---------------------------------------------------------------------------
bad <- 0
checks <- 0
for (kk in 1:4) {
  for (sr in SPACE_RES) {
    v <- res$unicity[res$k == kk & res$space_resolution == sr]
    v <- v[order(res$time_resolution[res$k == kk & res$space_resolution == sr])]
    checks <- checks + 1
    if (!all(diff(v) <= 1e-12)) bad <- bad + 1
  }
  for (tr in TIME_RES) {
    v <- res$unicity[res$k == kk & res$time_resolution == tr]
    v <- v[order(res$space_resolution[res$k == kk & res$time_resolution == tr])]
    checks <- checks + 1
    if (!all(diff(v) <= 1e-12)) bad <- bad + 1
  }
}
cat(sprintf("  time  axis: 16 sweeps\n  space axis: 16 sweeps\n"))
cat(sprintf("  monotonically non-increasing in %d/%d sweeps\n", checks - bad, checks))

## and in k, the other direction: more points can only help
bad_k <- 0
for (tr in TIME_RES) {
  for (sr in SPACE_RES) {
    v <- res$unicity[res$time_resolution == tr & res$space_resolution == sr]
    v <- v[order(res$k[res$time_resolution == tr & res$space_resolution == sr])]
    if (!all(diff(v) >= -1e-12)) bad_k <- bad_k + 1
  }
}
cat(sprintf("  non-decreasing in k in %d/%d sweeps\n", 16 - bad_k, 16))

## ---------------------------------------------------------------------------
cat("\n== 4. unicity is a lower bound, not an upper one ==\n")
## ---------------------------------------------------------------------------
cat(sprintf("  unicity <= expected_id_rate in %d/%d rows\n",
            sum(res$unicity <= res$expected_id_rate + 1e-12), nrow(res)))
gap <- res$expected_id_rate - res$unicity
worst <- res[which.max(gap), ]
cat(sprintf("  largest gap %.4f at k = %d, time %g, space %g:\n",
            max(gap), worst$k, worst$time_resolution, worst$space_resolution))
cat(sprintf("    unicity says %.4f of people are pinned down; an attacker\n",
            worst$unicity))
cat(sprintf("    guessing inside the anonymity set gets %.4f of them\n",
            worst$expected_id_rate))
cat("  Reporting unicity alone would understate the risk by that much.\n")

## ---------------------------------------------------------------------------
cat("\n== 5. how much coarsening it takes ==\n")
## ---------------------------------------------------------------------------
## The practical question: at k = 4, which settings hold the attacker under
## some tolerance? Reported against expected_id_rate, since that is the rate
## an actual attack achieves.
k4 <- res[res$k == 4, ]
k4 <- k4[order(k4$expected_id_rate), ]
for (i in seq_len(nrow(k4))) {
  cat(sprintf("  time %-4g space %-3g : unicity %.3f  attacker rate %.3f\n",
              k4$time_resolution[i], k4$space_resolution[i], k4$unicity[i],
              k4$expected_id_rate[i]))
}
cat("\n  Note how far the grid has to be coarsened before the attacker rate\n")
cat("  drops meaningfully: at daily resolution with locations merged 10 to a\n")
cat("  cell, four points still identify a substantial share of the population.\n")
