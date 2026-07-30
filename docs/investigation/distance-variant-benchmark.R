## Decide between the two distribution_distance() implementations that the
## two branches produced.
##
##  A = quantile vector, squared L2      (fix/adversarial-findings, Issue #5 scope)
##  B = mean-fill + MSE                  (fix/adversarial-p4p5)
##  C = mean-fill + sort BOTH + MSE      (hybrid: B plus the missing sort)
##
## The disagreement is empirical: p4p5 measured that quantile interpolation
## lowers reid_by_dist()'s success rate on noisy data and rejected it. But
## their fixture gives every person the SAME record count in RAW and ANON, so
## record count is a perfect identity signal there -- any count sensitivity
## inflates apparent success. This script therefore measures success both in
## that setting and in one where counts carry no identity information.

parse_v <- function(s, split = ":") as.numeric(strsplit(s, split = split)[[1]])

dd_A <- function(x, y, n_quantiles = 10) {
  xv <- parse_v(x); yv <- parse_v(y)
  probs <- seq(0, 1, length.out = n_quantiles)
  sum((stats::quantile(xv, probs, names = FALSE) - stats::quantile(yv, probs, names = FALSE))^2)
}

dd_B <- function(x, y) {
  xv <- parse_v(x); yv <- parse_v(y)
  d <- length(xv) - length(yv)
  if (d > 0) yv <- sort(c(yv, rep(mean(yv), d))) else if (d < 0) xv <- sort(c(xv, rep(mean(xv), -d)))
  mean((xv - yv)^2)
}

dd_C <- function(x, y) {
  xv <- parse_v(x); yv <- parse_v(y)
  d <- length(xv) - length(yv)
  if (d > 0) yv <- c(yv, rep(mean(yv), d)) else if (d < 0) xv <- c(xv, rep(mean(xv), -d))
  mean((sort(xv) - sort(yv))^2)
}

VARIANTS <- list(A_quantile = dd_A, B_meanfill_mse = dd_B, C_meanfill_sorted = dd_C)

cat("=====================================================\n")
cat("1. ORDER INVARIANCE  d('3:1:2','1:2:3') must be 0\n")
cat("=====================================================\n")
for (nm in names(VARIANTS)) {
  f <- VARIANTS[[nm]]
  cat(sprintf("  %-20s d = %-10.6g  symmetric(9:1 vs 3:1:2) = %s\n", nm,
    f("3:1:2", "1:2:3"),
    isTRUE(all.equal(f("3:1:2", "9:1"), f("9:1", "3:1:2")))))
}

cat("\n=====================================================\n")
cat("2. COUNT DEPENDENCE (p4p5's own fixture, n=400 random pairs)\n")
cat("=====================================================\n")
mk <- function(v) paste(v, collapse = ":")
set.seed(71)
n <- 400
lx <- sample(2:20, n, TRUE); ly <- sample(2:20, n, TRUE)
xs <- lapply(lx, function(k) sort(runif(k)))
ys <- lapply(ly, function(k) sort(runif(k)))
ld <- abs(lx - ly)
dd_old <- function(x, y) { # pre-fix baseline: sum, not mean
  xv <- parse_v(x); yv <- parse_v(y)
  d <- length(xv) - length(yv)
  if (d > 0) yv <- sort(c(yv, rep(mean(yv), d))) else if (d < 0) xv <- sort(c(xv, rep(mean(xv), -d)))
  sum((xv - yv)^2)
}
base <- vapply(seq_len(n), function(i) dd_old(mk(xs[[i]]), mk(ys[[i]])), numeric(1))
cat(sprintf("  %-20s cor=%+.4f  ratio(ld>=10 / ld==0)=%.2fx\n", "pre-fix (sum)",
  cor(base, ld), mean(base[ld >= 10]) / mean(base[ld == 0])))
for (nm in names(VARIANTS)) {
  f <- VARIANTS[[nm]]
  d <- vapply(seq_len(n), function(i) f(mk(xs[[i]]), mk(ys[[i]])), numeric(1))
  cat(sprintf("  %-20s cor=%+.4f  ratio(ld>=10 / ld==0)=%.2fx\n", nm,
    cor(d, ld), mean(d[ld >= 10]) / mean(d[ld == 0])))
}

## ---------------------------------------------------------------------
## 3. Reidentification success under noise
## ---------------------------------------------------------------------
reid_success <- function(raw_dist, anon_dist, f) {
  n <- length(raw_dist)
  hit <- 0L
  for (a in seq_len(n)) {
    d <- vapply(seq_len(n), function(r) f(raw_dist[r], anon_dist[a]), numeric(1))
    best <- which(d == min(d))
    ## random tie-break, matching the merged resolve_min_distance_ties()
    if (length(best) > 1) best <- best[sample.int(length(best), 1)]
    if (best == a) hit <- hit + 1L
  }
  hit
}

make_case <- function(people, size, noise_sd, vary_counts, seed) {
  set.seed(seed)
  raw <- lapply(seq_len(people), function(i) sort(runif(size)))
  anon <- lapply(seq_len(people), function(i) {
    v <- raw[[i]] + stats::rnorm(length(raw[[i]]), 0, noise_sd)
    if (vary_counts) {
      k <- sample(max(2, size - 6):(size + 6), 1)
      v <- if (k <= length(v)) sample(v, k) else c(v, sample(v, k - length(v), TRUE))
    }
    sort(v)
  })
  list(raw = vapply(raw, mk, character(1)), anon = vapply(anon, mk, character(1)))
}

run_grid <- function(vary_counts, label, seeds = 1:12, people = 30, size = 12) {
  cat("\n=====================================================\n")
  cat("3", label, "\n")
  cat("=====================================================\n")
  cat(sprintf("  %-10s %-20s %-20s %-20s\n", "noise_sd", names(VARIANTS)[1], names(VARIANTS)[2], names(VARIANTS)[3]))
  for (sd_ in c(0, 0.02, 0.05, 0.1, 0.2)) {
    means <- sapply(names(VARIANTS), function(nm) {
      f <- VARIANTS[[nm]]
      mean(vapply(seeds, function(s) {
        cs <- make_case(people, size, sd_, vary_counts, s)
        set.seed(1000 + s)
        reid_success(cs$raw, cs$anon, f)
      }, numeric(1)))
    })
    cat(sprintf("  %-10s %-20.2f %-20.2f %-20.2f\n", sd_, means[1], means[2], means[3]))
  }
  cat(sprintf("  (success out of %d people, mean over %d seeds)\n", people, length(seeds)))
}

run_grid(vary_counts = FALSE, label = "a. RAW/ANON record counts IDENTICAL (p4p5's setting: count leaks identity)")
run_grid(vary_counts = TRUE, label = "b. RAW/ANON record counts DIFFER randomly (count carries no identity)")
