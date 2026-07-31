## ---------------------------------------------------------------------------
## Adversarial probe 3b: same idea as p03, but with the *load-bearing* axis as
## the one that carries the extreme value.
##
## range normalisation is affine per axis, so it never reorders one axis's own
## candidates. The damage is purely in the RELATIVE WEIGHT between axes: an
## axis whose spread is inflated by one outlier is divided by that outlier and
## effectively drops out of the sum.
## ---------------------------------------------------------------------------
suppressMessages(pkgload::load_all(".", quiet = TRUE))

rate <- function(s) reid_evaluate(s, seeds = 1:20)$success_analytic

build <- function(sentinel = NA, one_sided = FALSE) {
  raw  <- create_dummy_qi_data(people = 200, seed = 1)
  anon <- create_dummy_qi_data(people = 200, seed = 1)
  set.seed(42)
  anon$AGE <- anon$AGE + sample(c(-2, 0, 2), nrow(anon), replace = TRUE)
  if (!is.na(sentinel)) {
    if (one_sided) {
      anon$AGE[1] <- sentinel          # age suppressed in the release only
    } else {
      raw$AGE[1] <- sentinel; anon$AGE[1] <- sentinel
    }
  }
  join_raw_anon_data(raw, anon)
}

report <- function(label, pairs, qi) {
  r <- rate(score_multi(pairs, qi, screen = "none"))
  k <- rate(score_multi(pairs, qi, normalize = "rank",   screen = "none"))
  z <- rate(score_multi(pairs, qi, normalize = "zscore", screen = "none"))
  cat(sprintf("  %-42s range(DEFAULT) %.4f | rank %.4f | zscore %.4f | ratio %.2fx\n",
              label, r, k, z, k / max(r, 1e-9)))
  invisible(c(range = r, rank = k, zscore = z))
}

cat("=== attacker knows AGE + SEX (SEX is weak, AGE carries the attack) ===\n")
qi <- c(AGE = "num", SEX = "char")
report("clean",                         build(),                 qi)
report("AGE=999 sentinel, both sides",  build(999),              qi)
report("AGE=999 sentinel, ANON only",   build(999, TRUE),        qi)
report("AGE=9999 sentinel, ANON only",  build(9999, TRUE),       qi)

cat("\n=== attacker knows AGE + SEX + VISIT_COUNT ===\n")
qi3 <- c(AGE = "num", SEX = "char", VISIT_COUNT = "num")
report("clean",                         build(),                 qi3)
report("AGE=999 sentinel, ANON only",   build(999, TRUE),        qi3)
report("AGE=9999 sentinel, ANON only",  build(9999, TRUE),       qi3)

cat("\n=== AGE alone, for reference ===\n")
for (lab in c("clean", "sent999_anon")) {
  p <- if (lab == "clean") build() else build(999, TRUE)
  cat(sprintf("  %-14s AGE alone %.4f | SEX alone %.4f\n", lab,
              rate(score_num(p, "AGE")), rate(score_char(p, "SEX"))))
}

cat("\n=== does anything warn? (default screen = warn) ===\n")
p <- build(9999, TRUE)
withCallingHandlers({
  s <- score_multi(p, qi)
  cat("  no warning raised\n")
}, warning = function(w) { cat("  [warning]", conditionMessage(w), "\n"); invokeRestart("muffleWarning") })
print(axis_report(s))

cat("\n=== printed evaluation of the under-reporting default ===\n")
print(reid_evaluate(score_multi(p, qi, screen = "none"), seeds = 1:20))
