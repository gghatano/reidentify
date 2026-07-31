## ---------------------------------------------------------------------------
## Adversarial probe 3: score_multi()'s default normalize = "range" is
## destroyed by a single extreme value.
##
## Realistic triggers, all of which occur in production QI data:
##   (a) a sentinel code for "unknown" (AGE = 999, INCOME = 9999999)
##   (b) top-coding, which is itself an anonymisation technique
##   (c) one genuine outlier (a 105-year-old, a whale customer)
##
## normalize = "range" divides by (max - min) over the WHOLE candidate table,
## so one pair with a huge distance compresses every other pair of that axis
## towards 0 and the axis silently stops counting.
## ---------------------------------------------------------------------------
suppressMessages(pkgload::load_all(".", quiet = TRUE))

rate <- function(s) reid_evaluate(s, seeds = 1:20)$success_analytic

make <- function(sentinel = NULL, n_sentinel = 0) {
  raw  <- create_dummy_qi_data(people = 200, seed = 1)
  anon <- create_dummy_qi_data(people = 200, seed = 1)
  set.seed(42)
  anon$AGE <- anon$AGE + sample(c(-2, 0, 2), nrow(anon), replace = TRUE)
  if (!is.null(sentinel)) {
    ## one (or a few) records whose age is unknown, coded with a sentinel on
    ## BOTH sides -- the value is simply carried through the release
    idx <- seq_len(n_sentinel)
    raw$AGE[idx]  <- sentinel
    anon$AGE[idx] <- sentinel
  }
  join_raw_anon_data(raw, anon)
}

qi <- c(AGE = "num", ZIP = "char", SEX = "char")

cat("== clean data (README's fixture) ==\n")
p0 <- make()
cat(sprintf("  score_multi range  %.4f\n", rate(score_multi(p0, qi, screen = "none"))))
cat(sprintf("  score_multi rank   %.4f\n",
            rate(score_multi(p0, qi, normalize = "rank", screen = "none"))))
cat(sprintf("  score_multi zscore %.4f\n",
            rate(score_multi(p0, qi, normalize = "zscore", screen = "none"))))

for (sent in c(999, 9999)) {
  for (ns in c(1, 3)) {
    cat(sprintf("\n== %d record(s) with AGE = %d (sentinel for 'unknown') ==\n",
                ns, sent))
    p <- make(sent, ns)
    r_range <- rate(score_multi(p, qi, screen = "none"))
    r_rank  <- rate(score_multi(p, qi, normalize = "rank", screen = "none"))
    r_z     <- rate(score_multi(p, qi, normalize = "zscore", screen = "none"))
    cat(sprintf("  score_multi range (DEFAULT)  %.4f\n", r_range))
    cat(sprintf("  score_multi rank             %.4f\n", r_rank))
    cat(sprintf("  score_multi zscore           %.4f\n", r_z))
    cat(sprintf("  -> default under-reports by  %.2fx vs rank\n", r_rank / r_range))
    ## does anything complain?
    cat("  warnings from the default call: ")
    w <- withCallingHandlers(
      { score_multi(p, qi); "none" },
      warning = function(x) { cat("[", conditionMessage(x), "] "); invokeRestart("muffleWarning") }
    )
    cat("\n")
    cat("  axis report (default screen):\n")
    print(axis_report(suppressWarnings(score_multi(p, qi))))
  }
}

cat("\n== the same effect from a single genuine outlier, no sentinel ==\n")
raw  <- create_dummy_qi_data(people = 200, seed = 1)
anon <- create_dummy_qi_data(people = 200, seed = 1)
set.seed(42)
anon$AGE <- anon$AGE + sample(c(-2, 0, 2), nrow(anon), replace = TRUE)
raw$SPEND_MEAN[1]  <- 50000    # one whale customer
anon$SPEND_MEAN[1] <- 50000
p <- join_raw_anon_data(raw, anon)
qi2 <- c(SPEND_MEAN = "num", ZIP = "char", SEX = "char")
cat(sprintf("  with whale,    range %.4f | rank %.4f\n",
            rate(score_multi(p, qi2, screen = "none")),
            rate(score_multi(p, qi2, normalize = "rank", screen = "none"))))
raw$SPEND_MEAN[1]  <- 60
anon$SPEND_MEAN[1] <- 60
p <- join_raw_anon_data(raw, anon)
cat(sprintf("  without whale, range %.4f | rank %.4f\n",
            rate(score_multi(p, qi2, screen = "none")),
            rate(score_multi(p, qi2, normalize = "rank", screen = "none"))))

cat("\n== printed output of the broken default (nothing looks wrong) ==\n")
p <- make(999, 1)
print(reid_evaluate(score_multi(p, qi, screen = "none"), seeds = 1:20))
