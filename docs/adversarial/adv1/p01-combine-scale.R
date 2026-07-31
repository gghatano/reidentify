## ---------------------------------------------------------------------------
## Adversarial probe 1 (line 1): combine_scores() does not normalise, but the
## README's "3 層 API" table says it does ("正規化して加重和").
##
## Hypothesis: following the README, a user who combines two informative axes
## whose SCORE scales differ by orders of magnitude gets a combined rate BELOW
## the better single axis. No error, no warning.
## ---------------------------------------------------------------------------
suppressMessages(pkgload::load_all(".", quiet = TRUE))

set.seed(1)
raw  <- create_dummy_qi_data(people = 200, seed = 1)
anon <- create_dummy_qi_data(people = 200, seed = 1)
set.seed(42)
anon$AGE <- anon$AGE + sample(c(-2, 0, 2), nrow(anon), replace = TRUE)

pairs <- join_raw_anon_data(raw, anon)

s_age  <- score_num(pairs, "AGE")
s_zip  <- score_char(pairs, "ZIP")
s_dist <- score_dist(pairs, "SPEND_DIST")
s_cnt  <- score_num(pairs, "VISIT_COUNT")

rate <- function(s) reid_evaluate(s, seeds = 1:20)$success_analytic

cat("== SCORE scale of each axis (range) ==\n")
for (nm in c("s_age", "s_zip", "s_dist", "s_cnt")) {
  v <- get(nm)$SCORE
  cat(sprintf("  %-8s min %12.4f  max %12.4f  sd %12.4f\n",
              nm, min(v), max(v), sd(v)))
}

cat("\n== single-axis success (analytic) ==\n")
singles <- c(AGE = rate(s_age), ZIP = rate(s_zip),
             SPEND_DIST = rate(s_dist), VISIT_COUNT = rate(s_cnt))
print(round(singles, 4))

cat("\n== combine_scores(), exactly as the README does it (no weights) ==\n")
combos <- list(
  "AGE+ZIP (README example)"      = list(s_age, s_zip),
  "AGE+SPEND_DIST"                = list(s_age, s_dist),
  "ZIP+SPEND_DIST"                = list(s_zip, s_dist),
  "AGE+ZIP+SPEND_DIST"            = list(s_age, s_zip, s_dist),
  "AGE+ZIP+VISIT_COUNT+SPEND_DIST" = list(s_age, s_zip, s_cnt, s_dist)
)
for (nm in names(combos)) {
  w <- withCallingHandlers(
    rate(combine_scores(combos[[nm]])),
    warning = function(x) cat("   [warning] ", conditionMessage(x), "\n")
  )
  cat(sprintf("  %-32s %.4f\n", nm, w))
}

cat("\n== the same combinations, but normalised first (the honest attack) ==\n")
for (nm in names(combos)) {
  n <- rate(combine_scores(normalize_scores(combos[[nm]], method = "rank")))
  cat(sprintf("  %-32s %.4f  (normalize = rank)\n", nm, n))
}
for (nm in names(combos)) {
  n <- rate(combine_scores(normalize_scores(combos[[nm]], method = "range")))
  cat(sprintf("  %-32s %.4f  (normalize = range)\n", nm, n))
}

cat("\n== score_multi() on the same columns (normalises by default) ==\n")
sm <- score_multi(pairs, c(AGE = "num", ZIP = "char", SPEND_DIST = "dist"),
                  screen = "none")
cat(sprintf("  score_multi AGE+ZIP+SPEND_DIST   %.4f\n", rate(sm)))
