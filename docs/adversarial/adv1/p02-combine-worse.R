## ---------------------------------------------------------------------------
## Adversarial probe 2: with combine_scores() (no normalisation), GIVING THE
## ATTACKER MORE KNOWLEDGE LOWERS THE REPORTED RISK.
##
## Realistic anonymisation: AGE noised, SPEND amounts noised (a "ノイズ付与"
## release). The attacker holds ZIP + AGE + the spend distribution.
## ---------------------------------------------------------------------------
suppressMessages(pkgload::load_all(".", quiet = TRUE))

raw  <- create_dummy_qi_data(people = 200, seed = 1)
anon <- create_dummy_qi_data(people = 200, seed = 1)

## ---- an ordinary anonymisation: noise on AGE and on every spend amount -----
set.seed(42)
anon$AGE <- anon$AGE + sample(c(-2, 0, 2), nrow(anon), replace = TRUE)

set.seed(43)
anon$SPEND_DIST <- vapply(anon$SPEND_DIST, function(s) {
  v <- as.numeric(strsplit(s, ":", fixed = TRUE)[[1]])
  paste(round(pmax(0, v + rnorm(length(v), sd = 30)), 1), collapse = ":")
}, character(1))

pairs <- join_raw_anon_data(raw, anon)

s_age  <- score_num(pairs, "AGE")
s_zip  <- score_char(pairs, "ZIP")
s_dist <- score_dist(pairs, "SPEND_DIST")

rate <- function(s) reid_evaluate(s, seeds = 1:20)$success_analytic

cat("== SCORE scale ==\n")
for (nm in c("s_age", "s_zip", "s_dist")) {
  v <- get(nm)$SCORE
  cat(sprintf("  %-8s max %14.2f  sd %14.2f\n", nm, max(v), sd(v)))
}

cat("\n== single axes ==\n")
cat(sprintf("  AGE         %.4f\n", rate(s_age)))
cat(sprintf("  ZIP         %.4f\n", rate(s_zip)))
cat(sprintf("  SPEND_DIST  %.4f\n", rate(s_dist)))

cat("\n== combine_scores(), README style (no normalisation) ==\n")
f <- function(lst) {
  withCallingHandlers(rate(combine_scores(lst)),
                      warning = function(w) cat("   [warning]", conditionMessage(w), "\n"))
}
cat(sprintf("  ZIP + AGE                    %.4f\n", f(list(s_zip, s_age))))
cat(sprintf("  ZIP + SPEND_DIST             %.4f\n", f(list(s_zip, s_dist))))
cat(sprintf("  ZIP + AGE + SPEND_DIST       %.4f   <- all three known\n",
            f(list(s_zip, s_age, s_dist))))

cat("\n== same knowledge, normalised (normalize_scores + combine_scores) ==\n")
g <- function(lst, m) rate(combine_scores(normalize_scores(lst, method = m)))
for (m in c("range", "rank", "zscore")) {
  cat(sprintf("  ZIP + AGE + SPEND_DIST  [%-6s]  %.4f\n", m,
              g(list(s_zip, s_age, s_dist), m)))
}

cat("\n== score_multi() on the same three columns ==\n")
sm <- score_multi(pairs, c(ZIP = "char", AGE = "num", SPEND_DIST = "dist"),
                  screen = "none")
cat(sprintf("  score_multi (normalize = range default)  %.4f\n", rate(sm)))
sm2 <- score_multi(pairs, c(ZIP = "char", AGE = "num", SPEND_DIST = "dist"),
                   normalize = "rank", screen = "none")
cat(sprintf("  score_multi (normalize = rank)           %.4f\n", rate(sm2)))

cat("\n== does axis screening notice? ==\n")
print(axis_informativeness(list(ZIP = s_zip, AGE = s_age, SPEND_DIST = s_dist)))

cat("\n== full reid_evaluate() print of the under-reporting call ==\n")
print(reid_evaluate(combine_scores(list(s_zip, s_age, s_dist)), seeds = 1:20))
