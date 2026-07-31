## ---------------------------------------------------------------------------
## Adversarial probe 4: reid_evaluate() computes n_true_missing but only PRINTS
## it when `blocked` is TRUE.
##
## `blocked` is decided by `nrow(scores) < n_anon * n_raw`, i.e. purely by the
## shape of the candidate table. A candidate table that is a *complete* cross
## join of a PARTIAL population is therefore never flagged -- even though every
## ANON record without a RAW counterpart is silently counted as a failure and
## drags the reported rate down.
##
## Realistic route: the attacker's auxiliary data covers only part of the
## published population (README's own W/M/S discussion), or the RAW table was
## subsampled "for speed", or the two ROW_NUMBER columns simply do not line up
## (renumbering during publication, a whitespace/zero-padding difference on
## import).
## ---------------------------------------------------------------------------
suppressMessages(pkgload::load_all(".", quiet = TRUE))

qi <- c(AGE = "num", ZIP = "char", SEX = "char")

mk <- function() {
  raw  <- create_dummy_qi_data(people = 200, seed = 1)
  anon <- create_dummy_qi_data(people = 200, seed = 1)
  set.seed(42)
  anon$AGE <- anon$AGE + sample(c(-2, 0, 2), nrow(anon), replace = TRUE)
  list(raw = raw, anon = anon)
}
d <- mk()

cat("############ A. full overlap (the README case) ############\n")
p_full <- join_raw_anon_data(d$raw, d$anon)
e_full <- reid_evaluate(score_multi(p_full, qi, screen = "none"), seeds = 1:20)
print(e_full)
cat(sprintf("\n  [object fields] blocked=%s  n_true_missing=%d  coverage=%.4f\n\n",
            e_full$blocked, e_full$n_true_missing, e_full$candidate_coverage))

cat("############ B. attacker's RAW covers only 100 of the 200 published ############\n")
p_half <- join_raw_anon_data(d$raw[1:100, ], d$anon)
e_half <- reid_evaluate(score_multi(p_half, qi, screen = "none"), seeds = 1:20)
print(e_half)
cat(sprintf("\n  [object fields] blocked=%s  n_true_missing=%d  coverage=%.4f\n",
            e_half$blocked, e_half$n_true_missing, e_half$candidate_coverage))
cat("  -> n_true_missing is 100/200 but NOTHING above says so.\n")
cat(sprintf("  -> rate among the 100 answerable records: %.4f\n",
            mean(e_half$per_record$RISK[e_half$per_record$ANON_ROW_NUMBER %in%
                                          d$raw$ROW_NUMBER[1:100]])))

cat("\n############ C. same population, but blocked instead ############\n")
cat("   (identical information loss; here the tool DOES warn)\n")
cand <- block_candidates(d$raw, d$anon, keys = "AGE")
e_blk <- reid_evaluate(score_multi(cand, qi, screen = "none"), seeds = 1:20)
print(e_blk)

cat("\n############ D. ROW_NUMBER does not line up at all ############\n")
cat("   (publication renumbered the rows: ANON ROW_NUMBER = 1001..1200)\n")
anon2 <- d$anon
anon2$ROW_NUMBER <- anon2$ROW_NUMBER + 1000
p_bad <- join_raw_anon_data(d$raw, anon2)
e_bad <- withCallingHandlers(
  reid_evaluate(score_multi(p_bad, qi, screen = "none"), seeds = 1:20),
  warning = function(w) cat("  [warning]", conditionMessage(w), "\n")
)
print(e_bad)
cat(sprintf("\n  [object fields] blocked=%s  n_true_missing=%d/%d\n",
            e_bad$blocked, e_bad$n_true_missing, e_bad$n_anon))
cat("  -> reads as 'perfectly safe'. No error, no warning, no note.\n")

cat("\n############ E. same for a whitespace difference on import ############\n")
anon3 <- d$anon
anon3$ROW_NUMBER <- paste0(" ", anon3$ROW_NUMBER)   # e.g. read.csv of ' 1, 2, ...'
p_ws <- join_raw_anon_data(d$raw, anon3)
e_ws <- reid_evaluate(score_multi(p_ws, qi, screen = "none"), seeds = 1:20)
cat(sprintf("  success_analytic = %.4f   blocked=%s  n_true_missing=%d/%d\n",
            e_ws$success_analytic, e_ws$blocked, e_ws$n_true_missing, e_ws$n_anon))

cat("\n############ F. what block_candidates() says about case D ############\n")
cb <- block_candidates(d$raw, anon2, keys = "ZIP")
print(attr(cb, "blocking"))
cat("  -> the blocking layer DOES say 'not measurable'. reid_evaluate() does not.\n")
