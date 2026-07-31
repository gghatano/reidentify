## Adversarial probe 10: the README's own headline combination example.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
raw  <- create_dummy_qi_data(people = 200, seed = 1)
anon <- create_dummy_qi_data(people = 200, seed = 1)
set.seed(42)
anon$AGE <- anon$AGE + sample(c(-2, 0, 2), nrow(anon), replace = TRUE)
pairs <- join_raw_anon_data(raw, anon)
s_age <- score_num(pairs, "AGE"); s_zip <- score_char(pairs, "ZIP")
rate <- function(s) reid_evaluate(s, seeds = 1:20)$success_analytic
cat(sprintf("README:  combine_scores(list(s_age, s_zip))            %.4f  (README prints 0.4303)\n",
            rate(combine_scores(list(s_age, s_zip)))))
cat(sprintf("         combine_scores(normalize_scores(.., 'range')) %.4f\n",
            rate(combine_scores(normalize_scores(list(s_age, s_zip), "range")))))
cat(sprintf("         combine_scores(normalize_scores(.., 'rank'))  %.4f\n",
            rate(combine_scores(normalize_scores(list(s_age, s_zip), "rank")))))
cat(sprintf("         score_multi(c(AGE='num', ZIP='char'))         %.4f\n",
            rate(score_multi(pairs, c(AGE="num", ZIP="char"), screen="none"))))
cat(sprintf("         order swapped: combine_scores(list(zip, age)) %.4f  <- order-independent\n",
            rate(combine_scores(list(s_zip, s_age)))))
cat(sprintf("         combine_scores(list(age,zip), weights=c(1,26)) %.4f  <- hand-tuned to sd ratio\n",
            rate(combine_scores(list(s_age, s_zip), weights = c(1, sd(s_age$SCORE)/sd(s_zip$SCORE))))))
cat("\nmatch_greedy on the README's `combined` vs the normalised one:\n")
cat(sprintf("  greedy  README   %.4f | normalised %.4f\n",
    mean(match_greedy(combine_scores(list(s_age,s_zip)), seed=1)$RESULT),
    mean(match_greedy(combine_scores(normalize_scores(list(s_age,s_zip),"rank")), seed=1)$RESULT)))
cat(sprintf("  optimal README   %.4f | normalised %.4f  (README prints 0.630)\n",
    mean(match_optimal(combine_scores(list(s_age,s_zip)), seed=1)$RESULT),
    mean(match_optimal(combine_scores(normalize_scores(list(s_age,s_zip),"rank")), seed=1)$RESULT)))
