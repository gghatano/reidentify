## ---------------------------------------------------------------------------
## Adversarial probe 9:
##  (A) score_multi()'s DEFAULT normalize = "range" is erased by one sentinel
##      value ("unknown age = 999"), when two continuous axes must share the
##      work. Confirms and quantifies what p03/p03b failed to trigger.
##  (B) Scoreboard-RH / sparse path.
##  (C) top_k_candidates() and lsh_candidates() defaults.
## ---------------------------------------------------------------------------
suppressMessages(pkgload::load_all(".", quiet = TRUE))
rate <- function(s) reid_evaluate(s, seeds = 1:20)$success_analytic

cat("############ (A) one sentinel value kills the default normalisation ############\n")
build <- function(sent = NA, k = 1) {
  raw  <- create_dummy_qi_data(people = 200, seed = 1)
  anon <- create_dummy_qi_data(people = 200, seed = 1)
  set.seed(42)
  anon$AGE <- anon$AGE + sample(c(-2, 0, 2), nrow(anon), replace = TRUE)
  anon$SPEND_MEAN <- anon$SPEND_MEAN + rnorm(nrow(anon), sd = 3)
  if (!is.na(sent)) anon$AGE[seq_len(k)] <- sent   # "unknown" carried through
  join_raw_anon_data(raw, anon)
}
qi <- c(AGE = "num", SPEND_MEAN = "num")
cat(sprintf("%-30s %10s %10s %10s | %10s %10s\n", "ANON AGE sentinel",
            "range*", "rank", "zscore", "AGE alone", "SPEND alone"))
for (sent in c(NA, 120, 999, 9999, 99999)) {
  p <- build(sent)
  r <- rate(score_multi(p, qi, screen = "none"))
  k <- rate(score_multi(p, qi, normalize = "rank",   screen = "none"))
  z <- rate(score_multi(p, qi, normalize = "zscore", screen = "none"))
  cat(sprintf("%-30s %10.4f %10.4f %10.4f | %10.4f %10.4f\n",
              if (is.na(sent)) "none (clean)" else sprintf("1 record AGE = %g", sent),
              r, k, z, rate(score_num(p, "AGE")), rate(score_num(p, "SPEND_MEAN"))))
}
cat("  * range is the DEFAULT of score_multi() / score_by_knowledge().\n")
p <- build(9999)
warns <- character(0)
invisible(withCallingHandlers(score_multi(p, qi),
  warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }))
cat(sprintf("  warnings from the default call: %s\n",
            if (length(warns)) paste(warns, collapse = " | ") else "NONE"))
cat("  axis report:\n"); print(axis_report(score_multi(p, qi, screen = "warn")))
cat("\n  printed evaluation of the broken default:\n")
print(reid_evaluate(score_multi(p, qi, screen = "none"), seeds = 1:20))

cat("\n############ (B) Scoreboard-RH on sparse data ############\n")
set.seed(99)
n <- 200; n_item <- 300
mat <- matrix(NA_real_, n, n_item)
for (i in seq_len(n)) {
  k <- sample(5:25, 1)
  mat[i, sample.int(n_item, k)] <- sample(1:5, k, replace = TRUE)
}
sb_anon <- data.frame(ROW_NUMBER = 1:n, mat)
names(sb_anon)[-1] <- paste0("I", seq_len(n_item))
## attacker knows a random half of each person's ratings
sb_aux <- sb_anon
for (i in seq_len(n)) {
  known <- which(!is.na(as.numeric(sb_anon[i, -1])))
  drop <- sample(known, floor(length(known) / 2))
  sb_aux[i, drop + 1] <- NA
}
items <- paste0("I", seq_len(n_item))
sb_pairs <- join_raw_anon_data(sb_aux, sb_anon)
s_sb <- score_scoreboard(sb_pairs, items, tolerance = 1)
cat(sprintf("  score_scoreboard + reid_evaluate  %.4f\n", rate(s_sb)))
for (phi in c(0, 0.5, 1.5, 3)) {
  r <- withCallingHandlers(
    mean(match_scoreboard_rh(s_sb, phi = phi)$RESULT),
    warning = function(w) { cat("   [warn]", substr(conditionMessage(w), 1, 80), "\n")
                            invokeRestart("muffleWarning") })
  cat(sprintf("  match_scoreboard_rh(phi = %-4g)    %.4f\n", phi, r))
}
cat(sprintf("  match_greedy on the same scores   %.4f\n",
            mean(match_greedy(s_sb, seed = 1)$RESULT)))
cat(sprintf("  score_jaccard on the same people  %.4f\n",
            {
              set_raw <- data.frame(ROW_NUMBER = 1:n,
                ITEMS = apply(sb_aux[, -1], 1, function(r) paste(which(!is.na(r)), collapse = ":")))
              set_anon <- data.frame(ROW_NUMBER = 1:n,
                ITEMS = apply(sb_anon[, -1], 1, function(r) paste(which(!is.na(r)), collapse = ":")))
              rate(score_jaccard(join_raw_anon_data(set_raw, set_anon), "ITEMS"))
            }))

cat("\n############ (C) candidate-reduction defaults ############\n")
raw  <- create_dummy_qi_data(people = 200, seed = 1)
anon <- create_dummy_qi_data(people = 200, seed = 1)
set.seed(42); anon$AGE <- anon$AGE + sample(c(-2, 0, 2), nrow(anon), replace = TRUE)
pairs <- join_raw_anon_data(raw, anon)
qi3 <- c(AGE = "num", ZIP = "char", SEX = "char")
s_full <- score_multi(pairs, qi3, screen = "none")
cat(sprintf("  full join                              %.4f\n", rate(s_full)))
for (k in c(1, 5, 10, 50)) {
  for (ti in c("keep", "random")) {
    tk <- suppressWarnings(top_k_candidates(s_full, k = k, ties = ti, seed = 1))
    e <- reid_evaluate(tk, seeds = 1:20)
    cat(sprintf("  top_k_candidates(k=%-3d ties=%-6s) %.4f   recall %.4f  blocked-flag %s\n",
                k, ti, e$success_analytic, attr(tk, "blocking")$recall, e$blocked))
  }
}
