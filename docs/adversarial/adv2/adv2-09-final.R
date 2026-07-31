## adversarial line 2 -- probe 9: 過小報告方向の確定と粗視化非単調性の再現
## run: Rscript docs/adversarial/adv2-09-final.R
suppressMessages(pkgload::load_all(".", quiet = TRUE))
options(width = 140)
hdr <- function(x) cat("\n==== ", x, " ====\n", sep = "")

## ---------------------------------------------------------------------------
hdr("1. blocked=FALSE のまま過小報告する現実的な構成")
## 公開データの大半が「RAW では別の県として記録されている人」であるケース。
set.seed(4242)
n_raw <- 400
raw <- data.frame(
  ROW_NUMBER = 1:n_raw,
  PREF = sample(c("Tokyo", "Osaka", "Aichi", "Fukuoka"), n_raw, TRUE,
                prob = c(0.1, 0.3, 0.3, 0.3)),
  AGE = sample(20:70, n_raw, TRUE),
  INCOME = round(rnorm(n_raw, 500, 120)),
  stringsAsFactors = FALSE)
in_tokyo <- which(raw$PREF == "Tokyo")
moved <- sample(setdiff(seq_len(n_raw), in_tokyo), 120)
sel <- sort(c(in_tokyo, moved))
anon <- raw[sel, ]; anon$PREF <- "Tokyo"
anon$AGE <- anon$AGE + sample(c(-1, 0, 1), nrow(anon), TRUE)
rownames(anon) <- NULL

cand <- suppressWarnings(block_candidates(raw, anon, keys = "PREF"))
b <- attr(cand, "blocking")
cat("block_candidates: recall = ", b$recall, " (", b$n_true_pairs_kept, "/",
    b$n_true_pairs, ")\n", sep = "")
qi <- c(AGE = "num", INCOME = "num")
e_b <- reid_evaluate(suppressWarnings(score_multi(cand, qi, screen = "none")),
                     seeds = 1:10, top_k = c(1, 5))
e_f <- reid_evaluate(suppressWarnings(score_multi(join_raw_anon_data(raw, anon), qi,
                                                 screen = "none")),
                     seeds = 1:10, top_k = c(1, 5))
cat(sprintf("ブロック後: success %.4f  blocked=%s  coverage=%.4f  n_true_missing=%d\n",
            e_b$success_analytic, e_b$blocked, e_b$candidate_coverage, e_b$n_true_missing))
cat(sprintf("総当たり  : success %.4f  blocked=%s\n", e_f$success_analytic, e_f$blocked))
cat(sprintf("過小報告率: %.1f%%\n", 100 * (1 - e_b$success_analytic / e_f$success_analytic)))
cat("\nこのときの print() 出力（BLOCKED バナーも n_true_missing も出ない）:\n")
print(e_b)

## ---------------------------------------------------------------------------
hdr("2. 粗視化の非単調性を再現（probe4 と同じ生成器を 400 回）")
set.seed(2)
hits <- list()
for (trial in 1:400) {
  n_people <- sample(6:20, 1); n_ev <- sample(3:8, 1)
  tx <- do.call(rbind, lapply(seq_len(n_people), function(i) {
    data.frame(ID = i, PLACE = sprintf("P%03d", sample(1:12, n_ev, TRUE)),
               TIME = sample(0:23, n_ev, TRUE), stringsAsFactors = FALSE)
  }))
  r <- tryCatch(spatiotemporal_unicity(tx, k = 2, space_resolution = c(1, 2, 4, 12),
                                       seed = 1), error = function(e) NULL)
  if (is.null(r) || any(is.na(r$unicity))) next
  r <- r[order(r$space_resolution), ]
  d <- diff(r$unicity)
  if (max(d) > 1e-12) hits[[length(hits) + 1]] <- list(d = max(d), trial = trial, r = r)
}
cat("違反した fixture: ", length(hits), " / 400\n", sep = "")
if (length(hits) > 0) {
  ord <- order(vapply(hits, function(h) -h$d, numeric(1)))
  for (i in head(ord, 3)) {
    cat("--- trial ", hits[[i]]$trial, " 違反幅 ", sprintf("%.4f", hits[[i]]$d), " ---\n", sep = "")
    print(hits[[i]]$r[, c("k", "space_resolution", "n_evaluated", "n_points",
                          "exhaustive", "unicity", "expected_id_rate", "mean_anonymity_set")])
  }
}

## ---------------------------------------------------------------------------
hdr("3. 決定的な最小反例: 粗視化で unicity が上がる")
## exhaustive = TRUE の範囲で作り、サンプリング由来のゆらぎを排除する。
tx <- data.frame(
  ID    = c(1,1,1, 2,2,2, 3,3,3),
  PLACE = c("A","B","C",  "A","B","D",  "A","C","D"),
  TIME  = c(1,1,1, 1,1,1, 1,1,1),
  stringsAsFactors = FALSE)
r <- spatiotemporal_unicity(tx, k = 2, space_resolution = c(1, 2), time_resolution = 1,
                            n_samples = 100, seed = 1)
print(r[, c("k", "space_resolution", "n_evaluated", "n_points", "exhaustive",
            "unicity", "expected_id_rate", "mean_anonymity_set")])
cat("space_resolution 1 -> 2 で PLACE {A,B},{C,D} が併合される。\n")
cat("併合は情報を減らす操作なので unicity は増えてはならない。\n")

## ---------------------------------------------------------------------------
hdr("4. まとめ: 攻めたが破れなかった不変条件（再掲実行）")
set.seed(77)
n <- 120
raw <- data.frame(ROW_NUMBER = 1:n, A = rnorm(n) * 10, B = sample(letters, n, TRUE),
                  stringsAsFactors = FALSE)
anon <- raw; anon$A <- anon$A + rnorm(n)
s <- suppressWarnings(score_multi(join_raw_anon_data(raw, anon), c(A = "num", B = "char")))
e <- reid_evaluate(s, seeds = 1:20, top_k = c(1, 2, 3, 5, 10, 50))
pr <- e$precision_recall
cat("top-k 単調          : ", all(diff(e$top_k$hit_rate) >= -1e-12), "\n")
cat("top-1 == success    : ", isTRUE(all.equal(e$top_k$hit_rate[1], e$success_analytic)), "\n")
cat("precision in [0,1]  : ", all(pr$precision >= 0 & pr$precision <= 1), "\n")
cat("recall 単調         : ", all(diff(pr$recall) >= -1e-12), "\n")
cat("analytic vs sim     : ", sprintf("%.4f vs %.4f (sd %.4f)", e$success_analytic,
                                      e$success_mean, e$success_sd), "\n")
cat("success >= baseline : ", e$success_analytic >= e$baseline$rate[1], "\n")
cat("match_optimal 1対1  : ",
    anyDuplicated(na.omit(match_optimal(s, seed = 1)$RAW_ROW_NUMBER)) == 0, "\n")
