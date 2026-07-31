## adversarial line 2 -- probe 8: BLOCKED 検出の一般的な死角 と 粗視化の非単調性
## run: Rscript docs/adversarial/adv2-08-blocked.R
suppressMessages(pkgload::load_all(".", quiet = TRUE))
options(width = 140)
hdr <- function(x) cat("\n==== ", x, " ====\n", sep = "")

## ---------------------------------------------------------------------------
hdr("1. 現実的な設定: 公開データは 1 県ぶん、攻撃者の RAW は全国")
## ANON 側のブロッキングキーが 1 値しか取らないと、生き残った候補表は
## 「生き残った ANON x 生き残った RAW」の完全長方形になる。
## reid_evaluate の判定式 nrow(scores) < n_anon * n_raw はこの形を検出できない。
set.seed(20260731)
n_raw <- 500
raw <- data.frame(
  ROW_NUMBER = 1:n_raw,
  PREF = sample(c("Tokyo", "Osaka", "Aichi", "Fukuoka"), n_raw, TRUE),
  AGE = sample(20:70, n_raw, TRUE),
  INCOME = round(rnorm(n_raw, 500, 120)),
  stringsAsFactors = FALSE
)
## 公開されるのは Tokyo 在住として記録された人 + 引っ越して県が変わった人
in_tokyo <- which(raw$PREF == "Tokyo")
moved <- sample(setdiff(seq_len(n_raw), in_tokyo), 40)   # RAW では他県、ANON では Tokyo
anon <- raw[sort(c(in_tokyo, moved)), ]
anon$PREF <- "Tokyo"
anon$AGE <- anon$AGE + sample(c(-1, 0, 1), nrow(anon), TRUE)
rownames(anon) <- NULL

cand <- suppressWarnings(block_candidates(raw, anon, keys = "PREF"))
b <- attr(cand, "blocking")
cat("block_candidates の自己申告:\n"); print(b)

qi <- c(AGE = "num", INCOME = "num")
s_blocked <- suppressWarnings(score_multi(cand, qi, screen = "none"))
s_full    <- suppressWarnings(score_multi(join_raw_anon_data(raw, anon), qi, screen = "none"))
e_b <- reid_evaluate(s_blocked, seeds = 1:10, top_k = c(1, 5))
e_f <- reid_evaluate(s_full, seeds = 1:10, top_k = c(1, 5))
cat("\n--- ブロック後の候補表を reid_evaluate に渡した結果 ---\n")
print(e_b)
cat(sprintf("\n  ev$blocked = %s   ev$candidate_coverage = %.4f   ev$n_true_missing = %d\n",
            e_b$blocked, e_b$candidate_coverage, e_b$n_true_missing))
cat("--- 総当たりで測った同じ攻撃 ---\n")
cat(sprintf("  blocked = %s   success_analytic = %.4f   n_anon = %d\n",
            e_f$blocked, e_f$success_analytic, e_f$n_anon))
cat(sprintf("\n過小報告: %.4f -> %.4f （%.1f%% の取りこぼし。BLOCKED バナーは出ない）\n",
            e_f$success_analytic, e_b$success_analytic,
            100 * (1 - e_b$success_analytic / e_f$success_analytic)))

## ---------------------------------------------------------------------------
hdr("2. 判定式が破れる一般条件の確認: 生き残り ANON 全員が同じ RAW 集合を持つとき")
for (case in c("ANON キーが 1 値", "ANON キーが 2 値")) {
  a2 <- anon
  if (case == "ANON キーが 2 値") a2$PREF[1:20] <- "Osaka"
  cd <- suppressWarnings(block_candidates(raw, a2, keys = "PREF"))
  ss <- suppressWarnings(score_multi(cd, qi, screen = "none"))
  ee <- reid_evaluate(ss, seeds = 1:5, top_k = 1)
  cat(sprintf("%-16s : n_pairs=%5d  n_anon*n_raw=%5.0f  blocked=%-5s  n_true_missing=%d\n",
              case, ee$n_pairs, ee$n_pairs_full, ee$blocked, ee$n_true_missing))
}

## ---------------------------------------------------------------------------
hdr("3. 粗視化の非単調性: 最大の違反幅を探す")
set.seed(2)
worst <- NULL
for (trial in 1:300) {
  n_people <- sample(5:15, 1); n_ev <- sample(3:8, 1)
  tx <- do.call(rbind, lapply(seq_len(n_people), function(i) {
    data.frame(ID = i, PLACE = sprintf("P%03d", sample(1:10, n_ev, TRUE)),
               TIME = sample(0:23, n_ev, TRUE), stringsAsFactors = FALSE)
  }))
  r <- tryCatch(spatiotemporal_unicity(tx, k = 2, space_resolution = c(1, 2, 5, 10),
                                       time_resolution = c(1, 6, 24), seed = 1),
                error = function(e) NULL)
  if (is.null(r) || any(is.na(r$unicity))) next
  for (tr in unique(r$time_resolution)) {
    sub <- r[r$time_resolution == tr, ]
    sub <- sub[order(sub$space_resolution), ]
    d <- max(diff(sub$unicity))
    if (d > 1e-12 && (is.null(worst) || d > worst$d)) worst <- list(d = d, sub = sub, trial = trial)
  }
  for (sr in unique(r$space_resolution)) {
    sub <- r[r$space_resolution == sr, ]
    sub <- sub[order(sub$time_resolution), ]
    d <- max(diff(sub$unicity))
    if (d > 1e-12 && (is.null(worst) || d > worst$d)) worst <- list(d = d, sub = sub, trial = trial)
  }
}
if (is.null(worst)) {
  cat("違反は見つからなかった\n")
} else {
  cat("最大の違反幅 = ", sprintf("%.4f", worst$d), " (trial ", worst$trial, ")\n", sep = "")
  print(worst$sub[, c("k", "time_resolution", "space_resolution", "n_evaluated",
                      "n_points", "exhaustive", "unicity", "expected_id_rate")])
}
