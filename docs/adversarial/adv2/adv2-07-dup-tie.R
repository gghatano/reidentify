## adversarial line 2 -- probe 7: 重複候補行が同点確率モデルを壊す
## run: Rscript docs/adversarial/adv2-07-dup-tie.R
suppressMessages(pkgload::load_all(".", quiet = TRUE))
options(width = 140)
hdr <- function(x) cat("\n==== ", x, " ====\n", sep = "")
mkscores <- function(df) { attr(df, "score_type") <- "distance"; df }

## ---------------------------------------------------------------------------
hdr("1. 真ペアを複製 -> 解析値だけが半分になり、シミュレーション値と食い違う")
raw  <- data.frame(ROW_NUMBER = 1:6, V = c(10, 20, 30, 40, 50, 60))
anon <- data.frame(ROW_NUMBER = 1:6, V = c(11, 19, 33, 44, 52, 58))
s <- as.data.frame(score_num(join_raw_anon_data(raw, anon), "V"))
e0 <- reid_evaluate(mkscores(s), seeds = 1:20, top_k = 1)
s_true2 <- mkscores(rbind(s, s[s$RAW_ROW_NUMBER == s$ANON_ROW_NUMBER, ]))
e1 <- reid_evaluate(s_true2, seeds = 1:20, top_k = 1)
cat(sprintf("複製なし  : analytic %.4f  simulated %.4f (sd %.4f)\n",
            e0$success_analytic, e0$success_mean, e0$success_sd))
cat(sprintf("真ペア複製: analytic %.4f  simulated %.4f (sd %.4f)   <-- 自己整合チェックが不一致\n",
            e1$success_analytic, e1$success_mean, e1$success_sd))
cat("エラーも警告も出ない。match_optimal / combine_scores / reid_result は重複を検出して停止する:\n")
cat("  match_optimal   -> ",
    tryCatch({match_optimal(s_true2); "通過(!)"}, error = function(e) "stop() する"), "\n")
cat("  combine_scores  -> ",
    tryCatch({combine_scores(list(s_true2)); "通過(!)"}, error = function(e) "stop() する"), "\n")
cat("  reid_evaluate   -> 通過（検出なし）\n")

## ---------------------------------------------------------------------------
hdr("2. 同点の中の『誤り候補』を複製 -> 解析値もシミュレーション値も揃って誤る")
## 数学的根拠: ANON1 から見て RAW1(真) と RAW2 が等距離なら、攻撃者の
## 成功確率は 1/2。同じ (ANON1,RAW2) の行が 2 本あっても候補集合は
## {RAW1, RAW2} のままなので、確率は 1/2 のはず。
raw  <- data.frame(ROW_NUMBER = 1:2, V = c(0, 10))
anon <- data.frame(ROW_NUMBER = 1:2, V = c(5, 5))
s <- as.data.frame(score_num(join_raw_anon_data(raw, anon), "V"))
print(s)
e_ok <- reid_evaluate(mkscores(s), seeds = 1:200, top_k = 1)
dup <- s[s$RAW_ROW_NUMBER == 2 & s$ANON_ROW_NUMBER == 1, , drop = FALSE]
s_dup <- mkscores(rbind(s, dup))
e_bad <- reid_evaluate(s_dup, seeds = 1:200, top_k = 1)
cat(sprintf("正しい候補表  : analytic %.4f  simulated %.4f   ANON1 の RISK = %.4f\n",
            e_ok$success_analytic, e_ok$success_mean,
            e_ok$per_record$RISK[e_ok$per_record$ANON_ROW_NUMBER == 1]))
cat(sprintf("誤り候補を複製: analytic %.4f  simulated %.4f   ANON1 の RISK = %.4f\n",
            e_bad$success_analytic, e_bad$success_mean,
            e_bad$per_record$RISK[e_bad$per_record$ANON_ROW_NUMBER == 1]))
cat("数学的に正しい ANON1 の RISK は 1/2 = 0.5。両方の経路が揃って 1/3 を報告する。\n")

## より大きい例で過小報告の大きさを測る
hdr("2b. n=100、全 ANON が 2 者同点。誤り側を r 本複製したときの報告値")
n <- 100
raw  <- data.frame(ROW_NUMBER = 1:n, V = rep(c(0, 10), length.out = n))
anon <- data.frame(ROW_NUMBER = 1:n, V = rep(5, n))
s <- as.data.frame(score_num(join_raw_anon_data(raw, anon), "V"))
for (r in 0:3) {
  ss <- s
  if (r > 0) {
    extra <- do.call(rbind, replicate(r, s[s$RAW_ROW_NUMBER != s$ANON_ROW_NUMBER, ],
                                      simplify = FALSE))
    ss <- rbind(s, extra)
  }
  e <- reid_evaluate(mkscores(ss), seeds = 1:20, top_k = 1)
  cat(sprintf("誤り候補の複製 %d 本: n_pairs=%5d  analytic=%.4f  simulated=%.4f  baseline=%.4f  lift=%.2f\n",
              r, e$n_pairs, e$success_analytic, e$success_mean, e$baseline$rate[1], e$lift))
}
cat("真の値はどの行でも 1/(候補 2 者) を各 ANON が持つので success = ",
    "半数の ANON が本人を候補に持つ構成による（下の per_record を参照）\n")

## ---------------------------------------------------------------------------
hdr("3. 浮動小数点: 単位を変えると max per-record risk が 0.5 -> 1.0 に変わる（再掲・確認）")
mk <- function(v_raw, v_anon) {
  score_num(join_raw_anon_data(
    data.frame(ROW_NUMBER = seq_along(v_raw), V = v_raw),
    data.frame(ROW_NUMBER = seq_along(v_anon), V = v_anon)), "V")
}
set.seed(11)
n <- 200
base <- sample(seq(1000, 99999), n)
delta <- sample(c(3, 5, 7, 11), n, TRUE)
raw_i <- c(base, base + 2 * delta)
an_i  <- base + delta
si <- mk(raw_i, an_i); sd_ <- mk(raw_i / 10, an_i / 10)
ei <- reid_evaluate(si, seeds = 1:20, top_k = c(1, 2))
ed <- reid_evaluate(sd_, seeds = 1:20, top_k = c(1, 2))
cat(sprintf("整数単位  : analytic %.6f  simulated %.6f  max_risk %.4f  TIE_SIZE=2 が %d 件\n",
            ei$success_analytic, ei$success_mean, ei$max_risk, sum(ei$per_record$TIE_SIZE == 2)))
cat(sprintf("1/10 単位 : analytic %.6f  simulated %.6f  max_risk %.4f  TIE_SIZE=2 が %d 件\n",
            ed$success_analytic, ed$success_mean, ed$max_risk, sum(ed$per_record$TIE_SIZE == 2)))
cat(sprintf("PR 表の行数: 整数 %d / 1-10 %d\n",
            nrow(ei$precision_recall), nrow(ed$precision_recall)))
cat(sprintf("PR 最上位行 precision: 整数 %.4f / 1-10 %.4f\n",
            ei$precision_recall$precision[1], ed$precision_recall$precision[1]))

## ---------------------------------------------------------------------------
hdr("4. top_k_candidates(ties='keep') は同点だらけでも recall 1 を保つか")
set.seed(4)
n <- 60
raw  <- data.frame(ROW_NUMBER = 1:n, V = sample(1:4, n, TRUE))
anon <- data.frame(ROW_NUMBER = 1:n, V = sample(1:4, n, TRUE))
s <- score_num(join_raw_anon_data(raw, anon), "V")
for (k in c(1, 3, 10)) {
  tk <- suppressWarnings(top_k_candidates(s, k = k, ties = "keep"))
  b <- attr(tk, "blocking")
  cat(sprintf("k=%2d ties=keep : kept %5d  recall %.4f  候補ゼロの ANON %d\n",
              k, b$n_pairs_kept, b$recall, b$n_anon_without_candidate))
  tk2 <- suppressWarnings(top_k_candidates(s, k = k, ties = "random", seed = 1))
  b2 <- attr(tk2, "blocking")
  cat(sprintf("k=%2d ties=random: kept %5d  recall %.4f\n", k, b2$n_pairs_kept, b2$recall))
}

## ---------------------------------------------------------------------------
hdr("5. reid_knowledge_curve: 知識が増えると成功率は単調非減少か")
set.seed(6)
d <- create_dummy_qi_data(people = 120, seed = 2)
d2 <- d
d2$AGE <- d2$AGE + sample(c(-2, 0, 2), nrow(d2), TRUE)
p <- join_raw_anon_data(d, d2)
kc <- suppressWarnings(reid_knowledge_curve(
  p, quasi_identifiers = c(AGE = "num", ZIP = "char", SEX = "char"),
  behavior = c(VISIT_COUNT = "num"), seeds = 1:10))
print(kc)
cat("W <= M <= S か: ", all(diff(kc$success_analytic) >= -1e-12), "\n")
