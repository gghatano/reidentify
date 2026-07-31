## adversarial line 2 -- probe 5: 統計的妥当性と残りの層
## run: Rscript docs/adversarial/adv2-05-stats.R
suppressMessages(pkgload::load_all(".", quiet = TRUE))
options(width = 140)
hdr <- function(x) cat("\n==== ", x, " ====\n", sep = "")

## ---------------------------------------------------------------------------
hdr("1. axis_informativeness の第一種の過誤率（帰無仮説どおりの軸で測る）")
## 帰無仮説: この軸は identity と独立。alpha = 0.05 なら
## 「informative」と判定される割合は 0.05 前後であるべき。
run_fp <- function(n, n_levels, reps, kind = "char") {
  set.seed(2026)
  hits <- 0; ps <- numeric(reps)
  for (i in seq_len(reps)) {
    raw  <- data.frame(ROW_NUMBER = 1:n,
                       X = sample(seq_len(n_levels), n, TRUE), stringsAsFactors = FALSE)
    anon <- data.frame(ROW_NUMBER = 1:n,
                       X = sample(seq_len(n_levels), n, TRUE), stringsAsFactors = FALSE)
    ## RAW と ANON を独立に引くので、この軸は identity について完全に無情報
    d <- join_raw_anon_data(raw, anon)
    s <- if (kind == "char") {
      score_char(d, "X", generalized = "ignore")
    } else {
      score_num(d, "X")
    }
    r <- axis_informativeness(list(X = s))
    ps[i] <- r$p_value
    if (isTRUE(r$informative)) hits <- hits + 1
  }
  list(rate = hits / reps, ps = ps)
}
for (nl in c(2, 5, 20)) {
  r <- run_fp(n = 60, n_levels = nl, reps = 200, kind = "num")
  cat(sprintf("n=60 水準数=%2d : informative と判定 %5.1f%% (期待 5%%)  p の中央値 %.3f  最小 %.4g\n",
              nl, 100 * r$rate, median(r$ps), min(r$ps)))
}
r <- run_fp(n = 200, n_levels = 5, reps = 100, kind = "num")
cat(sprintf("n=200 水準数= 5 : informative と判定 %5.1f%%  p の中央値 %.3f\n",
            100 * r$rate, median(r$ps)))

hdr("2. 無情報な軸で success と baseline は一致するか")
set.seed(5)
diffs <- replicate(100, {
  n <- 80
  raw  <- data.frame(ROW_NUMBER = 1:n, X = sample(1:5, n, TRUE))
  anon <- data.frame(ROW_NUMBER = 1:n, X = sample(1:5, n, TRUE))
  s <- score_num(join_raw_anon_data(raw, anon), "X")
  e <- reid_evaluate(s, seeds = 1:5, top_k = 1)
  e$success_analytic - e$baseline$rate[1]
})
cat(sprintf("success - baseline_random : 平均 %+.5f  sd %.5f  最小 %+.5f  最大 %+.5f\n",
            mean(diffs), sd(diffs), min(diffs), max(diffs)))

## ---------------------------------------------------------------------------
hdr("3. IDF: 重みと距離の非負性、未知の値の扱い")
set.seed(3)
n <- 40
raw  <- data.frame(ROW_NUMBER = 1:n, Z = sample(c("a", "b", "c"), n, TRUE),
                   stringsAsFactors = FALSE)
anon <- raw
anon$Z[1:5] <- "zzz_only_in_anon"     # ANON にしかない値
d <- join_raw_anon_data(raw, anon)
for (mth in c("idf", "inv_log", "inv", "none")) {
  s <- tryCatch(score_idf(d, "Z", method = mth), error = function(e) NULL)
  if (is.null(s)) { cat(mth, ": error\n"); next }
  cat(sprintf("%-8s SCORE range [%.4f, %.4f]  非負? %s  自己一致の距離 max %.4f\n",
              mth, min(s$SCORE), max(s$SCORE), all(s$SCORE >= -1e-12),
              max(s$SCORE[s$RAW_ROW_NUMBER == s$ANON_ROW_NUMBER])))
}
## 全員が同じ値 = 情報ゼロ -> IDF 重み 0 になるはず
raw2 <- data.frame(ROW_NUMBER = 1:10, Z = rep("same", 10), stringsAsFactors = FALSE)
d2 <- join_raw_anon_data(raw2, raw2)
s2 <- score_idf(d2, "Z")
cat("全員同値の IDF スコア: 相異なる値 = ", paste(unique(s2$SCORE), collapse = ","), "\n")

## ---------------------------------------------------------------------------
hdr("4. Scoreboard-RH: phi 棄却で試行数が減らないか / CONFIDENCE の範囲")
sb_anon <- data.frame(ROW_NUMBER = 1:6,
                      I1 = c(5, NA, 1, 2, NA, 4), I2 = c(NA, 2, 2, 3, 1, NA),
                      I3 = c(3, 4, NA, NA, 5, 1), I4 = c(NA, 1, 5, 2, NA, 3))
sb_aux <- sb_anon; sb_aux$I3 <- NA
sb_pairs <- join_raw_anon_data(sb_aux, sb_anon)
ss <- score_scoreboard(sb_pairs, c("I1", "I2", "I3", "I4"), tolerance = 1)
for (phi in c(0, 0.5, 1.5, 100)) {
  m <- tryCatch(suppressWarnings(match_scoreboard_rh(ss, phi = phi)),
                error = function(e) NULL)
  if (is.null(m)) { cat("phi=", phi, ": error\n"); next }
  cat(sprintf("phi=%6.1f  行数 %d  guessed %d  success %d  CONFIDENCE range [%.4f, %.4f]\n",
              phi, nrow(m), sum(!is.na(m$RAW_ROW_NUMBER)), sum(m$RESULT),
              min(m$CONFIDENCE, na.rm = TRUE), max(m$CONFIDENCE, na.rm = TRUE)))
}

## ---------------------------------------------------------------------------
hdr("5. 一般化: containment_counts の値域と、真値が必ず含まれるか")
h <- read_generalization_hierarchy(
  system.file("extdata", "generalization-jp.csv", package = "reidentify"))
areas <- c("千代田区", "港区", "新宿区", "横浜市", "川崎市", "大阪市")
g_raw <- data.frame(ROW_NUMBER = 1:6, AGE = c(21, 24, 33, 37, 38, 52),
                    AREA = areas, stringsAsFactors = FALSE)
g_anon <- data.frame(ROW_NUMBER = 1:6,
                     AGE = generalize_value(g_raw$AGE, "AGE", h, levels = 1),
                     AREA = generalize_value(g_raw$AREA, "AREA", h, levels = 1),
                     stringsAsFactors = FALSE)
g_pairs <- join_raw_anon_data(g_raw, g_anon)
cc <- containment_counts(g_pairs, c("AGE", "AREA"), hierarchy = h)
print(cc)
cat("TRUTH_CONTAINED 全 TRUE? ", all(cc$TRUTH_CONTAINED), "\n")
cat("NARROWED_TO in (0,1]?    ", all(cc$NARROWED_TO > 0 & cc$NARROWED_TO <= 1), "\n")
cat("INFORMATION == 1/N_CONTAINED? ",
    isTRUE(all.equal(cc$INFORMATION, 1 / cc$N_CONTAINED)), "\n")
sc <- score_containment(g_pairs, c("AGE", "AREA"), hierarchy = h)
cat("score_containment 非負? ", all(sc$SCORE >= 0),
    " / 真ペアのスコアが最小か: ",
    all(vapply(split(seq_len(nrow(sc)), sc$ANON_ROW_NUMBER), function(ix) {
      tr <- sc$RAW_ROW_NUMBER[ix] == sc$ANON_ROW_NUMBER[ix]
      sc$SCORE[ix][tr] <= min(sc$SCORE[ix]) + 1e-12
    }, logical(1))), "\n")

## ---------------------------------------------------------------------------
hdr("6. blocking_recall は本当に正解ペア保持率を測っているか")
set.seed(11)
n <- 50
raw  <- data.frame(ROW_NUMBER = 1:n, K = sample(c("a", "b", "c"), n, TRUE),
                   V = rnorm(n), stringsAsFactors = FALSE)
anon <- raw
flip <- sample(n, 15)
anon$K[flip] <- sample(c("a", "b", "c"), 15, TRUE)   # 15 件のキーを壊す
cand <- suppressWarnings(block_candidates(raw, anon, keys = "K"))
b <- attr(cand, "blocking")
truth_kept <- sum(raw$K[match(anon$ROW_NUMBER, raw$ROW_NUMBER)] == anon$K)
cat("報告 recall = ", b$recall, "  (", b$n_true_pairs_kept, "/", b$n_true_pairs, ")\n", sep = "")
cat("手計算の保持数 = ", truth_kept, " / ", n, " = ", truth_kept / n, "\n", sep = "")
br <- blocking_recall(cand, raw, anon)
cat("blocking_recall() 再計算: recall = ", br$recall,
    " kept_fraction = ", br$kept_fraction, "\n", sep = "")
cat("一致? ", isTRUE(all.equal(b$recall, truth_kept / n)), "\n")
