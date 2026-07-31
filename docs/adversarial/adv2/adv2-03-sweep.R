## adversarial line 2 -- probe 3: 不変条件のランダム掃引
## run: Rscript docs/adversarial/adv2-03-sweep.R
suppressMessages(pkgload::load_all(".", quiet = TRUE))
options(width = 140)
fails <- new.env(parent = emptyenv()); fails$n <- 0L
ok <- function(cond, label, extra = "") {
  if (isTRUE(cond)) return(invisible(TRUE))
  fails$n <- fails$n + 1L
  cat("  VIOLATION [", label, "] ", extra, "\n", sep = "")
  invisible(FALSE)
}
hdr <- function(x) cat("\n==== ", x, " ====\n", sep = "")

check_eval <- function(ev, tag) {
  pr <- ev$precision_recall
  ok(all(ev$per_record$RISK >= 0 & ev$per_record$RISK <= 1), "RISK in [0,1]", tag)
  ok(ev$success_analytic >= 0 && ev$success_analytic <= 1, "success in [0,1]", tag)
  ok(isTRUE(all.equal(ev$max_risk, max(ev$per_record$RISK))), "max_risk", tag)
  hk <- ev$top_k$hit_rate
  ok(all(diff(hk) >= -1e-12), "top-k monotone in k", paste(tag, paste(round(hk, 6), collapse = ",")))
  ok(all(hk >= -1e-12 & hk <= 1 + 1e-12), "top-k in [0,1]", tag)
  if (any(ev$top_k$k == 1)) {
    ok(isTRUE(all.equal(hk[ev$top_k$k == 1], ev$success_analytic)),
       "top-1 == success_analytic", tag)
  }
  if (is.data.frame(pr) && nrow(pr) > 0) {
    ok(all(pr$precision >= -1e-12 & pr$precision <= 1 + 1e-12), "precision in [0,1]",
       paste(tag, max(pr$precision)))
    ok(all(pr$recall >= -1e-12 & pr$recall <= 1 + 1e-12), "recall in [0,1]", tag)
    ok(all(diff(pr$n_attacked) >= 0), "n_attacked monotone as threshold falls", tag)
    ok(all(diff(pr$recall) >= -1e-12), "recall monotone as threshold falls", tag)
    ok(isTRUE(all.equal(pr$recall[nrow(pr)], ev$success_analytic)),
       "recall at lowest threshold == success", tag)
    ok(all(abs(pr$precision * pr$n_attacked / ev$n_anon - pr$recall) < 1e-10),
       "precision*coverage == recall", tag)
  } else {
    ok(FALSE, "precision_recall is a data frame", tag)
  }
  ## 解析値とシミュレーション値の一致（README が『壊れていたら気づける』根拠と呼ぶもの）
  se <- ev$success_sd / sqrt(ev$n_seeds)
  ok(abs(ev$success_analytic - ev$success_mean) <= max(4 * se, 0.06),
     "analytic vs simulated",
     sprintf("%s analytic=%.4f mean=%.4f sd=%.4f", tag, ev$success_analytic,
             ev$success_mean, ev$success_sd))
}

hdr("1. ランダムな数値/文字フィクスチャ 60 本での不変条件")
set.seed(20260731)
for (trial in 1:60) {
  n <- sample(2:40, 1)
  kind <- sample(c("num", "char", "rank", "multi"), 1)
  vals <- switch(
    sample(c("cont", "int", "few", "const", "skew"), 1),
    cont  = rnorm(n) * 10^sample(-3:3, 1),
    int   = sample(1:5, n, replace = TRUE),
    few   = sample(c(0, 1), n, replace = TRUE),
    const = rep(7, n),
    skew  = c(rep(1, n - 1), 1e6)
  )
  raw  <- data.frame(ROW_NUMBER = 1:n, V = vals, W = sample(letters[1:3], n, TRUE),
                     stringsAsFactors = FALSE)
  anon <- raw
  anon$V <- anon$V + rnorm(n, sd = sample(c(0, 0.1, 1), 1))
  d <- join_raw_anon_data(raw, anon)
  s <- tryCatch(switch(
    kind,
    num   = score_num(d, "V"),
    char  = score_char(d, "W"),
    rank  = score_num_rank(d, "V"),
    multi = suppressWarnings(score_multi(d, c(V = "num", W = "char")))
  ), error = function(e) NULL)
  if (is.null(s)) next
  ev <- tryCatch(reid_evaluate(s, seeds = 1:12, top_k = c(1, 2, 3, 5, 10, n)),
                 error = function(e) { cat("  eval error:", conditionMessage(e), "\n"); NULL })
  if (!is.null(ev)) check_eval(ev, sprintf("trial%02d/%s/n=%d", trial, kind, n))
}
cat("累計 violation = ", fails$n, "\n")

hdr("2. 退化ケース: n=1, n=2, 全件同値, 1 列のみ")
degen <- list(
  "n=1"          = data.frame(ROW_NUMBER = 1L, V = 5),
  "n=2 同値"     = data.frame(ROW_NUMBER = 1:2, V = c(5, 5)),
  "n=2 相異"     = data.frame(ROW_NUMBER = 1:2, V = c(5, 9)),
  "n=5 全件同値" = data.frame(ROW_NUMBER = 1:5, V = rep(3, 5)),
  "n=5 全件一意" = data.frame(ROW_NUMBER = 1:5, V = 1:5 * 10)
)
for (nm in names(degen)) {
  raw <- degen[[nm]]
  d <- join_raw_anon_data(raw, raw)
  s <- score_num(d, "V")
  cat("-- ", nm, "\n", sep = "")
  cf <- tryCatch(reid_confidence(s), error = function(e) conditionMessage(e))
  print(cf)
  ev <- tryCatch(reid_evaluate(s, seeds = 1:5, top_k = c(1, 2)),
                 error = function(e) { cat("   reid_evaluate ERROR: ", conditionMessage(e), "\n"); NULL })
  if (!is.null(ev)) {
    cat(sprintf("   success=%.4f baseline_random=%.4f lift=%s max_risk=%.4f pr_rows=%s\n",
                ev$success_analytic, ev$baseline$rate[1], format(ev$lift), ev$max_risk,
                if (is.null(ev$precision_recall)) "NULL" else nrow(ev$precision_recall)))
    check_eval(ev, nm)
    ok(ev$success_analytic >= ev$baseline$rate[1] - 1e-12,
       "success >= random baseline", sprintf("%s: %.4f vs %.4f", nm, ev$success_analytic,
                                             ev$baseline$rate[1]))
  }
}
cat("累計 violation = ", fails$n, "\n")

hdr("3. 距離の公理: 非負・対称・自己距離 0")
set.seed(7)
n <- 25
raw <- data.frame(ROW_NUMBER = 1:n, V = rnorm(n) * 100,
                  S = replicate(n, paste(sample(letters, 5), collapse = "")),
                  D = replicate(n, paste(sample(1:50, 4), collapse = ":")),
                  stringsAsFactors = FALSE)
anon <- raw
d_fwd <- join_raw_anon_data(raw, anon)
d_rev <- join_raw_anon_data(anon, raw)
for (fn in c("score_num", "score_char", "score_dist", "score_jaccard")) {
  tgt <- switch(fn, score_num = "V", score_char = "S", "D")
  f <- get(fn)
  a <- tryCatch(f(d_fwd, tgt), error = function(e) NULL)
  b <- tryCatch(f(d_rev, tgt), error = function(e) NULL)
  if (is.null(a) || is.null(b)) { cat(fn, ": skipped\n"); next }
  key_a <- paste(a$RAW_ROW_NUMBER, a$ANON_ROW_NUMBER)
  key_b <- paste(b$ANON_ROW_NUMBER, b$RAW_ROW_NUMBER)
  m <- match(key_a, key_b)
  sym <- max(abs(a$SCORE - b$SCORE[m]))
  self <- a$SCORE[a$RAW_ROW_NUMBER == a$ANON_ROW_NUMBER]
  cat(sprintf("%-15s min=%.6g  max asym |d(x,y)-d(y,x)|=%.3g  max self-distance=%.3g\n",
              fn, min(a$SCORE), sym, max(abs(self))))
  ok(all(a$SCORE >= 0), paste(fn, "non-negative"))
  ok(sym < 1e-12, paste(fn, "symmetric"))
  ok(max(abs(self)) < 1e-12, paste(fn, "d(x,x)==0"))
}

hdr("4. score_num_rank は単調変換で不変か（順位ベース手法の基本性質）")
set.seed(3)
n <- 40
v <- runif(n, 1, 100)
raw <- data.frame(ROW_NUMBER = 1:n, V = v)
anon <- data.frame(ROW_NUMBER = 1:n, V = v + rnorm(n, sd = 3))
base <- score_num_rank(join_raw_anon_data(raw, anon), "V")
for (tf in list(log = log, cube = function(x) x^3, expm = function(x) exp(x / 40),
                affine = function(x) 3 * x + 7)) {
  r2 <- data.frame(ROW_NUMBER = 1:n, V = tf(raw$V))
  a2 <- data.frame(ROW_NUMBER = 1:n, V = tf(anon$V))
  s2 <- score_num_rank(join_raw_anon_data(r2, a2), "V")
  ok(identical(base$SCORE, s2$SCORE), "score_num_rank monotone-invariant",
     paste("max diff", max(abs(base$SCORE - s2$SCORE))))
}
cat("score_num_rank 単調不変性チェック完了\n")

hdr("5. normalize_scores は各表の内部順位を変えないか")
set.seed(5)
s <- score_num(join_raw_anon_data(raw, anon), "V")
for (mth in c("range", "zscore", "rank", "none")) {
  s2 <- normalize_scores(s, method = mth)
  ok(identical(rank(s$SCORE, ties.method = "min"), rank(s2$SCORE, ties.method = "min")),
     paste("normalize_scores keeps ranking:", mth))
}
cat("normalize_scores 順位保存チェック完了\n")

hdr("6. match_optimal の 1 対 1 制約")
set.seed(9)
for (trial in 1:15) {
  n_r <- sample(3:12, 1); n_a <- sample(3:12, 1)
  rr <- data.frame(ROW_NUMBER = 1:n_r, V = rnorm(n_r) * 10)
  aa <- data.frame(ROW_NUMBER = 1:n_a, V = rnorm(n_a) * 10)
  s <- score_num(join_raw_anon_data(rr, aa), "V")
  m <- tryCatch(match_optimal(s, seed = trial), error = function(e) NULL)
  if (is.null(m)) next
  used <- m$RAW_ROW_NUMBER[!is.na(m$RAW_ROW_NUMBER)]
  ok(anyDuplicated(used) == 0, "match_optimal one-to-one",
     sprintf("n_raw=%d n_anon=%d dup=%s", n_r, n_a, anyDuplicated(used)))
  ok(nrow(m) == n_a, "match_optimal one row per ANON", sprintf("%d vs %d", nrow(m), n_a))
  ok(!anyDuplicated(m$ANON_ROW_NUMBER), "match_optimal ANON unique")
}
cat("match_optimal チェック完了\n")

hdr("7. README が保証する combine_scores の『正規化して加重和』")
raw2 <- data.frame(ROW_NUMBER = 1:5, A = c(1, 2, 3, 4, 5), B = c(1000, 2000, 3000, 4000, 5000))
d2 <- join_raw_anon_data(raw2, raw2)
sa <- score_num(d2, "A"); sb <- score_num(d2, "B")
cb <- combine_scores(list(sa, sb))
cat("A のスコア範囲 :", range(sa$SCORE), "\n")
cat("B のスコア範囲 :", range(sb$SCORE), "\n")
cat("combine 後の範囲:", range(cb$SCORE), "\n")
cat("combine == 単純和? ", isTRUE(all.equal(cb$SCORE, sa$SCORE + sb$SCORE)), "\n")

cat("\n================ 合計 violation = ", fails$n, " ================\n")
