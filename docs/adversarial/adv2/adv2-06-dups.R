## adversarial line 2 -- probe 6: 重複候補ペア・IDF・非 ASCII・型
## run: Rscript docs/adversarial/adv2-06-dups.R
suppressMessages(pkgload::load_all(".", quiet = TRUE))
options(width = 140)
hdr <- function(x) cat("\n==== ", x, " ====\n", sep = "")

dup_count <- function(s) sum(duplicated(paste(s$ANON_ROW_NUMBER, s$RAW_ROW_NUMBER, sep = "\r")))

## ---------------------------------------------------------------------------
hdr("1. block_candidates(keys = list(...)) の和集合は重複ペアを作るか")
set.seed(1)
n <- 30
raw  <- data.frame(ROW_NUMBER = 1:n,
                   ZIP = sample(c("A", "B", "C"), n, TRUE),
                   AGE = sample(20:29, n, TRUE),
                   V = rnorm(n) * 10, stringsAsFactors = FALSE)
anon <- raw
cand <- suppressWarnings(block_candidates(raw, anon, keys = list("ZIP", "AGE")))
cat("和集合ブロッキングの行数 = ", nrow(cand),
    "  重複 (ANON,RAW) ペア = ", dup_count(
      data.frame(ANON_ROW_NUMBER = cand$ANON_ROW_NUMBER, RAW_ROW_NUMBER = cand$RAW_ROW_NUMBER)),
    "\n", sep = "")
key <- paste(cand$ANON_ROW_NUMBER, cand$RAW_ROW_NUMBER, sep = "\r")
cat("重複ペア数 = ", sum(duplicated(key)), "\n", sep = "")

## ---------------------------------------------------------------------------
hdr("2. 候補表に重複ペアを混ぜたときの reid_evaluate の挙動")
## 数学的根拠: 同じ (ANON, RAW) ペアが 2 行あっても、攻撃者から見た候補集合は
## 変わらない。したがって risk / success / baseline は不変であるべき。
## match_optimal と reid_result は重複を検出して停止するが、reid_evaluate は？
raw2 <- data.frame(ROW_NUMBER = 1:6, V = c(10, 20, 30, 40, 50, 60))
anon2 <- data.frame(ROW_NUMBER = 1:6, V = c(11, 19, 33, 44, 52, 58))
s <- score_num(join_raw_anon_data(raw2, anon2), "V")
e0 <- reid_evaluate(s, seeds = 1:20, top_k = c(1, 2))
## 各 ANON の「本人以外の候補」を 1 行だけ複製する
dupe_rows <- which(s$RAW_ROW_NUMBER != s$ANON_ROW_NUMBER)
s2 <- rbind(as.data.frame(s), as.data.frame(s)[dupe_rows, ])
attr(s2, "score_type") <- "distance"
e1 <- reid_evaluate(s2, seeds = 1:20, top_k = c(1, 2))
cat(sprintf("重複なし: success=%.4f baseline_random=%.4f max_risk=%.4f n_pairs=%d blocked=%s\n",
            e0$success_analytic, e0$baseline$rate[1], e0$max_risk, e0$n_pairs, e0$blocked))
cat(sprintf("重複あり: success=%.4f baseline_random=%.4f max_risk=%.4f n_pairs=%d blocked=%s\n",
            e1$success_analytic, e1$baseline$rate[1], e1$max_risk, e1$n_pairs, e1$blocked))
## 真ペアの側を複製すると逆に上がるはず
dupe_true <- which(s$RAW_ROW_NUMBER == s$ANON_ROW_NUMBER)
s3 <- rbind(as.data.frame(s), as.data.frame(s)[dupe_true, ])
attr(s3, "score_type") <- "distance"
e2 <- reid_evaluate(s3, seeds = 1:20, top_k = c(1, 2))
cat(sprintf("真ペア複製: success=%.4f baseline_random=%.4f\n",
            e2$success_analytic, e2$baseline$rate[1]))
cat("エラー・警告は出たか: 上に出ていなければ出ていない\n")

hdr("2b. 元データの ROW_NUMBER が重複しているとき（明細を集約し忘れた形）")
raw3  <- data.frame(ROW_NUMBER = c(1, 1, 2, 3), V = c(10, 15, 20, 30))
anon3 <- data.frame(ROW_NUMBER = c(1, 2, 3),    V = c(10, 20, 30))
d3 <- join_raw_anon_data(raw3, anon3)
s3b <- score_num(d3, "V")
cat("候補表の行数 = ", nrow(s3b), " / 相異なるペア = ",
    length(unique(paste(s3b$ANON_ROW_NUMBER, s3b$RAW_ROW_NUMBER))), "\n", sep = "")
e3 <- tryCatch(reid_evaluate(s3b, seeds = 1:10, top_k = 1),
               error = function(e) { cat("ERROR: ", conditionMessage(e), "\n"); NULL })
if (!is.null(e3)) {
  cat(sprintf("success=%.4f n_anon=%d n_raw=%d n_pairs=%d blocked=%s\n",
              e3$success_analytic, e3$n_anon, e3$n_raw, e3$n_pairs, e3$blocked))
  print(e3$per_record)
}

## ---------------------------------------------------------------------------
hdr("3. IDF: 重みと距離の非負性、ANON 固有の値")
set.seed(3)
n <- 40
raw  <- data.frame(ROW_NUMBER = 1:n, Z = sample(c("a", "b", "c"), n, TRUE),
                   stringsAsFactors = FALSE)
anon <- raw
anon$Z[1:5] <- "only_in_anon"
d <- join_raw_anon_data(raw, anon)
for (w in c("idf", "inv_log", "inv", "none")) {
  s <- tryCatch(score_idf(d, "Z", weight = w), error = function(e) conditionMessage(e))
  if (is.character(s)) { cat(w, ": ERROR ", s, "\n"); next }
  self <- s$SCORE[s$RAW_ROW_NUMBER == s$ANON_ROW_NUMBER]
  cat(sprintf("%-8s SCORE [%.4f, %.4f] 非負=%s  自己ペアの距離 max=%.4f\n",
              w, min(s$SCORE), max(s$SCORE), all(s$SCORE >= -1e-12), max(self)))
}
raw2 <- data.frame(ROW_NUMBER = 1:10, Z = rep("same", 10), stringsAsFactors = FALSE)
s2 <- score_idf(join_raw_anon_data(raw2, raw2), "Z")
cat("全員同値の IDF スコア（相異なる値）= ", paste(unique(s2$SCORE), collapse = ","), "\n")

## ---------------------------------------------------------------------------
hdr("4. 非 ASCII / 空文字列 / 特殊文字")
vals <- c("東京都千代田区", "東京都港区", "", "a\tb", "école", "🙂🙂")
raw <- data.frame(ROW_NUMBER = seq_along(vals), S = vals, stringsAsFactors = FALSE)
d <- join_raw_anon_data(raw, raw)
s <- tryCatch(score_char(d, "S", generalized = "ignore"), error = function(e) conditionMessage(e))
if (is.character(s)) { cat("score_char ERROR: ", s, "\n") } else {
  self <- s$SCORE[s$RAW_ROW_NUMBER == s$ANON_ROW_NUMBER]
  cat("自己距離すべて 0? ", all(self == 0), "  非負? ", all(s$SCORE >= 0), "\n")
  cat("東京都千代田区 vs 東京都港区 の距離 = ",
      s$SCORE[s$RAW_ROW_NUMBER == 1 & s$ANON_ROW_NUMBER == 2], "\n")
  cat("空文字列を含むペアの距離 = ",
      s$SCORE[s$RAW_ROW_NUMBER == 3 & s$ANON_ROW_NUMBER == 1], "\n")
  ev <- tryCatch(reid_evaluate(s, seeds = 1:5, top_k = 1)$success_analytic,
                 error = function(e) conditionMessage(e))
  cat("reid_evaluate success = ", format(ev), "\n")
}
cat("unicity_fraction (非 ASCII) = ", unicity_fraction(raw, "S"), "\n")

## ---------------------------------------------------------------------------
hdr("5. ROW_NUMBER の型: 文字列・因子・負値・0")
mk <- function(ids) {
  raw <- data.frame(ROW_NUMBER = ids, V = seq_along(ids) * 10)
  d <- join_raw_anon_data(raw, raw)
  s <- score_num(d, "V")
  e <- reid_evaluate(s, seeds = 1:5, top_k = 1)
  sprintf("success=%.4f n_anon=%d baseline=%.4f mode=%.4f", e$success_analytic,
          e$n_anon, e$baseline$rate[1], e$baseline$rate[2])
}
for (nm in c("integer", "character", "negative", "zero-based", "double")) {
  ids <- switch(nm,
    integer = 1:5, character = as.character(1:5), negative = -2:2,
    "zero-based" = 0:4, double = c(1, 2, 3, 4, 5) + 0.5)
  cat(sprintf("%-12s : %s\n", nm, tryCatch(mk(ids), error = function(e) paste("ERROR", conditionMessage(e)))))
}

## 文字 ROW_NUMBER で 10 件以上（辞書順ソートの影響）
ids <- as.character(1:12)
raw <- data.frame(ROW_NUMBER = ids, V = (1:12) * 10, stringsAsFactors = FALSE)
s <- score_num(join_raw_anon_data(raw, raw), "V")
e <- reid_evaluate(s, seeds = 1:5, top_k = 1)
cat("character ROW_NUMBER n=12: success = ", e$success_analytic,
    " (期待 1.0)  n_anon = ", e$n_anon, "\n", sep = "")
