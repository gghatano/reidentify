## 敵対的検証 3: 距離関数そのものの妥当性
## 別セッションの主張(calc_KL が負値 / distribution_distance が件数差と相関0.99)を独立に再現確認する

suppressPackageStartupMessages(library(reidentify))
KL <- reidentify:::calc_KL
DD <- reidentify:::distribution_distance
mk <- function(v) paste(v, collapse = ":")

cat("=== 1. calc_KL: KL ダイバージェンスの公理を満たすか ===\n")
cat("公理: (i) 非負 (ii) d(x,x)==0 \n\n")
cases <- list(
  c("同一"          , mk(c(1,2,3))      , mk(c(1,2,3))),
  c("逆順"          , mk(c(1,2,3))      , mk(c(3,2,1))),
  c("スケール2倍"   , mk(c(1,2,3))      , mk(c(2,4,6))),
  c("裾が重い"      , mk(c(1,1,10))     , mk(c(1,1,1))),
  c("平坦 vs 尖り"  , mk(c(5,5,5,5))    , mk(c(1,1,1,20))),
  c("小さい値混在"  , mk(c(1,50,100))   , mk(c(100,50,1)))
)
neg <- 0
for (cs in cases) {
  v <- suppressWarnings(tryCatch(KL(cs[2], cs[3]), error = function(e) NA_real_))
  flag <- if (is.na(v)) "" else if (v < -1e-12) "  <-- ★負値(非負性違反)" else ""
  if (!is.na(v) && v < -1e-12) neg <- neg + 1
  cat(sprintf("  %-14s x=%-12s y=%-12s KL=%12.6f%s\n", cs[1], cs[2], cs[3], v, flag))
}
cat(sprintf("\n  負値の件数: %d / %d\n", neg, length(cases)))

cat("\n=== 2. calc_KL: 非対称性は KL の仕様だが、対称に使われていないか ===\n")
cat(sprintf("  KL(x,y)=%.6f  KL(y,x)=%.6f\n", KL(mk(c(1,1,10)), mk(c(1,1,1))), KL(mk(c(1,1,1)), mk(c(1,1,10)))))

cat("\n=== 3. distribution_distance: レコード件数差との相関 ===\n")
cat("分布の「形」を測るはずが、実際は「要素数の差」を測っていないか\n\n")
set.seed(71)
n <- 400
lx <- sample(2:20, n, replace = TRUE)
ly <- sample(2:20, n, replace = TRUE)
d <- numeric(n); lendiff <- numeric(n)
for (i in seq_len(n)) {
  x <- sort(runif(lx[i], 0, 1)); y <- sort(runif(ly[i], 0, 1))
  d[i] <- DD(mk(x), mk(y))
  lendiff[i] <- abs(lx[i] - ly[i])
}
cat(sprintf("  cor(距離, |要素数の差|) = %.4f\n", cor(d, lendiff)))
cat(sprintf("  要素数が同じ組だけ (n=%d) の距離の平均 = %.4f\n", sum(lendiff==0), mean(d[lendiff==0])))
cat(sprintf("  要素数の差 >= 10 の組 (n=%d) の距離の平均 = %.4f\n", sum(lendiff>=10), mean(d[lendiff>=10])))

cat("\n  --- 決定的な例: 分布の形は同一、要素数だけ違う ---\n")
a <- mk(rep(0.5, 4)); b <- mk(rep(0.5, 14))
cat(sprintf("  形が完全に同じ(全要素0.5) 4要素 vs 14要素 -> 距離 %.6f\n", DD(a, b)))
a2 <- mk(c(0.1, 0.9, 0.1, 0.9)); b2 <- mk(c(0.9, 0.1, 0.9, 0.1))
cat(sprintf("  要素数同じ・形は反転           4要素 vs 4要素  -> 距離 %.6f\n", DD(a2, b2)))
cat("  (前者 <= 後者 でないと「形の距離」として機能していない)\n")

cat("\n=== 4. distribution_distance: 対称性 d(x,y)==d(y,x) ===\n")
set.seed(1); bad <- 0
for (i in 1:200) {
  x <- mk(sort(runif(sample(2:12,1)))); y <- mk(sort(runif(sample(2:12,1))))
  if (abs(DD(x,y) - DD(y,x)) > 1e-12) bad <- bad + 1
}
cat(sprintf("  d(x,y) != d(y,x) となった件数: %d / 200\n", bad))

cat("\n=== 5. distribution_distance: 自己距離 d(x,x)==0 ===\n")
set.seed(2); bad0 <- 0
for (i in 1:200) {
  x <- mk(sort(runif(sample(2:12,1))))
  if (abs(DD(x,x)) > 1e-12) bad0 <- bad0 + 1
}
cat(sprintf("  d(x,x) != 0 となった件数: %d / 200\n", bad0))
