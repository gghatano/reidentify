## adversarial line 2 -- probe 4: 単調性（解像度・属性数・k）と決定性
## run: Rscript docs/adversarial/adv2-04-monotone.R
suppressMessages(pkgload::load_all(".", quiet = TRUE))
options(width = 140)
hdr <- function(x) cat("\n==== ", x, " ====\n", sep = "")

## ---------------------------------------------------------------------------
hdr("1. spatiotemporal_unicity: 時間解像度を粗くすると unicity は単調非増加か")
## 数学的根拠: 粗視化は点をまとめるので、任意の k 点集合の匿名集合は
## 細かい側の対応する集合を包含する。したがって「一意に決まる人」は増えない。
## ただし n_evaluated の母集団が解像度ごとに変わるなら、この論法は破れる。
set.seed(1)
found <- 0
for (trial in 1:40) {
  n_people <- sample(6:20, 1)
  n_ev <- sample(3:8, 1)
  tx <- do.call(rbind, lapply(seq_len(n_people), function(i) {
    data.frame(ID = i,
               PLACE = sprintf("P%03d", sample(1:6, n_ev, replace = TRUE)),
               TIME = sample(0:47, n_ev, replace = TRUE),
               stringsAsFactors = FALSE)
  }))
  r <- tryCatch(
    spatiotemporal_unicity(tx, k = 2, time_resolution = c(1, 6, 24, 48), seed = 1),
    error = function(e) NULL)
  if (is.null(r)) next
  r <- r[order(r$time_resolution), ]
  if (any(is.na(r$unicity))) next
  if (any(diff(r$unicity) > 1e-12)) {
    found <- found + 1
    if (found <= 3) {
      cat("--- 反例 trial ", trial, " (k=2) ---\n", sep = "")
      print(r[, c("k", "time_resolution", "n_evaluated", "n_points", "unicity",
                  "expected_id_rate", "mean_anonymity_set")])
    }
  }
}
cat("時間粗視化で unicity が上がった fixture: ", found, " / 40\n", sep = "")

hdr("2. 同じことを空間解像度で")
set.seed(2)
found2 <- 0
for (trial in 1:40) {
  n_people <- sample(6:20, 1); n_ev <- sample(3:8, 1)
  tx <- do.call(rbind, lapply(seq_len(n_people), function(i) {
    data.frame(ID = i,
               PLACE = sprintf("P%03d", sample(1:12, n_ev, replace = TRUE)),
               TIME = sample(0:23, n_ev, replace = TRUE),
               stringsAsFactors = FALSE)
  }))
  r <- tryCatch(
    spatiotemporal_unicity(tx, k = 2, space_resolution = c(1, 2, 4, 12), seed = 1),
    error = function(e) NULL)
  if (is.null(r)) next
  r <- r[order(r$space_resolution), ]
  if (any(is.na(r$unicity))) next
  if (any(diff(r$unicity) > 1e-12)) {
    found2 <- found2 + 1
    if (found2 <= 3) {
      cat("--- 反例 trial ", trial, " (k=2) ---\n", sep = "")
      print(r[, c("k", "space_resolution", "n_evaluated", "n_points", "unicity",
                  "expected_id_rate", "mean_anonymity_set")])
    }
  }
}
cat("空間粗視化で unicity が上がった fixture: ", found2, " / 40\n", sep = "")

hdr("3. spatiotemporal_unicity: k を増やすと unicity は単調非減少か")
## 数学的根拠: k+1 点を知る攻撃者は k 点しか知らない攻撃者より必ず強い。
set.seed(3)
found3 <- 0
for (trial in 1:40) {
  n_people <- sample(6:20, 1); n_ev <- sample(4:9, 1)
  tx <- do.call(rbind, lapply(seq_len(n_people), function(i) {
    data.frame(ID = i,
               PLACE = sprintf("P%03d", sample(1:8, n_ev, replace = TRUE)),
               TIME = sample(0:23, n_ev, replace = TRUE),
               stringsAsFactors = FALSE)
  }))
  r <- tryCatch(spatiotemporal_unicity(tx, k = 1:4, seed = 1), error = function(e) NULL)
  if (is.null(r) || any(is.na(r$unicity))) next
  r <- r[order(r$k), ]
  if (any(diff(r$unicity) < -1e-12)) {
    found3 <- found3 + 1
    if (found3 <= 3) {
      cat("--- 反例 trial ", trial, " ---\n", sep = "")
      print(r[, c("k", "n_evaluated", "unicity", "expected_id_rate", "mean_anonymity_set")])
    }
  }
}
cat("k を増やして unicity が下がった fixture: ", found3, " / 40\n", sep = "")

## ---------------------------------------------------------------------------
hdr("4. unicity_fraction: 属性追加の単調性（区切り文字なしの通常データ）")
set.seed(4)
bad <- 0
for (trial in 1:200) {
  n <- sample(4:25, 1)
  dd <- data.frame(
    A = sample(1:4, n, TRUE), B = sample(letters[1:3], n, TRUE),
    C = sample(c(TRUE, FALSE), n, TRUE), stringsAsFactors = FALSE)
  u1 <- unicity_fraction(dd, "A"); u2 <- unicity_fraction(dd, c("A", "B"))
  u3 <- unicity_fraction(dd, c("A", "B", "C"))
  if (u2 < u1 - 1e-12 || u3 < u2 - 1e-12) bad <- bad + 1
}
cat("通常データで単調性が破れた回数: ", bad, " / 200\n", sep = "")

## ---------------------------------------------------------------------------
hdr("5. 行順・シードに対する決定性")
set.seed(6)
n <- 60
raw <- data.frame(ROW_NUMBER = 1:n, V = sample(1:8, n, TRUE))
anon <- data.frame(ROW_NUMBER = 1:n, V = sample(1:8, n, TRUE))
s <- score_num(join_raw_anon_data(raw, anon), "V")
m1 <- match_greedy(s, seed = 42)
perm <- sample(nrow(s))
s_perm <- s[perm, , drop = FALSE]; attr(s_perm, "score_type") <- "distance"
m2 <- match_greedy(s_perm, seed = 42)
cat("行順を入れ替えても match_greedy が同一か: ",
    identical(m1$RAW_ROW_NUMBER, m2$RAW_ROW_NUMBER), "\n")
e1 <- reid_evaluate(s, seeds = 1:10, top_k = 1)
e2 <- reid_evaluate(s_perm, seeds = 1:10, top_k = 1)
cat("reid_evaluate success_analytic 一致: ",
    isTRUE(all.equal(e1$success_analytic, e2$success_analytic)),
    " / success_mean 一致: ", isTRUE(all.equal(e1$success_mean, e2$success_mean)), "\n")
o1 <- match_optimal(s, seed = 42); o2 <- match_optimal(s_perm, seed = 42)
cat("match_optimal 行順不変: ", identical(o1$RAW_ROW_NUMBER, o2$RAW_ROW_NUMBER), "\n")

cat("\n-- 同一呼び出しの再現性（10 回）--\n")
r <- replicate(10, paste(match_greedy(s, seed = 7)$RAW_ROW_NUMBER, collapse = ","))
cat("match_greedy(seed=7) 10 回すべて同一: ", length(unique(r)) == 1, "\n")
r2 <- replicate(5, reid_evaluate(s, seeds = 1:5, top_k = 1)$success_mean)
cat("reid_evaluate(seeds=1:5) 5 回すべて同一: ", length(unique(r2)) == 1, "\n")
set.seed(999); a <- match_greedy(s, seed = 7)$RAW_ROW_NUMBER
set.seed(1);   b <- match_greedy(s, seed = 7)$RAW_ROW_NUMBER
cat("外側の set.seed に影響されない: ", identical(a, b), "\n")

## ---------------------------------------------------------------------------
hdr("6. reid_evaluate: 候補を減らすと成功率は下がるか（ブロッキングの下界性）")
## README:「候補ペアを捨てるので、正解ペアを落とせばそのレコードは永久に特定できなくなり、
##          成功率は下がります」
set.seed(8)
worse <- 0; better <- 0
for (trial in 1:20) {
  n <- 40
  rr <- data.frame(ROW_NUMBER = 1:n, V = rnorm(n) * 20)
  aa <- data.frame(ROW_NUMBER = 1:n, V = rr$V + rnorm(n, sd = 4))
  s <- score_num(join_raw_anon_data(rr, aa), "V")
  full <- reid_evaluate(s, seeds = 1:10, top_k = 1)$success_analytic
  tk <- suppressWarnings(top_k_candidates(s, k = 5))
  bl <- reid_evaluate(tk, seeds = 1:10, top_k = 1)$success_analytic
  if (bl > full + 1e-12) better <- better + 1
  if (bl < full - 1e-12) worse <- worse + 1
}
cat("top_k_candidates(k=5) で成功率が上がった: ", better, " / 20  下がった: ", worse, "\n", sep = "")
