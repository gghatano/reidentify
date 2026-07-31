## adversarial line 2 -- probe 10: 仮説の反証確認
## run: Rscript docs/adversarial/adv2-10-checks.R
suppressMessages(pkgload::load_all(".", quiet = TRUE))
options(width = 140)
hdr <- function(x) cat("\n==== ", x, " ====\n", sep = "")

hdr("1. 粗視化の非単調性はサンプリング由来か（n_samples を上げて exhaustive にする）")
set.seed(2)
tx <- NULL
for (trial in 1:18) {
  n_people <- sample(6:20, 1); n_ev <- sample(3:8, 1)
  t2 <- do.call(rbind, lapply(seq_len(n_people), function(i) {
    data.frame(ID = i, PLACE = sprintf("P%03d", sample(1:12, n_ev, TRUE)),
               TIME = sample(0:23, n_ev, TRUE), stringsAsFactors = FALSE)
  }))
  if (trial == 18) tx <- t2
}
for (ns in c(20, 1000)) {
  r <- spatiotemporal_unicity(tx, k = 2, space_resolution = c(1, 2, 4, 12),
                              n_samples = ns, seed = 1)
  r <- r[order(r$space_resolution), ]
  cat("n_samples = ", ns, "\n", sep = "")
  print(r[, c("space_resolution", "n_evaluated", "exhaustive", "unicity")])
  cat("  単調非増加? ", all(diff(r$unicity) <= 1e-12), "\n")
}
cat("\nシードを変えたときの unicity のばらつき（n_samples=20, space_resolution=1）:\n")
v <- vapply(1:30, function(sd_) spatiotemporal_unicity(
  tx, k = 2, space_resolution = 1, n_samples = 20, seed = sd_)$unicity, numeric(1))
cat(sprintf("  mean %.4f  sd %.4f  range [%.4f, %.4f]  <- 返り値に sd 列は無い\n",
            mean(v), sd(v), min(v), max(v)))

hdr("2. 浮動小数点の同点崩れ: 過小報告と過大報告の内訳")
mk <- function(v_raw, v_anon) score_num(join_raw_anon_data(
  data.frame(ROW_NUMBER = seq_along(v_raw), V = v_raw),
  data.frame(ROW_NUMBER = seq_along(v_anon), V = v_anon)), "V")
set.seed(11)
n <- 200
base <- sample(seq(1000, 99999), n); delta <- sample(c(3, 5, 7, 11), n, TRUE)
raw_i <- c(base, base + 2 * delta); an_i <- base + delta
ei <- reid_evaluate(mk(raw_i, an_i), seeds = 1:20, top_k = 1)
ed <- reid_evaluate(mk(raw_i / 10, an_i / 10), seeds = 1:20, top_k = 1)
pi_ <- ei$per_record[order(ei$per_record$ANON_ROW_NUMBER), ]
pd_ <- ed$per_record[order(ed$per_record$ANON_ROW_NUMBER), ]
tab <- table(exact = pi_$RISK, computed = pd_$RISK)
print(tab)
cat(sprintf("厳密には 0.5 なのに 0 と報告（過小）: %d 件\n",
            sum(pi_$RISK == 0.5 & pd_$RISK == 0)))
cat(sprintf("厳密には 0.5 なのに 1 と報告（過大）: %d 件\n",
            sum(pi_$RISK == 0.5 & pd_$RISK == 1)))
cat(sprintf("max per-record risk: 厳密 %.2f -> 実装 %.2f\n", ei$max_risk, ed$max_risk))
cat(sprintf("PR 表の行数: 厳密 %d -> 実装 %d （余分な閾値はすべて margin ~1e-14 の雑音）\n",
            nrow(ei$precision_recall), nrow(ed$precision_recall)))
mg <- reid_confidence(mk(raw_i / 10, an_i / 10))$MARGIN
cat("1/10 表現の MARGIN のうち 1e-9 未満のもの: ", sum(mg < 1e-9), " / ", length(mg), "\n", sep = "")
cat("  その最大値 = ", max(mg[mg < 1e-9]), "\n", sep = "")

hdr("3. unicity_fraction のキー衝突: 現実的な引き金の確認")
cases <- list(
  "CR を含む文字列" = data.frame(A = c("x", "x\ry"), B = c("y\rz", "z"), stringsAsFactors = FALSE),
  "NA と文字列 NA"  = data.frame(A = c(NA, "NA"), B = c("z", "z"), stringsAsFactors = FALSE),
  "数値の 15 桁丸め" = data.frame(V = c(1, 1 + 1e-16), W = c(1, 1)),
  "数値 1 と文字 '1'" = data.frame(A = list(c(1, 1))[[1]], B = c("1", "1.0"), stringsAsFactors = FALSE)
)
for (nm in names(cases)) {
  d <- cases[[nm]]
  cols <- names(d)
  cat(sprintf("%-18s : 1 列 %.2f -> 全 %d 列 %.2f  (正しくは 1.00)\n",
              nm, unicity_fraction(d, cols[1]), length(cols), unicity_fraction(d, cols)))
}
cat("\nRAW にこの衝突があると unicity_fraction は『一意でない』側に外れる = 安全に見える方向\n")

hdr("4. README のコード例が主張する combine_scores の『正規化して加重和』")
raw <- data.frame(ROW_NUMBER = 1:5, A = 1:5, B = (1:5) * 1000)
d <- join_raw_anon_data(raw, raw)
sa <- score_num(d, "A"); sb <- score_num(d, "B"); cb <- combine_scores(list(sa, sb))
cat("A range ", paste(range(sa$SCORE), collapse = "-"),
    " / B range ", paste(range(sb$SCORE), collapse = "-"),
    " / combined range ", paste(range(cb$SCORE), collapse = "-"), "\n")
cat("combined == A + B（正規化なし）? ", isTRUE(all.equal(cb$SCORE, sa$SCORE + sb$SCORE)), "\n")
cat("A が結果に効いているか: A だけ入れ替えても割当が変わらないことを確認\n")
raw2 <- raw; raw2$A <- rev(raw2$A)
d2 <- join_raw_anon_data(raw2, raw)
cb2 <- combine_scores(list(score_num(d2, "A"), score_num(d2, "B")))
cat("  A を逆順にしても match_greedy の結果は同一? ",
    identical(match_greedy(cb, seed = 1)$RAW_ROW_NUMBER,
              match_greedy(cb2, seed = 1)$RAW_ROW_NUMBER), "\n")
