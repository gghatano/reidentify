## Issue #1: 現行実装の動作確認とバグ再現記録
##
## docs/reid-method-candidates.md §2.1 で挙げた 5 件の指摘が実際に再現するかを
## 実行して確かめるための調査スクリプト。R/ は一切変更しない。
##
## 実行方法:
##   Rscript docs/investigation/baseline-probe.R
##
## 注意: このスクリプトは「素の環境（dplyr/magrittr を attach しない）」と
## 「dev.R 相当の環境（attach する）」の両方を試す。#6 の import 漏れは
## attach の有無で挙動が変わるため、両方を記録しないと判定できない。

options(warn = 1) # 警告を発生箇所で即表示する

banner <- function(x) {
  cat("\n\n========================================\n")
  cat("== ", x, "\n", sep = "")
  cat("========================================\n")
}

hdr <- function(x) cat("\n---- ", x, " ----\n", sep = "")

## 例外・警告を捕まえて中身をそのまま出す
probe <- function(label, expr, show = TRUE) {
  hdr(label)
  warns <- character(0)
  val <- withCallingHandlers(
    tryCatch(force(expr), error = function(e) {
      cat("!! ERROR: ", conditionMessage(e), "\n", sep = "")
      structure(list(msg = conditionMessage(e)), class = "probe_error")
    }),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  if (length(warns)) {
    for (w in warns) cat("!! WARNING: ", w, "\n", sep = "")
  }
  if (show && !inherits(val, "probe_error")) {
    cat("-> VALUE:\n")
    if (is.data.frame(val)) {
      cat("   class:", paste(class(val), collapse = "/"),
        " dim:", paste(dim(val), collapse = "x"), "\n")
      cat("   names:", paste(names(val), collapse = ", "), "\n")
      print(utils::head(as.data.frame(val), 3))
    } else {
      print(val)
    }
  }
  invisible(val)
}

banner("A. 環境")
cat(R.version.string, "\n")
for (p in c("dplyr", "magrittr", "openssl", "stringi", "tibble", "philentropy", "testthat", "roxygen2", "pkgload")) {
  cat(sprintf("  %-12s %s\n", p, tryCatch(as.character(packageVersion(p)), error = function(e) "NOT INSTALLED")))
}
cat("devtools: ", tryCatch(as.character(packageVersion("devtools")), error = function(e) "NOT INSTALLED"), "\n")

## ---------------------------------------------------------------------------
banner("B. ソースからのパッケージ読み込み")
## installed 版ではなく作業ツリーのソースを読む
suppressMessages(pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE))
cat("load_all OK. exported:", paste(sort(getNamespaceExports("reidentify")), collapse = ", "), "\n")

## ---------------------------------------------------------------------------
banner("C. #6 NAMESPACE import 漏れ — dplyr/magrittr を attach しない状態")
cat("search():", paste(search(), collapse = " "), "\n")
cat("NAMESPACE の imports に含まれるか:\n")
imps <- parseNamespaceFile("reidentify", dirname(getwd()))
ns_imports <- unlist(lapply(imps$imports, function(x) if (length(x) > 1) x[[2]] else NA))
for (f in c("pull", "%<>%", "n", "select", "arrange", "all_of")) {
  cat(sprintf("  %-8s : %s\n", f, if (f %in% ns_imports) "imported" else "NOT imported"))
}
cat("  median   : ", if ("median" %in% ns_imports) "imported" else "NOT imported (stats)", "\n", sep = "")

set.seed(71)
dat_master_raw <- create_dummy_master_data(people = 50)

probe("reid_result() -- pull() を使用 (attach 無し)", {
  d <- data.frame(RAW_ROW_NUMBER = 1:3, ANON_ROW_NUMBER = 1:3, RESULT = c(TRUE, FALSE, TRUE))
  reid_result(d, method = "probe")
})

probe("reid_by_char() -- pull() を使用 (attach 無し)", {
  a <- dat_master_raw
  b <- dat_master_raw
  ra <- join_raw_anon_data(a, b)
  reid_by_char(ra, target = "CHAR")
})

probe("reid_by_num_rank() -- %<>% を使用 (attach 無し)", {
  a <- dat_master_raw
  b <- dat_master_raw
  ra <- join_raw_anon_data(a, b)
  reid_by_num_rank(ra, target = "NUM")
})

probe("transform_transaction_to_master() -- n(), median を使用 (attach 無し)", {
  tr <- create_dummy_transaction_data(people = 20, size = 3)
  transform_transaction_to_master(tr,
    ROW_NUMBER = "ROW_NUMBER", ID = "ID",
    STATIC_NUM = c("NUM_STATIC"), STATIC_CHAR = NULL,
    DYNAMIC_NUM = c("NUM_DYNAMIC"), DYNAMIC_CHAR = c("CHAR")
  )
})

## ---------------------------------------------------------------------------
banner("D. dplyr / magrittr を attach した状態 (dev.R 相当) での再実行")
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
cat("search():", paste(search(), collapse = " "), "\n")

probe("reid_result() (attach 有り)", {
  d <- data.frame(RAW_ROW_NUMBER = 1:3, ANON_ROW_NUMBER = 1:3, RESULT = c(TRUE, FALSE, TRUE))
  reid_result(d, method = "probe")
})

## ---------------------------------------------------------------------------
banner("E. 評価用データの作成 (master 形式)")
set.seed(71)
people <- 50
dat_raw <- create_dummy_master_data(people = people)
cat("create_dummy_master_data の返り値クラス:", paste(class(dat_raw), collapse = "/"), "\n")
print(head(as.data.frame(dat_raw), 3))

## ANON = RAW に小さなノイズを乗せたもの。ROW_NUMBER / ID は保持する
dat_anon <- dat_raw
dat_anon$NUM <- dat_anon$NUM + runif(people) * 0.02
dat_raw_anon <- join_raw_anon_data(dat_raw, dat_anon)
cat("cross join 後の次元:", paste(dim(dat_raw_anon), collapse = " x "), "\n")
cat("列名:", paste(names(dat_raw_anon), collapse = ", "), "\n")

## ---------------------------------------------------------------------------
banner("F. 各 reid_by_* の実行 (attach 有り)")

r_num <- probe("reid_by_num(target = 'NUM')", reid_by_num(dat_raw_anon, target = "NUM"))
if (!inherits(r_num, "probe_error")) {
  cat("   rows:", nrow(r_num), " 成功:", sum(r_num$RESULT), "\n")
  cat("   ", reid_result(r_num, method = "num"), "\n")
  cat("   選択された列:", paste(names(r_num), collapse = ", "), "\n")
}

r_rank <- probe("reid_by_num_rank(target = 'NUM')", reid_by_num_rank(dat_raw_anon, target = "NUM"))
if (!inherits(r_rank, "probe_error")) {
  cat("   rows:", nrow(r_rank), " 成功:", sum(r_rank$RESULT), "\n")
  cat("   選択された列:", paste(names(r_rank), collapse = ", "), "\n")
}

r_char <- probe("reid_by_char(target = 'CHAR')", reid_by_char(dat_raw_anon, target = "CHAR"))
if (!inherits(r_char, "probe_error")) {
  cat("   rows:", nrow(r_char), " 成功:", sum(r_char$RESULT), "\n")
}

## 分布列を作るために transaction -> master を通す
banner("G. transaction -> master と reid_by_dist")
set.seed(71)
tr <- create_dummy_transaction_data(people = 30, size = 5)
m <- probe("transform_transaction_to_master()", {
  transform_transaction_to_master(tr,
    ROW_NUMBER = "ROW_NUMBER", ID = "ID",
    STATIC_NUM = c("NUM_STATIC"), STATIC_CHAR = NULL,
    DYNAMIC_NUM = c("NUM_DYNAMIC"), DYNAMIC_CHAR = c("CHAR")
  )
})
if (!inherits(m, "probe_error")) {
  cat("   dim:", paste(dim(m), collapse = " x "), "\n")
  cat("   列名:", paste(names(m), collapse = ", "), "\n")
  cat("   期待した列 (ID, NUM_STATIC, *_MAX/_MEAN/_MEDIAN/_MIN, *_DIST, ROWCOUNT, ROW_NUMBER) と一致するか要確認\n")
  print(head(as.data.frame(m), 3))
}

probe("reid_by_dist(target = 'NUM_DYNAMIC_DIST')", {
  if (inherits(m, "probe_error")) stop("master 形式が作れないため実行不能")
  m2 <- m
  ra <- join_raw_anon_data(m, m2)
  reid_by_dist(ra, target = "NUM_DYNAMIC_DIST")
})

## ---------------------------------------------------------------------------
banner("H. 指摘 1 (#2) tidyeval — 列名変数がどう解決されるか")
hdr("dplyr::select(RAW = `raw_target`) が env の変数を見に行くか")
f <- function(dat, target) {
  raw_target <- paste("RAW_", target, sep = "")
  dplyr::select(dat, RAW = `raw_target`)
}
probe("最小再現: select(RAW = `raw_target`)", f(dat_raw_anon, "NUM"))

hdr("列名そのものが 'raw_target' の場合に何が起きるか（変数より列が優先されるはず）")
d_trap <- dat_raw_anon
d_trap$raw_target <- -999
probe("同名列が存在する場合", f(d_trap, "NUM"))

hdr("reid_by_num の実際の出力: RAW 列は RAW_NUM か、それとも罠列か")
probe("罠列付きデータで reid_by_num", reid_by_num(d_trap, target = "NUM"))

hdr("transform_transaction_to_master の select(`ID`, `STATIC_NUM`, `STATIC_CHAR`)")
probe("STATIC_CHAR = NULL のとき NULL 変数を select できるか", {
  g <- function(dat) {
    ID <- "ID"
    STATIC_NUM <- "NUM_STATIC"
    STATIC_CHAR <- NULL
    dplyr::select(dat, `ID`, `STATIC_NUM`, `STATIC_CHAR`)
  }
  g(tr)
})

## ---------------------------------------------------------------------------
banner("I. 指摘 2 (#3) タイブレークの行順依存")
hdr("RAW_ROW_NUMBER == RAW_ROW_NUMBER[1] の挙動")
probe("同点が多発するデータ (BIN 列: 値が 3 種しかない) で reid_by_num", {
  rr <- reid_by_num(dat_raw_anon, target = "BIN")
  cat("   rows:", nrow(rr), " 成功:", sum(rr$RESULT), "/", nrow(rr), "\n")
  cat("   成功率:", round(mean(rr$RESULT), 4), "\n")
  rr
})

hdr("join_raw_anon_data (merge) の行順はどうなっているか")
cat("   先頭 12 行の (RAW_ROW_NUMBER, ANON_ROW_NUMBER):\n")
print(head(dat_raw_anon[, c("RAW_ROW_NUMBER", "ANON_ROW_NUMBER")], 12))
g1 <- dat_raw_anon[dat_raw_anon$ANON_ROW_NUMBER == 1, "RAW_ROW_NUMBER"]
cat("   ANON_ROW_NUMBER==1 の群内の RAW_ROW_NUMBER 並び (先頭10):", head(g1, 10), "\n")
cat("   その群が昇順に並んでいるか:", !is.unsorted(g1), "\n")

hdr("同点集合の大きさ（BIN 列）")
tie_sizes <- tapply(
  abs(dat_raw_anon$RAW_BIN - dat_raw_anon$ANON_BIN),
  dat_raw_anon$ANON_ROW_NUMBER, function(v) sum(v == min(v))
)
cat("   ANON 1 件あたりの最小距離同点数: min =", min(tie_sizes),
  " median =", median(tie_sizes), " max =", max(tie_sizes), "\n")
cat("   1/同点数 の平均（＝ランダムタイブレークの理論成功率）:", round(mean(1 / tie_sizes), 4), "\n")

hdr("行順を無作為に並べ替えると結果が変わるか（変われば行順依存＝再現）")
set.seed(1)
N <- 200
rates <- sapply(1:N, function(i) {
  d <- dat_raw_anon[sample(nrow(dat_raw_anon)), ]
  mean(reid_by_num(d, target = "BIN")$RESULT)
})
orig_rate <- mean(reid_by_num(dat_raw_anon, target = "BIN")$RESULT)
cat("   元の行順（merge 出力そのまま）での成功率:", round(orig_rate, 4), "\n")
cat("   行順シャッフル", N, "回: 平均 =", round(mean(rates), 4),
  " sd =", round(sd(rates), 4),
  " 範囲 = [", round(min(rates), 4), ",", round(max(rates), 4), "]\n")
cat("   結果が行順で変動するか（＝再現の可否）:", (sd(rates) > 0), "\n")
cat("   元の行順が上位何%点か:", round(mean(rates <= orig_rate) * 100, 1), "%\n")

hdr("RAW_ROW_NUMBER 昇順に並べた場合（＝タイブレークが常に最小 RAW を選ぶ）")
d_asc <- dat_raw_anon[order(dat_raw_anon$ANON_ROW_NUMBER, dat_raw_anon$RAW_ROW_NUMBER), ]
d_desc <- dat_raw_anon[order(dat_raw_anon$ANON_ROW_NUMBER, -dat_raw_anon$RAW_ROW_NUMBER), ]
cat("   RAW 昇順:", round(mean(reid_by_num(d_asc, target = "BIN")$RESULT), 4), "\n")
cat("   RAW 降順:", round(mean(reid_by_num(d_desc, target = "BIN")$RESULT), 4), "\n")
cat("   → 同一データ・同一手法なのに並べ方だけで成功率が変わるなら行順依存は確定\n")

hdr("ランダムタイブレークだったら成功率はいくつになるか（ベースライン）")
tie_random <- function(d, target, seed) {
  set.seed(seed)
  rt <- paste0("RAW_", target)
  at <- paste0("ANON_", target)
  x <- data.frame(
    RAW_ROW_NUMBER = d$RAW_ROW_NUMBER, ANON_ROW_NUMBER = d$ANON_ROW_NUMBER,
    DISTANCE = abs(d[[rt]] - d[[at]])
  )
  x <- x[order(x$ANON_ROW_NUMBER, x$DISTANCE, runif(nrow(x))), ]
  x <- x[!duplicated(x$ANON_ROW_NUMBER), ]
  mean(x$RAW_ROW_NUMBER == x$ANON_ROW_NUMBER)
}
rr <- sapply(1:N, function(s) tie_random(dat_raw_anon, "BIN", s))
cat("   ランダムタイブレーク", N, "シード: 平均 =", round(mean(rr), 4), " sd =", round(sd(rr), 4), "\n")
cat("   ★ バイアス = 元の行順 - ランダム平均 =", round(orig_rate - mean(rr), 4), "\n")
cat("   ★ バイアス = RAW昇順  - ランダム平均 =",
  round(mean(reid_by_num(d_asc, target = "BIN")$RESULT) - mean(rr), 4), "\n")

hdr("people を増やして再測定（人数依存の確認）")
for (pp in c(100, 300)) {
  set.seed(71)
  dr <- create_dummy_master_data(people = pp)
  da <- dr
  ra <- join_raw_anon_data(dr, da)
  o <- mean(reid_by_num(ra, target = "BIN")$RESULT)
  asc <- mean(reid_by_num(ra[order(ra$ANON_ROW_NUMBER, ra$RAW_ROW_NUMBER), ], target = "BIN")$RESULT)
  rnd <- mean(sapply(1:50, function(s) tie_random(ra, "BIN", s)))
  cat(sprintf(
    "   people=%3d  元順=%.4f  RAW昇順=%.4f  ランダム=%.4f  バイアス(元順-乱)=%+.4f\n",
    pp, o, asc, rnd, o - rnd
  ))
}

hdr("ROW_NUMBER と ID の相関（ダミーデータ生成の性質）")
cat("   create_dummy_master_data: ID = ROW_NUMBER + 10000 →完全相関\n")
cat("   cor(ROW_NUMBER, ID) =", cor(dat_raw$ROW_NUMBER, dat_raw$ID), "\n")
cat("   ただし reid_by_* が突合に使うのは ROW_NUMBER のみ（ID は使っていない）点に注意\n")

## ---------------------------------------------------------------------------
banner("J. 指摘 3 (#4) calc_KL の正規化")
x <- "1:2:3:4"
y <- "2:2:2:2"
hdr("calc_KL の実装内部でどう正規化されているか")
xv <- c(1, 2, 3, 4)
yv <- c(2, 2, 2, 2)
cat("   x =", xv, " y =", yv, "\n")
cat("   max 正規化 (現行): x/max(x) =", xv / max(xv), " 合計 =", sum(xv / max(xv)), "\n")
cat("                      y/max(y) =", yv / max(yv), " 合計 =", sum(yv / max(yv)), "\n")
cat("   sum 正規化 (正):   x/sum(x) =", xv / sum(xv), " 合計 =", sum(xv / sum(xv)), "\n")
cat("                      y/sum(y) =", yv / sum(yv), " 合計 =", sum(yv / sum(yv)), "\n")
px <- xv / sum(xv)
py <- yv / sum(yv)
kl_true <- sum(px * log2(px / py)) # philentropy の既定単位は log2
cat("   正しい KL(x||y) の手計算値 (log2) =", kl_true, "\n")
cat("   同 (自然対数) =", sum(px * log(px / py)), "\n")
probe("calc_KL(x, y) の実行", calc_KL(x, y))

hdr("スケール不変性: y を 10 倍しても KL は変わらないはず（確率分布は同じ）")
probe("calc_KL('1:2:3:4', '20:20:20:20')", calc_KL("1:2:3:4", "20:20:20:20"))
cat("   （正しい KL なら上と同じ", kl_true, "になるはず）\n")

hdr("自分自身との KL は 0 であるべき")
probe("calc_KL('1:2:3:4', '1:2:3:4')", calc_KL("1:2:3:4", "1:2:3:4"))

hdr("philentropy::KL に総和 1 でないベクトルを渡すと何が起きるか")
probe("philentropy::KL(rbind(c(.25,.5,.75,1), c(1,1,1,1)))", philentropy::KL(rbind(c(.25, .5, .75, 1), c(1, 1, 1, 1))))

## ---------------------------------------------------------------------------
banner("K. 指摘 4 (#5) distribution_distance の長さ合わせ")
hdr("件数のみ異なり分布形状が同一のペア")
set.seed(7)
base <- sort(runif(200))
mk <- function(v) paste(sort(v), collapse = ":")
a10 <- mk(quantile(base, probs = seq(0, 1, length.out = 10)))
a20 <- mk(quantile(base, probs = seq(0, 1, length.out = 20)))
a40 <- mk(quantile(base, probs = seq(0, 1, length.out = 40)))
cat("   同一分布から 10 点 / 20 点 / 40 点を取ったもの（形状は同じ）\n")
probe("distribution_distance(a10, a10)", distribution_distance(a10, a10))
probe("distribution_distance(a10, a20)", distribution_distance(a10, a20))
probe("distribution_distance(a10, a40)", distribution_distance(a10, a40))
cat("   形状が同じなら距離は 0 付近であるべき。長さ差で増えるなら指摘は再現\n")

hdr("形状が違うが件数が同じペアと比較する")
b10 <- mk(quantile(sort(runif(200) * 3 + 5), probs = seq(0, 1, length.out = 10)))
probe("distribution_distance(a10, b10) 形状が大きく違う / 件数同じ", distribution_distance(a10, b10))

hdr("パディング側だけ sort されて非パディング側は sort されない件")
probe("distribution_distance('3:1:2', '1:2:3') 未ソート入力", distribution_distance("3:1:2", "1:2:3"))
probe("distribution_distance('1:2:3', '3:1:2') 未ソート入力", distribution_distance("1:2:3", "3:1:2"))
cat("   対称なら同値になるはず\n")

hdr("`.**2` の magrittr パイプが意図どおり動くか")
probe("distribution_distance('1:2', '3:4') = (1-3)^2+(2-4)^2 = 8 のはず", distribution_distance("1:2", "3:4"))

## ---------------------------------------------------------------------------
banner("L. 指摘 5 (#6) NAMESPACE import 漏れ — R CMD check 相当の静的確認")
hdr("R/*.R で使われている非修飾関数のうち NAMESPACE に無いもの")
src_files <- list.files("R", pattern = "[.]R$", full.names = TRUE, recursive = FALSE)
src <- unlist(lapply(src_files, function(p) paste0(basename(p), ":", seq_along(readLines(p, warn = FALSE)), ": ", readLines(p, warn = FALSE))))
src <- src[!grepl("^[^:]+:[0-9]+:\\s*#", src)] # roxygen / コメント行を除く

check_sym <- function(pattern, name) {
  hits <- grep(pattern, src, value = TRUE)
  cat(sprintf(
    "\n  [%s] NAMESPACE: %s / 使用箇所 %d 件\n",
    name, if (name %in% ns_imports) "imported" else "*** NOT imported ***", length(hits)
  ))
  for (h in hits) cat("      ", trimws(h), "\n")
}
check_sym("(^|[^:_.[:alnum:]])pull\\(", "pull")
check_sym("%<>%", "%<>%")
check_sym("(^|[^:_.[:alnum:]])n\\(\\)", "n")
check_sym("(^|[^:_.[:alnum:]])median(\\b|,|\\))", "median")

## ---------------------------------------------------------------------------
banner("M. 指摘外: tibble::data_frame() の非推奨 (#9)")
probe("create_dummy_master_data(people = 5)", create_dummy_master_data(people = 5))
probe("tibble::data_frame(a = 1) 直接呼び出し", tibble::data_frame(a = 1))

## ---------------------------------------------------------------------------
banner("O. 追加調査（実行して初めて分かった事象）")

hdr("O-1. distribution_distance: 同じ多重集合を順序違いで渡すと 0 にならない")
cat("   '3:1:2' と '1:2:3' は同じ多重集合。距離は 0 であるべき\n")
probe("distribution_distance('3:1:2', '1:2:3')", distribution_distance("3:1:2", "1:2:3"))
cat("   → 長さが等しいと sort が一切走らないため、入力の並び順に依存する\n")
cat("   参考: 長さが違う場合はパディング側だけ sort される（非対称な処理）\n")
probe("distribution_distance('3:1:2', '9:1')", distribution_distance("3:1:2", "9:1"))
probe("distribution_distance('9:1', '3:1:2')", distribution_distance("9:1", "3:1:2"))
cat("   → 上 2 つが一致しなければ距離関数として対称性を満たしていない\n")

hdr("O-2. distribution_distance: 件数差が距離に占める寄与を実データで測る")
set.seed(123)
pop <- runif(500)
mk2 <- function(k) paste(sort(sample(pop, k)), collapse = ":")
res <- data.frame()
for (k in c(10, 11, 13, 16, 20, 30, 50)) {
  d <- distribution_distance(mk2(10), mk2(k))
  res <- rbind(res, data.frame(n_x = 10, n_y = k, len_diff = k - 10, distance = d))
}
print(res)
cat("   → 同一母集団からの標本なので分布形状は同じ。距離が len_diff とともに増えるなら混入は再現\n")
cat("   相関 cor(len_diff, distance) =", round(cor(res$len_diff, res$distance), 4), "\n")

hdr("O-3. calc_KL: 非負性が破れるか（KL は定義上必ず >= 0）")
set.seed(5)
neg <- 0
vals <- numeric(0)
for (i in 1:50) {
  a <- paste(round(runif(6) * 10 + 1, 3), collapse = ":")
  b <- paste(round(runif(6) * 10 + 1, 3), collapse = ":")
  v <- suppressMessages(as.numeric(calc_KL(a, b)))
  vals <- c(vals, v)
  if (!is.na(v) && v < 0) neg <- neg + 1
}
cat("   ランダムな分布ペア 50 組: 負の値が", neg, "件 /50\n")
cat("   値域: [", round(min(vals, na.rm = TRUE), 4), ",", round(max(vals, na.rm = TRUE), 4), "]\n")
cat("   → 1 件でも負なら KL ダイバージェンスの定義（非負性）を満たしていない\n")

hdr("O-4. calc_KL: max 正規化と sum 正規化で候補の順位が変わるか（再識別への実害）")
kl_sum <- function(a, b) {
  av <- as.numeric(strsplit(a, ":")[[1]])
  bv <- as.numeric(strsplit(b, ":")[[1]])
  pa <- av / sum(av)
  pb <- bv / sum(bv)
  sum(pa * log2(pa / pb))
}
set.seed(9)
tgt <- paste(round(runif(6) * 10 + 1, 3), collapse = ":")
cands <- replicate(8, paste(round(runif(6) * 10 + 1, 3), collapse = ":"))
cur <- sapply(cands, function(c) suppressMessages(as.numeric(calc_KL(tgt, c))))
tru <- sapply(cands, function(c) kl_sum(tgt, c))
cmp <- data.frame(cand = seq_along(cands), calc_KL_current = round(cur, 4), KL_sum_norm = round(tru, 4),
  rank_current = rank(cur), rank_true = rank(tru))
print(cmp, row.names = FALSE)
cat("   Spearman 相関 =", round(cor(rank(cur), rank(tru)), 4), "\n")
cat("   argmin が一致するか:", which.min(cur) == which.min(tru), "\n")

hdr("O-5. reid_by_dist にはタイブレーク処理が無い（他の reid_by_* と非対称）")
cat("   reid_by_num/char/num_rank: filter(RAW_ROW_NUMBER == RAW_ROW_NUMBER[1]) あり\n")
cat("   reid_by_dist            : 無し → 同点時に ANON 1 件が複数行になりうる\n")
probe("同点を強制した入力で reid_by_dist の行数を確認", {
  n <- 20
  dm <- data.frame(
    ROW_NUMBER = 1:n, ID = 1:n,
    D = rep(c("1:2:3", "4:5:6"), length.out = n), stringsAsFactors = FALSE
  )
  ra <- join_raw_anon_data(dm, dm)
  out <- reid_by_dist(ra, target = "D")
  cat("   ANON 件数 =", n, " / 出力行数 =", nrow(out),
    " / ANON 重複あり =", any(duplicated(out$ANON_ROW_NUMBER)), "\n")
  cat("   reid_result の分母:", reid_result(out, method = "dist"), "\n")
  out
})

hdr("O-6. transform_transaction_to_master の集計列名（DYNAMIC_NUM が 1 列か複数かで変わる）")
set.seed(3)
tr2 <- create_dummy_transaction_data(people = 10, size = 3)
tr2$NUM_DYNAMIC_2 <- runif(nrow(tr2))
probe("DYNAMIC_NUM が 1 列のとき", {
  o <- transform_transaction_to_master(tr2,
    ID = "ID", ROW_NUMBER = "ROW_NUMBER",
    STATIC_NUM = "NUM_STATIC", DYNAMIC_NUM = "NUM_DYNAMIC", DYNAMIC_CHAR = "CHAR"
  )
  cat("   列名:", paste(names(o), collapse = ", "), "\n")
  o
}, show = FALSE)
probe("DYNAMIC_NUM が 2 列のとき", {
  o <- transform_transaction_to_master(tr2,
    ID = "ID", ROW_NUMBER = "ROW_NUMBER",
    STATIC_NUM = "NUM_STATIC", DYNAMIC_NUM = c("NUM_DYNAMIC", "NUM_DYNAMIC_2"), DYNAMIC_CHAR = "CHAR"
  )
  cat("   列名:", paste(names(o), collapse = ", "), "\n")
  o
}, show = FALSE)
cat("   → 1 列のときは MAX/MEAN/MEDIAN/MIN、2 列のときは <col>_MAX ... と命名規則が変わる\n")
cat("   （R/tmp/dev.R は NUM_DYNAMIC_MEAN を前提にしている＝1 列指定では動かない）\n")

hdr("O-7. reid_by_char / reid_by_num の返り値スキーマの不一致")
cat("   reid_by_num      :", paste(names(r_num), collapse = ", "), "\n")
cat("   reid_by_char     :", paste(names(r_char), collapse = ", "), "\n")
cat("   reid_by_num_rank :", paste(names(r_rank), collapse = ", "), "\n")
cat("   → reid_by_char は入力列を全部持ち回るため列構成が入力依存\n")

hdr("O-8. R/tmp/dev.R は現状そのままでは動かない（関数名の誤り）")
cat("   dev.R:56 は join_row_anon_data(...) を呼ぶが、実際の関数名は join_raw_anon_data\n")
cat("   exists('join_row_anon_data') =", exists("join_row_anon_data"), "\n")

banner("P. 終了")
cat("done\n")
