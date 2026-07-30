## 敵対的検証 2: 「再識別器として正しいか」の性質テスト
## 実装が通ることと、主張(reid-attack success rate の推定)が成立することは別

suppressPackageStartupMessages({
  library(reidentify); library(dplyr); library(magrittr)
})

N <- 30

make_master <- function(seed = 71, n = N) {
  set.seed(seed)
  dat <- create_dummy_transaction_data(people = n, size = 4) %>%
    dplyr::mutate(NUM_STATIC_2 = NUM_STATIC + 1,
                  NUM_DYNAMIC_2 = NUM_DYNAMIC + 1,
                  CHAR_STATIC = paste("CHAR", ID, sep = ""))
  suppressWarnings(transform_transaction_to_master(dat,
    ROW_NUMBER = "ROW_NUMBER",
    STATIC_NUM = c("NUM_STATIC", "NUM_STATIC_2"),
    DYNAMIC_NUM = c("BIN", "NUM_DYNAMIC", "NUM_DYNAMIC_2"),
    STATIC_CHAR = c("CHAR_STATIC"),
    DYNAMIC_CHAR = c("CHAR")))
}

tally <- function(r) {
  if (is.null(r) || !nrow(r)) return(c(success = NA, trial = 0, n_anon = 0))
  c(success = sum(r$RESULT, na.rm = TRUE), trial = nrow(r),
    n_anon = dplyr::n_distinct(r$ANON_ROW_NUMBER))
}

report <- function(label, r, expect) {
  t <- tally(r)
  cat(sprintf("%-38s success=%-5s trial=%-5s uniq_anon=%-5s | 期待: %s\n",
              label, t["success"], t["trial"], t["n_anon"], expect))
}

m <- make_master()
cat(sprintf("master rows = %d (N=%d)\n\n", nrow(m), N))

cat("=== A. 恒等性: ANON = RAW の完全コピー -> 100%% 再識別できねばおかしい ===\n")
dra_id <- join_raw_anon_data(m, m)
report("reid_by_num(NUM_DYNAMIC_MEAN)", reid_by_num(dra_id, "NUM_DYNAMIC_MEAN"), sprintf("success=trial=%d", N))
report("reid_by_char(CHAR_STATIC)", reid_by_char(dra_id, "CHAR_STATIC"), sprintf("success=trial=%d", N))
report("reid_by_num_rank(NUM_DYNAMIC_MEAN)", reid_by_num_rank(dra_id, "NUM_DYNAMIC_MEAN"), sprintf("success=trial=%d", N))
report("reid_by_dist(NUM_DYNAMIC_DIST)", reid_by_dist(dra_id, "NUM_DYNAMIC_DIST"), sprintf("success=trial=%d", N))

cat("\n=== B. 無情報: ANON 側を完全な乱数で置換 -> 成功はベースライン(~1)のはず ===\n")
set.seed(1)
m_rand <- m %>% dplyr::mutate(NUM_DYNAMIC_MEAN = runif(nrow(.)))
dra_rand <- join_raw_anon_data(m, m_rand)
report("reid_by_num(乱数)", reid_by_num(dra_rand, "NUM_DYNAMIC_MEAN"), "success ~ 1")

cat("\n=== C. 定数列(NUM_STATIC=全員10): 情報ゼロ・全件タイ ===\n")
report("reid_by_num(NUM_STATIC)", reid_by_num(dra_id, "NUM_STATIC"), sprintf("trial=%d に収まるか", N))

cat("\n=== D. 離散列 BIN_DIST: 同値が大量 -> タイ処理の有無が出る ===\n")
report("reid_by_dist(BIN_DIST)", reid_by_dist(dra_id, "BIN_DIST"), sprintf("trial=%d を超えないか", N))
report("reid_by_num(BIN_MEAN)", reid_by_num(dra_id, "BIN_MEAN"), sprintf("trial=%d を超えないか", N))

cat("\n=== E. 型の誤用: 文字列の分布列を reid_by_dist に渡す ===\n")
r <- suppressWarnings(tryCatch(reid_by_dist(dra_id, "CHAR_DIST"), error = function(e) {
  cat("  ERROR:", conditionMessage(e), "\n"); NULL }))
report("reid_by_dist(CHAR_DIST=文字列)", r, "エラーか、少なくとも黙って空にならないこと")

cat("\n=== F. row_number 引数は本当に効くか (列名を RECORD_ID にして渡す) ===\n")
m2 <- m %>% dplyr::rename(RECORD_ID = ROW_NUMBER)
dra2 <- join_raw_anon_data(m2, m2)
r <- tryCatch(reid_by_num(dra2, "NUM_DYNAMIC_MEAN", row_number = "RECORD_ID"),
              error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); NULL })
report("reid_by_num(row_number=RECORD_ID)", r, "引数が効くなら成功するはず")

cat("\n=== G. 列名衝突: データに 'raw_target' という列が実在したら ===\n")
dra3 <- dra_id %>% dplyr::mutate(raw_target = -999, anon_target = 999)
r <- tryCatch(reid_by_num(dra3, "NUM_DYNAMIC_MEAN"),
              error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); NULL })
report("reid_by_num(衝突列あり)", r, "A と同じ success=trial=30 になるべき")

cat("\n=== H. 決定性: 同じ入力を2回 ===\n")
a <- reid_by_num_rank(dra_id, "NUM_DYNAMIC_MEAN") %>% tally()
b <- reid_by_num_rank(dra_id, "NUM_DYNAMIC_MEAN") %>% tally()
cat(sprintf("  1回目 success=%s / 2回目 success=%s -> %s\n", a["success"], b["success"],
            if (identical(a, b)) "一致" else "★不一致(非決定的)"))

cat("\n=== I. 計算量: join_raw_anon_data は総当たり(N^2) ===\n")
for (n in c(100, 400, 800)) {
  mm <- make_master(seed = 7, n = n)
  tm <- system.time(d <- join_raw_anon_data(mm, mm))["elapsed"]
  cat(sprintf("  N=%-4d -> %d 行 (%.1f MB), join %.2f 秒\n",
              n, nrow(d), as.numeric(object.size(d)) / 1024^2, tm))
  rm(d); gc(verbose = FALSE)
}
