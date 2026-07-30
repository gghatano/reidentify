suppressPackageStartupMessages({library(reidentify); library(dplyr); library(magrittr)})
set.seed(71)
dat <- create_dummy_transaction_data(people = 30, size = 4) %>%
  dplyr::mutate(NUM_STATIC_2 = NUM_STATIC+1, NUM_DYNAMIC_2 = NUM_DYNAMIC+1, CHAR_STATIC = paste("CHAR", ID, sep=""))
m <- suppressWarnings(transform_transaction_to_master(dat, ROW_NUMBER="ROW_NUMBER",
  STATIC_NUM=c("NUM_STATIC","NUM_STATIC_2"), DYNAMIC_NUM=c("BIN","NUM_DYNAMIC","NUM_DYNAMIC_2"),
  STATIC_CHAR=c("CHAR_STATIC"), DYNAMIC_CHAR=c("CHAR")))
d <- join_raw_anon_data(m, m)
cat("ANON 実件数 =", nrow(m), " (完全コピーなので真の再識別率は 100%)\n\n")
cat("D の報告値 :", reid_result(reid_by_dist(d, "BIN_DIST"), method="dist/BIN"), "\n")
cat("E の報告値 :", reid_result(suppressWarnings(reid_by_dist(d, "CHAR_DIST")), method="dist/CHAR"), "\n")
g <- d %>% dplyr::mutate(raw_target = -999, anon_target = 999)
cat("G の報告値 :", reid_result(reid_by_num(g, "NUM_DYNAMIC_MEAN"), method="num(衝突列あり)"), "\n")
cat("参考(正常):", reid_result(reid_by_num(d, "NUM_DYNAMIC_MEAN"), method="num"), "\n")
