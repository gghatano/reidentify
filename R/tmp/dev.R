library(reidentify)
library(dplyr)
library(magrittr)

set.seed(71)
dat_anon = create_dummy_transaction_data(100)
dat_raw = dat_anon %>%
  dplyr::mutate(ID = ID + 1) %>%
  dplyr::mutate(NUM_DYNAMIC = NUM_DYNAMIC + runif(nrow(.)) *0.1 )

dat_anon %>% head
dat_raw %>% head

## transform transaction to master test
dat = create_dummy_transaction_data(100)
dat %<>% dplyr::mutate(NUM_STATIC_2 = NUM_STATIC + 1)
dat %<>% dplyr::mutate(NUM_DYNAMIC_2 = NUM_DYNAMIC + 1)
dat %<>% dplyr::mutate(CHAR_STATIC = paste("CHAR", ID, sep=""))
dat %>% dim
dat$ID %>% table %>% length()
dat_master = transform_transaction_to_master(dat,
                                             ROW_NUMBER = "ROW_NUMBER",
                                             STATIC_NUM = c("NUM_STATIC", "NUM_STATIC_2"),
                                             DYNAMIC_NUM = c("BIN", "NUM_DYNAMIC", "NUM_DYNAMIC_2"),
                                             STATIC_CHAR = c("CHAR_STATIC"),
                                             DYNAMIC_CHAR = c("CHAR"))
dat_master %>% arrange(ID)
dat_master %>% dim
dat_master$ID %>% table %>% length
dat_master %>% arrange(ID) %>% dplyr::select(ID, "CHAR_DIST") %>% distinct %>% dim

dat_master %>% dim
dat_master %>% as.data.frame %>% head
dat_master %>% write.csv("dat_master.csv", quote = FALSE, row.names = FALSE)

#system("head dat_master.csv")
#system("python3 ./pandas_profiling.py")

dat_master$ROW_NUMBER %>% table
dat2_master = dat_master %>%
  mutate(NUM_DYNAMIC_MEAN = NUM_DYNAMIC_MEAN + runif(nrow(.)) * 0.1) %>%
  mutate(NUM_STATIC_2 = NUM_STATIC_2 + runif(nrow(.)) * 1) %>%
  mutate(ID = paste("ID_", ID, sep=""))

dat_raw_anon = join_row_anon_data(dat_master, dat2_master)

## 分布間の距離
# result_dist = reid_by_dist(dat_raw_anon = dat_raw_anon, target = "NUM_DYNAMIC_DIST")
# reid_result(dat_reid_result = result_dist, method = "dist") %>% print

## 文字列の一致度合い
result_char = reid_by_char(dat_raw_anon = dat_raw_anon, target = "CHAR_STATIC")
reid_result(dat_reid_result = result_char, method = "CHAR") %>% print

## 順位でマッチング
result_num_rank = reid_by_num_rank(dat_raw_anon = dat_raw_anon, target = "ID")
result_num_rank %>% reid_result(method = "rank of num static max") %>% print
result_num_rank$ANON_RANK %>% table

## 数値の距離
result_num = reid_by_num(dat_raw_anon = dat_raw_anon, target = "NUM_DYNAMIC_MEAN")
result_num
reid_result(dat_reid_result = result_num, method = "NUM_DYNAMIC_MEAN") %>% print
