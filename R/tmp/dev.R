library(reidentify)
library(dplyr)

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
dat_master = transform_transaction_to_master(dat, ROW_NUMBER = "ROW_NUMBER", STATIC_NUM = c("NUM_STATIC", "NUM_STATIC_2"), DYNAMIC_NUM = c("NUM_DYNAMIC", "NUM_DYNAMIC_2"))
dat_master %>% dim
dat_master %>% as.data.frame %>% head
dat_master %>% write.csv("dat_master.csv", quote = FALSE, row.names = FALSE)
#system("head dat_master.csv")
#system("python3 ./pandas_profiling.py")

