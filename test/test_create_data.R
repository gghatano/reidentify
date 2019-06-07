## テスト用コード

# create_dummy_master_data("hoge")
# create_dummy_master_data(-10)
# create_dummy_transaction_data("hoge")
# create_dummy_transaction_data(-10)
# create_dummy_transaction_data(10, "hoge")
# create_dummy_transaction_data(people = 10, size = "hoge")

library(reidentify)

ROW_NUMBER = 1:people
RAW_ID = ROW_NUMBER + 10000
ANON_ID = stri_rand_strings(n = people, length = 10)
RAW_NUM = runif(n = people)
ANON_NUM = RAW_NUM + runif(n = people) * 0.01
RAW_BIN = sample(x = c(0,1,100), prob = c(20,20,1), size = people, replace = TRUE)
ANON_BIN = RAW_BIN %>% if_else(. >= 100, 1, .)
RAW_CHAR = stri_rand_strings(n = people, length = 2)
ANON_CHAR = stri_rand_strings(n = people, length = 2)

dat_raw = data_frame(RAW_ROW_NUMBER = ROW_NUMBER,
                      RAW_ID = RAW_ID,
                      RAW_NUM = RAW_NUM,
                      RAW_BIN = RAW_BIN,
                      RAW_CHAR = RAW_CHAR)

dat_anon = data_frame(ANON_ROW_NUMBER = ROW_NUMBER,
                      ANON_ID = ANON_ID,
                      ANON_NUM = ANON_NUM,
                      ANON_BIN = ANON_BIN,
                      ANON_CHAR = ANON_CHAR)

# create_dummy_master_data("hoge")
# create_dummy_master_data(-10)
# create_dummy_transaction_data("hoge")
# create_dummy_transaction_data(-10)
# create_dummy_transaction_data(10, "hoge")
# create_dummy_transaction_data(people = 10, size = "hoge")
create_dummy_master_data(people = as.integer(100))
reate_row_anon_joined_data(dat_raw, dat_anon)
