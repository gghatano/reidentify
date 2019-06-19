reid_master_category = function(raw_anon_data, target_column = "ID"){
  #' 再識別用のダミーマスターデータ生成
  #'
  #' @param people 人数
  #'
  #' @importFrom tidyverse
  #' @importFrom magrittr
  #' @importFrom stringi
  #' @importFrom openssl
  #' @export
  #'

  target_column_raw = paste("RAW_" , target_column, sep="")
  target_column_anon = paste("ANON_" , target_column, sep="")

  ## 分母
  attempt = max(raw_anon_data$ANON_ROW_NUMBER) %>% print

  raw_anon_data %>%
    mutate(distance = (`target_column_raw` == `target_column_anon`)) %>%
    group_by(`target_column_raw`) %>%
    filter(distance == 1) %>%
    print

  0 %>% return
}


## test
dat = create_dummy_master_data()
reid_master_category(raw_anon_data = dat, target_column = "ID")

## 値のL1距離最小でマッチング関数
dat_reid_num_result =
  dat %>%
  mutate(DISTANCE = abs(RAW_NUM - ANON_NUM)) %>%
  select(RAW_ROW_NUMBER, ANON_ROW_NUMBER, DISTANCE) %>%
  group_by(ANON_ROW_NUMBER) %>%
  summarize(RAW_ROW_NUMBER = RAW_ROW_NUMBER[which.min(DISTANCE)])
dat_reid_num_result %>%
  mutate(RESULT = (ANON_ROW_NUMBER == RAW_ROW_NUMBER)) %>%
  pull(RESULT) %>%
  mean

## binary変数の一致でマッチング
dat_reid_bin_result =
  dat %>%
  mutate(DISTANCE = abs(RAW_BIN - ANON_BIN)) %>%
  select(RAW_ROW_NUMBER, ANON_ROW_NUMBER, DISTANCE) %>%
  group_by(ANON_ROW_NUMBER) %>%
  summarize(RAW_ROW_NUMBER = RAW_ROW_NUMBER[which.min(DISTANCE)])
dat_reid_bin_result %>%
  mutate(RESULT = (ANON_ROW_NUMBER == RAW_ROW_NUMBER)) %>%
  arrange(desc(RESULT))


## binary変数の一致でマッチング
dat_reid_category_result =
  dat %>%
  mutate(DISTANCE = (RAW_CHAR == ANON_CHAR)) %>%
  select(RAW_ROW_NUMBER, ANON_ROW_NUMBER, DISTANCE) %>%
  group_by(ANON_ROW_NUMBER) %>%
  summarize(RAW_ROW_NUMBER = RAW_ROW_NUMBER[which.min(DISTANCE)])
dat_reid_category_result %>%
  mutate(RESULT = (ANON_ROW_NUMBER == RAW_ROW_NUMBER)) %>%
  pull(RESULT) %>% table
