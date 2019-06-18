transform_transaction_to_master <- function(dat, ID = "ID", collapse = ":"){
  #' トランザクションデータを無理やりマスタの形にする
  #'
  #' @param dat トランザクションデータ
  #' @param ID  識別子の列名
  #' @param collapse 圧縮時の区切り文字
  #'
  #' @importFrom dplyr %>%
  #' @importFrom magrittr %>%
  #' @export
  #'

  ## マスタへの変更
  dat_master =
    dat %>%
    group_by(`ID`) %>%
    summarise_all(~paste(sort(.), collapse = collapse))

  ## 行数カウント
  dat_rowcount =
    dat %>%
    group_by(`ID`) %>%
    summarise(ROWCOUNT = n())

  ## マージしてreturn
  dat_master %>%
    inner_join(dat_rowcount, by = ID) %>% return
}


