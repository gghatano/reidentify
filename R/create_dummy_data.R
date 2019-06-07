library(tidyverse)
library(magrittr)
library(stringi)
library(openssl)

## ダミーデータを作る(マスター)
create_dummy_master_data = function(people = 100){
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
  ## エラー処理
  if(!is.numeric(people)){
    stop("整数を入力してください")
  } else if(people < 1){
    stop("人数(自然数)を入力してください")
  }

  # people = データに含まれる人数
  # 人数分のデータが生成される
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
  # [dat_raw, dat_anon] %>% return
  merge(dat_raw, dat_anon, all = TRUE) %>% as.tibble() %>% return
}


## ダミーデータを作る(トランザクション)
create_dummy_transaction_data = function(people = 100, size = 2){
  #' 再識別用のダミーマスターデータ生成
  #'
  #' @param people 人数
  #' @param size 平均トランザクション数
  #'
  #' @importFrom tidyverse
  #' @importFrom magrittr
  #' @importFrom stringi
  #' @importFrom openssl
  #' @examples
  #' data_tran = create_dummy_transaction_data(people = 10, size = 4)
  #' @export

  if(!is.integer(people)){
    stop("peopleには整数を指定してください")
  } else if(!is.numeric(size)){
    stop("sizeには数値を指定してください")
  } else if(people < 1){
    stop("人数(自然数)を指定してください")
  } else if(size < 0) {
    stop("sizeには平均トランザクション数(正の数)を指定してください")
  }

  row_num = people * size

  ROW_NUMBER = 1:row_num
  RAW_ID = sample(x = 1:people, size = row_num, replace = TRUE)
  ANON_ID = md5(as.character(RAW_ID))
  RAW_NUM = runif(n = row_num)
  ANON_NUM = RAW_NUM + runif(n = row_num) * 0.01
  RAW_BIN = sample(x = c(0,1,100), prob = c(20,20,1), size = row_num, replace = TRUE)
  ANON_BIN = RAW_BIN %>% if_else(. >= 100, 1, .)
  RAW_CHAR = stri_rand_strings(n = row_num, length = 2)
  ANON_CHAR = RAW_CHAR

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

  merge(dat_raw, dat_anon, all = TRUE) %>% as.tibble() %>% return
}


## 再識別をしやすいように、加工前後のデータにヘッダを付けてクロスジョインする
join_row_anon_data = function(raw, anon, raw_header = "RAW_", anon_header = "ANON_"){
  #' 再識別アルゴリズム適用に便利な形のデータを作る
  #'
  #' @param raw 匿名加工前データ(data.frame
  #' @param anon 匿名加工後データ(data.frame)
  #' @param raw_header 加工前データの列の先頭に着けるラベル(デフォルトはRAW_)
  #' @param anon_header 加工後データの列の先頭に着けるラベル(デフォルトはAN_)
  #'
  #' @importFrom tidyverse
  #' @importFrom magrittr
  #' @importFrom stringi
  #' @importFrom openssl
  #' @export

  ## 引数のエラー処理
  if(is.data.frame(raw) + is.data.frame(anon) != 2){
    stop("data.frameを指定して下さい")
  }

  ## 列名の処理
  names(raw) = paste(raw_header, names(raw), sep = "")
  names(anon) = paste(anon_header, names(anon), sep = "")

  ## クロスジョインする
  merge(raw, anon, all = TRUE) %>% as.tibble %>% return

}

