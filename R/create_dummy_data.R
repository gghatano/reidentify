## ダミーデータを作る(マスター)
create_dummy_master_data = function(people = 100){
  #' 再識別用のダミーマスターデータ生成
  #'
  #' @param people 人数
  #'
  #' @importFrom tibble data_frame
  #' @importFrom dplyr %>%
  #' @importFrom stringi stri_rand_strings
  #' @importFrom openssl md5
  #' @export
  #'
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
  RAW_NUM = runif(n = people)
  RAW_BIN = sample(x = c(0,1,100), prob = c(20,20,1), size = people, replace = TRUE)
  RAW_CHAR = stringi::stri_rand_strings(n = people, length = 2)

  dat_raw = tibble::data_frame(
                        ROW_NUMBER = ROW_NUMBER,
                        ID = RAW_ID,
                        NUM = RAW_NUM,
                        BIN = RAW_BIN,
                        CHAR = RAW_CHAR)

  dat_raw %>% return
}


## ダミーデータを作る(トランザクション)
create_dummy_transaction_data = function(people = 100, size = 2){
  #' 再識別用のダミーマスターデータ生成
  #'
  #' @param people 人数
  #' @param size 平均トランザクション数
  #'
  #' @importFrom tibble data_frame
  #' @importFrom dplyr %>%
  #' @importFrom stringi stri_rand_strings
  #' @importFrom openssl md5
  #' @examples
  #' data_tran = create_dummy_transaction_data(people = 10, size = 4)
  #' @export


  if(!is.numeric(people)){
    stop("peopleには数字を指定してください")
  } else if(!is.numeric(size)){
    stop("sizeには数値を指定してください")
  } else if(people < 1){
    stop("人数(自然数)を指定してください")
  } else if(size <= 0) {
    stop("sizeには平均トランザクション数(正の数)を指定してください")
  }

  row_num = people * size

  ROW_NUMBER = 1:row_num
  RAW_ID = sample(x = 1:people, size = row_num, replace = TRUE)
  RAW_NUM_STATIC = rep(10, row_num)
  RAW_NUM_DYNAMIC = runif(n = row_num)
  RAW_BIN = sample(x = c(0,1,100), prob = c(20,20,1), size = row_num, replace = TRUE)
  RAW_CHAR = stringi::stri_rand_strings(n = row_num, length = 2)

  dat_raw = tibble::data_frame(
                        ROW_NUMBER = ROW_NUMBER,
                        ID = RAW_ID,
                        NUM_STATIC = RAW_NUM_STATIC,
                        NUM_DYNAMIC = RAW_NUM_DYNAMIC,
                        BIN = RAW_BIN,
                        CHAR = RAW_CHAR)

  dat_raw %>% return
}


## 再識別をしやすいように、加工前後のマスターデータにヘッダを付けてクロスジョインする
join_row_anon_data = function(raw, anon, raw_header = "RAW_", anon_header = "ANON_") {
  #' 再識別アルゴリズム適用に便利な形のデータを作る
  #'
  #' @param raw 匿名加工前データ(data.frame
  #' @param anon 匿名加工後データ(data.frame)
  #' @param raw_header 加工前データの列の先頭に着けるラベル(デフォルトはRAW_)
  #' @param anon_header 加工後データの列の先頭に着けるラベル(デフォルトはAN_)
  #'
  #' @importFrom tibble data_frame
  #' @importFrom tibble as_tibble
  #' @importFrom dplyr %>%
  #' @importFrom stringi stri_rand_strings
  #' @importFrom openssl md5
  #' @export


  ## 引数のエラー処理
  if(is.data.frame(raw) + is.data.frame(anon) != 2){
    stop("data.frameを指定して下さい")
  }

  ## 列名の処理
  names(raw) = paste(raw_header, names(raw), sep = "")
  names(anon) = paste(anon_header, names(anon), sep = "")

  ## クロスジョインする
  merge(raw, anon, all = TRUE) %>% return
}

