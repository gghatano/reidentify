## ダミーデータを作る(マスター)
create_dummy_master_data <- function(people = 100) {
  #' create dummy master data
  #'
  #' @param people number of people
  #'
  #' @return a tibble with columns ROW_NUMBER, ID, NUM, BIN, CHAR containing
  #'   `people` rows of randomly generated dummy master data.
  #'
  #' @importFrom tibble tibble
  #' @importFrom dplyr %>%
  #' @importFrom stringi stri_rand_strings
  #' @importFrom stats runif
  #' @encoding UTF-8
  #' @export
  #'
  #'
  ## error handling
  if (!is.numeric(people)) {
    stop("people is integer ( > 0)")
  } else if (people < 1) {
    stop("people is integer ( > 0)")
  }

  ROW_NUMBER <- 1:people
  RAW_ID <- ROW_NUMBER + 10000
  RAW_NUM <- runif(n = people)
  RAW_BIN <- sample(x = c(0, 1, 100), prob = c(20, 20, 1), size = people, replace = TRUE)
  RAW_CHAR <- stringi::stri_rand_strings(n = people, length = 2)

  dat_raw <- tibble::tibble(
    ROW_NUMBER = ROW_NUMBER,
    ID = RAW_ID,
    NUM = RAW_NUM,
    BIN = RAW_BIN,
    CHAR = RAW_CHAR
  )

  dat_raw %>% return()
}


create_dummy_transaction_data <- function(people = 100, size = 2) {
  #' create dummy transaction data
  #'
  #' @param people number of people
  #' @param size mean record number
  #'
  #' @return a tibble with columns ROW_NUMBER, ID, NUM_STATIC, NUM_DYNAMIC,
  #'   BIN, CHAR containing `people * size` rows of randomly generated dummy
  #'   transaction data.
  #'
  #' @importFrom tibble tibble
  #' @importFrom dplyr %>%
  #' @importFrom stringi stri_rand_strings
  #' @importFrom stats runif
  #' @examples
  #' data_tran = create_dummy_transaction_data(people = 10, size = 4)
  #' @export
  #' @encoding UTF-8


  if (!is.numeric(people)) {
    stop("people is integer ( > 0 )")
  } else if (!is.numeric(size)) {
    stop("size is integer ( > 0 )")
  } else if (people < 1) {
    stop("people is integer ( > 0 )")
  } else if (size <= 0) {
    stop("size is integer ( > 0 )")
  }

  row_num <- people * size

  ROW_NUMBER <- 1:row_num
  RAW_ID <- sample(x = 1:people, size = row_num, replace = TRUE)
  RAW_NUM_STATIC <- rep(10, row_num)
  RAW_NUM_DYNAMIC <- runif(n = row_num)
  RAW_BIN <- sample(x = c(0, 1, 100), prob = c(20, 20, 1), size = row_num, replace = TRUE)
  RAW_CHAR <- stringi::stri_rand_strings(n = row_num, length = 2)

  dat_raw <- tibble::tibble(
    ROW_NUMBER = ROW_NUMBER,
    ID = RAW_ID,
    NUM_STATIC = RAW_NUM_STATIC,
    NUM_DYNAMIC = RAW_NUM_DYNAMIC,
    BIN = RAW_BIN,
    CHAR = RAW_CHAR
  )

  dat_raw %>% return()
}


join_raw_anon_data <- function(raw, anon, raw_header = "RAW_", anon_header = "ANON_") {
  #' create reid-format data from raw and anon data frame
  #'
  #' @param raw  raw data frame
  #' @param anon anonymized data frame
  #' @param raw_header strings which is added for columns from raw data
  #' @param anon_header strings which is added for columns from anon data
  #'
  #' @return a data frame: the cross join (every RAW row paired with every
  #'   ANON row) of `raw` and `anon`, with their column names prefixed by
  #'   `raw_header`/`anon_header` respectively.
  #'
  #' @importFrom dplyr %>%
  #' @export
  #' @encoding UTF-8


  ## error handling
  if (is.data.frame(raw) + is.data.frame(anon) != 2) {
    stop("raw and anon are data frame")
  }

  ## convert column names
  names(raw) <- paste(raw_header, names(raw), sep = "")
  names(anon) <- paste(anon_header, names(anon), sep = "")

  ## cross join
  merge(raw, anon, all = TRUE) %>% return()
}
