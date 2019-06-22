reid_by_num <- function(dat_raw_anon, target, row_number = "ROW_NUMBER"){
  #' reidentify by single num static column
  #'
  #' @param dat_raw_anon dataframe of raw_anon form
  #' @param target target column
  #' @param row_number row number column name(default: "ROW_NUMBER")
  #'
  #' @importFrom dplyr group_by
  #' @importFrom dplyr summarise_all
  #' @importFrom dplyr summarise
  #' @importFrom dplyr distinct
  #' @importFrom dplyr inner_join
  #' @importFrom magrittr %>%
  #' @export

  raw_target = paste("RAW_", target, sep="")
  anon_target = paste("ANON_", target, sep="")
  raw_row_number = paste("RAW_", row_number, sep="")
  anon_row_number = paste("ANON_", row_number, sep="")

  dat_raw_anon %>%
    select(RAW_ROW_NUMBER = `RAW_ROW_NUMBER`, ANON_ROW_NUMBER = `ANON_ROW_NUMBER`, RAW = `raw_target`, ANON = `anon_target`) %>%
    mutate(DISTANCE = abs(RAW - ANON)) %>%
    group_by(ANON_ROW_NUMBER) %>%
    filter(DISTANCE == min(DISTANCE)) %>%
    ungroup() %>%
    mutate(RESULT = (ANON_ROW_NUMBER == RAW_ROW_NUMBER)) %>%
    return
}

reid_result = function(dat_reid_result,
                       raw_row_number = "RAW_ROW_NUMBER", anon_row_number = "ANON_ROW_NUMBER", result = "RESULT",
                       method = NULL){
  #' craete text of reidentify result ( method: ******, success/trial : ***** / ******)
  #'
  #' @param dat_reid_result reid result data frame (RAW_ROW_NUMBER, ANON_ANON_NUMBER, RESULT)
  #' @param anon_row_number
  #' @param raw_row_number
  #' @param result true or false
  #' @param method reid method name
  #'
  #' @importFrom dplyr group_by
  #' @importFrom dplyr summarise_all
  #' @importFrom dplyr summarise
  #' @importFrom dplyr distinct
  #' @importFrom dplyr inner_join
  #' @importFrom magrittr %>%
  #' @export

  result_vec = dat_reid_result %>%
    pull(`result`)

  trial = length(result_vec)
  success = sum(result_vec)

  result_text = paste(" method:", method, ", success / trial : ", success, "/", trial, sep=" ") %>% return
}




reid_by_char <- function(dat_raw_anon, target, row_number = "ROW_NUMBER"){
  #' reidentify by character static column
  #'
  #' @param dat_raw_anon dataframe of raw_anon form
  #' @param target target column
  #' @param row_number row number column name(default: "ROW_NUMBER")
  #'
  #' @importFrom dplyr group_by
  #' @importFrom dplyr summarise_all
  #' @importFrom dplyr summarise
  #' @importFrom dplyr distinct
  #' @importFrom dplyr inner_join
  #' @importFrom magrittr %>%
  #' @export

  raw_target = paste("RAW_", target, sep="")
  anon_target = paste("ANON_", target, sep="")
  raw_row_number = paste("RAW_", row_number, sep="")
  anon_row_number = paste("ANON_", row_number, sep="")

  raw_target_col = dat_raw_anon %>% pull(`raw_target`) %>% as.character
  anon_target_col = dat_raw_anon %>% pull(`anon_target`) %>% as.character

  vec_distance = mapply(FUN = function(x,y){return (adist(x,y)[[1]])},
                    anon_target_col, raw_target_col)

  dat_raw_anon$DISTANCE = vec_distance

  dat_raw_anon %>%
    dplyr::select(RAW_ROW_NUMBER = `RAW_ROW_NUMBER`, ANON_ROW_NUMBER = `ANON_ROW_NUMBER`, DISTANCE) %>%
    dplyr::mutate(RESULT = (RAW_ROW_NUMBER == ANON_ROW_NUMBER)) %>%
    dplyr::group_by(ANON_ROW_NUMBER) %>%
    filter(DISTANCE == min(DISTANCE)) %>%
    ungroup %>%
    return

}





reid_by_dist <- function(dat_raw_anon, target, row_number = "ROW_NUMBER"){
  #' reidentify by single list column (list A, B, C is expressed by "A:B:C")
  #'
  #' @param dat_raw_anon dataframe of raw_anon form
  #' @param target target column
  #' @param row_number row number column name(default: "ROW_NUMBER")
  #'
  #' @importFrom dplyr group_by
  #' @importFrom dplyr summarise_all
  #' @importFrom dplyr summarise
  #' @importFrom dplyr distinct
  #' @importFrom dplyr inner_join
  #' @importFrom magrittr %>%
  #' @export
  #
  0 %>% return

}
