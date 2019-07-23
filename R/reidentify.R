reid_by_num <- function(dat_raw_anon, target, row_number = "ROW_NUMBER"){
  #' reidentify by single num static column by using L2 norm
  #'
  #' @param dat_raw_anon dataframe of raw_anon form
  #' @param target target column
  #' @param row_number row number column name(default: "ROW_NUMBER")
  #'
  #' @importFrom dplyr group_by
  #' @importFrom dplyr ungroup
  #' @importFrom dplyr filter
  #' @importFrom dplyr mutate
  #' @importFrom magrittr %>%
  #' @export

  raw_target = paste("RAW_", target, sep="")
  anon_target = paste("ANON_", target, sep="")
  raw_row_number = paste("RAW_", row_number, sep="")
  anon_row_number = paste("ANON_", row_number, sep="")

  dat_raw_anon %>%
    dplyr::select(RAW_ROW_NUMBER = `RAW_ROW_NUMBER`, ANON_ROW_NUMBER = `ANON_ROW_NUMBER`, RAW = `raw_target`, ANON = `anon_target`) %>%
    dplyr::mutate(DISTANCE = abs(RAW - ANON)) %>%
    dplyr::group_by(ANON_ROW_NUMBER) %>%
    dplyr::filter(DISTANCE == min(DISTANCE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(ANON_ROW_NUMBER) %>%
    dplyr::filter(RAW_ROW_NUMBER == RAW_ROW_NUMBER[1]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(RESULT = (ANON_ROW_NUMBER == RAW_ROW_NUMBER)) %>%
    return
}

reid_result = function(dat_reid_result,
                       raw_row_number = "RAW_ROW_NUMBER", anon_row_number = "ANON_ROW_NUMBER", result = "RESULT",
                       method = NULL){
  #' craete text of reidentify result ( method: ******, success/trial : ***** / ******)
  #'
  #' @param dat_reid_result reid result data frame (RAW_ROW_NUMBER, ANON_ANON_NUMBER, RESULT)
  #' @param anon_row_number column name of row number in ANON data
  #' @param raw_row_number column name of row number in RAW data
  #' @param result true or false
  #' @param method reid method name
  #'
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
  #' @importFrom dplyr ungroup
  #' @importFrom dplyr filter
  #' @importFrom dplyr mutate
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
    dplyr::mutate(RAW_ROW_NUMBER = `RAW_ROW_NUMBER`, ANON_ROW_NUMBER = `ANON_ROW_NUMBER`, DISTANCE) %>%
    dplyr::mutate(RESULT = (RAW_ROW_NUMBER == ANON_ROW_NUMBER)) %>%
    dplyr::group_by(ANON_ROW_NUMBER) %>%
    dplyr::filter(DISTANCE == min(DISTANCE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(ANON_ROW_NUMBER) %>%
    dplyr::filter(RAW_ROW_NUMBER == RAW_ROW_NUMBER[1]) %>%
    dplyr::ungroup() %>%
    return

}

reid_by_dist <- function(dat_raw_anon, target, row_number = "ROW_NUMBER", split = ":"){
  #' reidentify by single list column (list A, B, C is expressed by "A:B:C")
  #'
  #' @param dat_raw_anon dataframe of raw_anon form
  #' @param target target column
  #' @param row_number row number column name(default: "ROW_NUMBER")
  #' @param split character for split _DIST value (default: ":")
  #'
  #' @importFrom dplyr group_by
  #' @importFrom dplyr ungroup
  #' @importFrom dplyr filter
  #' @importFrom dplyr mutate
  #' @importFrom magrittr %>%
  #' @importFrom magrittr %>%
  #' @export
  #
  raw_target = paste("RAW_", target, sep="")
  anon_target = paste("ANON_", target, sep="")
  raw_row_number = paste("RAW_", row_number, sep="")
  anon_row_number = paste("ANON_", row_number, sep="")

  raw_target_col = dat_raw_anon %>% pull(`raw_target`) %>% as.character
  anon_target_col = dat_raw_anon %>% pull(`anon_target`) %>% as.character

  ## calc distribution distance
  # distance = mapply(FUN = calc_KL, raw_target_col, anon_target_col)
  distance = mapply(FUN = distribution_distance, raw_target_col, anon_target_col)

  dat_raw_anon$DISTANCE = distance

  dat_raw_anon %>%
    dplyr::select(RAW_ROW_NUMBER = `RAW_ROW_NUMBER`, ANON_ROW_NUMBER = `ANON_ROW_NUMBER`, DISTANCE) %>%
    dplyr::group_by(ANON_ROW_NUMBER) %>%
    dplyr::filter(DISTANCE == min(DISTANCE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(RESULT = (ANON_ROW_NUMBER == RAW_ROW_NUMBER)) %>%
    return

}

calc_KL = function(x,y, split = ":"){
  #' calc KL divergence from 2 character vector which has distribution expression (A:B:C:...)
  #'
  #' @param x vector
  #' @param y vector
  #' @param split split (default: ":")
  #'
  #' @importFrom philentropy KL
  #' @importFrom magrittr %>%
  #'
  ## normalize vector
  x_list = strsplit(x, split = split)[[1]] %>% as.numeric
  x_list = x_list / max(x_list)
  y_list = strsplit(y, split = split)[[1]] %>% as.numeric
  y_list = y_list / max(y_list)
  dat = rbind(x_list, y_list)

  philentropy::KL(dat) %>% return
}

distribution_distance = function(x,y, split = ":"){
  #' calculation distributin distance(L2) from 2 character vector which has distribution expression (A:B:C:...)
  #'
  #' @param x vector
  #' @param y vector
  #' @param split split (default: ":")
  #'
  #' @importFrom magrittr %>%


  x_list = strsplit(x, split = split)[[1]] %>% as.numeric
  y_list = strsplit(y, split = split)[[1]] %>% as.numeric

  ## match the length of 2 vector
  x_length= length(x_list)
  y_length= length(y_list)
  diff_x_y = x_length - y_length

  ## fill by mean value
  if (diff_x_y == 0){

  } else if (diff_x_y > 0) {
    y_list = c(y_list, rep(mean(y_list), diff_x_y)) %>% sort
  } else {
    x_list = c(x_list, rep(mean(x_list), -1 * diff_x_y)) %>% sort
  }

  ## calc distance
  distance = (x_list - y_list) %>% .**2 %>% sum
  distance  %>% return
}

reid_by_num_rank <- function(dat_raw_anon, target, row_number = "ROW_NUMBER"){
  #' reidentify by single num static by using rank
  #'
  #' @param dat_raw_anon dataframe of raw_anon form
  #' @param target target column
  #' @param row_number row number column name(default: "ROW_NUMBER")
  #'
  #' @importFrom dplyr group_by
  #' @importFrom dplyr ungroup
  #' @importFrom dplyr filter
  #' @importFrom dplyr mutate
  #' @importFrom magrittr %>%
  #' @export

  raw_target = paste("RAW_", target, sep="")
  anon_target = paste("ANON_", target, sep="")
  raw_row_number = paste("RAW_", row_number, sep="")
  anon_row_number = paste("ANON_", row_number, sep="")

  ## check the rank
  dat_anon_rank =
    dat_raw_anon %>%
    dplyr:: select(`anon_row_number`, `anon_target`) %>%
    dplyr::distinct()
  dat_anon_rank$ANON_RANK = rank(dplyr::pull(dat_anon_rank, `anon_target`), ties.method = "random")
  dat_anon_rank %<>%
    dplyr::select(`anon_row_number`, ANON_RANK)

   dat_raw_rank =
    dat_raw_anon %>%
    dplyr:: select(`raw_row_number`, `raw_target`) %>%
    dplyr::distinct()
   dat_raw_rank$RAW_RANK = rank(dplyr::pull(dat_raw_rank, `raw_target`), ties.method = "random")
   dat_raw_rank %<>%
    dplyr::select(`raw_row_number`, RAW_RANK)

  dat_raw_anon %>%
    dplyr::inner_join(dat_raw_rank, by = raw_row_number) %>%
    dplyr::inner_join(dat_anon_rank, by = anon_row_number) %>%
    dplyr::mutate(DISTANCE = abs(ANON_RANK - RAW_RANK)) %>%
    dplyr::group_by(ANON_ROW_NUMBER) %>%
    dplyr::filter(DISTANCE == min(DISTANCE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(ANON_ROW_NUMBER) %>%
    dplyr::filter(RAW_ROW_NUMBER == RAW_ROW_NUMBER[1]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(RESULT = (ANON_ROW_NUMBER == RAW_ROW_NUMBER)) %>%
    dplyr::select(ANON_ROW_NUMBER, RAW_ROW_NUMBER, `anon_target`, `raw_target`, ANON_RANK, RAW_RANK, DISTANCE, RESULT) %>%
    return

}

