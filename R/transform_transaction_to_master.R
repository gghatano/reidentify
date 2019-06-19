transform_transaction_to_master <- function(dat, ROW_NUMBER = "ROW_NUMBER", ID = "ID", collapse = ":"){
  #' transform transaction -> master
  #'
  #' @param dat transaction data frame
  #' @param ID  identifier name
  #' @param collapse separation character (defalt ":")
  #'
  #' @importFrom dplyr group_by
  #' @importFrom dplyr summarise_all
  #' @importFrom dplyr summarise
  #' @importFrom dplyr inner_join
  #' @importFrom magrittr %>%
  #' @export
  #'

  ## transform to master
  dat_master =
    dat %>%
    dplyr::group_by(`ID`) %>%
    dplyr::summarise_all(~paste(sort(.), collapse = collapse)) %>%
    dplyr::ungroup() # %>%
  new_row_number =
    dat_master$ROW_NUMBER %>%
    sapply(., function(x){ unique(strsplit(x, split=":")[[1]])})
  dat_master$ROW_NUMBER = new_row_number

  ## row count
  dat_rowcount =
    dat %>%
    dplyr::group_by(`ID`) %>%
    dplyr::summarise(ROWCOUNT = n())

  ## merge and return
  dat_master %>%
    dplyr::inner_join(dat_rowcount, by = ID) %>% return
}

## preprocess of transaction data
transform_transaction_rownumber_to_id = function(dat, ID = "ID"){
  #' transform transaction row number to master row number
  #'
  #' @param dat transaction data frame
  #' @param ID  identifier name
  #' @param collapse separation character (defalt ":")
  #'
  #' @importFrom dplyr group_by
  #' @importFrom dplyr select
  #' @importFrom dplyr summarise
  #' @importFrom dplyr inner_join
  #' @importFrom dplyr mutate
  #' @importFrom magrittr %>%
  #' @export

  dat = create_dummy_transaction_data(100)
  dat_rownumber =
    dat %>% dplyr::select(ROW_NUMBER, `ID`) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarize(NEW_ROW_NUMBER = min(ROW_NUMBER))
  dat %>%
    dplyr::inner_join(dat_rownumber, by = `ID`) %>%
    dplyr::mutate(ROW_NUMBER = NEW_ROW_NUMBER) %>%
    dplyr::select(-NEW_ROW_NUMBER) %>%
    return
}
