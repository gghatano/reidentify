transform_transaction_to_master <- function(dat, ROW_NUMBER = "ROW_NUMBER", ID = "ID", collapse = ":",
                                            STATIC_NUM = NULL, STATIC_CHAR = NULL, DYNAMIC_NUM = NULL, DYNAMIC_CHAR = NULL) {
  #' transform transaction -> master
  #'
  #' @param dat transaction data frame
  #' @param ROW_NUMBER column name for row number in the create data frame
  #' @param ID  identifier name
  #' @param collapse separation character (defalt ":")
  #' @param STATIC_NUM list of column name which shows STATIC NUMBER attribute
  #' @param STATIC_CHAR list of column name which shows STATIC CHARACTER attribute
  #' @param DYNAMIC_NUM list of column name which shows DYNAMIC NUMBER attribute
  #' @param DYNAMIC_CHAR list of column name which shows DYNAMIC CHARACTER attribute
  #'
  #' @importFrom dplyr group_by
  #' @importFrom dplyr summarise_all
  #' @importFrom dplyr summarise
  #' @importFrom dplyr distinct
  #' @importFrom dplyr inner_join
  #' @importFrom dplyr ungroup
  #' @importFrom magrittr %>%
  #' @export

  ## transform to master
  dat_master <-
    dat %>%
    dplyr::select(`ID`, `STATIC_NUM`, `STATIC_CHAR`) %>%
    dplyr::group_by(`ID`) %>%
    dplyr::distinct() %>%
    ungroup()

  ## max and mean and min and...
  dat_master_statistic <-
    dat %>%
    dplyr::select(`ID`, `DYNAMIC_NUM`) %>%
    dplyr::group_by(`ID`) %>%
    dplyr::summarise_all(list(MAX = max, MEAN = mean, MEDIAN = median, MIN = min)) %>%
    dplyr::ungroup()

  # distribution
  dat_master_dist <-
    dat %>%
    dplyr::select(`ID`, `DYNAMIC_NUM`, `DYNAMIC_CHAR`) %>%
    dplyr::group_by(`ID`) %>%
    dplyr::summarise_all(list(DIST = ~ paste(sort(.), collapse = collapse))) %>%
    dplyr::ungroup()

  # row count
  dat_master_rowcount <-
    dat %>%
    dplyr::group_by(`ID`) %>%
    dplyr::summarise(ROWCOUNT = n()) %>%
    dplyr::ungroup()

  dat_master_rownumber <-
    dat %>%
    dplyr::select(`ID`, `ROW_NUMBER`) %>%
    dplyr::group_by(`ID`) %>%
    dplyr::summarise_all(min) %>%
    dplyr::ungroup()

  dat_master %>%
    dplyr::inner_join(dat_master_statistic, by = `ID`) %>%
    dplyr::inner_join(dat_master_dist, by = `ID`) %>%
    dplyr::inner_join(dat_master_rowcount, by = `ID`) %>%
    dplyr::inner_join(dat_master_rownumber, by = `ID`) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    return()
}
