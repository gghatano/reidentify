#' transform transaction -> master
#'
#' @param dat transaction data frame
#' @param ROW_NUMBER column name for row number in the create data frame
#' @param ID  identifier name
#' @param collapse separator used to join the per-ID values of a `_DIST`
#'   column (default ":"). It is handed to `paste(collapse = )` and is
#'   therefore a **literal string**. Pass the same value as `split` when
#'   the resulting `_DIST` column is later scored by [score_dist()] /
#'   [reid_by_dist()]; those read the column back with a literal separator
#'   too, so the two sides are symmetric and any character -- including
#'   regex metacharacters such as `"|"` or `"."` -- round-trips (Issue #32).
#'   Do not use a separator that occurs inside the values themselves.
#' @param STATIC_NUM list of column name which shows STATIC NUMBER attribute
#' @param STATIC_CHAR list of column name which shows STATIC CHARACTER attribute
#' @param DYNAMIC_NUM list of column name which shows DYNAMIC NUMBER attribute
#' @param DYNAMIC_CHAR list of column name which shows DYNAMIC CHARACTER attribute
#'
#' @section Column naming:
#'
#' The aggregate columns are **always** named `<source column>_<statistic>`,
#' whatever the number of columns given:
#'
#' * `DYNAMIC_NUM` yields `<col>_MAX`, `<col>_MEAN`, `<col>_MEDIAN`,
#'   `<col>_MIN` for every column;
#' * `DYNAMIC_NUM` and `DYNAMIC_CHAR` each additionally yield `<col>_DIST`.
#'
#' This used to depend on how many columns were passed.
#' `dplyr::summarise_all()` with a named function list only prefixes the
#' result with the source column name when two or more columns survive
#' grouping; with a single column it used the bare function names, so
#' `DYNAMIC_NUM = "NUM_DYNAMIC"` produced `MAX`/`MEAN`/`MEDIAN`/`MIN` while
#' `DYNAMIC_NUM = c("BIN", "NUM_DYNAMIC")` produced `BIN_MAX`/
#' `NUM_DYNAMIC_MAX`/... Downstream code therefore could not hard-code a
#' column name (Issue #26).
#'
#' The column *order* is unchanged: statistic-major
#' (`<col1>_MAX, <col2>_MAX, ..., <col1>_MEAN, ...`), matching what
#' `summarise_all()` produced for the multi-column case.
#'
#' @return a data frame with one row per distinct `ID`, combining the
#'   STATIC_NUM/STATIC_CHAR columns as-is, `<col>_MAX`/`_MEAN`/`_MEDIAN`/
#'   `_MIN` summaries of DYNAMIC_NUM, `collapse`-joined distributions
#'   (`<col>_DIST`) of DYNAMIC_NUM and DYNAMIC_CHAR, a ROWCOUNT column and
#'   the minimum `ROW_NUMBER` per `ID`.
#'
#' @examples
#' tran <- data.frame(
#'   ROW_NUMBER = 1:6,
#'   ID         = c("a", "a", "a", "b", "b", "b"),
#'   SEX        = c("M", "M", "M", "F", "F", "F"),
#'   AMOUNT     = c(100, 200, 300, 10, 20, 30)
#' )
#' m <- transform_transaction_to_master(tran, STATIC_CHAR = "SEX",
#'                                      DYNAMIC_NUM = "AMOUNT")
#' m
#'
#' # the aggregate columns are named <column>_<statistic> whatever the number
#' # of columns given (Issue #26), so downstream code can hard-code them
#' names(m)
#'
#' # AMOUNT_DIST round-trips through score_dist() / reid_by_dist() as long as
#' # `collapse` and `split` agree
#' m$AMOUNT_DIST
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise_all
#' @importFrom dplyr summarise
#' @importFrom dplyr across
#' @importFrom dplyr distinct
#' @importFrom dplyr inner_join
#' @importFrom dplyr ungroup
#' @importFrom dplyr n
#' @importFrom dplyr .data
#' @importFrom magrittr %>%
#' @importFrom stats median
#' @export
transform_transaction_to_master <- function(dat, ROW_NUMBER = "ROW_NUMBER", ID = "ID", collapse = ":",
                                            STATIC_NUM = NULL, STATIC_CHAR = NULL, DYNAMIC_NUM = NULL, DYNAMIC_CHAR = NULL) {
  ## transform to master
  dat_master <-
    dat %>%
    dplyr::select(dplyr::all_of(c(ID, STATIC_NUM, STATIC_CHAR))) %>%
    dplyr::group_by(.data[[ID]]) %>%
    dplyr::distinct() %>%
    ungroup()

  ## Reorder aggregate columns to statistic-major, i.e. all columns' MAX,
  ## then all columns' MEAN, and so on. dplyr::across() emits them
  ## column-major; summarise_all() used to emit them statistic-major, and
  ## Issue #26 is about the *names*, so the order is kept as it was.
  statistic_major <- function(dat_agg, cols, fns) {
    ## NB: paste() recycles a zero-length vector to "", so the no-columns case
    ## has to be short-circuited rather than falling through to paste().
    if (length(cols) == 0) {
      return(dat_agg)
    }
    ordered <- unlist(lapply(fns, function(f) paste(cols, f, sep = "_")))
    dat_agg[, c(ID, ordered), drop = FALSE]
  }

  ## max and mean and min and...
  ## across(.names = "{.col}_{.fn}") names the results the same way whether one
  ## column or several were passed; summarise_all() did not.
  stat_fns <- list(MAX = max, MEAN = mean, MEDIAN = median, MIN = min)
  dat_master_statistic <-
    dat %>%
    dplyr::select(dplyr::all_of(c(ID, DYNAMIC_NUM))) %>%
    dplyr::group_by(.data[[ID]]) %>%
    dplyr::summarise(dplyr::across(
      dplyr::all_of(DYNAMIC_NUM), stat_fns,
      .names = "{.col}_{.fn}"
    )) %>%
    dplyr::ungroup() %>%
    statistic_major(DYNAMIC_NUM, names(stat_fns))

  # distribution
  dist_cols <- c(DYNAMIC_NUM, DYNAMIC_CHAR)
  dat_master_dist <-
    dat %>%
    dplyr::select(dplyr::all_of(c(ID, dist_cols))) %>%
    dplyr::group_by(.data[[ID]]) %>%
    dplyr::summarise(dplyr::across(
      dplyr::all_of(dist_cols),
      list(DIST = ~ paste(sort(.), collapse = collapse)),
      .names = "{.col}_{.fn}"
    )) %>%
    dplyr::ungroup()

  # row count
  dat_master_rowcount <-
    dat %>%
    dplyr::group_by(.data[[ID]]) %>%
    dplyr::summarise(ROWCOUNT = dplyr::n()) %>%
    dplyr::ungroup()

  dat_master_rownumber <-
    dat %>%
    dplyr::select(dplyr::all_of(c(ID, ROW_NUMBER))) %>%
    dplyr::group_by(.data[[ID]]) %>%
    dplyr::summarise_all(min) %>%
    dplyr::ungroup()

  dat_master %>%
    dplyr::inner_join(dat_master_statistic, by = ID) %>%
    dplyr::inner_join(dat_master_dist, by = ID) %>%
    dplyr::inner_join(dat_master_rowcount, by = ID) %>%
    dplyr::inner_join(dat_master_rownumber, by = ID) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    return()
}
