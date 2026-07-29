check_raw_anon_columns_exist <- function(dat_raw_anon, cols, fn_name) {
  #' stop with a clear, actionable error message if any of `cols` is missing
  #' from `dat_raw_anon`, instead of letting reid_by_*() fail downstream with
  #' a confusing low-level error (e.g. base R's "replacement has 0 rows,
  #' data has NNN" from reid_by_char()/reid_by_dist() indexing a
  #' non-existent column with `[[`, which gives no hint about which column
  #' name was actually wrong).
  #'
  #' @param dat_raw_anon dataframe of raw_anon form
  #' @param cols character vector of (already RAW_/ANON_-prefixed) column
  #'   names that the caller is about to look up in `dat_raw_anon`
  #' @param fn_name name of the calling reid_by_*() function, used in the
  #'   error message
  #'
  #' @return invisible NULL if all `cols` are present; otherwise stops with
  #'   an error naming the missing column(s).
  #'
  #' @keywords internal

  missing_cols <- setdiff(cols, names(dat_raw_anon))
  if (length(missing_cols) > 0) {
    stop(
      fn_name, "(): column(s) not found in dat_raw_anon: ",
      paste(missing_cols, collapse = ", "),
      ". Check the `target`/`row_number` arguments (these are looked up ",
      "*after* RAW_/ANON_ prefixing) against the columns actually present: ",
      paste(names(dat_raw_anon), collapse = ", "),
      call. = FALSE
    )
  }

  invisible(NULL)
}

resolve_min_distance_ties <- function(dat_with_distance) {
  #' pick exactly one RAW record per ANON record from a RAW/ANON candidate
  #' table that has a DISTANCE column, keeping only the row(s) whose
  #' DISTANCE is minimal within each ANON_ROW_NUMBER group and then, if more
  #' than one RAW record is still tied for the minimum, keeping only the
  #' first RAW_ROW_NUMBER encountered. Shared by reid_by_num(), reid_by_char(),
  #' reid_by_dist() and reid_by_num_rank() so their tie-breaking logic cannot
  #' drift out of sync (see phase 3 fix for the reid_by_dist() tie-handling
  #' defect).
  #'
  #' Also guards against silently reporting an empty/short result: if DISTANCE
  #' is NA for every row, or if some ANON_ROW_NUMBER ends up with zero rows
  #' after tie-breaking (which happens when every candidate DISTANCE for that
  #' ANON record is NA), this stops with an error instead of quietly
  #' shrinking the result (and thus the reid_result() trial count).
  #'
  #' @param dat_with_distance data frame with (at least) RAW_ROW_NUMBER,
  #'   ANON_ROW_NUMBER and DISTANCE columns
  #'
  #' @return `dat_with_distance`, filtered down to exactly one row per
  #'   ANON_ROW_NUMBER (the minimal-DISTANCE RAW candidate, first
  #'   RAW_ROW_NUMBER on remaining ties).
  #'
  #' @keywords internal
  #'
  #' @importFrom dplyr group_by
  #' @importFrom dplyr ungroup
  #' @importFrom dplyr filter
  #' @importFrom dplyr .data
  #' @importFrom magrittr %>%

  n_anon_before <- length(unique(dat_with_distance$ANON_ROW_NUMBER))

  if (nrow(dat_with_distance) == 0 || all(is.na(dat_with_distance$DISTANCE))) {
    stop(
      "resolve_min_distance_ties(): DISTANCE could not be computed for any ",
      "record (all values are NA, or there are no rows at all). This usually ",
      "means the target column could not be compared numerically/character-",
      "wise; check the reid_by_*() input.",
      call. = FALSE
    )
  }

  dat_result <-
    dat_with_distance %>%
    dplyr::group_by(.data$ANON_ROW_NUMBER) %>%
    dplyr::filter(.data$DISTANCE == min(.data$DISTANCE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$ANON_ROW_NUMBER) %>%
    dplyr::filter(.data$RAW_ROW_NUMBER == .data$RAW_ROW_NUMBER[1]) %>%
    dplyr::ungroup()

  n_anon_after <- length(unique(dat_result$ANON_ROW_NUMBER))

  if (nrow(dat_result) == 0 || n_anon_after < n_anon_before) {
    stop(
      "resolve_min_distance_ties(): one or more ANON records were dropped ",
      "because their DISTANCE to every RAW record was NA. Check the input ",
      "for non-numeric or missing values in the target column.",
      call. = FALSE
    )
  }

  dat_result %>% return()
}

reid_by_num <- function(dat_raw_anon, target, row_number = "ROW_NUMBER") {
  #' reidentify by single num static column by using L2 norm
  #'
  #' @param dat_raw_anon dataframe of raw_anon form
  #' @param target target column
  #' @param row_number name of the row-number column *before* the RAW_/ANON_
  #'   prefixing done by join_raw_anon_data() (default: "ROW_NUMBER"), i.e.
  #'   dat_raw_anon is expected to contain columns
  #'   paste0("RAW_", row_number) / paste0("ANON_", row_number). The output
  #'   always names these two columns RAW_ROW_NUMBER / ANON_ROW_NUMBER
  #'   regardless of `row_number`, so it can be passed straight into
  #'   reid_result()'s defaults even when a non-default row_number was used.
  #'
  #' @return a data frame with columns RAW_ROW_NUMBER, ANON_ROW_NUMBER, RAW,
  #'   ANON, DISTANCE and RESULT (logical): exactly one row per ANON record,
  #'   the RAW record closest in `target` by absolute difference, and whether
  #'   that guess was correct.
  #'
  #' @importFrom dplyr group_by
  #' @importFrom dplyr ungroup
  #' @importFrom dplyr filter
  #' @importFrom dplyr mutate
  #' @importFrom dplyr .data
  #' @importFrom magrittr %>%
  #' @export

  raw_target <- paste("RAW_", target, sep = "")
  anon_target <- paste("ANON_", target, sep = "")
  raw_row_number <- paste("RAW_", row_number, sep = "")
  anon_row_number <- paste("ANON_", row_number, sep = "")
  check_raw_anon_columns_exist(dat_raw_anon, c(raw_target, anon_target, raw_row_number, anon_row_number), "reid_by_num")

  dat_raw_anon %>%
    dplyr::select(RAW_ROW_NUMBER = dplyr::all_of(raw_row_number), ANON_ROW_NUMBER = dplyr::all_of(anon_row_number), RAW = dplyr::all_of(raw_target), ANON = dplyr::all_of(anon_target)) %>%
    dplyr::mutate(DISTANCE = abs(.data$RAW - .data$ANON)) %>%
    resolve_min_distance_ties() %>%
    dplyr::mutate(RESULT = (.data$ANON_ROW_NUMBER == .data$RAW_ROW_NUMBER)) %>%
    return()
}

reid_result <- function(dat_reid_result,
                        raw_row_number = "RAW_ROW_NUMBER", anon_row_number = "ANON_ROW_NUMBER", result = "RESULT",
                        method = NULL) {
  #' craete text of reidentify result ( method: ******, success/trial : ***** / ******)
  #'
  #' @param dat_reid_result reid result data frame (RAW_ROW_NUMBER, ANON_ANON_NUMBER, RESULT)
  #' @param anon_row_number column name of row number in ANON data
  #' @param raw_row_number column name of row number in RAW data
  #' @param result true or false
  #' @param method reid method name
  #'
  #' @return a character scalar of the form
  #'   `" method: <method> , success / trial :  <success> / <trial> "`,
  #'   where `trial` is the number of rows in `dat_reid_result` and
  #'   `success` is the number of TRUE values in its `result` column;
  #'   `success` is always <= `trial`.
  #'
  #' @importFrom magrittr %>%
  #' @export

  ## defensive check (phase 3): reid_by_dist() previously had no tie-handling
  ## step, so a single ANON record could show up as several rows and
  ## silently inflate `trial` while under-reporting the reidentification
  ## rate -- the worst possible failure mode for a safety-checking tool. If
  ## that regresses again (here or in any caller-supplied data frame), stop
  ## instead of quietly reporting a wrong percentage.
  anon_row_number_vec <- dat_reid_result[[anon_row_number]]
  if (anyDuplicated(anon_row_number_vec) > 0) {
    stop(
      "reid_result(): column \"", anon_row_number, "\" has duplicate values. ",
      "Each ANON record must contribute exactly one row after tie-breaking; ",
      "duplicates would inflate 'trial' and under-report the ",
      "reidentification rate. Check the reid_by_*() tie-handling that ",
      "produced this data frame.",
      call. = FALSE
    )
  }

  result_vec <- dat_reid_result[[result]]

  trial <- length(result_vec)
  success <- sum(result_vec)

  result_text <- paste(" method:", method, ", success / trial : ", success, "/", trial, sep = " ") %>% return()
}




reid_by_char <- function(dat_raw_anon, target, row_number = "ROW_NUMBER") {
  #' reidentify by character static column
  #'
  #' @param dat_raw_anon dataframe of raw_anon form
  #' @param target target column
  #' @param row_number name of the row-number column *before* the RAW_/ANON_
  #'   prefixing done by join_raw_anon_data() (default: "ROW_NUMBER"), i.e.
  #'   dat_raw_anon is expected to contain columns
  #'   paste0("RAW_", row_number) / paste0("ANON_", row_number). The output
  #'   always names these two columns RAW_ROW_NUMBER / ANON_ROW_NUMBER
  #'   regardless of `row_number`, so it can be passed straight into
  #'   reid_result()'s defaults even when a non-default row_number was used.
  #'
  #' @return a data frame with columns RAW_ROW_NUMBER, ANON_ROW_NUMBER,
  #'   DISTANCE and RESULT (logical): exactly one row per ANON record, the
  #'   RAW record closest in `target` by (Levenshtein) edit distance, and
  #'   whether that guess was correct.
  #'
  #' @importFrom dplyr group_by
  #' @importFrom dplyr ungroup
  #' @importFrom dplyr filter
  #' @importFrom dplyr mutate
  #' @importFrom dplyr .data
  #' @importFrom magrittr %>%
  #' @importFrom utils adist
  #' @export

  raw_target <- paste("RAW_", target, sep = "")
  anon_target <- paste("ANON_", target, sep = "")
  raw_row_number <- paste("RAW_", row_number, sep = "")
  anon_row_number <- paste("ANON_", row_number, sep = "")
  check_raw_anon_columns_exist(dat_raw_anon, c(raw_target, anon_target, raw_row_number, anon_row_number), "reid_by_char")

  raw_target_col <- as.character(dat_raw_anon[[raw_target]])
  anon_target_col <- as.character(dat_raw_anon[[anon_target]])

  vec_distance <- mapply(
    FUN = function(x, y) {
      return(adist(x, y)[[1]])
    },
    anon_target_col, raw_target_col
  )

  dat_raw_anon$DISTANCE <- vec_distance

  dat_raw_anon %>%
    dplyr::mutate(RAW_ROW_NUMBER = .data[[raw_row_number]], ANON_ROW_NUMBER = .data[[anon_row_number]], DISTANCE = .data$DISTANCE) %>%
    dplyr::mutate(RESULT = (.data$RAW_ROW_NUMBER == .data$ANON_ROW_NUMBER)) %>%
    resolve_min_distance_ties() %>%
    return()
}

reid_by_dist <- function(dat_raw_anon, target, row_number = "ROW_NUMBER", split = ":") {
  #' reidentify by distribution column (list A, B, C is expressed by "A:B:C")
  #'
  #' @param dat_raw_anon dataframe of raw_anon form
  #' @param target target column
  #' @param row_number name of the row-number column *before* the RAW_/ANON_
  #'   prefixing done by join_raw_anon_data() (default: "ROW_NUMBER"), i.e.
  #'   dat_raw_anon is expected to contain columns
  #'   paste0("RAW_", row_number) / paste0("ANON_", row_number). The output
  #'   always names these two columns RAW_ROW_NUMBER / ANON_ROW_NUMBER
  #'   regardless of `row_number`, so it can be passed straight into
  #'   reid_result()'s defaults even when a non-default row_number was used.
  #' @param split character for split _DIST value (default: ":")
  #'
  #' @return a data frame with columns RAW_ROW_NUMBER, ANON_ROW_NUMBER,
  #'   DISTANCE and RESULT (logical): exactly one row per ANON record, the
  #'   RAW record closest in `target` by distribution distance, and whether
  #'   that guess was correct.
  #'
  #' @importFrom dplyr group_by
  #' @importFrom dplyr ungroup
  #' @importFrom dplyr filter
  #' @importFrom dplyr mutate
  #' @importFrom dplyr .data
  #' @importFrom magrittr %>%
  #' @export
  #
  raw_target <- paste("RAW_", target, sep = "")
  anon_target <- paste("ANON_", target, sep = "")
  raw_row_number <- paste("RAW_", row_number, sep = "")
  anon_row_number <- paste("ANON_", row_number, sep = "")
  check_raw_anon_columns_exist(dat_raw_anon, c(raw_target, anon_target, raw_row_number, anon_row_number), "reid_by_dist")

  raw_target_col <- as.character(dat_raw_anon[[raw_target]])
  anon_target_col <- as.character(dat_raw_anon[[anon_target]])

  ## calc distribution distance
  # distance = mapply(FUN = calc_KL, raw_target_col, anon_target_col)
  distance <- mapply(FUN = distribution_distance, raw_target_col, anon_target_col)

  dat_raw_anon$DISTANCE <- distance

  dat_raw_anon %>%
    dplyr::select(RAW_ROW_NUMBER = dplyr::all_of(raw_row_number), ANON_ROW_NUMBER = dplyr::all_of(anon_row_number), "DISTANCE") %>%
    resolve_min_distance_ties() %>%
    dplyr::mutate(RESULT = (.data$ANON_ROW_NUMBER == .data$RAW_ROW_NUMBER)) %>%
    return()
}

parse_dist_values <- function(str, split, side) {
  #' parse a "A:B:C" style distribution string into a numeric vector,
  #' stopping with a clear error instead of silently returning NA when an
  #' element cannot be interpreted as a number (phase 3 fix for the
  #' distribution_distance()/calc_KL() defect where a non-numeric target
  #' column produced all-NA distances and reid_result() quietly reported
  #' "0 / 0", which reads as "could not be reidentified = safe").
  #'
  #' Distinguishes, where possible, a genuinely non-numeric element (R's
  #' `as.numeric()` raises "NAs introduced by coercion") from an explicit
  #' missing value already encoded in the data (e.g. an "NA" token, which
  #' `as.numeric()` parses cleanly to NA with no warning).
  #'
  #' @param str character scalar, e.g. "1:2:3"
  #' @param split split character (default ":")
  #' @param side label used in the error message ("x" or "y")
  #'
  #' @return numeric vector parsed from `str`; stops with an error instead of
  #'   returning NA-containing output.
  #'
  #' @keywords internal

  parts <- strsplit(str, split = split)[[1]]

  had_coercion_warning <- FALSE
  values <- withCallingHandlers(
    as.numeric(parts),
    warning = function(w) {
      if (grepl("NAs introduced by coercion", conditionMessage(w), fixed = TRUE)) {
        had_coercion_warning <<- TRUE
        invokeRestart("muffleWarning")
      }
    }
  )

  if (had_coercion_warning) {
    bad <- unique(parts[is.na(values)])
    stop(
      "distribution_distance()/calc_KL(): could not convert the ", side,
      " value \"", str, "\" to numeric after splitting on \"", split, "\" ",
      "(non-numeric element(s): ", paste(bad, collapse = ", "), "). ",
      "This function requires a numeric distribution column (e.g. \"1:2:3\"); ",
      "a character/categorical column was passed instead.",
      call. = FALSE
    )
  }

  if (anyNA(values)) {
    stop(
      "distribution_distance()/calc_KL(): the ", side, " value \"", str, "\" ",
      "contains a missing element after numeric conversion (e.g. an explicit ",
      "\"NA\" token); cannot compute a distribution distance when values are ",
      "missing.",
      call. = FALSE
    )
  }

  values
}

calc_KL <- function(x, y, split = ":") {
  #' calc KL divergence from 2 character vector which has distribution expression (A:B:C:...)
  #'
  #' @param x vector
  #' @param y vector
  #' @param split split (default: ":")
  #'
  #' @return numeric scalar, the KL divergence between the normalized
  #'   distributions parsed from `x` and `y`.
  #'
  #' @keywords internal
  #'
  #' @importFrom philentropy KL
  #' @importFrom magrittr %>%
  #'
  ## normalize vector
  x_list <- parse_dist_values(x, split, "x")
  x_list <- x_list / max(x_list)
  y_list <- parse_dist_values(y, split, "y")
  y_list <- y_list / max(y_list)
  dat <- rbind(x_list, y_list)

  philentropy::KL(dat) %>% return()
}

distribution_distance <- function(x, y, split = ":") {
  #' calculate distribution distance (by using L2 norm) from 2 character vector which has distribution expression (A:B:C:...)
  #'
  #' @param x vector
  #' @param y vector
  #' @param split split (default: ":")
  #'
  #' @return numeric scalar, the sum of squared differences (L2 norm) between
  #'   the (length-matched) numeric vectors parsed from `x` and `y`.
  #'
  #' @keywords internal
  #'
  #' @importFrom magrittr %>%


  x_list <- parse_dist_values(x, split, "x")
  y_list <- parse_dist_values(y, split, "y")

  ## match the length of 2 vector
  x_length <- length(x_list)
  y_length <- length(y_list)
  diff_x_y <- x_length - y_length

  ## fill by mean value
  if (diff_x_y == 0) {

  } else if (diff_x_y > 0) {
    y_list <- c(y_list, rep(mean(y_list), diff_x_y)) %>% sort()
  } else {
    x_list <- c(x_list, rep(mean(x_list), -1 * diff_x_y)) %>% sort()
  }

  ## calc distance
  ## (written without the magrittr `.` placeholder: `. ** 2` triggers an
  ## R CMD check "no visible binding for global variable '.'" NOTE)
  distance <- sum((x_list - y_list)^2)
  distance %>% return()
}

reid_by_num_rank <- function(dat_raw_anon, target, row_number = "ROW_NUMBER") {
  #' reidentify by single num static by using rank
  #'
  #' Rank ties are resolved deterministically (see `rank(..., ties.method =
  #' "min")` below): tied values receive the *same* rank rather than an
  #' arbitrary distinct one, so the same input always yields the same
  #' output. Any residual ambiguity this creates (several RAW candidates at
  #' DISTANCE == 0 for one ANON record) is resolved, as for every other
  #' reid_by_*() function, by resolve_min_distance_ties()'s "first
  #' RAW_ROW_NUMBER encountered" rule.
  #'
  #' @param dat_raw_anon dataframe of raw_anon form
  #' @param target target column
  #' @param row_number name of the row-number column *before* the RAW_/ANON_
  #'   prefixing done by join_raw_anon_data() (default: "ROW_NUMBER"), i.e.
  #'   dat_raw_anon is expected to contain columns
  #'   paste0("RAW_", row_number) / paste0("ANON_", row_number). The output
  #'   always names these two columns RAW_ROW_NUMBER / ANON_ROW_NUMBER
  #'   regardless of `row_number`, so it can be passed straight into
  #'   reid_result()'s defaults even when a non-default row_number was used.
  #'
  #' @return a data frame with columns ANON_ROW_NUMBER, RAW_ROW_NUMBER, the
  #'   raw `target` columns, ANON_RANK, RAW_RANK, DISTANCE and RESULT
  #'   (logical): exactly one row per ANON record, the RAW record closest in
  #'   rank of `target`, and whether that guess was correct.
  #'
  #' @importFrom dplyr group_by
  #' @importFrom dplyr ungroup
  #' @importFrom dplyr filter
  #' @importFrom dplyr mutate
  #' @importFrom dplyr .data
  #' @importFrom magrittr %>%
  #' @importFrom magrittr %<>%
  #' @export

  raw_target <- paste("RAW_", target, sep = "")
  anon_target <- paste("ANON_", target, sep = "")
  raw_row_number <- paste("RAW_", row_number, sep = "")
  anon_row_number <- paste("ANON_", row_number, sep = "")
  check_raw_anon_columns_exist(dat_raw_anon, c(raw_target, anon_target, raw_row_number, anon_row_number), "reid_by_num_rank")

  ## rank(..., na.last = TRUE) (the default) does NOT propagate NA: it
  ## silently assigns missing values a real, high rank instead of erroring
  ## or returning NA, which would let an ANON/RAW record with a genuinely
  ## missing target value be reported as a confident (even DISTANCE == 0)
  ## reidentification match. Stop instead of letting that happen silently.
  if (anyNA(dat_raw_anon[[anon_target]]) || anyNA(dat_raw_anon[[raw_target]])) {
    stop(
      "reid_by_num_rank(): target column \"", target, "\" contains NA/missing ",
      "values in RAW and/or ANON. rank(..., na.last = TRUE) would silently ",
      "assign missing values a real rank instead of erroring, which could ",
      "report a false reidentification match. Remove or explicitly handle ",
      "missing values before calling reid_by_num_rank().",
      call. = FALSE
    )
  }

  ## check the rank
  ## ties.method = "min": deterministic (fixes the reid_by_num_rank()
  ## non-determinism defect -- "random" gave a different result every run
  ## for tie-heavy columns) and, more importantly, semantically correct for
  ## a reidentification-risk tool: genuinely tied values are indistinguishable
  ## in the data, so they should collapse to the same rank instead of being
  ## arbitrarily split into a fake total order by incidental row position.
  dat_anon_rank <-
    dat_raw_anon %>%
    dplyr::select(dplyr::all_of(c(anon_row_number, anon_target))) %>%
    dplyr::distinct()
  dat_anon_rank$ANON_RANK <- rank(dat_anon_rank[[anon_target]], ties.method = "min")
  dat_anon_rank %<>%
    dplyr::select(dplyr::all_of(anon_row_number), "ANON_RANK")

  dat_raw_rank <-
    dat_raw_anon %>%
    dplyr::select(dplyr::all_of(c(raw_row_number, raw_target))) %>%
    dplyr::distinct()
  dat_raw_rank$RAW_RANK <- rank(dat_raw_rank[[raw_target]], ties.method = "min")
  dat_raw_rank %<>%
    dplyr::select(dplyr::all_of(raw_row_number), "RAW_RANK")

  dat_raw_anon %>%
    dplyr::inner_join(dat_raw_rank, by = raw_row_number) %>%
    dplyr::inner_join(dat_anon_rank, by = anon_row_number) %>%
    dplyr::mutate(RAW_ROW_NUMBER = .data[[raw_row_number]], ANON_ROW_NUMBER = .data[[anon_row_number]]) %>%
    dplyr::mutate(DISTANCE = abs(.data$ANON_RANK - .data$RAW_RANK)) %>%
    resolve_min_distance_ties() %>%
    dplyr::mutate(RESULT = (.data$ANON_ROW_NUMBER == .data$RAW_ROW_NUMBER)) %>%
    dplyr::select("ANON_ROW_NUMBER", "RAW_ROW_NUMBER", dplyr::all_of(c(anon_target, raw_target)), "ANON_RANK", "RAW_RANK", "DISTANCE", "RESULT") %>%
    return()
}
