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
check_raw_anon_columns_exist <- function(dat_raw_anon, cols, fn_name) {
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

#' evaluate `code` with the RNG seeded to `seed`, restoring the caller's
#' RNG state afterwards
#'
#' `seed = NULL` runs `code` against the ambient RNG stream unchanged, so a
#' caller can still get reproducibility with a plain `set.seed()` before the
#' call. Any other value makes the call self-contained and repeatable without
#' perturbing the caller's stream.
#'
#' @param seed integer seed, or NULL to use the ambient RNG stream
#' @param code expression to evaluate
#'
#' @return the value of `code`
#'
#' @keywords internal
with_local_seed <- function(seed, code) {
  if (is.null(seed)) {
    return(code)
  }

  has_old <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (has_old) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(
      suppressWarnings(rm(".Random.seed", envir = globalenv())),
      add = TRUE
    )
  }

  set.seed(seed)
  code
}

#' pick exactly one RAW record per ANON record from a RAW/ANON candidate
#' table that has a DISTANCE column, keeping only the row(s) whose
#' DISTANCE is minimal within each ANON_ROW_NUMBER group and then, if more
#' than one RAW record is still tied for the minimum, picking one of the
#' tied candidates uniformly at random. Shared by reid_by_num(),
#' reid_by_char(), reid_by_dist() and reid_by_num_rank() so their
#' tie-breaking logic cannot drift out of sync.
#'
#' Tie-breaking used to keep `RAW_ROW_NUMBER[1]`, i.e. whichever tied
#' candidate happened to come first in the input. That made the reported
#' success rate depend on the row order of a cross join, which is not a
#' property of the data: on a 50-person fixture, reshuffling the input rows
#' moved the rate over [0.02, 0.14] around a mean of 0.058. It also
#' concentrated every success onto the first record of each tie group, which
#' systematically distorts per-record risk even when the overall mean is
#' unaffected. Random tie-breaking makes the estimator unbiased per record
#' and lets the run-to-run spread be measured (see reid_stability()).
#'
#' Also guards against silently reporting an empty/short result: if DISTANCE
#' is NA for every row, or if some ANON_ROW_NUMBER ends up with zero rows
#' after tie-breaking (which happens when every candidate DISTANCE for that
#' ANON record is NA), this stops with an error instead of quietly
#' shrinking the result (and thus the reid_result() trial count).
#'
#' @param dat_with_distance data frame with (at least) RAW_ROW_NUMBER,
#'   ANON_ROW_NUMBER and DISTANCE columns
#' @param seed integer seed for the random tie-break, or NULL (default) to
#'   use the ambient RNG stream
#'
#' @return `dat_with_distance`, filtered down to exactly one row per
#'   ANON_ROW_NUMBER (a uniformly chosen minimal-DISTANCE RAW candidate),
#'   ordered by ANON_ROW_NUMBER.
#'
#' @keywords internal
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr .data
#' @importFrom magrittr %>%
resolve_min_distance_ties <- function(dat_with_distance, seed = NULL) {
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

  dat_min <-
    dat_with_distance %>%
    dplyr::group_by(.data$ANON_ROW_NUMBER) %>%
    dplyr::filter(.data$DISTANCE == min(.data$DISTANCE)) %>%
    dplyr::ungroup()

  ## Put the surviving candidates into a canonical order first, so that the
  ## draw depends only on the data and the seed -- never on the row order of
  ## the input. Without this, a fixed seed applied to the same data in a
  ## different row order still yields a different pick, which is exactly the
  ## input-order sensitivity this change is meant to remove.
  dat_min <- dat_min[
    order(dat_min$ANON_ROW_NUMBER, dat_min$RAW_ROW_NUMBER), ,
    drop = FALSE
  ]

  ## Break ties uniformly at random: shuffle every surviving candidate, then
  ## keep the first occurrence of each ANON record.
  dat_result <- with_local_seed(seed, {
    shuffled <- dat_min[sample.int(nrow(dat_min)), , drop = FALSE]
    shuffled[!duplicated(shuffled$ANON_ROW_NUMBER), , drop = FALSE]
  })

  ## Restore a deterministic output order so that only the *choice* among
  ## tied candidates is random, never the row order of the result.
  dat_result <- dat_result[order(dat_result$ANON_ROW_NUMBER), , drop = FALSE]

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
#' @param seed integer seed for the random tie-break among equally distant
#'   candidates (default 0L, so a plain call is reproducible). Pass a
#'   different value, or use reid_stability(), to see the run-to-run
#'   spread. NULL uses the ambient RNG stream instead.
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
reid_by_num <- function(dat_raw_anon, target, row_number = "ROW_NUMBER", seed = 0L) {
  raw_target <- paste("RAW_", target, sep = "")
  anon_target <- paste("ANON_", target, sep = "")
  raw_row_number <- paste("RAW_", row_number, sep = "")
  anon_row_number <- paste("ANON_", row_number, sep = "")
  check_raw_anon_columns_exist(dat_raw_anon, c(raw_target, anon_target, raw_row_number, anon_row_number), "reid_by_num")

  dat_raw_anon %>%
    dplyr::select(RAW_ROW_NUMBER = dplyr::all_of(raw_row_number), ANON_ROW_NUMBER = dplyr::all_of(anon_row_number), RAW = dplyr::all_of(raw_target), ANON = dplyr::all_of(anon_target)) %>%
    dplyr::mutate(DISTANCE = abs(.data$RAW - .data$ANON)) %>%
    resolve_min_distance_ties(seed = seed) %>%
    dplyr::mutate(RESULT = (.data$ANON_ROW_NUMBER == .data$RAW_ROW_NUMBER)) %>%
    return()
}

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
reid_result <- function(dat_reid_result,
                        raw_row_number = "RAW_ROW_NUMBER", anon_row_number = "ANON_ROW_NUMBER", result = "RESULT",
                        method = NULL) {
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
#' @param seed integer seed for the random tie-break among equally distant
#'   candidates (default 0L, so a plain call is reproducible). Pass a
#'   different value, or use reid_stability(), to see the run-to-run
#'   spread. NULL uses the ambient RNG stream instead.
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
reid_by_char <- function(dat_raw_anon, target, row_number = "ROW_NUMBER", seed = 0L) {
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
    resolve_min_distance_ties(seed = seed) %>%
    return()
}

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
#' @param seed integer seed for the random tie-break among equally distant
#'   candidates (default 0L, so a plain call is reproducible). Pass a
#'   different value, or use reid_stability(), to see the run-to-run
#'   spread. NULL uses the ambient RNG stream instead.
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
reid_by_dist <- function(dat_raw_anon, target, row_number = "ROW_NUMBER", split = ":", seed = 0L) {
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
    resolve_min_distance_ties(seed = seed) %>%
    dplyr::mutate(RESULT = (.data$ANON_ROW_NUMBER == .data$RAW_ROW_NUMBER)) %>%
    return()
}

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
parse_dist_values <- function(str, split, side) {
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

#' calc KL divergence from 2 character vectors which have distribution expression (A:B:C:...)
#'
#' Both `x` and `y` are parsed to numeric vectors and normalized to sum to
#' 1 (a true probability distribution) before being handed to
#' `philentropy::KL()`. Earlier this normalized by dividing by the
#' *maximum* element instead of the *sum*, which does not produce a
#' distribution that sums to 1 -- the KL divergence formula is only
#' guaranteed non-negative, and only 0 for identical inputs, when both
#' inputs are genuine probability distributions. With the max-based
#' normalization it could (and did) return negative values, e.g.
#' `calc_KL("1:1:10", "1:1:1")` used to return approximately -0.664 (see
#' phase 6 investigation); with sum-based normalization the KL divergence
#' is always >= 0, and is exactly 0 when `x` and `y` describe the same
#' distribution.
#'
#' Zero elements: if some but not all elements of a (sum-normalized)
#' distribution are exactly 0, a literal KL divergence formula would
#' involve `log(0)`. `philentropy::KL()` avoids this itself: by default it
#' substitutes a small `epsilon` (1e-05) for zero entries before taking
#' logs, so a distribution with some zero elements still yields a finite
#' (rather than `NaN`/`Inf`) result; this function relies on that built-in
#' behavior rather than re-implementing its own epsilon handling. The
#' degenerate case where *every* element of `x` or `y` is 0 (sum is 0, so
#' the `/ sum(...)` normalization itself is 0/0) is rejected with an
#' explicit error instead of silently producing `NaN`, consistent with how
#' `parse_dist_values()` already refuses other degenerate inputs elsewhere
#' in this file.
#'
#' `philentropy::KL()` also prints an informational message
#' ("Metric: 'kullback-leibler' with unit: 'log2'; ...") to the console on
#' every call. Since this is an internal helper (not part of the public
#' API), that message is suppressed here so it cannot leak into a caller's
#' console output.
#'
#' `x` and `y` must describe the same support (same number of elements).
#' Previously a length mismatch was passed straight to `rbind()`, which
#' silently recycled the shorter vector and so compared the wrong outcomes
#' against each other. Negative inputs are rejected for the same reason: they
#' cannot be normalised into a probability distribution. To compare
#' distributions of differing length, use `distribution_distance()`.
#'
#' @param x vector
#' @param y vector
#' @param split split (default: ":")
#' @param epsilon substituted by `philentropy::KL()` for a zero denominator so
#'   the divergence stays finite (default 1e-05, philentropy's own default);
#'   pass 0 to allow the mathematically exact `Inf`. Note philentropy applies
#'   this guard whether or not the caller asks, so it is passed explicitly
#'   rather than left implicit.
#'
#' @return numeric scalar >= 0, the KL divergence between the
#'   sum-normalized distributions parsed from `x` and `y`; 0 when `x` and
#'   `y` describe the same distribution.
#'
#' @keywords internal
#'
#' @importFrom philentropy KL
#' @importFrom magrittr %>%
calc_KL <- function(x, y, split = ":", epsilon = 1e-05) {
  ## normalize vector to a true probability distribution (sums to 1)
  x_list <- parse_dist_values(x, split, "x")
  y_list <- parse_dist_values(y, split, "y")

  ## KL is only defined over a shared support. rbind() would silently recycle
  ## the shorter vector, quietly comparing the wrong outcomes against each
  ## other, so reject the case outright.
  if (length(x_list) != length(y_list)) {
    stop(
      "calc_KL(): x and y must describe the same support, but have ",
      length(x_list), " and ", length(y_list), " elements. KL divergence is ",
      "not defined between distributions over different supports; use ",
      "distribution_distance() to compare distributions of differing length.",
      call. = FALSE
    )
  }

  if (any(x_list < 0) || any(y_list < 0)) {
    stop(
      "calc_KL(): x and y must be non-negative counts or weights; ",
      "negative values cannot be normalised into a probability distribution.",
      call. = FALSE
    )
  }

  x_sum <- sum(x_list)
  y_sum <- sum(y_list)
  if (x_sum == 0 || y_sum == 0) {
    stop(
      "calc_KL(): cannot normalize a distribution whose elements are all ",
      "zero (x sum = ", x_sum, ", y sum = ", y_sum, "); a KL divergence is ",
      "undefined for a distribution with no probability mass.",
      call. = FALSE
    )
  }
  x_list <- x_list / x_sum
  y_list <- y_list / y_sum
  dat <- rbind(x_list, y_list)

  ## suppressMessages(): philentropy::KL() prints "Metric: 'kullback-leibler'
  ## with unit: 'log2'" on every call, which would leak into callers' output.
  ## unname(): it returns a length-1 vector named "kullback-leibler", which is
  ## a surprising thing to hand back as "the KL divergence, a numeric scalar".
  suppressMessages(philentropy::KL(dat, epsilon = epsilon)) %>%
    unname() %>%
    return()
}

#' calculate distribution distance from 2 character vectors which have distribution expression (A:B:C:...)
#'
#' Measures how different the two *shapes* of the underlying per-record
#' values are, independent of how many records each side has. Both sides are
#' reduced to the same fixed-length vector of evenly spaced quantiles, and
#' the distance is the squared L2 distance between those vectors.
#'
#' This replaces an earlier approach that padded the shorter side with its
#' own mean and subtracted element-wise, which had two defects:
#'
#' 1. the number of padded elements -- i.e. the difference in record counts
#'    -- leaked directly into the distance. Record count is a separate signal
#'    and belongs in its own score (see #22), not smuggled into a
#'    distribution distance.
#' 2. only the padded side was sorted, so two equal-length inputs were
#'    compared in whatever order they happened to arrive:
#'    `distribution_distance("3:1:2", "1:2:3")` returned a non-zero distance
#'    for what are two identical multisets.
#'
#' `quantile()` sorts internally and always yields `n_quantiles` values, so
#' both defects are removed by construction.
#'
#' Two candidate fixes were developed in parallel and compared head to head
#' before this one was adopted (see the merge notes on Issue #5):
#'
#' - mean-fill padding with the aggregation changed from sum to mean, which
#'   normalises away the length scaling but leaves the ordering defect and
#'   only reduces the count correlation from 0.61 to 0.37;
#' - this quantile vector, which drives the count correlation to 0.16 and
#'   makes the distance exactly order-invariant.
#'
#' The mean-fill variant initially looked better because it scored a slightly
#' higher reidentification rate under noise. That measurement used a fixture
#' in which every person had the *same* record count in RAW and ANON, so
#' record count was itself a perfect identity signal and any count
#' sensitivity inflated the apparent success. Re-run with record counts that
#' differ between RAW and ANON -- the situation this function exists to
#' handle -- the quantile form reidentifies 16-46% *more* records across
#' noise levels. A stronger attack is the desired direction for a tool whose
#' job is to upper-bound reidentification risk.
#'
#' @param x vector
#' @param y vector
#' @param split split (default: ":")
#' @param n_quantiles number of evenly spaced quantiles used to represent
#'   each distribution (default 10). The returned distance is a sum over
#'   these points, so it scales with `n_quantiles`; compare only distances
#'   computed with the same value.
#'
#' @return numeric scalar >= 0, the squared L2 distance between the two
#'   quantile vectors. Symmetric, and 0 exactly when the two distributions
#'   have the same shape -- including when they hold different numbers of
#'   observations, or list the same values in a different order.
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
#' @importFrom stats quantile
distribution_distance <- function(x, y, split = ":", n_quantiles = 10) {
  x_list <- parse_dist_values(x, split, "x")
  y_list <- parse_dist_values(y, split, "y")

  if (length(n_quantiles) != 1 || is.na(n_quantiles) || n_quantiles < 2) {
    stop(
      "distribution_distance(): `n_quantiles` must be a single number >= 2.",
      call. = FALSE
    )
  }

  ## quantile() sorts internally and always returns n_quantiles values, so the
  ## result depends only on the shape of each distribution -- not on how many
  ## observations it holds, nor on the order they were written in.
  probs <- seq(0, 1, length.out = n_quantiles)
  q_x <- stats::quantile(x_list, probs = probs, names = FALSE, type = 7)
  q_y <- stats::quantile(y_list, probs = probs, names = FALSE, type = 7)

  distance <- sum((q_x - q_y)^2)
  distance %>% return()
}

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
#' @param seed integer seed for the random tie-break among equally distant
#'   candidates (default 0L, so a plain call is reproducible). Pass a
#'   different value, or use reid_stability(), to see the run-to-run
#'   spread. NULL uses the ambient RNG stream instead.
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
reid_by_num_rank <- function(dat_raw_anon, target, row_number = "ROW_NUMBER", seed = 0L) {
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
    resolve_min_distance_ties(seed = seed) %>%
    dplyr::mutate(RESULT = (.data$ANON_ROW_NUMBER == .data$RAW_ROW_NUMBER)) %>%
    dplyr::select("ANON_ROW_NUMBER", "RAW_ROW_NUMBER", dplyr::all_of(c(anon_target, raw_target)), "ANON_RANK", "RAW_RANK", "DISTANCE", "RESULT") %>%
    return()
}

#' run a reid_by_*() attack over several tie-break seeds and summarise the
#' spread of the success rate
#'
#' When a target column has ties, a single run reports one draw from a
#' distribution of possible outcomes, not a fixed property of the data. A
#' point estimate on its own is therefore not interpretable: on a 50-person
#' fixture with a low-cardinality column the rate ranges over [0.02, 0.14]
#' depending only on which tied candidate is picked. This runs the same
#' attack across `seeds` and reports the mean together with the standard
#' deviation, so the uncertainty is visible in the result rather than hidden.
#'
#' A near-zero `sd` means the target column is effectively collision-free and
#' the point estimate can be read directly; a large `sd` means the single-run
#' number should not be quoted without it.
#'
#' @param reid_fn a reid_by_*() function (or its name) taking
#'   `(dat_raw_anon, target, ..., seed)`
#' @param dat_raw_anon dataframe of raw_anon form
#' @param target target column
#' @param seeds integer vector of tie-break seeds (default 1:20)
#' @param ... further arguments passed on to `reid_fn`
#'
#' @return an object of class "reid_stability": a list with `per_seed` (a
#'   data frame of seed / success / trial / rate), and the summary fields
#'   `mean`, `sd`, `min`, `max`, `trial` and `n_seeds`
#'
#' @importFrom stats sd
#' @export
reid_stability <- function(reid_fn, dat_raw_anon, target, seeds = 1:20, ...) {
  reid_fn <- match.fun(reid_fn)

  if (length(seeds) < 2) {
    stop(
      "reid_stability(): need at least 2 seeds to report a standard ",
      "deviation; got ", length(seeds), ".",
      call. = FALSE
    )
  }
  if (anyDuplicated(seeds) > 0) {
    stop("reid_stability(): `seeds` must not contain duplicates.", call. = FALSE)
  }

  rows <- lapply(seeds, function(s) {
    r <- reid_fn(dat_raw_anon, target = target, seed = s, ...)
    data.frame(seed = s, success = sum(r$RESULT), trial = nrow(r))
  })
  per_seed <- do.call(rbind, rows)
  per_seed$rate <- per_seed$success / per_seed$trial

  structure(
    list(
      per_seed = per_seed,
      mean = mean(per_seed$rate),
      sd = stats::sd(per_seed$rate),
      min = min(per_seed$rate),
      max = max(per_seed$rate),
      trial = unique(per_seed$trial),
      n_seeds = length(seeds)
    ),
    class = "reid_stability"
  )
}

#' print a reid_stability summary
#'
#' @param x a "reid_stability" object
#' @param ... ignored
#'
#' @return `x`, invisibly
#'
#' @export
print.reid_stability <- function(x, ...) {
  cat(sprintf(
    "reid stability over %d tie-break seeds (trial = %s)\n",
    x$n_seeds, paste(x$trial, collapse = "/")
  ))
  cat(sprintf(
    "  success rate: mean %.4f  sd %.4f  range [%.4f, %.4f]\n",
    x$mean, x$sd, x$min, x$max
  ))
  invisible(x)
}
