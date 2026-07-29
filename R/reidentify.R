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
#' @keywords internal
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
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
    dplyr::group_by(ANON_ROW_NUMBER) %>%
    dplyr::filter(DISTANCE == min(DISTANCE)) %>%
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
  ## keep the first occurrence of each ANON record. Shuffling the whole table
  ## and taking first-per-group draws one candidate uniformly from each tie
  ## group in a single pass.
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
#' @param row_number row number column name(default: "ROW_NUMBER")
#' @param seed integer seed for the random tie-break among equally distant
#'   candidates, or NULL (default) to use the ambient RNG stream
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @export
reid_by_num <- function(dat_raw_anon, target, row_number = "ROW_NUMBER", seed = NULL) {
  raw_target <- paste("RAW_", target, sep = "")
  anon_target <- paste("ANON_", target, sep = "")
  raw_row_number <- paste("RAW_", row_number, sep = "")
  anon_row_number <- paste("ANON_", row_number, sep = "")

  dat_raw_anon %>%
    dplyr::select(RAW_ROW_NUMBER = RAW_ROW_NUMBER, ANON_ROW_NUMBER = ANON_ROW_NUMBER, RAW = dplyr::all_of(raw_target), ANON = dplyr::all_of(anon_target)) %>%
    dplyr::mutate(DISTANCE = abs(RAW - ANON)) %>%
    resolve_min_distance_ties(seed = seed) %>%
    dplyr::mutate(RESULT = (ANON_ROW_NUMBER == RAW_ROW_NUMBER)) %>%
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
#' @param row_number row number column name(default: "ROW_NUMBER")
#' @param seed integer seed for the random tie-break among equally distant
#'   candidates, or NULL (default) to use the ambient RNG stream
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom utils adist
#' @export
reid_by_char <- function(dat_raw_anon, target, row_number = "ROW_NUMBER", seed = NULL) {
  raw_target <- paste("RAW_", target, sep = "")
  anon_target <- paste("ANON_", target, sep = "")
  raw_row_number <- paste("RAW_", row_number, sep = "")
  anon_row_number <- paste("ANON_", row_number, sep = "")

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
    dplyr::mutate(RAW_ROW_NUMBER = `RAW_ROW_NUMBER`, ANON_ROW_NUMBER = `ANON_ROW_NUMBER`, DISTANCE) %>%
    dplyr::mutate(RESULT = (RAW_ROW_NUMBER == ANON_ROW_NUMBER)) %>%
    resolve_min_distance_ties(seed = seed) %>%
    return()
}

#' reidentify by distribution column (list A, B, C is expressed by "A:B:C")
#'
#' @param dat_raw_anon dataframe of raw_anon form
#' @param target target column
#' @param row_number row number column name(default: "ROW_NUMBER")
#' @param split character for split _DIST value (default: ":")
#' @param seed integer seed for the random tie-break among equally distant
#'   candidates, or NULL (default) to use the ambient RNG stream
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @export
reid_by_dist <- function(dat_raw_anon, target, row_number = "ROW_NUMBER", split = ":", seed = NULL) {
  #
  raw_target <- paste("RAW_", target, sep = "")
  anon_target <- paste("ANON_", target, sep = "")
  raw_row_number <- paste("RAW_", row_number, sep = "")
  anon_row_number <- paste("ANON_", row_number, sep = "")

  raw_target_col <- as.character(dat_raw_anon[[raw_target]])
  anon_target_col <- as.character(dat_raw_anon[[anon_target]])

  ## calc distribution distance
  # distance = mapply(FUN = calc_KL, raw_target_col, anon_target_col)
  distance <- mapply(FUN = distribution_distance, raw_target_col, anon_target_col)

  dat_raw_anon$DISTANCE <- distance

  dat_raw_anon %>%
    dplyr::select(RAW_ROW_NUMBER = RAW_ROW_NUMBER, ANON_ROW_NUMBER = ANON_ROW_NUMBER, DISTANCE) %>%
    resolve_min_distance_ties(seed = seed) %>%
    dplyr::mutate(RESULT = (ANON_ROW_NUMBER == RAW_ROW_NUMBER)) %>%
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

#' Kullback-Leibler divergence D(x || y) between two distributions written
#' as "A:B:C:..." strings
#'
#' Both inputs are read as unnormalised counts/weights over a shared, ordered
#' support and are converted to probability vectors by dividing by their sum.
#' The result is in bits (log base 2), matching `philentropy::KL()`.
#'
#' This function previously normalised by `max()` rather than `sum()`, so it
#' was fed vectors that were not probability distributions. The consequences
#' were not cosmetic:
#'
#' - the returned value could be negative, which a KL divergence never is
#'   (11 of 50 random pairs came out negative, over a range of
#'   [-1.5949, 4.6112]);
#' - `x = "1:2:3:4"`, `y = "2:2:2:2"` returned -1.311278 where the true
#'   divergence is 0.1535607 bits;
#' - it changed which candidate looked closest. Over 8 candidates the rank
#'   correlation against the correct value was only 0.79 and the argmin
#'   differed, so nearest-neighbour matching picked a different record.
#'
#' `philentropy::KL()` does not warn when handed vectors that do not sum to
#' 1, so none of this surfaced at the call site.
#'
#' Zero entries need care: any outcome with `y_i == 0 < x_i` makes the true
#' divergence infinite. `epsilon` is handed to `philentropy::KL()`, which
#' substitutes it for a zero denominator, keeping the result finite and
#' large. The default matches `philentropy`'s own. Pass `epsilon = 0` to
#' disable the guard and get the mathematically exact `Inf`.
#'
#' For comparing distributions of differing length or unequal support, this
#' is the wrong tool -- KL needs a shared support. Prefer the quantile-vector
#' distance in `distribution_distance()`, or the Wasserstein distance planned
#' in #19.
#'
#' @param x vector
#' @param y vector
#' @param split split (default: ":")
#' @param epsilon substituted for a zero denominator so the divergence stays
#'   finite (default 1e-05, as in `philentropy::KL()`); use 0 to allow `Inf`
#'
#' @return the KL divergence D(x || y) in bits
#'
#' @importFrom philentropy KL
#' @importFrom magrittr %>%
calc_KL <- function(x, y, split = ":", epsilon = 1e-05) {
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

  if (sum(x_list) <= 0 || sum(y_list) <= 0) {
    stop(
      "calc_KL(): x and y must each contain at least one positive value; ",
      "an all-zero vector cannot be normalised into a probability ",
      "distribution.",
      call. = FALSE
    )
  }

  ## Normalise by the SUM so that both rows are genuine probability vectors.
  ## This is the whole fix: the previous code divided by max(), which does not
  ## produce a distribution and let the "divergence" go negative.
  p <- x_list / sum(x_list)
  q <- y_list / sum(y_list)

  ## philentropy::KL() substitutes `epsilon` for a zero denominator. Note it
  ## defaults to 1e-05 and applies that guard whether or not the caller asks,
  ## so pass the value through explicitly rather than leaving it implicit.
  philentropy::KL(rbind(p, q), epsilon = epsilon) %>% return()
}

#' calculate distribution distance from 2 character vectors which hold a
#' distribution expression (A:B:C:...)
#'
#' Both distributions are reduced to a fixed-length vector of evenly spaced
#' quantiles, and the distance is the squared L2 distance between those two
#' vectors. The result therefore depends only on the *shape* of each
#' distribution, not on how many observations it contains.
#'
#' This replaces an earlier approach that padded the shorter side with its
#' own mean and subtracted element-wise, which had two defects:
#'
#' 1. the number of padded elements -- i.e. the difference in record counts
#'    -- leaked directly into the distance. Across samples drawn from a
#'    single population (identical shape, differing n) the old distance
#'    correlated with the count difference at r = 0.99. Record count is a
#'    separate signal and belongs in its own score (see #22), not smuggled
#'    into a distribution distance.
#' 2. only the padded side was sorted, so two equal-length inputs were
#'    compared in whatever order they happened to arrive:
#'    `distribution_distance("3:1:2", "1:2:3")` returned 6 for what are two
#'    identical multisets.
#'
#' `quantile()` sorts internally and always yields `n_quantiles` values, so
#' both defects are removed by construction.
#'
#' @param x vector
#' @param y vector
#' @param split split (default: ":")
#' @param n_quantiles number of evenly spaced quantiles used to represent
#'   each distribution (default 10). The returned distance is a sum over
#'   these points, so it scales with `n_quantiles`; compare only distances
#'   computed with the same value.
#'
#' @return squared L2 distance between the two quantile vectors
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

  probs <- seq(0, 1, length.out = n_quantiles)
  q_x <- stats::quantile(x_list, probs = probs, names = FALSE, type = 7)
  q_y <- stats::quantile(y_list, probs = probs, names = FALSE, type = 7)

  distance <- sum((q_x - q_y)^2)
  distance %>% return()
}

#' reidentify by single num static by using rank
#'
#' @param dat_raw_anon dataframe of raw_anon form
#' @param target target column
#' @param row_number row number column name(default: "ROW_NUMBER")
#' @param seed integer seed for the random tie-break among equally distant
#'   candidates, or NULL (default) to use the ambient RNG stream
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
reid_by_num_rank <- function(dat_raw_anon, target, row_number = "ROW_NUMBER", seed = NULL) {
  raw_target <- paste("RAW_", target, sep = "")
  anon_target <- paste("ANON_", target, sep = "")
  raw_row_number <- paste("RAW_", row_number, sep = "")
  anon_row_number <- paste("ANON_", row_number, sep = "")

  ## This function has a second source of randomness besides the distance
  ## tie-break: rank(ties.method = "random") breaks rank ties at random too.
  ## Both must fall under `seed`, otherwise the same seed still gives
  ## different answers. Seed once for the whole body and let the tie-break
  ## draw from that same (now deterministic) stream.
  with_local_seed(seed, {
    ## check the rank
    dat_anon_rank <-
      dat_raw_anon %>%
      dplyr::select(dplyr::all_of(c(anon_row_number, anon_target))) %>%
      dplyr::distinct()
    dat_anon_rank$ANON_RANK <- rank(dat_anon_rank[[anon_target]], ties.method = "random")
    dat_anon_rank %<>%
      dplyr::select(dplyr::all_of(anon_row_number), ANON_RANK)

    dat_raw_rank <-
      dat_raw_anon %>%
      dplyr::select(dplyr::all_of(c(raw_row_number, raw_target))) %>%
      dplyr::distinct()
    dat_raw_rank$RAW_RANK <- rank(dat_raw_rank[[raw_target]], ties.method = "random")
    dat_raw_rank %<>%
      dplyr::select(dplyr::all_of(raw_row_number), RAW_RANK)

    dat_raw_anon %>%
      dplyr::inner_join(dat_raw_rank, by = raw_row_number) %>%
      dplyr::inner_join(dat_anon_rank, by = anon_row_number) %>%
      dplyr::mutate(DISTANCE = abs(ANON_RANK - RAW_RANK)) %>%
      resolve_min_distance_ties(seed = NULL) %>%
      dplyr::mutate(RESULT = (ANON_ROW_NUMBER == RAW_ROW_NUMBER)) %>%
      dplyr::select(ANON_ROW_NUMBER, RAW_ROW_NUMBER, dplyr::all_of(c(anon_target, raw_target)), ANON_RANK, RAW_RANK, DISTANCE, RESULT)
  })
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
