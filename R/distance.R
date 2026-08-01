## ---------------------------------------------------------------------------
## Distribution-distance primitives.
##
## These parse a "A:B:C" style distribution string and reduce two of them to a
## single number. They used to live in R/reidentify.R alongside the legacy
## reid_by_dist() wrapper; the wrapper was removed in 3.0.0 but score_dist()
## (R/score.R) and the set-similarity scores (R/setsim.R) still depend on
## distribution_distance() and validate_split(), so they moved here.
## ---------------------------------------------------------------------------

#' check that a `split` separator is a usable literal string
#'
#' `split` is always interpreted literally (see [parse_dist_values()]), so
#' the only remaining traps are a non-string, a vector, `NA`, or the empty
#' string -- `strsplit(x, "", fixed = TRUE)` splits between every character,
#' which would silently turn "123" into c(1, 2, 3). Rejecting these loudly is
#' preferable for a tool whose job is to *measure* risk.
#'
#' @param split the separator to check
#'
#' @return `invisible(TRUE)`; stops with an error otherwise.
#'
#' @keywords internal
validate_split <- function(split) {
  if (!is.character(split) || length(split) != 1L || is.na(split)) {
    stop(
      "`split` must be a single non-NA character string, used as a literal ",
      "separator (e.g. \":\").",
      call. = FALSE
    )
  }
  if (!nzchar(split)) {
    stop(
      "`split` must not be the empty string: that would split the value ",
      "between every character (\"123\" -> 1, 2, 3). Pass the literal ",
      "separator used to build the column, e.g. \":\".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' parse a "A:B:C" style distribution string into a numeric vector,
#' stopping with a clear error instead of silently returning NA when an
#' element cannot be interpreted as a number (phase 3 fix for the
#' distribution_distance()/calc_KL() defect where a non-numeric target
#' column produced all-NA distances and the caller quietly reported
#' "0 / 0", which reads as "could not be reidentified = safe").
#'
#' Distinguishes, where possible, a genuinely non-numeric element (R's
#' `as.numeric()` raises "NAs introduced by coercion") from an explicit
#' missing value already encoded in the data (e.g. an "NA" token, which
#' `as.numeric()` parses cleanly to NA with no warning).
#'
#' @param str character scalar, e.g. "1:2:3"
#' @param split separator, treated as a **literal string** and never as a
#'   regular expression (default ":")
#' @param side label used in the error message ("x" or "y")
#'
#' @return numeric vector parsed from `str`; stops with an error instead of
#'   returning NA-containing output.
#'
#' @keywords internal
parse_dist_values <- function(str, split, side) {
  ## `fixed = TRUE`: the separator is a literal string, not a regular
  ## expression (Issue #32). strsplit()'s default is regex, so a separator
  ## that happens to be a metacharacter used to misbehave:
  ##   split = "|" -- an empty alternation, so the value was split between
  ##     every character ("123" -> 1, 2, 3). When the resulting tokens all
  ##     parsed as numbers this produced a *silently wrong* distance: e.g.
  ##     distribution_distance("123", "132", split = "|") was 0, i.e. two
  ##     different amounts reported as the same distribution.
  ##   split = "." -- matched any character, so every token was empty.
  ##   split = "(" / "[" -- not a valid regex at all, so strsplit() warned
  ##     "TRE pattern compilation error" and returned the input unsplit.
  ## The remaining metacharacters usually surfaced below as a "could not
  ## convert ... to numeric" error, but that message blames the *column*
  ## for what is really a separator problem. paste(collapse = ) on the
  ## producing side (transform_transaction_to_master()) has always been
  ## literal, so this also restores symmetry between the two sides.
  validate_split(split)
  parts <- strsplit(str, split = split, fixed = TRUE)[[1]]

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
#' @param split separator between the elements of the distribution string
#'   (default: ":"). Treated as a **literal string**, never as a regular
#'   expression, so metacharacters such as `"|"`, `"."` or `"$"` are safe.
#'   Must be a single non-empty string.
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
#' @param split separator between the elements of the distribution string
#'   (default: ":"). Treated as a **literal string**, never as a regular
#'   expression, so metacharacters such as `"|"`, `"."` or `"$"` are safe.
#'   Must be a single non-empty string.
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
