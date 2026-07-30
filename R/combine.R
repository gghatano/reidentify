## ---------------------------------------------------------------------------
## Integration layer (Issue #11)
##
## Combines several score tables over the same (RAW, ANON) candidate pairs
## into one, so that "attribute x distance definition" combinations can be
## built up and handed to the assignment layer as a single score.
## ---------------------------------------------------------------------------

#' combine several score tables into one weighted score
#'
#' Each input is a score table over the *same* set of (RAW, ANON) candidate
#' pairs. The result holds the weighted sum of their SCORE columns.
#'
#' The pair sets are required to match exactly, and a pair missing from one
#' input is an error rather than a silently dropped row: dropping candidate
#' pairs would shrink the attacker's search space and therefore report a
#' *lower* reidentification rate than the truth, which is the failure
#' direction a safety-checking tool must never take quietly
#' (docs/lessons-learned.md section 2).
#'
#' SCALING IS THE CALLER'S JOB. The metrics in this package live on very
#' different scales -- an edit distance is a small integer, a squared L2
#' quantile distance is unbounded -- so an unweighted sum lets whichever
#' metric happens to have the largest spread dominate. `combine_scores()`
#' deliberately does not normalise: choosing a defensible normalisation
#' (standardisation, Mahalanobis, learned weights) is Issue #14. Use
#' `weights` to compensate in the meantime, and read a combined score as
#' "this particular weighted sum", not as a scale-free distance.
#'
#' @param scores a list of score tables (each with columns RAW_ROW_NUMBER,
#'   ANON_ROW_NUMBER and SCORE), all covering the same candidate pairs and all
#'   of the same `score_type`
#' @param weights numeric vector of weights, one per element of `scores`
#'   (default: 1 for each, so combining a single score table returns it
#'   unchanged). Must be non-negative and not all zero -- a negative weight
#'   would flip that component's orientation, turning "far apart" into
#'   "good match".
#'
#' @return a "reid_scores" table over the same candidate pairs, ordered as the
#'   first element of `scores`, whose SCORE is the weighted sum and whose
#'   `score_type` is that of the inputs.
#'
#' @examples
#' raw <- data.frame(ROW_NUMBER = 1:5, V = c(10, 20, 30, 40, 50), W = c(1, 1, 2, 2, 3))
#' d <- join_raw_anon_data(raw, raw)
#' combined <- combine_scores(list(score_num(d, "V"), score_num(d, "W")), weights = c(1, 10))
#' match_greedy(combined)
#'
#' @export
combine_scores <- function(scores, weights = NULL) {
  if (!is.list(scores) || is.data.frame(scores)) {
    stop("`scores` must be a *list* of score tables; got a single ",
         class(scores)[1], ". Wrap it in list().", call. = FALSE)
  }
  if (length(scores) == 0) {
    stop("`scores` must contain at least one score table.", call. = FALSE)
  }

  types <- vapply(
    seq_along(scores),
    function(i) validate_reid_scores(scores[[i]], paste0("scores[[", i, "]]")),
    character(1)
  )
  if (length(unique(types)) > 1) {
    stop("all score tables must have the same score_type, but got: ",
         paste(unique(types), collapse = ", "),
         ". A distance and a similarity cannot be summed -- they point in ",
         "opposite directions.", call. = FALSE)
  }

  if (is.null(weights)) {
    weights <- rep(1, length(scores))
  }
  if (!is.numeric(weights) || length(weights) != length(scores)) {
    stop("`weights` must be a numeric vector with one entry per score table (",
         length(scores), " expected, got ", length(weights), ").", call. = FALSE)
  }
  if (anyNA(weights) || any(weights < 0)) {
    stop("`weights` must be non-negative and non-missing; a negative weight ",
         "would reverse that component's orientation.", call. = FALSE)
  }
  if (all(weights == 0)) {
    stop("`weights` must not be all zero: the combined score would be ",
         "constant and every candidate would tie.", call. = FALSE)
  }

  base <- scores[[1]]
  key <- paste(base$ANON_ROW_NUMBER, base$RAW_ROW_NUMBER, sep = "\r")
  if (anyDuplicated(key) > 0) {
    stop("`scores[[1]]` contains duplicated (ANON_ROW_NUMBER, ",
         "RAW_ROW_NUMBER) pairs; a score table must hold each candidate ",
         "pair exactly once.", call. = FALSE)
  }

  total <- weights[1] * base$SCORE

  for (i in seq_along(scores)[-1]) {
    s <- scores[[i]]
    k <- paste(s$ANON_ROW_NUMBER, s$RAW_ROW_NUMBER, sep = "\r")
    if (anyDuplicated(k) > 0) {
      stop("`scores[[", i, "]]` contains duplicated (ANON_ROW_NUMBER, ",
           "RAW_ROW_NUMBER) pairs.", call. = FALSE)
    }
    idx <- match(key, k)
    if (anyNA(idx) || length(k) != length(key)) {
      stop("`scores[[", i, "]]` does not cover the same (ANON_ROW_NUMBER, ",
           "RAW_ROW_NUMBER) candidate pairs as `scores[[1]]` (",
           length(key), " pairs vs ", length(k), ", ", sum(is.na(idx)),
           " unmatched). Combining scores over different candidate sets ",
           "would silently drop candidates and under-report the ",
           "reidentification rate.", call. = FALSE)
    }
    total <- total + weights[i] * s$SCORE[idx]
  }

  new_reid_scores(
    raw_row_number = base$RAW_ROW_NUMBER,
    anon_row_number = base$ANON_ROW_NUMBER,
    score = total,
    score_type = types[1]
  )
}
