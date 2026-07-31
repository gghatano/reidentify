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
#' deliberately does not normalise: normalisation is a separate, explicit
#' step ([normalize_scores()], or [score_multi()] which does it for you). Use
#' `weights` to compensate otherwise, and read a combined score as "this
#' particular weighted sum", not as a scale-free distance.
#'
#' WHAT DOMINATION ACTUALLY COSTS. The component with the widest weighted
#' spread decides the ranking; the others only break its ties. Whether that
#' hurts depends on the *dominant* component, and the effect runs in both
#' directions -- so a large scale gap is a condition to check, not a defect
#' by itself:
#'
#' \itemize{
#'   \item dominant component also the most informative: the sum is as good
#'     as, or better than, the normalised sum. On the fixture in
#'     `docs/default-changes.md` an under-weighted informative axis still
#'     reached 0.8417, identical to the normalised combination.
#'   \item dominant component the *less* informative one: the sum tracks that
#'     component and adding attributes lowers the measured rate. On the same
#'     fixture, ZIP alone scores 0.2000 and normalised ZIP + SPEND_DIST
#'     scores 0.4450, but the unnormalised sum -- where SPEND_DIST's spread is
#'     roughly 16000x ZIP's -- scores 0.0300. A safety tool reporting 0.03
#'     where 0.45 is achievable understates the risk 15-fold, and nothing
#'     errors.
#' }
#'
#' Because the harmful case cannot be told from the harmless one without
#' running the attack, `combine_scores()` warns when the weighted spreads of
#' two components differ by more than 10x. 10 is where the loss first becomes
#' visible in the calibration recorded in `docs/default-changes.md` (ratio 3:
#' under 1%; ratio 10: 0.4450 -> 0.3900; ratio 30: 0.2500; ratio 100: 0.1350)
#' and is also the largest gap that costs nothing when the dominant component
#' is the informative one, so it does not fire on the harmless direction.
#' Silence the check with `scale_check = "none"` when the scale gap is
#' deliberate, as in an IDF block where the magnitude *is* the evidence.
#'
#' @param scores a list of score tables (each with columns RAW_ROW_NUMBER,
#'   ANON_ROW_NUMBER and SCORE), all covering the same candidate pairs and all
#'   of the same `score_type`
#' @param weights numeric vector of weights, one per element of `scores`
#'   (default: 1 for each, so combining a single score table returns it
#'   unchanged). Must be non-negative and not all zero -- a negative weight
#'   would flip that component's orientation, turning "far apart" into
#'   "good match".
#' @param scale_check `"warn"` (default) to warn when the weighted standard
#'   deviations of two components differ by more than 10x, `"none"` to skip
#'   the check. Components with zero weight or constant SCORE are excluded:
#'   they add a constant and cannot change any ranking.
#'
#' @return a "reid_scores" table over the same candidate pairs, ordered as the
#'   first element of `scores`, whose SCORE is the weighted sum and whose
#'   `score_type` is that of the inputs.
#'
#' @seealso [normalize_scores()] to put the components on a common scale
#'   first, and [score_multi()] which normalises and combines in one call.
#'
#' @examples
#' raw <- data.frame(ROW_NUMBER = 1:5, V = c(10, 20, 30, 40, 50), W = c(1, 1, 2, 2, 3))
#' d <- join_raw_anon_data(raw, raw)
#' parts <- normalize_scores(list(score_num(d, "V"), score_num(d, "W")), "range")
#' combined <- combine_scores(parts, weights = c(1, 2))
#' match_greedy(combined)
#'
#' @importFrom stats sd
#' @export
combine_scores <- function(scores, weights = NULL,
                           scale_check = c("warn", "none")) {
  scale_check <- match.arg(scale_check)
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
  ## Spread of each component's *contribution to the sum*, i.e. after its
  ## weight: that is what decides the ranking, and it is what the caller can
  ## act on. A caller who has already compensated with `weights` must not be
  ## warned about the raw scale gap they just corrected.
  spreads <- numeric(length(scores))
  spreads[1] <- stats::sd(as.numeric(total))

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
    contrib <- weights[i] * as.numeric(s$SCORE[idx])
    spreads[i] <- stats::sd(contrib)
    total <- total + contrib
  }

  if (identical(scale_check, "warn")) {
    warn_scale_domination(spreads, weights, names(scores))
  }

  new_reid_scores(
    raw_row_number = base$RAW_ROW_NUMBER,
    anon_row_number = base$ANON_ROW_NUMBER,
    score = total,
    score_type = types[1]
  )
}

## Ratio of weighted spreads above which combine_scores() warns. Calibrated on
## the fixture recorded in docs/default-changes.md. When the dominant
## component is the less informative one the measured success rate falls away
## past this point (ratio 3: 0.4400, 10: 0.3900, 30: 0.2500, 100: 0.1350,
## against 0.4450 normalised); when it is the more informative one there is no
## loss at all up to ratio 10, so warning here does not fire on the harmless
## direction.
SCALE_DOMINATION_RATIO <- 10

#' warn when one component's weighted spread dominates the sum
#'
#' @param spreads standard deviation of each component's weighted contribution
#' @param weights the weights that produced them
#' @param nms names of the score list, or NULL
#'
#' @return NULL, invisibly; called for the warning
#'
#' @keywords internal
warn_scale_domination <- function(spreads, weights, nms = NULL) {
  ## A component with zero weight, or one whose score never varies, adds a
  ## constant to every candidate. It cannot reorder anything, so it is neither
  ## dominated nor dominating -- including it would raise the ratio to Inf and
  ## fire on a component that provably does not matter.
  keep <- which(weights > 0 & is.finite(spreads) & spreads > 0)
  if (length(keep) < 2) {
    return(invisible(NULL))
  }

  hi <- keep[which.max(spreads[keep])]
  lo <- keep[which.min(spreads[keep])]
  ratio <- spreads[hi] / spreads[lo]
  if (!isTRUE(ratio > SCALE_DOMINATION_RATIO)) {
    return(invisible(NULL))
  }

  label <- function(i) {
    if (!is.null(nms) && length(nms) >= i && !is.na(nms[i]) && nzchar(nms[i])) {
      paste0("`", nms[i], "` (scores[[", i, "]])")
    } else {
      paste0("scores[[", i, "]]")
    }
  }

  warning(
    "combine_scores(): the components are on very different scales -- ",
    label(hi), " has ", format(ratio, digits = 3),
    "x the weighted spread of ", label(lo),
    " (sd ", format(spreads[hi], digits = 3), " vs ",
    format(spreads[lo], digits = 3),
    "). The widest component decides the ranking and the others only break ",
    "its ties. That is harmless when the dominant component is also the most ",
    "informative, but when it is not, adding attributes LOWERS the measured ",
    "reidentification rate and the result understates the risk. Put the ",
    "components on a common scale first -- ",
    "combine_scores(normalize_scores(scores, \"range\")) or score_multi() -- ",
    "or set weights to compensate. Pass scale_check = \"none\" if the scale ",
    "gap is intended.",
    call. = FALSE
  )
  invisible(NULL)
}
