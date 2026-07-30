## ---------------------------------------------------------------------------
## Assignment layer (Issue #11)
##
## Takes a score table from the score layer (R/score.R) or the integration
## layer (R/combine.R) and decides, for each ANON record, which RAW record the
## attacker would guess.
##
## Only the greedy (independent per-ANON argmin) rule lives here. Globally
## optimal assignment (linear assignment / Hungarian) is Issue #15 and
## deliberately out of scope, as is margin/eccentricity-based confidence
## (Issue #16).
## ---------------------------------------------------------------------------

#' assign each ANON record to its best-scoring RAW record, independently
#'
#' For every ANON record this picks the RAW record with the best (by default:
#' smallest) SCORE. Assignments are made independently per ANON record, so the
#' same RAW record may be claimed by several ANON records -- that is what
#' "greedy" means here. Enforcing a one-to-one assignment is Issue #15.
#'
#' Ties are broken uniformly at random via [resolve_min_distance_ties()], so a
#' record that is genuinely indistinguishable from `k` others is credited with
#' a `1/k` chance rather than being deterministically awarded to whichever
#' candidate happened to sort first (Issue #3).
#'
#' `CONFIDENCE` is `1 / (number of RAW records tied at the best score)`: the
#' probability that this particular draw picked the right record, *given* that
#' the right record is among the tied best candidates. It is 1 exactly when the
#' best candidate is unique. It deliberately says nothing about how far ahead
#' of the runners-up the winner is -- margin / eccentricity confidence is
#' Issue #16.
#'
#' @param scores a score table: a data frame with columns RAW_ROW_NUMBER,
#'   ANON_ROW_NUMBER and SCORE, normally produced by a `score_*()` function or
#'   by [combine_scores()]. Its `score_type` attribute decides whether the best
#'   score is the smallest ("distance", the default) or the largest
#'   ("similarity").
#' @param seed integer seed for the random tie-break (default 0L, so a plain
#'   call is reproducible). NULL uses the ambient RNG stream instead.
#'
#' @return a data frame with exactly one row per ANON record, ordered by
#'   ANON_ROW_NUMBER, with columns ANON_ROW_NUMBER, RAW_ROW_NUMBER,
#'   CONFIDENCE (numeric, in (0, 1]) and RESULT (logical: whether the guessed
#'   RAW record is in fact the one the ANON record came from).
#'
#' @examples
#' raw <- data.frame(ROW_NUMBER = 1:5, V = c(10, 20, 30, 40, 50))
#' d <- join_raw_anon_data(raw, raw)
#' match_greedy(score_num(d, "V"))
#'
#' @export
match_greedy <- function(scores, seed = 0L) {
  score_type <- validate_reid_scores(scores, "scores")

  ## Internally everything is minimised. A similarity is negated rather than
  ## inverted so the transformation is monotone and never divides by zero.
  distance <- if (identical(score_type, "similarity")) -scores$SCORE else scores$SCORE

  work <- data.frame(
    RAW_ROW_NUMBER = scores$RAW_ROW_NUMBER,
    ANON_ROW_NUMBER = scores$ANON_ROW_NUMBER,
    DISTANCE = distance,
    SCORE_ROW = seq_len(nrow(scores)),
    stringsAsFactors = FALSE
  )

  picked <- resolve_min_distance_ties(work, seed = seed)

  ## size of the tie group each winner was drawn from, i.e. how many RAW
  ## records were indistinguishable from it under this score
  n_tied <- tapply(
    work$DISTANCE, work$ANON_ROW_NUMBER,
    function(v) sum(v == min(v))
  )
  tie_size <- as.numeric(n_tied[as.character(picked$ANON_ROW_NUMBER)])

  out <- data.frame(
    ANON_ROW_NUMBER = picked$ANON_ROW_NUMBER,
    RAW_ROW_NUMBER = picked$RAW_ROW_NUMBER,
    CONFIDENCE = 1 / tie_size,
    RESULT = (picked$ANON_ROW_NUMBER == picked$RAW_ROW_NUMBER),
    stringsAsFactors = FALSE
  )

  ## Which row of `scores` each winner came from. The reid_by_*() wrappers use
  ## this to recover the per-pair detail columns they have always reported;
  ## it is an implementation detail, not part of the documented return value.
  attr(out, "score_row") <- picked$SCORE_ROW

  out
}
