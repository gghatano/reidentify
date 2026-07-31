## ---------------------------------------------------------------------------
## Confidence (Issue #16)
##
## A real attacker does not attack every record. They attack the ones they are
## sure about, because a wrong claim costs them and a right one pays. "10% of
## records identified with 95% precision" is a worse outcome for the data
## publisher than "30% at 30%", and only a confidence measure makes the
## difference visible.
##
## The package started with one confidence measure, 1 / (size of the tie group
## at the best score). It is exactly right as a probability -- given that the
## true record is among the tied best, that is the chance of drawing it -- but
## it is nearly useless as a *ranking*, because it collapses every record with
## a unique best candidate onto the single value 1. On continuous scores that
## is almost every record, so the precision-recall sweep in reid_evaluate()
## gets one threshold and reports one point: the overall success rate. The
## metric that exists to show the shape of the risk shows nothing.
##
## Margin (eccentricity) fixes the resolution. It asks how far ahead of the
## runner-up the winner is, in units of the spread of that record's own
## candidate scores -- the criterion Narayanan & Shmatikov (2008) use in
## Scoreboard-RH. It is not a probability and this file does not pretend
## otherwise; it is an ordering, and ordering is what a threshold sweep needs.
## ---------------------------------------------------------------------------

#' per-ANON-record confidence in the best-scoring candidate
#'
#' Reduces a score table to one row per ANON record describing how clear-cut
#' that record's best candidate is, under either of two measures.
#'
#' @section The two measures:
#'
#' \describe{
#'   \item{`"tie"`}{`1 / TIE_SIZE`, where `TIE_SIZE` counts the RAW records
#'     sharing the best score. A calibrated probability: it is the chance that
#'     a uniform draw among the tied best candidates lands on the right one,
#'     *given* that the right one is in that group. Its weakness is
#'     resolution -- every record whose best candidate is unique scores 1,
#'     however marginal that win was.}
#'   \item{`"margin"`}{the eccentricity
#'     `(second best score - best score) / sd(candidate scores)`. It
#'     distinguishes a runaway winner from a photo finish, so it orders
#'     records that `"tie"` cannot separate. Because the runner-up is the
#'     second *best candidate* rather than the second distinct score, a tie at
#'     the top gives margin 0: it subsumes `"tie"`'s information instead of
#'     discarding it. It is **not** a probability -- a value of 2 does not
#'     mean anything on a 0-1 scale -- and it is only comparable between
#'     records scored the same way. Dividing by the record's own sd is what
#'     makes it comparable at all, since the raw margin inherits the units of
#'     the score.}
#' }
#'
#' @section Eccentricity does not carry between data sets:
#'
#' This is the single most important caveat about `"margin"`, and it applies
#' with full force now that `"margin"` is the **default** (Issue #44).
#' Eccentricity is a ratio to *the record's own* candidate spread, so its
#' numeric range is a property of the score table, not of the risk. Measured
#' maxima:
#'
#' * dense 150-person two-attribute numeric fixture: **0.45**
#' * sparse transaction-style scores (Issue #23): **4.86**
#'
#' An order of magnitude apart, on data whose *risk* is not an order of
#' magnitude apart -- the dense fixture had 98% of records correctly
#' reidentified, and eccentricity still separated correct guesses from
#' incorrect ones there (mean 0.167 against 0.007), which is what the measure
#' is for. What it is not for is comparison across tables. So:
#'
#' * **A threshold is not portable.** `min_confidence = 0.3` may attack the
#'   confident tenth of one table and every record of another.
#' * **The constant 1.5 from Narayanan & Shmatikov does not transfer.** Their
#'   sparse scores put a true match several sd above the field; on dense data
#'   it rejects every record, and a tool that reports zero reidentifications
#'   because its threshold was on the wrong scale looks exactly like a tool
#'   reporting that the data is safe (docs/lessons-learned.md section 2).
#'   [match_greedy()] and [match_optimal()] warn when that happens, but the
#'   warning is a backstop, not a substitute for choosing the threshold.
#' * **Two eccentricities are comparable only within one score table.** Do not
#'   read "data set A reached 0.9, B only 0.4" as A being riskier; use
#'   `success_analytic` or `max_risk` from [reid_evaluate()] for that, which
#'   are unaffected by the choice of confidence measure.
#'
#' Pick thresholds from the observed distribution, never from a constant:
#'
#' ```
#' stats::quantile(reid_confidence(scores, "margin")$CONFIDENCE, 0.9)
#' ```
#'
#' `"tie"` has none of this problem -- it is a probability in `(0, 1]` and
#' means the same thing everywhere -- which is the trade for its near-total
#' lack of resolution. On the 150-person fixture above it took a single
#' distinct value (1) across all 150 records, while `"margin"` took 150.
#'
#' @section Changed defaults:
#'
#' `method` defaulted to `"tie"` when this function was added (Issue #16) and
#' defaults to `"margin"` from Issue #44 onwards, as do the `confidence`
#' arguments of [match_greedy()], [match_optimal()] and [reid_evaluate()].
#' Only the `CONFIDENCE` column moves: `TIE_SIZE`, `MARGIN`, `SD_SCORE` and
#' `ECCENTRICITY` are reported under either setting, and the risk quantities
#' in [reid_evaluate()] do not depend on the choice at all. Code that compared
#' `CONFIDENCE` against a literal, or that fed it to `min_confidence`, will
#' see different numbers than before; pass `method = "tie"` (or
#' `confidence = "tie"`) for the old behaviour. See `docs/default-changes.md`.
#'
#' Both measures are computed on the internally minimised scale, so a
#' "similarity" score table gives the same answer as the distance table it
#' negates to.
#'
#' @section Edge cases:
#'
#' A record whose candidates all share one score has margin 0 and
#' eccentricity 0: nothing distinguishes them. A record with a *single*
#' candidate has no runner-up to be confused with, and its eccentricity is
#' `Inf` -- the honest limit, and harmless to a threshold sweep, which only
#' ever sorts and compares.
#'
#' @param scores a score table (see [score_num()])
#' @param method `"margin"` (default since Issue #44) or `"tie"` (the
#'   default up to and including Issue #16)
#' @param tolerance relative tolerance for deciding that two candidate scores
#'   are tied, default `sqrt(.Machine$double.eps)` (Issue #61). Without it
#'   `TIE_SIZE` and `MARGIN` are not invariant to a change of units: on a
#'   200-record fixture, expressing the same values in 1/10 units left 198 of
#'   200 records with a `MARGIN` below 1e-9 -- non-zero only as an artefact of
#'   binary representation -- and turned 108 genuine two-way ties into unique
#'   wins. Pass `tolerance = 0` for the exact `==` comparison used before #61;
#'   see `docs/default-changes.md`.
#'
#' @return a data frame with one row per ANON record, ordered by
#'   ANON_ROW_NUMBER, with columns ANON_ROW_NUMBER, N_CANDIDATES, BEST_SCORE,
#'   SECOND_SCORE, TIE_SIZE, MARGIN, SD_SCORE, ECCENTRICITY and CONFIDENCE.
#'   `CONFIDENCE` is `1 / TIE_SIZE` or `ECCENTRICITY` according to `method`;
#'   the other columns are reported either way so the choice can be second
#'   guessed without recomputing.
#'
#' @references Narayanan, A. and Shmatikov, V. (2008) Robust
#'   De-anonymization of Large Sparse Datasets. IEEE Symposium on Security
#'   and Privacy.
#'
#' @examples
#' raw <- data.frame(ROW_NUMBER = 1:6, V = c(1, 1, 2, 5, 9, 14))
#' d <- join_raw_anon_data(raw, raw)
#' reid_confidence(score_num(d, "V"), method = "margin")
#'
#' @importFrom stats sd
#' @export
reid_confidence <- function(scores, method = c("margin", "tie"),
                            tolerance = reid_tie_tolerance()) {
  method <- match.arg(method)
  score_type <- validate_reid_scores(scores, "scores")
  validate_tie_tolerance(tolerance, "reid_confidence")
  ## TIE_SIZE counts rows and SD_SCORE is taken over rows, so a candidate pair
  ## listed twice corrupts both -- the same defect, on the same contract, as
  ## Issue #60 in match_greedy() / reid_evaluate().
  validate_unique_candidate_pairs(scores, "reid_confidence")

  value <- if (identical(score_type, "similarity")) -scores$SCORE else scores$SCORE
  if (anyNA(value)) {
    stop("reid_confidence(): `scores$SCORE` contains NA; every candidate pair ",
         "must have a score before its confidence can be computed.",
         call. = FALSE)
  }

  anon <- scores$ANON_ROW_NUMBER
  levels_anon <- sort(unique(anon))
  raw_groups <- split(value, factor(anon, levels = levels_anon))

  ## Issue #61: near-equal candidate scores are collapsed onto their group
  ## minimum before any tie test, so TIE_SIZE, SECOND_SCORE and MARGIN do not
  ## depend on the units the data happens to be expressed in.
  ## snap_tied_values() preserves min(), so BEST_SCORE is untouched.
  groups <- lapply(raw_groups, snap_tied_values, tolerance = tolerance)

  n_candidates <- lengths(groups)
  best <- vapply(groups, min, numeric(1))
  tie_size <- vapply(seq_along(groups), function(i) {
    sum(groups[[i]] == best[i])
  }, numeric(1))

  ## The runner-up is the *second best candidate*, not the second distinct
  ## score. The difference matters: if two candidates tie for first, the
  ## attacker is flipping a coin, and taking the gap to the next distinct
  ## value instead would report that coin flip as a confident win whenever the
  ## third candidate happens to be far away. Narayanan & Shmatikov's max2 is
  ## the second highest score for the same reason, so a tie at the top gives
  ## margin 0 and the measure subsumes tie detection rather than ignoring it.
  second <- vapply(groups, function(v) {
    if (length(v) < 2) NA_real_ else sort(v, partial = 2L)[2L]
  }, numeric(1))

  ## SD_SCORE is the spread of the candidate scores, not a tie test, so it is
  ## taken on the values as given. (The two differ by less than the tolerance
  ## by construction; using the raw values keeps the column meaning exactly
  ## what its name says.)
  sd_score <- vapply(raw_groups, function(v) {
    if (length(v) < 2) 0 else stats::sd(v)
  }, numeric(1))

  ## MARGIN is 0, not NA, when every candidate ties: the attacker really does
  ## have no gap to work with, and NA would drop the record out of a threshold
  ## sweep instead of placing it at the bottom where it belongs.
  margin <- ifelse(is.na(second), 0, second - best)

  eccentricity <- ifelse(
    n_candidates < 2, Inf,
    ifelse(sd_score > 0, margin / sd_score, 0)
  )

  data.frame(
    ANON_ROW_NUMBER = levels_anon,
    N_CANDIDATES = as.numeric(n_candidates),
    BEST_SCORE = unname(best),
    SECOND_SCORE = second,
    TIE_SIZE = tie_size,
    MARGIN = unname(margin),
    SD_SCORE = unname(sd_score),
    ECCENTRICITY = unname(eccentricity),
    CONFIDENCE = if (identical(method, "margin")) {
      unname(eccentricity)
    } else {
      1 / tie_size
    },
    stringsAsFactors = FALSE
  )
}

#' apply a confidence measure and a confidence threshold to an assignment
#'
#' Shared by [match_greedy()] and [match_optimal()] so the two cannot drift
#' apart on what "confidence" and "declined" mean.
#'
#' Below-threshold records are reported as `RAW_ROW_NUMBER = NA` with
#' `RESULT = FALSE` -- the attacker declining to claim them -- rather than
#' being deleted from the result. Deleting them would shrink the denominator
#' that [reid_result()] divides by and inflate the reported reidentification
#' rate, which is the failure direction this package must not take quietly
#' (docs/lessons-learned.md section 2).
#'
#' @param out an assignment table (ANON_ROW_NUMBER, RAW_ROW_NUMBER,
#'   CONFIDENCE, RESULT)
#' @param scores the score table the assignment came from
#' @param confidence `"tie"` or `"margin"`
#' @param min_confidence records scoring below this decline to guess
#' @param tolerance relative tie tolerance, see [reid_confidence()]
#'
#' @return `out` with CONFIDENCE replaced and sub-threshold rows declined
#'
#' @keywords internal
apply_confidence <- function(out, scores, confidence, min_confidence,
                             tolerance = reid_tie_tolerance()) {
  if (!is.numeric(min_confidence) || length(min_confidence) != 1 ||
      is.na(min_confidence)) {
    stop("`min_confidence` must be a single number.", call. = FALSE)
  }

  if (identical(confidence, "margin")) {
    conf <- reid_confidence(scores, method = "margin", tolerance = tolerance)
    out$CONFIDENCE <- conf$CONFIDENCE[match(out$ANON_ROW_NUMBER,
                                            conf$ANON_ROW_NUMBER)]
  }

  if (min_confidence > 0) {
    declined <- out$CONFIDENCE < min_confidence
    out$RAW_ROW_NUMBER[declined] <- NA
    out$RESULT[declined] <- FALSE

    ## A threshold on the wrong scale silently produces "no reidentifications"
    ## -- indistinguishable from a genuinely safe data set, and believed for
    ## the same reason (docs/lessons-learned.md section 2). Say so.
    if (all(declined)) {
      warning("`min_confidence` = ", min_confidence, " rejected every one of ",
              nrow(out), " record(s), so the result is an unconditional zero ",
              "rather than a measurement. Eccentricity has no fixed scale; ",
              "pick a threshold from the observed values, e.g. ",
              "stats::quantile(reid_confidence(scores, \"margin\")$CONFIDENCE, 0.9).",
              call. = FALSE)
    }
  }

  out
}
