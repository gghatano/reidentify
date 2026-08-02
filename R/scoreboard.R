## ---------------------------------------------------------------------------
## Scoreboard-RH (Issue #23)
##
## Narayanan & Shmatikov (2008), "Robust De-anonymization of Large Sparse
## Datasets" -- the Netflix Prize attack. What makes it the threat model worth
## implementing is not that it is powerful on complete data; it is that the
## attacker needs only a *fragment* of the target's history. A handful of
## remembered items, none of them individually identifying, is enough, because
## sparse high-dimensional data has almost no two records alike.
##
## The paper's algorithm is three ideas stacked, and this package already had
## two of them:
##
##   1. weight each attribute by how rare it is        -> Issue #17 (R/idf.R)
##   2. accept near matches, not just exact ones       -> here
##   3. answer only when the winner is far enough
##      ahead of the runner-up, else refuse            -> Issue #16
##      (R/confidence.R)
##
## So this file is deliberately thin: score_scoreboard() supplies (2) and the
## sparse-support weighting of (1), and Scoreboard-RH itself is
## match_scoreboard_rh(), a named wrapper around the confidence machinery that
## already exists. Naming it matters more than the ten lines of code: the
## algorithm is cited by name, and a reader should be able to find it.
##
## ORIENTATION. This score is a *similarity* -- more agreement is a better
## match -- and says so via score_type, rather than being flipped into a
## distance. R/idf.R can flip because its per-record total weight is constant
## for a given ANON record; here it is not, because the attacker's support set
## differs from candidate to candidate. Flipping anyway would silently reorder
## candidates, and a silently reordered score is how an unsafe data set comes
## to look safe (docs/lessons-learned.md section 2).
## ---------------------------------------------------------------------------

#' similarity of two vectors of attribute values, allowing near matches
#'
#' @param a,b vectors of values, of equal length
#' @param tol tolerance: values within `tol` count as matching. Applies to
#'   numeric columns only.
#' @param partial `"step"` (inside the tolerance is a full match, outside is
#'   nothing) or `"linear"` (a match decaying linearly to 0 at `tol`)
#'
#' @return numeric vector in \[0, 1\], with 0 wherever either side is missing
#'
#' @keywords internal
scoreboard_similarity <- function(a, b, tol, partial) {
  n <- length(a)
  present <- !is.na(a) & !is.na(b)

  if (is.numeric(a) && is.numeric(b)) {
    gap <- abs(a - b)
    sim <- if (tol > 0 && identical(partial, "linear")) {
      pmax(0, 1 - gap / tol)
    } else {
      as.numeric(gap <= tol)
    }
  } else {
    ## Non-numeric columns have no metric to be near in, so `tol` cannot mean
    ## anything for them and exact agreement is the only honest reading.
    sim <- as.numeric(as.character(a) == as.character(b))
  }

  out <- rep(0, n)
  out[present] <- sim[present]
  out
}

#' score candidate pairs the way Narayanan & Shmatikov's Scoreboard does
#'
#' The similarity between an attacker's fragmentary knowledge of a person and
#' a released record: for every attribute the attacker knows, add the rarity
#' weight of that attribute if the two records agree there, allowing near
#' matches within `tolerance`.
#'
#' This is the score half of Scoreboard-RH. The decision half -- answer only
#' when the winner is far enough clear of the runner-up -- is
#' [match_scoreboard_rh()].
#'
#' @section Sparsity is the point:
#'
#' `NA` means "this record has no value for this attribute", and that is a
#' first-class state here rather than an error. It is what makes the method
#' work on transaction-shaped data, where any one person touches a tiny
#' fraction of the possible items, and it is how partial attacker knowledge is
#' expressed: set every attribute the attacker does *not* know to `NA` on the
#' `aux_side`, and only the rest are scored.
#'
#' Other `score_*()` functions in this package reject `NA` because there a
#' missing value is a data problem that could otherwise be scored as a
#' confident match. Here a missing value contributes exactly 0 and can never
#' manufacture agreement, so admitting it is safe.
#'
#' @section Weighting:
#'
#' Each attribute is weighted by the inverse of how many records have it at
#' all -- its support -- so that an item almost nobody has counts for much
#' more than one everybody has. The paper writes this weight as
#' `1 / log(|supp(i)|)`, which is infinite for an attribute exactly one record
#' has; the default `"inv_log"` uses the package's `1 / log(support + 1)`
#' instead, for the reason already documented on [idf_weight()] -- the
#' singleton is the case the method exists for, so the formula must not
#' diverge there.
#'
#' Note that this weights an *attribute* by its support, whereas
#' [score_idf()] weights a *value* by its frequency. Both are rarity
#' weightings; they are answers to different questions, and on wide sparse
#' data it is the attribute one that carries the signal.
#'
#' @inheritParams score_idf
#' @param targets character vector of attribute columns, *before* the
#'   RAW_/ANON_ prefixing done by [join_raw_anon_data()]
#' @param tolerance how close two values must be to count as agreeing. Either
#'   one number for all of `targets`, or a vector named by target. Numeric
#'   columns only; ignored for the rest, where agreement is exact. Default 0,
#'   i.e. exact matching.
#' @param partial `"step"` (default: anything within `tolerance` is a full
#'   match) or `"linear"` (credit decaying to 0 at `tolerance`).
#' @param aux_side which side carries the attacker's knowledge and therefore
#'   defines which attributes are scored: `"raw"` (default -- RAW is the
#'   attacker's background knowledge in this package, matching the paper's
#'   `supp(aux)`) or `"anon"`.
#' @param source which side the support counts are taken over (default
#'   `"anon"`, the released table, which any attacker can count for
#'   themselves).
#' @param weight rarity weighting scheme, passed to [idf_weight()]. `"none"`
#'   gives unweighted overlap counting, the baseline the weighting has to
#'   beat.
#' @param generalized what to do when one of `targets` turns out to hold
#'   generalised values on the ANON side: `"stop"` (default), `"warn"` or
#'   `"ignore"`. A raw value never equals the region that contains it, so
#'   every candidate pair scores 0 similarity and the attribute contributes
#'   nothing -- silently (Issue #100). See [score_containment()].
#'
#' @return a "reid_scores" table with `score_type` `"similarity"`: larger
#'   means a better match.
#'
#' @references Narayanan, A. and Shmatikov, V. (2008) Robust
#'   De-anonymization of Large Sparse Datasets. IEEE Symposium on Security
#'   and Privacy, 111-125.
#'
#' @seealso [match_scoreboard_rh()] for the decision rule, [score_idf()] for
#'   value-frequency weighting.
#'
#' @examples
#' ## three people, four sparse items; the attacker knows two items each
#' anon <- data.frame(
#'   ROW_NUMBER = 1:3,
#'   I1 = c(5, NA, 1), I2 = c(NA, 2, 2), I3 = c(3, 4, NA), I4 = c(NA, 1, 5)
#' )
#' aux <- anon
#' aux$I3 <- NA
#' d <- join_raw_anon_data(aux, anon)
#' s <- score_scoreboard(d, c("I1", "I2", "I3", "I4"), tolerance = 1)
#' match_scoreboard_rh(s)
#'
#' @export
score_scoreboard <- function(dat_raw_anon, targets, row_number = "ROW_NUMBER",
                             tolerance = 0,
                             partial = c("step", "linear"),
                             aux_side = c("raw", "anon"),
                             source = c("anon", "raw", "pooled"),
                             weight = c("inv_log", "idf", "inv", "none"),
                             generalized = c("stop", "warn", "ignore"),
                             .fn_name = "score_scoreboard") {
  generalized <- match.arg(generalized)
  partial <- match.arg(partial)
  aux_side <- match.arg(aux_side)
  source <- match.arg(source)
  weight <- match.arg(weight)

  if (!is.character(targets) || length(targets) == 0) {
    stop(.fn_name, "(): `targets` must be a character vector naming at least ",
         "one attribute column.", call. = FALSE)
  }
  if (anyDuplicated(targets) > 0) {
    stop(.fn_name, "(): `targets` names the same column more than once (",
         paste(unique(targets[duplicated(targets)]), collapse = ", "),
         "), which would count its evidence twice.", call. = FALSE)
  }

  tol <- resolve_scoreboard_tolerance(tolerance, targets, .fn_name)

  total <- NULL
  raw_row <- NULL
  anon_row <- NULL

  for (t in targets) {
    cols <- reid_score_columns(dat_raw_anon, t, row_number, .fn_name,
                               generalized)
    raw_value <- dat_raw_anon[[cols$raw_target]]
    anon_value <- dat_raw_anon[[cols$anon_target]]

    if (is.null(total)) {
      raw_row <- dat_raw_anon[[cols$raw_row_number]]
      anon_row <- dat_raw_anon[[cols$anon_row_number]]
      total <- rep(0, nrow(dat_raw_anon))
    }

    ## Support: how many distinct records carry this attribute at all. Counted
    ## over records, not over rows of the candidate table -- a cross join
    ## repeats each record once per candidate, so counting rows would scale
    ## every support by the number of candidates.
    counts <- scoreboard_support(dat_raw_anon, cols, source)
    w <- idf_weight(counts$support, counts$n_records, method = weight)

    aux_value <- if (identical(aux_side, "raw")) raw_value else anon_value
    sim <- scoreboard_similarity(raw_value, anon_value, tol[[t]], partial)

    ## Attributes outside the attacker's support are not scored at all: they
    ## are not evidence of anything, in either direction.
    sim[is.na(aux_value)] <- 0

    total <- total + w * sim
  }

  new_reid_scores(
    raw_row_number = raw_row,
    anon_row_number = anon_row,
    score = total,
    score_type = "similarity"
  )
}

#' how many distinct records carry a given attribute
#'
#' @param dat_raw_anon dataframe of raw_anon form
#' @param cols resolved column names from [reid_prefixed_columns()]
#' @param source which side to count over
#'
#' @return a list with `support` (records having the attribute) and
#'   `n_records` (records counted)
#'
#' @keywords internal
scoreboard_support <- function(dat_raw_anon, cols, source) {
  side <- function(value_col, key_col) {
    keep <- !duplicated(dat_raw_anon[[key_col]])
    dat_raw_anon[[value_col]][keep]
  }

  values <- switch(
    source,
    anon = side(cols$anon_target, cols$anon_row_number),
    raw = side(cols$raw_target, cols$raw_row_number),
    pooled = c(side(cols$raw_target, cols$raw_row_number),
               side(cols$anon_target, cols$anon_row_number))
  )

  list(support = sum(!is.na(values)), n_records = length(values))
}

#' expand a scalar or named tolerance into one value per target
#'
#' @param tolerance user-supplied tolerance
#' @param targets attribute column names
#' @param fn_name name used in error messages
#'
#' @return a named list of tolerances, one per target
#'
#' @keywords internal
resolve_scoreboard_tolerance <- function(tolerance, targets, fn_name) {
  if (!is.numeric(tolerance) || anyNA(tolerance) || any(tolerance < 0)) {
    stop(fn_name, "(): `tolerance` must be non-negative and non-missing.",
         call. = FALSE)
  }

  if (length(tolerance) == 1 && is.null(names(tolerance))) {
    out <- as.list(rep(tolerance, length(targets)))
    names(out) <- targets
    return(out)
  }

  if (is.null(names(tolerance))) {
    stop(fn_name, "(): `tolerance` must be either a single number or a vector ",
         "named by target column, so it cannot be silently paired with the ",
         "wrong attribute.", call. = FALSE)
  }

  missing_tol <- setdiff(targets, names(tolerance))
  if (length(missing_tol) > 0) {
    stop(fn_name, "(): `tolerance` has no entry for target(s): ",
         paste(missing_tol, collapse = ", "), ".", call. = FALSE)
  }

  out <- as.list(tolerance[targets])
  names(out) <- targets
  out
}

#' the RH decision rule: answer only when the winner is clearly ahead
#'
#' Narayanan & Shmatikov's Scoreboard-RH pairs the score with a refusal: the
#' attacker computes the eccentricity of the score distribution -- how far the
#' best candidate is ahead of the runner-up, in units of the spread of that
#' record's scores -- and outputs no answer at all when it falls below a
#' threshold. That is what makes the attack *robust*: it trades recall for
#' precision, and an attacker who only claims what they are sure of is the
#' realistic one.
#'
#' This is a thin wrapper: it is [match_greedy()] with
#' `confidence = "margin"`, which is where the eccentricity already lives
#' (Issue #16). It exists so the algorithm can be found under the name the
#' paper gives it.
#'
#' @section Choosing phi:
#'
#' The paper's threshold of 1.5 is not portable, and using it blind is a trap
#' worth naming: eccentricity is scaled by the sd of a record's candidate
#' scores, so what counts as "clearly ahead" depends entirely on the score and
#' the number of candidates. On some of this package's fixtures no record
#' anywhere reaches 1.5, and a blind `phi = 1.5` would return zero
#' reidentifications -- output that is indistinguishable from a genuinely safe
#' data set. A threshold rejecting everything raises a warning for exactly
#' that reason. Take `phi` from the observed distribution
#' (`reid_confidence(scores, "margin")$CONFIDENCE`), or leave it at 0 and read
#' the precision-recall curve from [reid_evaluate()] instead.
#'
#' @param scores a score table, normally from [score_scoreboard()]
#' @param phi eccentricity threshold below which the attacker declines to
#'   answer (default 0: always answer, so the result is the plain best-match
#'   attack)
#' @param seed integer seed for the tie-break (default 0L)
#' @param assignment `"greedy"` (default, each ANON record chosen
#'   independently, as in the paper) or `"optimal"` to additionally impose the
#'   one-to-one constraint via [match_optimal()]
#' @param ... passed to the underlying matcher
#'
#' @return a data frame as returned by [match_greedy()]: ANON_ROW_NUMBER,
#'   RAW_ROW_NUMBER, CONFIDENCE, RESULT. Records the attacker declined have
#'   `RAW_ROW_NUMBER = NA` and `RESULT = FALSE`, and keep their row, so the
#'   trial count is unaffected by how selective the attacker was.
#'
#' @references Narayanan, A. and Shmatikov, V. (2008) Robust
#'   De-anonymization of Large Sparse Datasets. IEEE Symposium on Security
#'   and Privacy, 111-125.
#'
#' @examples
#' anon <- data.frame(
#'   ROW_NUMBER = 1:3,
#'   I1 = c(5, NA, 1), I2 = c(NA, 2, 2), I3 = c(3, 4, NA), I4 = c(NA, 1, 5)
#' )
#' d <- join_raw_anon_data(anon, anon)
#' match_scoreboard_rh(score_scoreboard(d, c("I1", "I2", "I3", "I4")))
#'
#' @export
match_scoreboard_rh <- function(scores, phi = 0, seed = 0L,
                                assignment = c("greedy", "optimal"), ...) {
  assignment <- match.arg(assignment)

  if (!is.numeric(phi) || length(phi) != 1 || is.na(phi) || phi < 0) {
    stop("match_scoreboard_rh(): `phi` must be a single non-negative number; ",
         "it is an eccentricity threshold.", call. = FALSE)
  }

  if (identical(assignment, "optimal")) {
    match_optimal(scores, seed = seed, confidence = "margin",
                  min_confidence = phi, ...)
  } else {
    match_greedy(scores, seed = seed, confidence = "margin",
                 min_confidence = phi, ...)
  }
}
