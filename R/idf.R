## ---------------------------------------------------------------------------
## Rarity-weighted exact matching (Issue #17)
##
## Every score so far treats all agreements on a column as equally telling. But
## "lives in Tokyo" and "lives in a village of 300" are not the same evidence:
## the first narrows the field by a factor of ten, the second identifies
## somebody. Weighting an agreement by the rarity of the value it agrees on --
## the inverse document frequency idea from information retrieval -- costs
## nothing in attacker knowledge, because the frequencies can be counted off
## the released ANON table itself.
##
## ORIENTATION. The natural formulation is a similarity,
##
##     sim(raw, anon) = sum over columns of  w(v) * 1[raw_v == anon_v]
##
## but this package keeps every score as a distance (R/score.R). The two are
## interchangeable *here*, because the weights are keyed on the ANON record's
## value: writing W = sum over columns of w(v_anon), which depends only on the
## ANON record and not on which RAW candidate is being considered,
##
##     dist(raw, anon) = W - sim(raw, anon)
##                     = sum over columns of  w(v) * 1[raw_v != anon_v]
##
## and subtracting a per-ANON-record constant cannot reorder that record's
## candidates. So the distance form ranks identically to the similarity form
## while staying on the orientation the rest of the package uses -- no
## sign-flipped score can sneak in (docs/lessons-learned.md section 2).
##
## NO PER-COLUMN NORMALISATION. score_multi() rescales each column so that no
## column dominates by accident (Issue #14). That must not be applied inside an
## IDF block: the relative size of the weights *is* the method. The block is
## therefore summed at its own scale first, and only the finished block is
## normalised against the other attributes.
## ---------------------------------------------------------------------------

#' count how often each value of a column occurs
#'
#' Counting is done over *distinct records*, deduplicated by row number, not
#' over the rows of the candidate table: a cross join repeats every record once
#' per candidate, so counting the rows would report frequencies proportional to
#' how many candidates each record happens to have.
#'
#' @inheritParams score_num
#' @param source which side to count: `"anon"` (default), `"raw"` or
#'   `"pooled"`. The default is `"anon"` because the released table is
#'   available to any attacker -- estimating rarity from it needs no knowledge
#'   the adversary does not already have, which is what makes this weighting
#'   cheap to justify.
#'
#' @return a data frame with columns `VALUE`, `COUNT` and `SHARE`, ordered by
#'   decreasing count, plus an `n_records` attribute giving the number of
#'   records counted
#'
#' @examples
#' d <- create_dummy_qi_data(people = 30, seed = 1)
#' value_frequencies(join_raw_anon_data(d, d), "ZIP")
#'
#' @export
value_frequencies <- function(dat_raw_anon, target, row_number = "ROW_NUMBER",
                              source = c("anon", "raw", "pooled"),
                              .fn_name = "value_frequencies") {
  source <- match.arg(source)
  cols <- reid_prefixed_columns(dat_raw_anon, target, row_number, .fn_name)

  side <- function(value_col, key_col) {
    keep <- !duplicated(dat_raw_anon[[key_col]])
    as.character(dat_raw_anon[[value_col]][keep])
  }

  values <- switch(
    source,
    anon = side(cols$anon_target, cols$anon_row_number),
    raw = side(cols$raw_target, cols$raw_row_number),
    pooled = c(side(cols$raw_target, cols$raw_row_number),
               side(cols$anon_target, cols$anon_row_number))
  )

  if (anyNA(values)) {
    stop(.fn_name, "(): column \"", target, "\" contains NA on the ", source,
         " side. A missing value has no frequency, and treating it as its own ",
         "category would make every NA record look rare -- i.e. more ",
         "identifiable -- for the wrong reason.", call. = FALSE)
  }

  tab <- table(values)
  out <- data.frame(
    VALUE = names(tab),
    COUNT = as.integer(tab),
    stringsAsFactors = FALSE
  )
  out$SHARE <- out$COUNT / length(values)
  out <- out[order(-out$COUNT, out$VALUE), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "n_records") <- length(values)
  out
}

#' turn value counts into rarity weights
#'
#' @param count integer vector of occurrence counts
#' @param n number of records the counts were taken over
#' @param method the weighting scheme:
#'   \describe{
#'     \item{`"idf"`}{`log(n / count)`, the standard inverse document
#'       frequency. A value every record shares gets weight exactly 0, which
#'       is the honest answer: agreeing on it rules nothing out.}
#'     \item{`"inv_log"`}{`1 / log(count + 1)`, the form named on Issue #17.
#'       The `+ 1` is not cosmetic -- `1 / log(1)` is infinite, and a singleton
#'       value is precisely the case the method exists to handle, so the
#'       unshifted formula diverges exactly where it matters most.}
#'     \item{`"inv"`}{`1 / count`, which falls off far faster than either log
#'       form.}
#'     \item{`"none"`}{constant 1 -- plain unweighted exact matching (Hamming
#'       distance), kept as the comparison baseline Issue #17 asks to beat.}
#'   }
#'
#' @return numeric vector of non-negative weights
#'
#' @keywords internal
idf_weight <- function(count, n, method = c("idf", "inv_log", "inv", "none")) {
  method <- match.arg(method)
  ## A value the frequency table has never seen cannot be rarer than a
  ## singleton, so it is floored there rather than given infinite weight.
  count <- pmax(as.numeric(count), 1)

  switch(
    method,
    idf = pmax(log(n / count), 0),
    inv_log = 1 / log(count + 1),
    inv = 1 / count,
    none = rep(1, length(count))
  )
}

#' score one column by rarity-weighted exact matching
#'
#' A candidate pair scores 0 when the two records carry the same value, and the
#' rarity weight of that value when they do not. Rare values therefore
#' discriminate strongly and near-universal ones barely at all.
#'
#' @inheritParams score_num
#' @inheritParams value_frequencies
#' @param weight rarity weighting scheme; see [idf_weight()]. `"none"` gives
#'   plain unweighted exact matching.
#'
#' @return a "reid_scores" table whose SCORE is 0 on agreement and the value's
#'   rarity weight on disagreement (a distance: smaller is a better match)
#'
#' @examples
#' d <- create_dummy_qi_data(people = 30, seed = 1)
#' j <- join_raw_anon_data(d, d)
#' match_greedy(score_idf(j, "ZIP"))
#'
#' @export
score_idf <- function(dat_raw_anon, target, row_number = "ROW_NUMBER",
                      source = c("anon", "raw", "pooled"),
                      weight = c("idf", "inv_log", "inv", "none"),
                      .fn_name = "score_idf") {
  source <- match.arg(source)
  weight <- match.arg(weight)

  cols <- reid_prefixed_columns(dat_raw_anon, target, row_number, .fn_name)
  freq <- value_frequencies(dat_raw_anon, target, row_number = row_number,
                            source = source, .fn_name = .fn_name)

  raw_value <- as.character(dat_raw_anon[[cols$raw_target]])
  anon_value <- as.character(dat_raw_anon[[cols$anon_target]])
  if (anyNA(raw_value) || anyNA(anon_value)) {
    stop(.fn_name, "(): column \"", target, "\" contains NA. Whether two ",
         "missing values \"agree\" is a modelling decision, and guessing it ",
         "here would silently change the reported risk.", call. = FALSE)
  }

  ## Keyed on the ANON value: see the note at the top of this file for why
  ## that is what makes the distance form rank identically to the similarity
  ## form the method is usually written in.
  counts <- freq$COUNT[match(anon_value, freq$VALUE)]
  counts[is.na(counts)] <- 0L
  w <- idf_weight(counts, attr(freq, "n_records"), method = weight)

  new_reid_scores(
    raw_row_number = dat_raw_anon[[cols$raw_row_number]],
    anon_row_number = dat_raw_anon[[cols$anon_row_number]],
    score = w * (raw_value != anon_value)
  )
}

#' score several columns at once by rarity-weighted exact matching
#'
#' The sum of [score_idf()] over `targets`: the total rarity weight of the
#' columns on which the two records disagree. Equivalently (up to a per-ANON
#' record constant, which cannot change that record's ranking) the total rarity
#' weight of the columns on which they agree.
#'
#' The columns are summed at their own scale, deliberately without the
#' per-column normalisation [score_multi()] applies: the relative size of the
#' weights across values *and* across columns is the method itself, so
#' rescaling each column to a common range would throw away exactly what is
#' being computed. The finished block can then be normalised against other
#' attributes, which is what `score_multi()` does with `"idf"` targets.
#'
#' @inheritParams score_idf
#' @param targets character vector of column names, *before* the RAW_/ANON_
#'   prefixing done by [join_raw_anon_data()]
#'
#' @return a "reid_scores" table (a distance: smaller is a better match)
#'
#' @examples
#' d <- create_dummy_qi_data(people = 30, seed = 1)
#' j <- join_raw_anon_data(d, d)
#' match_greedy(score_idf_match(j, c("ZIP", "SEX")))
#'
#' @export
score_idf_match <- function(dat_raw_anon, targets, row_number = "ROW_NUMBER",
                            source = c("anon", "raw", "pooled"),
                            weight = c("idf", "inv_log", "inv", "none"),
                            .fn_name = "score_idf_match") {
  source <- match.arg(source)
  weight <- match.arg(weight)

  if (!is.character(targets) || length(targets) == 0) {
    stop(.fn_name, "(): `targets` must be a character vector naming at least ",
         "one column.", call. = FALSE)
  }
  if (anyDuplicated(targets) > 0) {
    stop(.fn_name, "(): `targets` names the same column more than once (",
         paste(unique(targets[duplicated(targets)]), collapse = ", "),
         "), which would count its evidence twice.", call. = FALSE)
  }

  parts <- lapply(targets, function(t) {
    score_idf(dat_raw_anon, t, row_number = row_number, source = source,
              weight = weight, .fn_name = .fn_name)
  })

  ## No normalisation: combine_scores() sums the raw columns, which is the
  ## whole point of an IDF block.
  combine_scores(parts)
}
