## ---------------------------------------------------------------------------
## Unicity (Issue #21)
##
## Every other measurement in this package is the success rate of a particular
## attack, so a good result only ever supports the weak claim "method X did not
## get in". Unicity is method-independent: it asks how many records are already
## distinguishable from every other record given p attributes.
##
## What it bounds, precisely: unicity is the fraction that can be pinned down
## with *certainty* from those attributes. It is a lower bound on the expected
## success rate of an actual attack, not an upper bound -- a record sharing its
## attribute values with m - 1 others counts as 0 here, but an attacker guessing
## among the m still wins it with probability 1/m. On a 40-person fixture with
## unicity 0.950 on (AGE, ZIP, SEX), the matching attack restricted to those
## same three columns scores 0.975 for exactly that reason.
##
## de Montjoye et al.'s unicity curves are the reference form: sweep p, and
## report the fraction of records that are unique.
## ---------------------------------------------------------------------------

#' fraction of records that are unique on a given set of columns
#'
#' A record is unique with respect to `columns` when no other record in `dat`
#' carries exactly the same combination of values on those columns.
#'
#' Non-numeric columns compare by R's own equality, as used by [match()]: `NA`
#' is a value of its own and is not the string `"NA"`, and nothing is confused
#' with anything else because of how it prints. Numeric columns compare up to
#' the same relative `tolerance` the score and assignment layers use for ties
#' (Issue #61), so `0.1 + 0.2` and `0.3` count as one value here just as
#' [reid_evaluate()] counts them as tied. `tolerance = 0` restores exact
#' comparison on both sides.
#'
#' Sharing the tolerance is load-bearing, not cosmetic. Unicity is documented
#' as a *lower bound* on the success rate of a real attack. If unicity compared
#' doubles bit-for-bit while the attack compared them within a tolerance,
#' records that no attack can separate would be counted as unique and unicity
#' would rise above the attack it is supposed to bound.
#'
#' The two are not the same test, and do not need to be: the tolerance applies
#' to *values* here and to the *distances between them* in the score layer. On
#' `c(1e15, 1e15 + 1)` unicity reports 0 while the attack reports 1, because
#' the attack sees distances 0 and 1 rather than two values a relative 1e-15
#' apart. Unicity is the more conservative of the two, which is the only
#' direction its contract allows.
#'
#' Adding attributes can never lower the result: the equivalence classes of a
#' larger attribute set refine those of a smaller one, and refining a class of
#' size 1 cannot destroy it. `unicity_fraction(dat, S) <= unicity_fraction(dat,
#' T)` holds for every `S` contained in `T`, and is pinned down as a property
#' test.
#'
#' @param dat a data frame with one row per individual (master form)
#' @param columns character vector of column names
#' @param tolerance relative tolerance for calling two numeric values the same,
#'   defaulting to the package-wide tie tolerance (see [reid_tie_tolerance()]);
#'   0 compares doubles exactly
#'
#' @return the proportion of rows of `dat` that are unique on `columns`, in
#'   \[0, 1\]. With `columns` empty, every record looks identical and the
#'   result is 0 (unless `dat` has a single row).
#'
#' @examples
#' dat <- data.frame(A = c(1, 1, 2, 2), B = c(1, 2, 1, 2))
#' unicity_fraction(dat, "A")
#' unicity_fraction(dat, c("A", "B"))
#'
#' @export
unicity_fraction <- function(dat, columns,
                             tolerance = reid_tie_tolerance()) {
  if (!is.data.frame(dat)) {
    stop("`dat` must be a data frame with one row per individual.", call. = FALSE)
  }
  validate_tie_tolerance(tolerance, "unicity_fraction")
  missing_cols <- setdiff(columns, names(dat))
  if (length(missing_cols) > 0) {
    stop("column(s) not found in `dat`: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  if (nrow(dat) == 0) {
    return(0)
  }

  if (length(columns) == 0) {
    ## No attributes at all: every record is indistinguishable from every
    ## other, so only a single-record data set is "unique".
    return(as.numeric(nrow(dat) == 1))
  }

  key <- unicity_key(dat, columns, tolerance)
  mean(!(duplicated(key) | duplicated(key, fromLast = TRUE)))
}

#' collision-free key for a combination of column values
#'
#' Encodes each column as equivalence-class codes and joins the codes, so that
#' two rows share a key exactly when they agree on every column.
#'
#' The obvious implementation -- `paste(as.character(col1), as.character(col2),
#' sep = "\\r")`, which is also what `duplicated.data.frame()` does -- has three
#' failure modes, and **all three collapse distinct records onto one key** and
#' therefore report a *lower* unicity than the truth. That is the safe-looking
#' direction, which a safety-checking tool must never take quietly
#' (docs/lessons-learned.md section 2):
#'
#' 1. a value can contain the separator. `A = c("x", "x\\ry")` with
#'    `B = c("y\\rz", "z")` yields `"x\\ry\\rz"` for both rows; a carriage
#'    return reaches a column routinely, e.g. from a CRLF-quoted CSV field.
#' 2. `as.character()` prints a double to 15 significant digits, so
#'    `0.1 + 0.2` and `0.3`, or `1e15` and `1e15 + 1`, produce the same text.
#' 3. `NA` prints as `"NA"`, which is also a perfectly ordinary string value.
#'
#' `match(x, unique(x))` avoids all three: it compares the values themselves
#' rather than their printed form, keeps `NA` as a class of its own, and
#' returns small integers whose decimal representation cannot contain the
#' separator -- so joining the codes is injective.
#'
#' Numeric columns go through [snap_tied_values()] first, so that near-equal
#' doubles land in one class -- the same rule the score and assignment layers
#' apply to ties (Issue #61). Without it, unicity would count records as
#' distinguishable that `reid_evaluate()` treats as tied, and would then report
#' *more* uniqueness than the attack it is meant to lower-bound.
#'
#' @param dat a data frame
#' @param columns character vector of column names, at least one
#' @param tolerance relative tolerance for numeric columns; 0 compares exactly
#'
#' @return a character vector with one key per row of `dat`
#'
#' @keywords internal
unicity_key <- function(dat, columns, tolerance = reid_tie_tolerance()) {
  codes <- lapply(columns, function(cn) {
    v <- dat[[cn]]
    if (!is.null(dim(v))) {
      stop("column `", cn, "` is a matrix or data frame column; unicity needs ",
           "one value per record.", call. = FALSE)
    }
    ## Integers and logicals are exact already; snapping them would only cost
    ## time. Factors and characters have no notion of "near".
    if (is.double(v)) {
      v <- snap_tied_values(v, tolerance)
    }
    match(v, unique(v))
  })
  do.call(paste, c(codes, list(sep = "\r")))
}

#' measure unicity as a function of the number of known attributes
#'
#' Sweeps `p` and, for each value, evaluates [unicity_fraction()] over
#' subsets of `attributes` of that size. Small `p` are enumerated exhaustively
#' whenever there are no more than `n_samples` subsets, which makes those rows
#' exact rather than sampled; larger `p` fall back to a random sample of
#' subsets.
#'
#' The result is a plain data frame so it can go straight into a report.
#'
#' `attributes` must not include a record identifier: a column that is unique
#' by construction (a row number, a customer ID) would drive the curve to 1
#' and say nothing about the data.
#'
#' @param dat a data frame with one row per individual (master form)
#' @param attributes character vector of the attribute columns an attacker
#'   might know
#' @param p integer vector of subset sizes to evaluate (default: every size
#'   from 1 to `length(attributes)`)
#' @param n_samples maximum number of attribute subsets evaluated per `p`
#'   (default 100)
#' @param seed integer seed for the subset sampling (default 0L, so a plain
#'   call is reproducible); NULL uses the ambient RNG stream
#' @param tolerance relative tolerance for calling two numeric values the same,
#'   passed to [unicity_fraction()]
#'
#' @return a data frame with one row per value of `p` and columns
#'   \describe{
#'     \item{p}{number of known attributes}
#'     \item{n_subsets}{how many attribute subsets were evaluated}
#'     \item{exhaustive}{TRUE when every subset of that size was evaluated, so
#'       `unicity_mean` is exact rather than estimated}
#'     \item{unicity_mean, unicity_sd, unicity_min, unicity_max}{fraction of
#'       records that are unique, averaged over those subsets, with its spread}
#'   }
#'
#' @examples
#' d <- create_dummy_qi_data(people = 50, seed = 1)
#' unicity(d, attributes = c("AGE", "ZIP", "SEX", "VISIT_COUNT", "SPEND_MEAN"))
#'
#' @importFrom stats sd
#' @importFrom utils combn
#' @export
unicity <- function(dat, attributes, p = seq_along(attributes),
                    n_samples = 100, seed = 0L,
                    tolerance = reid_tie_tolerance()) {
  if (!is.data.frame(dat)) {
    stop("`dat` must be a data frame with one row per individual.", call. = FALSE)
  }
  if (!is.character(attributes) || length(attributes) == 0) {
    stop("`attributes` must be a character vector naming at least one column.",
         call. = FALSE)
  }
  if (anyDuplicated(attributes) > 0) {
    stop("`attributes` must not contain duplicates.", call. = FALSE)
  }
  missing_cols <- setdiff(attributes, names(dat))
  if (length(missing_cols) > 0) {
    stop("column(s) not found in `dat`: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  n_attr <- length(attributes)
  p <- sort(unique(as.integer(p)))
  bad_p <- p[p < 1 | p > n_attr]
  if (length(bad_p) > 0) {
    stop("`p` must lie between 1 and length(attributes) (", n_attr, "); got ",
         paste(bad_p, collapse = ", "), ".", call. = FALSE)
  }
  if (!is.numeric(n_samples) || length(n_samples) != 1 || n_samples < 1) {
    stop("`n_samples` must be a single number >= 1.", call. = FALSE)
  }

  rows <- with_local_seed(seed, {
    lapply(p, function(this_p) {
      n_total <- choose(n_attr, this_p)

      if (n_total <= n_samples) {
        ## Exhaustive: the reported mean is the exact expectation over all
        ## subsets of this size, not an estimate of it.
        subsets <- as.list(as.data.frame(utils::combn(n_attr, this_p)))
        exhaustive <- TRUE
      } else {
        subsets <- lapply(seq_len(n_samples), function(i) {
          sort(sample.int(n_attr, this_p))
        })
        ## Distinct subsets only: a repeated draw would silently weight that
        ## subset twice in the mean.
        subsets <- unique(subsets)
        exhaustive <- FALSE
      }

      values <- vapply(
        subsets,
        function(idx) unicity_fraction(dat, attributes[idx], tolerance),
        numeric(1)
      )

      data.frame(
        p = this_p,
        n_subsets = length(values),
        exhaustive = exhaustive,
        unicity_mean = mean(values),
        ## A single subset has no spread to report; sd() would give NA, which
        ## reads as "missing" rather than "exact".
        unicity_sd = if (length(values) > 1) stats::sd(values) else 0,
        unicity_min = min(values),
        unicity_max = max(values),
        stringsAsFactors = FALSE
      )
    })
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
