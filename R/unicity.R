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
#' @param dat a data frame with one row per individual (master form)
#' @param columns character vector of column names
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
unicity_fraction <- function(dat, columns) {
  if (!is.data.frame(dat)) {
    stop("`dat` must be a data frame with one row per individual.", call. = FALSE)
  }
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

  ## "\r" is used as the field separator because it cannot appear in the
  ## colon-joined distribution strings this package produces, so two different
  ## value combinations cannot be flattened into the same key.
  key <- do.call(paste, c(lapply(columns, function(cn) as.character(dat[[cn]])), sep = "\r"))
  mean(!(duplicated(key) | duplicated(key, fromLast = TRUE)))
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
                    n_samples = 100, seed = 0L) {
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
        function(idx) unicity_fraction(dat, attributes[idx]),
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
