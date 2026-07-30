## ---------------------------------------------------------------------------
## Multi-attribute integration (Issue #14)
##
## Until now an attack could only ever use one attribute at a time, so the
## reported success rate was the risk from the *weakest* realistic adversary.
## A real attacker joins on everything they hold at once.
##
## Combining attributes needs two things that the score layer deliberately left
## out (see the note in combine_scores()):
##
##  1. A normalisation, because the per-attribute scores live on wildly
##     different scales -- an edit distance is a small integer, a squared L2
##     quantile distance is unbounded -- so a plain sum is decided by whichever
##     metric happens to have the widest spread. Without it, *adding* an
##     attribute the attacker knows can make the attack weaker, which is the
##     failure that forced the stopgap normalisation into
##     score_by_knowledge() for Issue #13.
##
##  2. A way to stop counting redundant attributes twice. Two strongly
##     correlated columns carry roughly one column's worth of evidence, but a
##     weighted sum gives them two columns' worth of influence, at the expense
##     of an independent column that may be more informative. Mahalanobis
##     distance, using the covariance of the reference (RAW) population,
##     whitens that redundancy away.
##
## NORMALISATION IS GLOBAL, NOT PER-ANON-RECORD. Each column is rescaled using
## statistics over the whole candidate table, not separately within each ANON
## record's candidate set. Per-record rescaling was considered and rejected:
## it would make the combined score of two different ANON records
## incomparable, and reid_evaluate() reads scores across records (the mode
## baseline, the precision-recall sweep). A global affine or monotone rescaling
## per column leaves every per-record ranking unchanged, so it can only change
## the *relative weight* of columns -- which is the whole point -- and never
## silently reorders a single record's candidates.
## ---------------------------------------------------------------------------

#' rescale score tables so several attributes can be summed
#'
#' Puts the SCORE column of one or more score tables onto a common scale, so
#' that [combine_scores()] adds comparable quantities. Every method is a
#' monotone transformation applied identically to every candidate pair, so the
#' ranking *within* a single score table is untouched; only the relative
#' influence of the tables on their sum changes.
#'
#' A column whose score never varies is mapped to constant 0 under every
#' method: it cannot discriminate between candidates, so it must not shift the
#' combined total either.
#'
#' @param scores a score table (see [score_num()]) or a list of them
#' @param method one of
#'   \describe{
#'     \item{`"range"`}{min-max rescaling to \[0, 1\]. Bounded and easy to
#'       read, but a single extreme pair compresses everything else.}
#'     \item{`"zscore"`}{subtract the mean, divide by the standard deviation.
#'       Scale-free; unbounded, and the result can be negative (harmless -- a
#'       constant shift is the same for every candidate).}
#'     \item{`"rank"`}{replace each score by its rank, rescaled to \[0, 1\] --
#'       that is, the empirical CDF of the score. Invariant to any monotone
#'       reparameterisation of the metric, so an edit distance and an unbounded
#'       L2 distance contribute equally regardless of their units. Ties share
#'       the average rank, so a heavily tied column spans less than the full
#'       \[0, 1\] and therefore counts for less -- which is the right answer,
#'       since a column that puts many candidates on the same score
#'       discriminates between fewer of them.}
#'     \item{`"none"`}{leave the scores alone.}
#'   }
#'
#' @return an object of the same shape as `scores` (a single score table, or a
#'   list of them), with rescaled SCORE columns and unchanged `score_type`
#'
#' @examples
#' raw <- data.frame(ROW_NUMBER = 1:5, V = c(10, 20, 30, 40, 50), W = c(1, 1, 2, 2, 3))
#' d <- join_raw_anon_data(raw, raw)
#' s <- normalize_scores(list(score_num(d, "V"), score_num(d, "W")), method = "range")
#' match_greedy(combine_scores(s))
#'
#' @export
normalize_scores <- function(scores, method = c("range", "zscore", "rank", "none")) {
  method <- match.arg(method)

  if (is.data.frame(scores)) {
    return(normalize_one_score(scores, method, "scores"))
  }
  if (!is.list(scores)) {
    stop("`scores` must be a score table or a list of score tables; got a ",
         class(scores)[1], ".", call. = FALSE)
  }

  out <- lapply(
    seq_along(scores),
    function(i) normalize_one_score(scores[[i]], method, paste0("scores[[", i, "]]"))
  )
  names(out) <- names(scores)
  out
}

#' rescale a single score table
#'
#' @param s a score table
#' @param method normalisation method, already matched
#' @param arg argument name used in error messages
#'
#' @return a "reid_scores" table
#'
#' @keywords internal
#'
#' @importFrom stats sd
normalize_one_score <- function(s, method, arg = "scores") {
  score_type <- validate_reid_scores(s, arg)

  v <- as.numeric(s$SCORE)
  if (anyNA(v)) {
    stop("`", arg, "$SCORE` contains NA; a score cannot be normalised before ",
         "it has a value for every candidate pair.", call. = FALSE)
  }
  n <- length(v)
  flat <- rep(0, n)

  out <- switch(
    method,
    none = v,
    range = {
      rng <- range(v)
      span <- rng[2] - rng[1]
      if (span > 0) (v - rng[1]) / span else flat
    },
    zscore = {
      spread <- stats::sd(v)
      if (isTRUE(spread > 0)) (v - mean(v)) / spread else flat
    },
    rank = {
      if (n > 1 && diff(range(v)) > 0) {
        (rank(v, ties.method = "average") - 1) / (n - 1)
      } else {
        flat
      }
    }
  )

  new_reid_scores(
    raw_row_number = s$RAW_ROW_NUMBER,
    anon_row_number = s$ANON_ROW_NUMBER,
    score = out,
    score_type = score_type
  )
}

#' score several numeric columns jointly by Mahalanobis distance
#'
#' Treats the chosen columns as one vector-valued attribute and scores a
#' candidate pair by the Mahalanobis distance between the RAW and ANON vectors,
#'
#' \deqn{D = \sqrt{(x_{RAW} - x_{ANON})^\top S^{-1} (x_{RAW} - x_{ANON})}}
#'
#' where \eqn{S} is the covariance matrix of the reference population
#' (by default the distinct RAW records).
#'
#' WHY NOT JUST ADD THE COLUMNS UP. A weighted sum treats every column as an
#' independent piece of evidence. Two columns that are strongly correlated
#' carry roughly one column's worth of information but get two columns' worth
#' of influence, which comes out of the budget of whatever independent column
#' is competing with them. \eqn{S^{-1}} removes exactly that double counting,
#' and at the same time up-weights directions in which the population barely
#' varies -- a small disagreement along such a direction is far more
#' surprising, and therefore far more identifying, than the same disagreement
#' along a direction the population is spread out over anyway.
#'
#' The covariance is estimated from *distinct records*, deduplicated by row
#' number, not from the candidate pairs: a cross join repeats every record once
#' per candidate, and estimating from the repeated rows would silently weight
#' each record by how many candidates it happens to have.
#'
#' @inheritParams score_num
#' @param targets character vector of numeric column names, *before* the
#'   RAW_/ANON_ prefixing done by [join_raw_anon_data()]
#' @param cov_from which side to estimate the covariance from: `"raw"`
#'   (default -- the attacker's own reference population), `"anon"`, or
#'   `"pooled"` (the distinct records of both sides stacked).
#' @param ridge non-negative shrinkage applied to the diagonal of the
#'   covariance matrix, as a multiple of its mean diagonal entry (default
#'   1e-6). Needed because redundant columns -- exactly the case Mahalanobis
#'   exists for -- make \eqn{S} ill-conditioned or singular.
#' @param squared return the squared distance instead of the distance
#'   (default FALSE). The two give identical rankings; the square root is the
#'   default because it shares the units of the underlying columns.
#'
#' @return a "reid_scores" table whose SCORE is the Mahalanobis distance
#'   (a distance: smaller is a better match)
#'
#' @examples
#' set.seed(1)
#' n <- 30
#' a <- rnorm(n)
#' raw <- data.frame(ROW_NUMBER = 1:n, A = a, B = 2 * a + rnorm(n, sd = 0.1))
#' anon <- raw
#' anon$A <- round(anon$A, 1)
#' anon$B <- round(anon$B, 1)
#' d <- join_raw_anon_data(raw, anon)
#' match_greedy(score_mahalanobis(d, c("A", "B")))
#'
#' @importFrom stats cov var
#' @export
score_mahalanobis <- function(dat_raw_anon, targets, row_number = "ROW_NUMBER",
                              cov_from = c("raw", "anon", "pooled"),
                              ridge = 1e-6, squared = FALSE,
                              .fn_name = "score_mahalanobis") {
  cov_from <- match.arg(cov_from)

  if (!is.character(targets) || length(targets) == 0) {
    stop(.fn_name, "(): `targets` must be a character vector naming at least ",
         "one column.", call. = FALSE)
  }
  if (anyDuplicated(targets) > 0) {
    stop(.fn_name, "(): `targets` names the same column more than once (",
         paste(unique(targets[duplicated(targets)]), collapse = ", "),
         "). A repeated column would be counted twice in the covariance.",
         call. = FALSE)
  }
  if (!is.numeric(ridge) || length(ridge) != 1 || is.na(ridge) || ridge < 0) {
    stop(.fn_name, "(): `ridge` must be a single non-negative number.",
         call. = FALSE)
  }

  cols <- lapply(targets, function(t) {
    reid_prefixed_columns(dat_raw_anon, t, row_number, .fn_name)
  })

  as_numeric_column <- function(nm) {
    v <- dat_raw_anon[[nm]]
    if (!is.numeric(v)) {
      stop(.fn_name, "(): column \"", nm, "\" is ", class(v)[1],
           ", not numeric. Mahalanobis distance is defined on numeric ",
           "vectors; use score_char()/score_dist() for the other kinds and ",
           "combine the results with combine_scores().", call. = FALSE)
    }
    as.numeric(v)
  }

  raw_mat <- vapply(cols, function(cc) as_numeric_column(cc$raw_target),
                    numeric(nrow(dat_raw_anon)))
  anon_mat <- vapply(cols, function(cc) as_numeric_column(cc$anon_target),
                     numeric(nrow(dat_raw_anon)))
  raw_mat <- matrix(raw_mat, nrow = nrow(dat_raw_anon))
  anon_mat <- matrix(anon_mat, nrow = nrow(dat_raw_anon))

  if (anyNA(raw_mat) || anyNA(anon_mat)) {
    stop(.fn_name, "(): target column(s) contain NA/missing values. A missing ",
         "coordinate would drop out of the quadratic form and report a ",
         "smaller distance -- i.e. a more confident match -- than the data ",
         "supports. Remove or impute them first.", call. = FALSE)
  }

  raw_key <- dat_raw_anon[[cols[[1]]$raw_row_number]]
  anon_key <- dat_raw_anon[[cols[[1]]$anon_row_number]]

  ref <- switch(
    cov_from,
    raw = raw_mat[!duplicated(raw_key), , drop = FALSE],
    anon = anon_mat[!duplicated(anon_key), , drop = FALSE],
    pooled = rbind(
      raw_mat[!duplicated(raw_key), , drop = FALSE],
      anon_mat[!duplicated(anon_key), , drop = FALSE]
    )
  )
  if (nrow(ref) < 2) {
    stop(.fn_name, "(): need at least 2 distinct ", cov_from, " records to ",
         "estimate a covariance matrix; got ", nrow(ref), ".", call. = FALSE)
  }

  spread <- apply(ref, 2, stats::var)
  keep <- is.finite(spread) & spread > 0
  if (!any(keep)) {
    stop(.fn_name, "(): every target column is constant on the ", cov_from,
         " side, so the covariance matrix is zero and no candidate can be ",
         "told from another.", call. = FALSE)
  }
  if (any(!keep)) {
    warning(.fn_name, "(): dropping constant column(s) ",
            paste(targets[!keep], collapse = ", "),
            " -- zero variance on the ", cov_from, " side makes the ",
            "covariance matrix singular, and a column that never varies ",
            "cannot discriminate between candidates anyway.", call. = FALSE)
  }

  cov_mat <- stats::cov(ref[, keep, drop = FALSE])
  cov_mat <- cov_mat + diag(ridge * mean(diag(cov_mat)), nrow(cov_mat))

  inv <- tryCatch(
    solve(cov_mat),
    error = function(e) {
      stop(.fn_name, "(): the covariance matrix of (",
           paste(targets[keep], collapse = ", "),
           ") could not be inverted even after ridge = ", ridge,
           ". Increase `ridge`, or drop one of a pair of exactly collinear ",
           "columns. Original error: ", conditionMessage(e), call. = FALSE)
    }
  )

  delta <- raw_mat[, keep, drop = FALSE] - anon_mat[, keep, drop = FALSE]
  quad <- rowSums((delta %*% inv) * delta)
  ## A quadratic form with a positive definite matrix is non-negative; any
  ## negative value here is floating point noise around zero, and letting it
  ## through would produce NaN from sqrt().
  quad[quad < 0] <- 0

  new_reid_scores(
    raw_row_number = raw_key,
    anon_row_number = anon_key,
    score = if (isTRUE(squared)) quad else sqrt(quad)
  )
}

#' expand a column-to-score-type specification
#'
#' Accepts the named form used by [attacker_knowledge()]
#' (`c(AGE = "num", ZIP = "char")`) and the shorthand of a plain character
#' vector of column names, which is read as all-numeric.
#'
#' @param targets the specification to expand
#' @param fn_name name used in error messages
#'
#' @return a named character vector of column name to score type
#'
#' @keywords internal
expand_target_spec <- function(targets, fn_name) {
  if (!is.character(targets) || length(targets) == 0) {
    stop(fn_name, "(): `targets` must be a character vector naming at least ",
         "one column, optionally named by score type ",
         "(e.g. c(AGE = \"num\", ZIP = \"char\")).", call. = FALSE)
  }

  if (is.null(names(targets)) || all(names(targets) == "")) {
    ## Shorthand: c("A", "B") means "these columns, all numeric".
    out <- rep("num", length(targets))
    names(out) <- targets
    targets <- out
  }
  if (any(names(targets) == "")) {
    stop(fn_name, "(): `targets` mixes named and unnamed entries. Give every ",
         "column a score type, or none of them.", call. = FALSE)
  }

  unknown <- setdiff(unique(unname(targets)), c("num", "char", "dist", "rank"))
  if (length(unknown) > 0) {
    stop(fn_name, "(): unknown score type(s): ",
         paste(unknown, collapse = ", "),
         ". Expected \"num\", \"char\", \"dist\" or \"rank\".", call. = FALSE)
  }
  if (anyDuplicated(names(targets)) > 0) {
    stop(fn_name, "(): `targets` names the same column more than once (",
         paste(unique(names(targets)[duplicated(names(targets))]), collapse = ", "),
         ").", call. = FALSE)
  }

  targets
}

#' build one score table per declared column
#'
#' @inheritParams score_multi
#' @param targets an already-expanded named character vector
#' @param fn_name name used in error messages
#'
#' @return a named list of "reid_scores" tables, in the order of `targets`
#'
#' @keywords internal
build_target_scores <- function(dat_raw_anon, targets, row_number, split, fn_name) {
  out <- lapply(seq_along(targets), function(i) {
    target <- names(targets)[i]
    type <- unname(targets[i])
    fn <- score_fn_for_type(type)
    if (identical(type, "dist")) {
      fn(dat_raw_anon, target, row_number = row_number, split = split)
    } else {
      fn(dat_raw_anon, target, row_number = row_number)
    }
  })
  names(out) <- names(targets)
  out
}

#' attack several attributes at once
#'
#' Scores every declared column, puts the results on a common scale and adds
#' them up, so that a candidate is judged on everything the attacker holds
#' rather than on one attribute at a time.
#'
#' @section Choosing `method`:
#'
#' `"weighted"` scores each column separately, normalises, and takes the
#' weighted sum. Every column is treated as independent evidence.
#'
#' `"mahalanobis"` scores all `"num"` columns *jointly* with
#' [score_mahalanobis()], using the covariance of the reference population, and
#' adds any remaining `"char"` / `"dist"` / `"rank"` columns as separate
#' normalised terms. The Mahalanobis block is given the combined weight of the
#' numeric columns it absorbed, so the two methods spend the same total weight
#' on the same columns and their success rates can be compared directly.
#' Use it when some of the numeric columns are correlated -- see
#' [score_mahalanobis()] for why a plain sum double-counts them.
#'
#' @inheritParams score_num
#' @param targets either a named character vector mapping column name to score
#'   type -- `c(AGE = "num", ZIP = "char")`, the same form
#'   [attacker_knowledge()] uses -- or a plain character vector of column
#'   names, which is read as all-numeric.
#' @param weights numeric vector of per-column weights, one per entry of
#'   `targets`, in the same order (default: all 1)
#' @param normalize normalisation applied to each component before summing;
#'   see [normalize_scores()]. The default `"range"` is bounded and matches
#'   the stopgap that Issue #13 needed.
#' @param method `"weighted"` (default) or `"mahalanobis"`; see above
#' @param split separator passed to [score_dist()] for `"dist"` columns
#' @param cov_from,ridge passed to [score_mahalanobis()] when
#'   `method = "mahalanobis"`
#'
#' @return a "reid_scores" table over the candidate pairs of `dat_raw_anon`
#'
#' @examples
#' d <- create_dummy_qi_data(people = 20, seed = 1)
#' j <- join_raw_anon_data(d, d)
#' s <- score_multi(j, c(AGE = "num", ZIP = "char", VISIT_COUNT = "num"))
#' match_greedy(s)
#'
#' @export
score_multi <- function(dat_raw_anon, targets, row_number = "ROW_NUMBER",
                        weights = NULL,
                        normalize = c("range", "zscore", "rank", "none"),
                        method = c("weighted", "mahalanobis"),
                        split = ":", cov_from = c("raw", "anon", "pooled"),
                        ridge = 1e-6, .fn_name = "score_multi") {
  normalize <- match.arg(normalize)
  method <- match.arg(method)
  cov_from <- match.arg(cov_from)

  targets <- expand_target_spec(targets, .fn_name)

  if (is.null(weights)) {
    weights <- rep(1, length(targets))
  }
  if (!is.numeric(weights) || length(weights) != length(targets)) {
    stop(.fn_name, "(): `weights` must be a numeric vector with one entry per ",
         "target column (", length(targets), " expected, got ",
         length(weights), ").", call. = FALSE)
  }

  if (identical(method, "weighted")) {
    parts <- build_target_scores(dat_raw_anon, targets, row_number, split, .fn_name)
    part_weights <- weights
  } else {
    is_num <- unname(targets) == "num"
    if (!any(is_num)) {
      stop(.fn_name, "(): method = \"mahalanobis\" needs at least one column ",
           "of type \"num\"; a covariance matrix is only defined over numeric ",
           "coordinates. Use method = \"weighted\" instead.", call. = FALSE)
    }

    mahal <- score_mahalanobis(
      dat_raw_anon,
      targets = names(targets)[is_num],
      row_number = row_number,
      cov_from = cov_from,
      ridge = ridge,
      .fn_name = .fn_name
    )
    rest <- build_target_scores(
      dat_raw_anon, targets[!is_num], row_number, split, .fn_name
    )

    parts <- c(list(mahalanobis = mahal), rest)
    ## The block stands in for every numeric column, so it inherits their
    ## combined weight: "weighted" and "mahalanobis" then spend the same total
    ## weight on the same set of columns, and the difference between their
    ## success rates is attributable to the metric rather than to a change in
    ## how much the numeric columns count for.
    part_weights <- c(sum(weights[is_num]), weights[!is_num])
  }

  combine_scores(normalize_scores(parts, method = normalize), weights = part_weights)
}
