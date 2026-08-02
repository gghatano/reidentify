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
#' WHEN THAT ARGUMENT FAILS (Issue #59). Up-weighting the low-variance
#' directions is only right if the release did not *also* perturb the data
#' along them. It is the ratio of the release's perturbation to the population
#' spread **in the whitened directions** that decides the outcome, and strong
#' correlation makes some of those directions narrow -- so the metric is at its
#' most fragile in exactly the situation it is recommended for.
#'
#' Measured on a 3-column fixture (A, B correlated at `rho`; C independent;
#' `success_analytic` over 200 records, the full sweep is in
#' `docs/default-changes.md`):
#'
#' \tabular{lrrrr}{
#'   perturbation \tab rho \tab kappa(S) \tab weighted \tab mahalanobis \cr
#'   isotropic \tab 0.90 \tab 26.7 \tab 0.9600 \tab 0.9200 \cr
#'   isotropic \tab 0.99 \tab 273.4 \tab 0.9350 \tab 0.5600 \cr
#'   isotropic \tab 0.999 \tab 2755.6 \tab 0.8850 \tab 0.2100 \cr
#'   follows S \tab 0.99 \tab 273.4 \tab 0.8900 \tab 0.9800 \cr
#'   follows S \tab 0.999 \tab 2755.6 \tab 0.8400 \tab 0.9800
#' }
#'
#' The same covariance, the same condition number, opposite conclusions: with
#' an isotropic perturbation the whitened attack is 4.2x weaker than a plain
#' weighted sum, and with a perturbation that follows the population covariance
#' it is 1.2x stronger. **A high condition number therefore says the answer is
#' fragile, not that it is wrong.** `score_mahalanobis()` warns above 100 and
#' tells you to compare against `method = "weighted"`; do that before quoting
#' either number.
#'
#' Raising `ridge` is not a fix, only a retreat: at `ridge = 1` both regimes
#' land on the weighted sum's own figure (0.8950 / 0.8650), which is to say the
#' metric has stopped doing anything. That is why the default is left at 1e-6.
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
#'   `"pooled"` (the distinct records of both sides stacked). The default is a
#'   threat-model choice, not a tuning knob: a real attacker whitens with the
#'   population they hold. `"anon"` folds the release's own perturbation into
#'   the covariance and so is less fragile when \eqn{S} is ill-conditioned
#'   (measured 0.3350 against 0.2100 at kappa 2756), but it is still far below
#'   the weighted sum there, so it is a diagnostic rather than a rescue.
#' @param ridge non-negative shrinkage applied to the diagonal of the
#'   covariance matrix, as a multiple of its mean diagonal entry (default
#'   1e-6). Needed because redundant columns -- exactly the case Mahalanobis
#'   exists for -- make \eqn{S} ill-conditioned or singular. It cannot repair
#'   an ill-conditioned result: a ridge large enough to matter turns the
#'   metric into the weighted sum it was supposed to improve on.
#' @param squared return the squared distance instead of the distance
#'   (default FALSE). The two give identical rankings; the square root is the
#'   default because it shares the units of the underlying columns.
#'
#' @param generalized what to do when one of `targets` turns out to hold
#'   generalised values on the ANON side: `"stop"` (default), `"warn"` or
#'   `"ignore"`. A generalised column is also non-numeric, so this only
#'   decides which of the two errors is raised. See [score_containment()].
#'
#' @return a "reid_scores" table whose SCORE is the Mahalanobis distance
#'   (a distance: smaller is a better match)
#'
#' @examples
#' set.seed(1)
#' n <- 30
#' a <- rnorm(n)
#' ## correlated, but not so nearly-collinear that the whitening becomes
#' ## fragile -- see the condition-number discussion above
#' raw <- data.frame(ROW_NUMBER = 1:n, A = a, B = 2 * a + rnorm(n, sd = 1))
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
                              generalized = c("stop", "warn", "ignore"),
                              .fn_name = "score_mahalanobis") {
  cov_from <- match.arg(cov_from)
  generalized <- match.arg(generalized)

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
    reid_score_columns(dat_raw_anon, t, row_number, .fn_name, generalized)
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

  warn_ill_conditioned_cov(cov_mat, targets[keep], cov_from, ridge, .fn_name)

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

## Condition number of the (ridged) covariance above which score_mahalanobis()
## calls the result fragile. Calibrated in docs/default-changes.md: with an
## isotropic release perturbation the loss against a weighted sum is under 10%
## up to kappa 54 and then grows (136: 1.33x, 273: 1.67x, 2756: 4.21x).
MAHALANOBIS_CONDITION_LIMIT <- 100

#' warn when whitening rests on a nearly-singular covariance
#'
#' @param cov_mat the covariance matrix, ridge already applied
#' @param targets names of the columns it was estimated from
#' @param cov_from which side it came from
#' @param ridge the ridge that was applied
#' @param fn_name calling function, for the message
#'
#' @return NULL, invisibly; called for the warning
#'
#' @keywords internal
warn_ill_conditioned_cov <- function(cov_mat, targets, cov_from, ridge,
                                     fn_name = "score_mahalanobis") {
  if (nrow(cov_mat) < 2) {
    return(invisible(NULL))
  }
  cond <- tryCatch(kappa(cov_mat, exact = TRUE), error = function(e) Inf)
  if (is.finite(cond) && cond <= MAHALANOBIS_CONDITION_LIMIT) {
    return(invisible(NULL))
  }

  warning(
    fn_name, "(): the ", cov_from, " covariance of (",
    paste(targets, collapse = ", "), ") is ill-conditioned (condition number ",
    format(cond, digits = 4), ", ridge = ", ridge,
    "). It is invertible, so nothing fails, but S^-1 magnifies the direction ",
    "the population barely varies in, and the result is then decided by how ",
    "the release perturbed the data rather than by how identifying the ",
    "columns are. Measured on a 3-column fixture, above this limit the ",
    "whitened attack ran from 1.7x to 4.2x WEAKER than the plain weighted sum ",
    "when the release's perturbation was isotropic, and 1.1x to 1.2x STRONGER ",
    "when it followed the population covariance -- opposite conclusions from ",
    "the same condition numbers. Compare ",
    "against method = \"weighted\" before quoting this number; ",
    "cov_from = \"anon\" and a larger `ridge` both pull it back towards the ",
    "weighted sum. See docs/default-changes.md.",
    call. = FALSE
  )
  invisible(NULL)
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

  unknown <- setdiff(unique(unname(targets)), reid_score_types())
  if (length(unknown) > 0) {
    stop(fn_name, "(): unknown score type(s): ",
         paste(unknown, collapse = ", "),
         ". Expected one of ",
         paste0("\"", reid_score_types(), "\"", collapse = ", "), ".",
         call. = FALSE)
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
build_target_scores <- function(dat_raw_anon, targets, row_number, split, fn_name,
                                source = "anon", weight = "idf",
                                hierarchy = NULL, rules = NULL) {
  out <- lapply(seq_along(targets), function(i) {
    target <- names(targets)[i]
    type <- unname(targets[i])
    fn <- score_fn_for_type(type)
    if (identical(type, "idf")) {
      ## Single-column form of the IDF score. score_multi() normally hands the
      ## "idf" columns to score_idf_match() as one block; this branch exists so
      ## that a column can also be scored on its own, which is what the axis
      ## screening below needs.
      fn(dat_raw_anon, target, row_number = row_number,
         source = source, weight = weight)
    } else if (identical(type, "containment")) {
      ## Likewise the single-column form of containment: the block below
      ## intersects the declared columns, but axis screening has to see what
      ## each one narrows to on its own.
      fn(dat_raw_anon, target, row_number = row_number,
         hierarchy = hierarchy, rules = rules)
    } else if (type %in% reid_split_score_types()) {
      fn(dat_raw_anon, target, row_number = row_number, split = split)
    } else {
      fn(dat_raw_anon, target, row_number = row_number)
    }
  })
  names(out) <- names(targets)
  out
}

## ---------------------------------------------------------------------------
## Uninformative-axis screening (Issue #35)
##
## Adding an attribute to the combination is supposed to make the attack
## stronger. It does not always. Under an anonymisation that scales every
## person's activity by roughly the same factor -- publishing 60% of each
## person's events, say -- the record-count axis carries literally no
## information: every count shrinks together, so the nearest count is somebody
## else who was simply less active to begin with. Measured on the Issue #22
## fixture, that axis scores exactly the random-assignment baseline.
##
## Summed in at equal weight, a dead axis is not neutral. Its scores are noise,
## and the noise reorders candidates that the live axes had ranked correctly:
## on that fixture the combination came out *below* the static-attribute-only
## attack in 4 of 8 seeds. The user reads a smaller number after adding
## evidence, and a smaller number is exactly what nobody questions -- so
## dangerous data is signed off as safe. This is docs/lessons-learned.md
## section 2 in its purest form, and the only defence is to make the tool
## notice.
##
## THE TEST IS ON RANKS, NOT ON HITS. The obvious statistic is the axis's own
## single-guess success rate against the random-assignment baseline, and it was
## tried first. It does not work: the baseline is 1/n, so the null expects
## about *one* hit in the entire table, and on a sixty-record fixture an axis
## has to score three or four hits before the difference clears alpha = 0.05.
## Measured against the package's own fixtures, that test called five axes with
## real signal uninformative -- including two that a correlated-attribute
## benchmark depends on -- because two hits where one was expected is simply
## not evidence.
##
## What is measured instead is where the true RAW record *ranks* among the
## candidates. Under the null the true record is exchangeable with the others,
## so its mid-rank is uniform over the observed rank multiset, with mean
## (n+1)/2 and a variance computable exactly from that multiset. Summing the
## per-record deviations gives one statistic per axis, and because it is a sum
## of n independent bounded terms its normal approximation is sound -- no
## sampling, no seed, nothing to reproduce.
##
## The separation this buys, measured across the Issue #22 activity fixture,
## the correlated-attribute benchmark, create_dummy_qi_data() and a pure-noise
## control:
##
##   dead axes    z = 0.36 .. 0.83   (count under 60% subsampling; an
##                                    independent random column)
##   live axes    z = 5.59 .. 17.5   (everything else, including a two-valued
##                                    SEX column and axes the hit-count test
##                                    could not distinguish from chance)
##
## An order of magnitude of daylight, with no overlap. Worth recording that
## PR #34 reported the dead count axis putting the true record at a median rank
## of 56 out of 120, against 60 for chance, which looks like a trace of
## surviving signal; over the whole table the mean is 0.49 of the way down and
## z = 0.36, so it is not one.
##
## THIS USES THE GROUND TRUTH. Deciding which axes work requires knowing which
## RAW record each ANON record came from, so screening models an attacker who
## has been told which of their attributes are worth using. That is a stronger
## adversary than one who has not, which is the correct direction for a risk
## assessment to err in, but it is an assumption and it is stated here rather
## than buried.
##
## WHAT IT DOES NOT CATCH. The test asks whether an axis carries information at
## all, not whether it earns its weight. An axis that is informative but weak,
## or informative but redundant with a stronger one, can still drag a
## sum-of-normalised-scores down; that is a question about weighting, which
## Issue #14 left open on purpose.
## ---------------------------------------------------------------------------

#' does an axis carry any information about identity?
#'
#' Measures each score table on its own and tests whether it ranks the true RAW
#' record above chance. An axis that does not is worse than useless in a
#' combination: summed in at equal weight its scores act as noise, reorder
#' candidates the informative axes had right, and pull the reported
#' reidentification rate *down*. A safety assessment that moves down for that
#' reason is an under-estimate, so the condition is worth detecting explicitly.
#'
#' @section What the null is:
#'
#' Under "this axis carries no information about identity" the true RAW record
#' is exchangeable with the other candidates of its ANON record, so its
#' mid-rank is uniform over that record's rank multiset: mean `(n + 1) / 2`,
#' and a variance that follows exactly from the multiset (and so accounts for
#' ties without an approximation). The statistic is the total, over ANON
#' records, of how far above the middle the true record was ranked, and its
#' normal approximation is sound because it is a sum of many independent
#' bounded terms. Nothing is sampled and there is no seed.
#'
#' @section Why ranks and not the success rate:
#'
#' Comparing an axis's own single-guess success rate against the
#' random-assignment baseline is the obvious test and is far too blunt: the
#' baseline is `1 / n`, so the null expects about one hit in the whole table
#' and no realistic number of records gives it any power. Measured on this
#' package's fixtures it called five axes with real signal uninformative. The
#' rank test separates the same fixtures with no overlap at all -- dead axes at
#' `z` between 0.4 and 0.8, live ones from 5.6 up. `success`, `baseline` and
#' `lift` are still reported, because they are what a risk report is written
#' in; they are not what `informative` is decided on.
#'
#' @section This needs the answer key:
#'
#' A record's rank is only defined relative to the RAW record it actually came
#' from, which the score layer reads off matching row numbers. Screening
#' therefore describes an attacker who already knows which of their attributes
#' work. Where no ANON record has a matching RAW record the question cannot be
#' asked at all, and every column of the result is `NA`.
#'
#' @param scores a score table, or a (preferably named) list of them, each over
#'   the same candidate pairs
#' @param alpha significance level for `informative` (default 0.05)
#'
#' @return a data frame of class "reid_axis_report", one row per axis, with
#'   \describe{
#'     \item{axis}{name of the score table}
#'     \item{n_anon}{number of ANON records measured}
#'     \item{success}{exact expected single-guess success rate of this axis
#'       alone}
#'     \item{baseline}{success rate of random assignment on the same candidate
#'       sets}
#'     \item{lift}{`success / baseline`}
#'     \item{mean_rank_pct}{average position of the true record in the
#'       candidate ranking, as a fraction: 0.5 is chance, smaller is better for
#'       the attacker}
#'     \item{z}{rank statistic in units of its null standard deviation}
#'     \item{p_value}{one-sided p-value against the null}
#'     \item{informative}{`p_value < alpha`}
#'   }
#'
#' @examples
#' d <- create_dummy_qi_data(people = 40, seed = 1)
#' j <- join_raw_anon_data(d, d)
#' axis_informativeness(list(AGE = score_num(j, "AGE"), SEX = score_char(j, "SEX")))
#'
#' @seealso [score_multi()], whose `screen` argument runs this over the
#'   declared columns, and [axis_report()], which reads the result back off a
#'   combined score.
#'
#' @export
axis_informativeness <- function(scores, alpha = 0.05) {
  if (is.data.frame(scores)) {
    scores <- list(scores)
  }
  if (!is.list(scores) || length(scores) == 0) {
    stop("`scores` must be a score table or a non-empty list of score tables.",
         call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) ||
      alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number strictly between 0 and 1.",
         call. = FALSE)
  }

  nms <- names(scores)
  if (is.null(nms)) {
    nms <- rep("", length(scores))
  }
  nms[nms == ""] <- paste0("axis", seq_along(scores))[nms == ""]

  rows <- lapply(seq_along(scores), function(i) {
    one_axis_informativeness(scores[[i]], nms[i], alpha,
                             paste0("scores[[", i, "]]"))
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "alpha") <- alpha
  class(out) <- c("reid_axis_report", class(out))
  out
}

#' test one axis for information about identity
#'
#' @param s a score table
#' @param axis name to report the axis under
#' @param alpha significance level
#' @param arg argument name used in error messages
#'
#' @return a one-row data frame
#'
#' @keywords internal
#'
#' @importFrom stats pnorm
one_axis_informativeness <- function(s, axis, alpha, arg = "scores") {
  score_type <- validate_reid_scores(s, arg)

  per_anon <- reid_per_anon(s)
  n_anon <- nrow(per_anon)

  ## success/baseline/lift are the reporting columns, computed exactly the way
  ## reid_evaluate() computes them so the screen and the headline number can
  ## never quote different figures for the same axis.
  risk <- top_k_probability(per_anon$N_BETTER, per_anon$TRUE_TIE_SIZE, 1)
  has_true <- !is.na(per_anon$TRUE_RANK)
  n_i <- as.numeric(per_anon$N_CANDIDATES)

  if (!any(has_true)) {
    ## No ANON record has a RAW counterpart, so "did this axis find anybody"
    ## has no answer. Reporting 0 here would read as "this axis is dead", which
    ## is a claim about the data the measurement cannot support.
    return(data.frame(
      axis = axis, n_anon = n_anon,
      success = NA_real_, baseline = NA_real_, lift = NA_real_,
      mean_rank_pct = NA_real_, z = NA_real_, p_value = NA_real_,
      informative = NA,
      stringsAsFactors = FALSE
    ))
  }

  observed <- sum(risk)
  null_mean <- sum(ifelse(has_true, 1 / n_i, 0))

  rk <- axis_rank_statistic(s, score_type)

  z <- if (rk$variance > 0) rk$statistic / sqrt(rk$variance) else 0
  ## A zero null variance means every candidate of every record ties, so the
  ## true record sits at the middle by construction and no outcome could have
  ## been surprising.
  p_value <- if (rk$variance > 0) stats::pnorm(z, lower.tail = FALSE) else 1

  data.frame(
    axis = axis,
    n_anon = n_anon,
    success = observed / n_anon,
    baseline = null_mean / n_anon,
    lift = if (null_mean > 0) observed / null_mean else NA_real_,
    mean_rank_pct = rk$mean_rank_pct,
    z = z,
    p_value = p_value,
    informative = p_value < alpha,
    stringsAsFactors = FALSE
  )
}

#' how far above chance an axis ranks the true RAW record
#'
#' For each ANON record, the candidates are mid-ranked by score and the true
#' record's rank is compared with the centre `(n + 1) / 2` that a rank drawn
#' uniformly from the same multiset would average. Mid-ranks are used so that a
#' tie contributes its honest average position rather than the optimistic end
#' of the tie group, and the null variance is taken from the observed multiset
#' rather than the `(n^2 - 1) / 12` of distinct values, so a heavily tied axis
#' is credited with the small spread it actually has.
#'
#' @param s a score table
#' @param score_type "distance" or "similarity"
#'
#' @return a list with `statistic` (total deviation above the centre),
#'   `variance` (its null variance), `n_used` (records with a true
#'   counterpart) and `mean_rank_pct`
#'
#' @keywords internal
axis_rank_statistic <- function(s, score_type = "distance") {
  value <- if (identical(score_type, "similarity")) -s$SCORE else s$SCORE
  anon <- s$ANON_ROW_NUMBER
  raw <- s$RAW_ROW_NUMBER
  levels_anon <- sort(unique(anon))
  groups <- split(seq_along(anon), factor(anon, levels = levels_anon))

  statistic <- 0
  variance <- 0
  n_used <- 0L
  pct_total <- 0

  for (i in seq_along(groups)) {
    idx <- groups[[i]]
    is_true <- raw[idx] == levels_anon[i]
    if (!any(is_true)) {
      next
    }
    v <- value[idx]
    n <- length(idx)
    r <- rank(v, ties.method = "average")
    centre <- (n + 1) / 2

    statistic <- statistic + (centre - r[is_true][1])
    variance <- variance + mean((r - centre)^2)
    pct_total <- pct_total + r[is_true][1] / n
    n_used <- n_used + 1L
  }

  list(statistic = statistic, variance = variance, n_used = n_used,
       mean_rank_pct = if (n_used > 0) pct_total / n_used else NA_real_)
}

#' print an axis informativeness report
#'
#' @param x a "reid_axis_report" data frame
#' @param ... ignored
#'
#' @return `x`, invisibly
#'
#' @export
print.reid_axis_report <- function(x, ...) {
  alpha <- attr(x, "alpha")
  cat(sprintf("axis informativeness (%d axis/axes, alpha = %s)\n",
              nrow(x), format(if (is.null(alpha)) NA else alpha)))
  for (i in seq_len(nrow(x))) {
    if (is.na(x$p_value[i])) {
      cat(sprintf("  %-20s not measurable: no ANON record has a RAW counterpart\n",
                  x$axis[i]))
      next
    }
    cat(sprintf(
      "  %-20s success %.4f  baseline %.4f  lift %6.2fx  rank %.3f  z %6.2f  p = %.4f  %s\n",
      x$axis[i], x$success[i], x$baseline[i], x$lift[i], x$mean_rank_pct[i],
      x$z[i], x$p_value[i],
      if (isTRUE(x$informative[i])) "informative" else "no signal"
    ))
  }
  if ("kept" %in% names(x) && any(!x$kept)) {
    cat(sprintf("  excluded from the combination: %s\n",
                paste(x$axis[!x$kept], collapse = ", ")))
  }
  invisible(x)
}

#' read the per-axis report off a combined score
#'
#' [score_multi()] records which axes it screened and what it found, so the
#' contribution of each attribute can be checked after the fact rather than
#' having to be re-derived from the inputs.
#'
#' @param scores a score table produced by [score_multi()] or
#'   [score_by_knowledge()]
#'
#' @return the "reid_axis_report" data frame attached to `scores`, or NULL when
#'   the score was produced with `screen = "none"` or did not come from
#'   [score_multi()]
#'
#' @examples
#' d <- create_dummy_qi_data(people = 40, seed = 1)
#' j <- join_raw_anon_data(d, d)
#' axis_report(score_multi(j, c(AGE = "num", SEX = "char")))
#'
#' @export
axis_report <- function(scores) {
  attr(scores, "axes")
}

#' warn about, or remove, axes that do not beat the baseline
#'
#' @param report a "reid_axis_report"
#' @param screen "warn", "drop" or "none"
#' @param fn_name name used in the warning
#'
#' @return a logical vector, one per axis, saying which to keep
#'
#' @keywords internal
apply_axis_screen <- function(report, screen, fn_name) {
  keep <- rep(TRUE, nrow(report))
  dead <- !is.na(report$informative) & !report$informative
  if (!any(dead)) {
    return(keep)
  }

  detail <- paste(
    sprintf(paste0("\"%s\" (ranks the true record %.3f of the way down the ",
                   "candidate list, against 0.5 for chance; z = %.2f, ",
                   "p = %.3f; own success %.4f vs baseline %.4f)"),
            report$axis[dead], report$mean_rank_pct[dead], report$z[dead],
            report$p_value[dead], report$success[dead], report$baseline[dead]),
    collapse = "; "
  )
  consequence <- paste0(
    "An axis with no signal contributes noise, not evidence: summed in at ",
    "equal weight it can reorder candidates the other axes had right and push ",
    "the combined success rate *below* what those axes achieve alone, so the ",
    "reported reidentification risk would be an under-estimate."
  )

  if (identical(screen, "drop") && all(dead)) {
    ## Dropping everything would leave nothing to score with. The honest report
    ## in that case is the chance-level number the axes actually produce, not
    ## an error and not a silently different attack.
    warning(fn_name, "(): no axis shows any signal: ", detail,
            ". Keeping all of them, because dropping every axis would leave ",
            "no attack at all -- the combined result is expected to sit at ",
            "chance level.", call. = FALSE)
    return(keep)
  }

  if (identical(screen, "drop")) {
    keep <- !dead
    warning(fn_name, "(): excluding axis/axes that show no signal: ", detail,
            ". ", consequence,
            " Pass screen = \"warn\" to keep them and only be told.",
            call. = FALSE)
    return(keep)
  }

  warning(fn_name, "(): axis/axes that show no signal, i.e. that do not rank ",
          "the true record better than chance: ", detail, ". ", consequence,
          " Pass screen = \"drop\" to exclude them, give them weight 0, or ",
          "screen = \"none\" to silence this check.", call. = FALSE)
  keep
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
#' adds any remaining columns as separate normalised terms. The Mahalanobis
#' block is given the combined weight of the numeric columns it absorbed, so
#' the two methods spend the same total weight on the same columns and their
#' success rates can be compared directly. Use it when some of the numeric
#' columns are correlated -- see [score_mahalanobis()] for why a plain sum
#' double-counts them.
#'
#' @section Columns scored as a block:
#'
#' Three kinds of column are *not* scored one at a time. Columns declared
#' `"idf"` are handed together to [score_idf_match()], because the relative
#' size of the rarity weights across columns is the method itself and
#' normalising each column separately would discard it. Columns declared
#' `"containment"` go together to [score_containment()], because the published
#' regions are **intersected** -- each attribute the attacker holds cuts the
#' candidate set again, and the cuts multiply rather than add. Under
#' `method = "mahalanobis"` the `"num"` columns are likewise handled together.
#' In every case the block receives the summed weight of the columns it
#' absorbed, and is normalised as a single component against the rest.
#'
#' @section Generalised columns:
#'
#' Declare a column the release publishes as regions (`"[30,40)"`,
#' `"135****"`, 東京都) as `"containment"`. Every other type compares a raw
#' value against a printed region and so measures the region's shape, not the
#' risk; those types refuse such a column rather than returning a number
#' (Issue #100). See [is_generalized_value()] for what is detected, and pass a
#' `hierarchy` for categorical generalisations, which no structural test can
#' recognise.
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
#' @param source,weight passed to [score_idf_match()] for `"idf"` columns
#' @param hierarchy,rules passed to [score_containment()] for `"containment"`
#'   columns; a hierarchy is what makes a *categorical* generalisation
#'   (千代田区 published as 東京都) scorable at all
#' @param screen what to do about a column that, measured on its own, does not
#'   rank the true record better than chance (see [axis_informativeness()]):
#'   \describe{
#'     \item{`"warn"`}{(default) report it and change nothing. The returned
#'       score is identical to `screen = "none"`.}
#'     \item{`"drop"`}{leave it out of the combination -- equivalent to giving
#'       it weight 0, except that under `method = "mahalanobis"` it also leaves
#'       the covariance block. If *every* column fails, all of them are kept
#'       and the warning says so: dropping them all would leave no attack.}
#'     \item{`"none"`}{skip the check.}
#'   }
#'   Screening reads the RAW/ANON row-number correspondence, i.e. the ground
#'   truth, so it models an attacker who has been told which of their
#'   attributes are worth using.
#' @param alpha significance level for the screen, passed to
#'   [axis_informativeness()]; ignored when `screen = "none"`
#'
#' @return a "reid_scores" table over the candidate pairs of `dat_raw_anon`.
#'   Unless `screen = "none"` it carries the per-axis report in its `"axes"`
#'   attribute, readable with [axis_report()].
#'
#' @examples
#' d <- create_dummy_qi_data(people = 20, seed = 1)
#' j <- join_raw_anon_data(d, d)
#' s <- score_multi(j, c(AGE = "num", ZIP = "char", VISIT_COUNT = "num"))
#' match_greedy(s)
#' axis_report(s)
#'
#' @export
score_multi <- function(dat_raw_anon, targets, row_number = "ROW_NUMBER",
                        weights = NULL,
                        normalize = c("range", "zscore", "rank", "none"),
                        method = c("weighted", "mahalanobis"),
                        split = ":", cov_from = c("raw", "anon", "pooled"),
                        ridge = 1e-6,
                        source = c("anon", "raw", "pooled"),
                        weight = c("idf", "inv_log", "inv", "none"),
                        hierarchy = NULL, rules = NULL,
                        screen = c("warn", "drop", "none"),
                        alpha = 0.05,
                        .fn_name = "score_multi") {
  normalize <- match.arg(normalize)
  method <- match.arg(method)
  cov_from <- match.arg(cov_from)
  source <- match.arg(source)
  weight <- match.arg(weight)
  screen <- match.arg(screen)

  targets <- expand_target_spec(targets, .fn_name)

  if (is.null(weights)) {
    weights <- rep(1, length(targets))
  }
  if (!is.numeric(weights) || length(weights) != length(targets)) {
    stop(.fn_name, "(): `weights` must be a numeric vector with one entry per ",
         "target column (", length(targets), " expected, got ",
         length(weights), ").", call. = FALSE)
  }

  ## ---- screen the declared columns before any of them are blocked up ------
  ## Screening happens per *declared column*, not per component of the sum, so
  ## that a dead numeric column is caught even when method = "mahalanobis"
  ## would otherwise hide it inside a block that works on the strength of its
  ## other coordinates.
  singles <- NULL
  report <- NULL
  if (!identical(screen, "none")) {
    singles <- build_target_scores(
      dat_raw_anon, targets, row_number, split, .fn_name,
      source = source, weight = weight,
      hierarchy = hierarchy, rules = rules
    )
    report <- axis_informativeness(singles, alpha = alpha)
    keep <- apply_axis_screen(report, screen, .fn_name)
    report$kept <- keep
    if (!all(keep)) {
      targets <- targets[keep]
      weights <- weights[keep]
      singles <- singles[keep]
    }
  }

  types <- unname(targets)
  blocks <- list()
  block_weights <- numeric(0)
  ## Columns a block has taken over, so build_target_scores() does not also
  ## score them one at a time and count their evidence twice.
  absorbed <- rep(FALSE, length(targets))

  if (identical(method, "mahalanobis")) {
    is_num <- types == "num"
    if (!any(is_num)) {
      stop(.fn_name, "(): method = \"mahalanobis\" needs at least one column ",
           "of type \"num\"; a covariance matrix is only defined over numeric ",
           "coordinates. Use method = \"weighted\" instead.", call. = FALSE)
    }
    blocks$mahalanobis <- score_mahalanobis(
      dat_raw_anon,
      targets = names(targets)[is_num],
      row_number = row_number,
      cov_from = cov_from,
      ridge = ridge,
      .fn_name = .fn_name
    )
    ## The block stands in for every numeric column, so it inherits their
    ## combined weight: "weighted" and "mahalanobis" then spend the same total
    ## weight on the same set of columns, and the difference between their
    ## success rates is attributable to the metric rather than to a change in
    ## how much the numeric columns count for.
    block_weights <- c(block_weights, sum(weights[is_num]))
    absorbed <- absorbed | is_num
  }

  is_idf <- types == "idf"
  if (any(is_idf)) {
    blocks$idf <- score_idf_match(
      dat_raw_anon,
      targets = names(targets)[is_idf],
      row_number = row_number,
      source = source,
      weight = weight,
      .fn_name = .fn_name
    )
    block_weights <- c(block_weights, sum(weights[is_idf]))
    absorbed <- absorbed | is_idf
  }

  ## The published regions of several columns are intersected, not added: a
  ## record has to survive every one of them to stay a candidate. Scoring the
  ## columns separately and summing the normalised results would give each
  ## exclusion a *partial* vote, so a record ruled out by one attribute could
  ## still outrank one that survives them all -- the under-report direction
  ## again (docs/lessons-learned.md section 2).
  is_cont <- types == "containment"
  if (any(is_cont)) {
    blocks$containment <- score_containment(
      dat_raw_anon,
      targets = names(targets)[is_cont],
      row_number = row_number,
      hierarchy = hierarchy,
      rules = rules,
      .fn_name = .fn_name
    )
    block_weights <- c(block_weights, sum(weights[is_cont]))
    absorbed <- absorbed | is_cont
  }

  rest <- if (is.null(singles)) {
    build_target_scores(
      dat_raw_anon, targets[!absorbed], row_number, split, .fn_name,
      source = source, weight = weight,
      hierarchy = hierarchy, rules = rules
    )
  } else {
    ## Screening already built exactly these tables, with exactly these
    ## arguments. Rebuilding them would double the cost of every screened call
    ## and, worse, leave two code paths that could drift apart.
    singles[!absorbed]
  }

  parts <- c(blocks, rest)
  part_weights <- c(block_weights, weights[!absorbed])

  out <- combine_scores(normalize_scores(parts, method = normalize),
                        weights = part_weights)
  attr(out, "axes") <- report
  warn_combination_weaker_than_best_axis(out, report, alpha, .fn_name)
  out
}

#' warn when combining attributes measures *less* risk than one of them alone
#'
#' "The attacker knows more, so the reported risk went down" is not a finding,
#' it is a broken measurement: every axis the combination uses is available on
#' its own, so an attacker would simply use the better one. #35 catches the
#' version of this caused by an uninformative axis; #59 found a second cause,
#' a whitening that is dominated by the release's perturbation, which single
#' axis screening cannot see because it scores each column in isolation.
#'
#' @param combined the combined score table
#' @param report the per-axis report from [axis_informativeness()], or NULL
#'   when screening was switched off
#' @param alpha significance level, only used to reuse the same statistic
#' @param fn_name calling function, for the message
#'
#' @return NULL, invisibly; called for the warning
#'
#' @keywords internal
warn_combination_weaker_than_best_axis <- function(combined, report, alpha,
                                                   fn_name) {
  if (is.null(report) || !nrow(report) || all(is.na(report$success))) {
    return(invisible(NULL))
  }
  ## Only when the screen found nothing. An axis that failed screening has
  ## already been reported, with the same remedy attached; saying it twice in
  ## different words trains the reader to skip both. This check exists for the
  ## case the screen is structurally blind to -- every column informative on
  ## its own, and the combination still worse than one of them.
  if (!all(report$informative %in% TRUE)) {
    return(invisible(NULL))
  }
  best <- which.max(report$success)
  best_success <- report$success[best]

  got <- one_axis_informativeness(combined, "combined", alpha)$success
  if (is.na(got) || is.na(best_success) || got >= best_success) {
    return(invisible(NULL))
  }

  warning(
    fn_name, "(): the combination measures LESS risk (", format(got, digits = 4),
    ") than `", report$axis[best], "` measures on its own (",
    format(best_success, digits = 4),
    "). An attacker holding the columns you declared also holds that one, so ",
    "the combined figure understates the risk and must not be reported as the ",
    "result. Look at attr(x, \"axes\") for the per-axis numbers, then use ",
    "`weights` to stop the weaker axes outvoting the stronger one, or switch ",
    "`method` -- a whitened metric can lose to a plain weighted sum when the ",
    "covariance is ill-conditioned (Issue #59).",
    call. = FALSE
  )
  invisible(NULL)
}
