## ---------------------------------------------------------------------------
## Attacker knowledge model W / M / S (Issue #13)
##
## join_raw_anon_data() cross-joins RAW against ANON, which silently assumes
## the attacker holds the original data in full. As an upper bound that is
## correct, but it is the *only* thing the package could measure, so a data
## set that is safe against every realistic adversary and one that is safe
## against nobody produced the same kind of number.
##
## This file makes the assumption explicit and adjustable: an
## attacker_knowledge object says which columns the adversary is allowed to
## look at, and score_by_knowledge() builds an attack from exactly those.
##
##   W (weak)   part of the quasi-identifiers only     public statistics, rosters
##   M (medium) all quasi-identifiers + coarse behaviour  another service's member data
##   S (strong) the RAW record itself                  data holder, insider, post-leak
##
## Restricting *columns* is the mechanism; which attack runs on those columns
## is unchanged (Issue #13 explicitly excludes new methods).
## ---------------------------------------------------------------------------

#' declare what a modelled attacker knows
#'
#' @param level one of "W" (weak), "M" (medium) or "S" (strong)
#' @param quasi_identifiers named character vector mapping column name to the
#'   kind of score to use for it: `"num"`, `"char"`, `"dist"` or `"rank"`.
#'   E.g. `c(AGE = "num", ZIP = "char")`.
#' @param behavior named character vector, same form, of coarse behavioural
#'   features (visit counts, spend summaries, ...). Visible from level M.
#' @param identifiers named character vector, same form, of columns that
#'   effectively fingerprint the original record. Visible only at level S.
#' @param weak_subset character vector naming the quasi-identifier columns a
#'   level-W attacker sees. Defaults to the first `floor(n / 2)` (at least
#'   one) of `quasi_identifiers`, in the order given -- pass this explicitly
#'   whenever the split matters to the conclusion.
#'
#' @return an object of class "attacker_knowledge"; `$visible` is the named
#'   character vector of column-to-score-type entries the attacker may use.
#'
#' @examples
#' attacker_knowledge(
#'   "M",
#'   quasi_identifiers = c(AGE = "num", ZIP = "char", SEX = "char"),
#'   behavior = c(VISIT_COUNT = "num"),
#'   identifiers = c(FINGERPRINT = "num")
#' )
#'
#' @export
attacker_knowledge <- function(level = c("W", "M", "S"),
                               quasi_identifiers,
                               behavior = NULL,
                               identifiers = NULL,
                               weak_subset = NULL) {
  level <- match.arg(level)

  check_spec <- function(x, arg) {
    if (is.null(x) || (is.character(x) && length(x) == 0)) {
      ## "no columns of this kind" is a legitimate specification; the
      ## emptiness of quasi_identifiers is reported by its own check below,
      ## with a message that says what is actually wrong.
      return(character(0))
    }
    if (!is.character(x) || is.null(names(x)) || any(names(x) == "")) {
      stop("`", arg, "` must be a *named* character vector mapping column ",
           "name to score type, e.g. c(AGE = \"num\", ZIP = \"char\").",
           call. = FALSE)
    }
    unknown <- setdiff(unique(x), c("num", "char", "dist", "rank"))
    if (length(unknown) > 0) {
      stop("`", arg, "` has unknown score type(s): ",
           paste(unknown, collapse = ", "),
           ". Expected \"num\", \"char\", \"dist\" or \"rank\".", call. = FALSE)
    }
    x
  }

  quasi_identifiers <- check_spec(quasi_identifiers, "quasi_identifiers")
  if (length(quasi_identifiers) == 0) {
    stop("`quasi_identifiers` must name at least one column: every knowledge ",
         "level is defined relative to them.", call. = FALSE)
  }
  behavior <- check_spec(behavior, "behavior")
  identifiers <- check_spec(identifiers, "identifiers")

  if (is.null(weak_subset)) {
    n_weak <- max(1L, floor(length(quasi_identifiers) / 2))
    weak_subset <- names(quasi_identifiers)[seq_len(n_weak)]
  }
  unknown_weak <- setdiff(weak_subset, names(quasi_identifiers))
  if (length(unknown_weak) > 0) {
    stop("`weak_subset` names column(s) that are not quasi-identifiers: ",
         paste(unknown_weak, collapse = ", "), call. = FALSE)
  }

  visible <- switch(
    level,
    W = quasi_identifiers[weak_subset],
    M = c(quasi_identifiers, behavior),
    S = c(quasi_identifiers, behavior, identifiers)
  )

  structure(
    list(
      level = level,
      visible = visible,
      quasi_identifiers = quasi_identifiers,
      behavior = behavior,
      identifiers = identifiers,
      weak_subset = weak_subset
    ),
    class = "attacker_knowledge"
  )
}

#' print an attacker knowledge model
#'
#' @param x an "attacker_knowledge" object
#' @param ... ignored
#'
#' @return `x`, invisibly
#'
#' @export
print.attacker_knowledge <- function(x, ...) {
  label <- c(W = "weak", M = "medium", S = "strong")[[x$level]]
  cat(sprintf("attacker knowledge: level %s (%s)\n", x$level, label))
  cat(sprintf(
    "  visible columns (%d): %s\n",
    length(x$visible),
    paste(sprintf("%s[%s]", names(x$visible), x$visible), collapse = ", ")
  ))
  cat(sprintf(
    "  withheld (%d): %s\n",
    length(setdiff(names(c(x$quasi_identifiers, x$behavior, x$identifiers)), names(x$visible))),
    paste(setdiff(names(c(x$quasi_identifiers, x$behavior, x$identifiers)), names(x$visible)), collapse = ", ")
  ))
  invisible(x)
}

#' pick the score function for a declared score type
#'
#' @param type one of "num", "char", "dist", "rank"
#'
#' @return the corresponding `score_*()` function
#'
#' @keywords internal
score_fn_for_type <- function(type) {
  switch(
    type,
    num = score_num,
    char = score_char,
    dist = score_dist,
    rank = score_num_rank,
    stop("unknown score type \"", type, "\".", call. = FALSE)
  )
}

#' score a RAW/ANON table using only the columns an attacker can see
#'
#' Builds one score per visible column with the score function that column's
#' type declares, and combines them with [combine_scores()].
#'
#' NORMALISATION. The per-column scores are rescaled to \[0, 1\] by default.
#' Without it, an unweighted sum is dominated by whichever column happens to
#' have the widest numeric range, so *adding* a column the attacker knows can
#' make the attack worse -- which would make the whole W/M/S comparison
#' meaningless. A column with no variation at all contributes exactly 0,
#' because it cannot discriminate between candidates.
#'
#' The combination itself is done by [score_multi()] (Issue #14); this function
#' only decides *which* columns are handed to it. `normalize` and `method`
#' therefore accept everything `score_multi()` does, including the correlation
#' aware `method = "mahalanobis"`. The default is unchanged (`"range"` /
#' `"weighted"`), which is the stopgap Issue #13 needed.
#'
#' @param dat_raw_anon dataframe of raw_anon form
#' @param knowledge an [attacker_knowledge()] object
#' @param row_number name of the row-number column *before* RAW_/ANON_
#'   prefixing (default "ROW_NUMBER")
#' @param weights numeric vector of per-column weights, one per visible
#'   column, in the order of `knowledge$visible` (default: all 1)
#' @param normalize normalisation applied to each column before combining;
#'   see [normalize_scores()]. Default `"range"`.
#' @param method `"weighted"` (default) or `"mahalanobis"`; see [score_multi()]
#' @param split separator passed to [score_dist()] for `"dist"` columns
#' @param cov_from,ridge passed to [score_mahalanobis()] when
#'   `method = "mahalanobis"`
#'
#' @return a "reid_scores" table over the same candidate pairs as
#'   `dat_raw_anon`
#'
#' @examples
#' d <- create_dummy_qi_data(people = 20, seed = 1)
#' j <- join_raw_anon_data(d, d)
#' k <- dummy_qi_knowledge("M")
#' match_greedy(score_by_knowledge(j, k))
#'
#' @export
score_by_knowledge <- function(dat_raw_anon, knowledge, row_number = "ROW_NUMBER",
                               weights = NULL,
                               normalize = c("range", "zscore", "rank", "none"),
                               method = c("weighted", "mahalanobis"),
                               split = ":", cov_from = c("raw", "anon", "pooled"),
                               ridge = 1e-6) {
  if (!inherits(knowledge, "attacker_knowledge")) {
    stop("`knowledge` must be an attacker_knowledge object; see ",
         "attacker_knowledge().", call. = FALSE)
  }

  score_multi(
    dat_raw_anon,
    targets = knowledge$visible,
    row_number = row_number,
    weights = weights,
    normalize = match.arg(normalize),
    method = match.arg(method),
    split = split,
    cov_from = match.arg(cov_from),
    ridge = ridge,
    .fn_name = "score_by_knowledge"
  )
}

#' compare reidentification risk across the W / M / S knowledge levels
#'
#' Runs the same attack machinery three times on the same data, each time
#' restricted to what that knowledge level allows the attacker to see, and
#' reports the resulting risk side by side. This is the form the knowledge
#' model is meant to be read in: a single number for "the" reidentification
#' rate is only interpretable once the assumed adversary is stated.
#'
#' @param dat_raw_anon dataframe of raw_anon form
#' @param quasi_identifiers,behavior,identifiers,weak_subset passed to
#'   [attacker_knowledge()]
#' @param levels which knowledge levels to evaluate (default all three, in
#'   increasing order of knowledge)
#' @param row_number name of the row-number column before RAW_/ANON_ prefixing
#' @param seeds tie-break seeds passed to [reid_evaluate()]
#' @param ... further arguments passed to [score_by_knowledge()]
#'
#' @return a data frame with one row per level and columns `level`,
#'   `n_visible`, `success_analytic`, `success_mean`, `success_sd`,
#'   `baseline_random`, `lift` and `max_risk`
#'
#' @examples
#' d <- create_dummy_qi_data(people = 25, seed = 1)
#' j <- join_raw_anon_data(d, d)
#' reid_knowledge_curve(
#'   j,
#'   quasi_identifiers = c(AGE = "num", ZIP = "char", SEX = "char"),
#'   behavior = c(VISIT_COUNT = "num", SPEND_MEAN = "num"),
#'   identifiers = c(FINGERPRINT = "num"),
#'   weak_subset = "ZIP",
#'   seeds = 1:5
#' )
#'
#' @export
reid_knowledge_curve <- function(dat_raw_anon,
                                 quasi_identifiers,
                                 behavior = NULL,
                                 identifiers = NULL,
                                 weak_subset = NULL,
                                 levels = c("W", "M", "S"),
                                 row_number = "ROW_NUMBER",
                                 seeds = 1:20,
                                 ...) {
  rows <- lapply(levels, function(lv) {
    k <- attacker_knowledge(
      lv,
      quasi_identifiers = quasi_identifiers,
      behavior = behavior,
      identifiers = identifiers,
      weak_subset = weak_subset
    )
    s <- score_by_knowledge(dat_raw_anon, k, row_number = row_number, ...)
    e <- reid_evaluate(s, seeds = seeds)

    data.frame(
      level = lv,
      n_visible = length(k$visible),
      success_analytic = e$success_analytic,
      success_mean = e$success_mean,
      success_sd = e$success_sd,
      baseline_random = e$baseline$rate[e$baseline$method == "random"],
      lift = e$lift,
      max_risk = e$max_risk,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
