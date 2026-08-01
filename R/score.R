## ---------------------------------------------------------------------------
## Score layer (Issue #11)
##
## Historically each reid_by_*() function computed a distance *and* picked the
## argmin in one indivisible step, so "attribute x distance definition x
## assignment rule" could not be combined. This file holds the first of the
## three layers: functions that turn a RAW/ANON candidate table into a long
## score table
##
##     (RAW_ROW_NUMBER, ANON_ROW_NUMBER, SCORE)
##
## and nothing else. Integration lives in combine_scores() (R/combine.R) and
## assignment in match_greedy() (R/match.R).
##
## SCORE ORIENTATION. Every score in this package is a *dissimilarity*: the
## smaller the value, the more likely the two records describe the same
## person. That choice is recorded explicitly on the object (attribute
## `score_type`, see new_reid_scores()) rather than left as a naming
## convention, because a silently sign-flipped score would make an unsafe
## data set look safe -- the exact failure direction this package exists to
## catch (docs/lessons-learned.md section 2). match_greedy() reads the
## attribute instead of assuming.
## ---------------------------------------------------------------------------

#' stop with a clear, actionable error message if any of `cols` is missing
#' from `dat_raw_anon`, instead of letting the caller fail downstream with
#' a confusing low-level error (e.g. base R's "replacement has 0 rows,
#' data has NNN" from indexing a non-existent column with `[[`, which gives
#' no hint about which column name was actually wrong).
#'
#' @param dat_raw_anon dataframe of raw_anon form
#' @param cols character vector of (already RAW_/ANON_-prefixed) column
#'   names that the caller is about to look up in `dat_raw_anon`
#' @param fn_name name of the calling score_*() function, used in the
#'   error message
#'
#' @return invisible NULL if all `cols` are present; otherwise stops with
#'   an error naming the missing column(s).
#'
#' @keywords internal
check_raw_anon_columns_exist <- function(dat_raw_anon, cols, fn_name) {
  missing_cols <- setdiff(cols, names(dat_raw_anon))
  if (length(missing_cols) > 0) {
    stop(
      fn_name, "(): column(s) not found in dat_raw_anon: ",
      paste(missing_cols, collapse = ", "),
      ". Check the `target`/`row_number` arguments (these are looked up ",
      "*after* RAW_/ANON_ prefixing) against the columns actually present: ",
      paste(names(dat_raw_anon), collapse = ", "),
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' resolve the RAW_/ANON_-prefixed column names a score_*() call
#' needs, and check up front that all four exist
#'
#' Every score function needs exactly the same four columns, derived the same
#' way from `target` and `row_number`. Deriving them in one place keeps the
#' four functions (and their error messages) from drifting apart.
#'
#' @param dat_raw_anon dataframe of raw_anon form
#' @param target target column name, *before* RAW_/ANON_ prefixing
#' @param row_number row-number column name, *before* RAW_/ANON_ prefixing
#' @param fn_name name of the user-facing function, used in the error message
#'   so the message names the function the caller actually called
#'
#' @return a list with elements `raw_target`, `anon_target`,
#'   `raw_row_number`, `anon_row_number`
#'
#' @keywords internal
reid_prefixed_columns <- function(dat_raw_anon, target, row_number, fn_name) {
  cols <- list(
    raw_target = paste("RAW_", target, sep = ""),
    anon_target = paste("ANON_", target, sep = ""),
    raw_row_number = paste("RAW_", row_number, sep = ""),
    anon_row_number = paste("ANON_", row_number, sep = "")
  )

  check_raw_anon_columns_exist(
    dat_raw_anon,
    c(cols$raw_target, cols$anon_target, cols$raw_row_number, cols$anon_row_number),
    fn_name
  )

  cols
}

#' construct a score table
#'
#' @param raw_row_number vector of RAW record identifiers
#' @param anon_row_number vector of ANON record identifiers
#' @param score numeric vector of scores, one per (RAW, ANON) candidate pair
#' @param score_type either "distance" (smaller = more likely the same person,
#'   the orientation every score in this package uses) or "similarity"
#'   (larger = more likely). Stored on the object so the assignment layer
#'   never has to guess which way round the score runs.
#'
#' @return an object of class "reid_scores": a data frame with columns
#'   RAW_ROW_NUMBER, ANON_ROW_NUMBER and SCORE, carrying a `score_type`
#'   attribute.
#'
#' @keywords internal
new_reid_scores <- function(raw_row_number, anon_row_number, score,
                            score_type = c("distance", "similarity")) {
  score_type <- match.arg(score_type)

  out <- data.frame(
    RAW_ROW_NUMBER = raw_row_number,
    ANON_ROW_NUMBER = anon_row_number,
    SCORE = unname(score),
    stringsAsFactors = FALSE
  )
  attr(out, "score_type") <- score_type
  class(out) <- c("reid_scores", class(out))
  out
}

#' collision-free identifier for a (ANON, RAW) candidate pair
#'
#' The score layer needs to say "these two rows name the same candidate pair"
#' in three places: the duplicate-pair guard of [validate_unique_candidate_pairs()]
#' and the two key builds inside [combine_scores()]. All three used to paste the
#' two row numbers with `"\\r"` between them (Issue #73). Row numbers come out
#' of the *user's* data, so that assumed something about a column this package
#' does not own, and `("a", "b\\rc")` and `("a\\rb", "c")` pasted to the same
#' string.
#'
#' The two directions this took were measured before the change:
#'
#' * in the guard, a collision **rejected a perfectly valid score table** and
#'   named a pair that does not in fact repeat -- loud and wrong, but not the
#'   dangerous direction;
#' * in `combine_scores()`, it was the dangerous one. The guard there exists to
#'   refuse two score tables built over *different* candidate sets, because
#'   combining those "would silently drop candidates and under-report the
#'   reidentification rate". A collision let exactly that through: two tables
#'   agreeing on no pair at all were combined without complaint, pairing each
#'   score with the wrong candidate.
#'
#' Coding is done over everything passed at once, so callers spanning several
#' score tables must concatenate first and split the result afterwards -- codes
#' assigned per table would not be comparable between tables, which is the same
#' reason [blocking_pass_keys()] codes both sides together.
#'
#' `as.character()` comes first because the score layer has always compared row
#' numbers as written: the two sides are supplied independently and routinely
#' disagree on storage type (a factor against a character, an integer against a
#' double).
#'
#' @param anon,raw the two row-number vectors, of equal length
#'
#' @return a character vector with one key per element
#'
#' @keywords internal
candidate_pair_key <- function(anon, raw) {
  reid_value_key(list(
    reid_class_codes(as.character(anon)),
    reid_class_codes(as.character(raw))
  ))
}

#' check that `x` looks like a score table, and return its orientation
#'
#' Accepts any data frame carrying the three score-layer columns, so callers
#' can build score tables by hand or pipe one through dplyr (which drops the
#' class and attributes). A table with no recorded orientation is treated as
#' "distance", the package-wide default documented in `new_reid_scores()`.
#'
#' @param x object to validate
#' @param arg argument name used in the error message
#'
#' @return the score type of `x`, one of "distance" or "similarity"
#'
#' @keywords internal
validate_reid_scores <- function(x, arg = "scores") {
  if (!is.data.frame(x)) {
    stop("`", arg, "` must be a data frame of score-layer output ",
         "(RAW_ROW_NUMBER, ANON_ROW_NUMBER, SCORE).", call. = FALSE)
  }

  required <- c("RAW_ROW_NUMBER", "ANON_ROW_NUMBER", "SCORE")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop("`", arg, "` is missing score-layer column(s): ",
         paste(missing_cols, collapse = ", "),
         ". A score table must have RAW_ROW_NUMBER, ANON_ROW_NUMBER and SCORE.",
         call. = FALSE)
  }

  score_type <- attr(x, "score_type")
  if (is.null(score_type)) {
    score_type <- "distance"
  }
  if (!identical(score_type, "distance") && !identical(score_type, "similarity")) {
    stop("`", arg, "` has an unknown score_type attribute: ",
         paste(format(score_type), collapse = " "),
         ". Expected \"distance\" or \"similarity\".", call. = FALSE)
  }

  score_type
}

#' print a score table
#'
#' @param x a "reid_scores" object
#' @param ... passed on to the data frame print method
#'
#' @return `x`, invisibly
#'
#' @export
print.reid_scores <- function(x, ...) {
  cat(sprintf(
    "reid scores (%s): %d candidate pair(s), %d ANON x %d RAW record(s)\n",
    attr(x, "score_type") %||% "distance",
    nrow(x),
    length(unique(x$ANON_ROW_NUMBER)),
    length(unique(x$RAW_ROW_NUMBER))
  ))
  print(utils::head(as.data.frame(x), 6L), ...)
  if (nrow(x) > 6L) {
    cat(sprintf("# ... %d more pair(s)\n", nrow(x) - 6L))
  }
  invisible(x)
}

`%||%` <- function(x, y) if (is.null(x)) y else x


#' score a numeric column by absolute difference
#'
#' The first of the three layers: it reports how far apart every (RAW, ANON)
#' candidate pair is and decides nothing. Feed the result to [match_greedy()]
#' or [match_optimal()].
#'
#' @param dat_raw_anon dataframe of raw_anon form
#' @param target target column
#' @param row_number name of the row-number column *before* the RAW_/ANON_
#'   prefixing done by [join_raw_anon_data()] (default: "ROW_NUMBER")
#' @param .fn_name name used in error messages; a function that wraps this one
#'   passes its own name so the message points at the function the user
#'   actually called
#'
#' @return a "reid_scores" table: one row per (RAW, ANON) candidate pair with
#'   columns RAW_ROW_NUMBER, ANON_ROW_NUMBER and SCORE, where SCORE is
#'   `abs(RAW - ANON)` (a distance: smaller is a better match).
#'
#' @examples
#' raw  <- data.frame(ROW_NUMBER = 1:5, V = c(10, 20, 30, 40, 50))
#' anon <- data.frame(ROW_NUMBER = 1:5, V = c(11, 19, 33, 38, 52))
#' d <- join_raw_anon_data(raw, anon)
#' score_num(d, "V")
#'
#' # the score layer only ranks candidates; the assignment layer picks one
#' match_greedy(score_num(d, "V"))
#'
#' @export
score_num <- function(dat_raw_anon, target, row_number = "ROW_NUMBER",
                      .fn_name = "score_num") {
  cols <- reid_prefixed_columns(dat_raw_anon, target, row_number, .fn_name)

  ## Subtracting a character column raises "non-numeric argument to binary
  ## operator", which names neither the function nor the column, and above all
  ## does not say what to do about a *generalised* column -- the case that
  ## actually brings callers here (Issue #40).
  is_text <- function(v) is.character(v) || is.factor(v)
  if (is_text(dat_raw_anon[[cols$raw_target]]) ||
        is_text(dat_raw_anon[[cols$anon_target]])) {
    stop(non_numeric_target_message(
      dat_raw_anon, cols, target, .fn_name,
      alternative = paste0("Use score_char() or score_idf() for a categorical ",
                           "column, or convert \"", target, "\" to numeric.")
    ), call. = FALSE)
  }

  new_reid_scores(
    raw_row_number = dat_raw_anon[[cols$raw_row_number]],
    anon_row_number = dat_raw_anon[[cols$anon_row_number]],
    score = abs(dat_raw_anon[[cols$raw_target]] - dat_raw_anon[[cols$anon_target]])
  )
}

#' score a character column by Levenshtein edit distance
#'
#' **Generalised columns are refused.** Edit distance between a raw value and
#' a published *region* -- `adist("37", "[30,40)")` is 6 -- measures the length
#' of the bracket string and nothing else, but it is a plausible-looking number
#' and no error is raised. On
#' `docs/investigation/generalization-benchmark.R` that misuse reports a
#' success rate of 0.1017 where [score_containment()] reports 0.4450, so the
#' release looks about four times safer than it is (Issue #40, and
#' `docs/lessons-learned.md` section 2). This stops instead; see
#' [is_generalized_value()] for exactly what is detected, and note that a
#' *categorical* generalisation (千代田区 published as 東京都) cannot be
#' detected structurally at all.
#'
#' @inheritParams score_num
#' @param generalized what to do when `target` turns out to hold generalised
#'   values on the ANON side: `"stop"` (default), `"warn"` (compute the edit
#'   distances anyway, having said so) or `"ignore"` (skip the check). Use
#'   `"ignore"` only when the column is meant to be compared literally, e.g.
#'   when RAW and ANON carry the *same* already-binned values.
#'
#' @return a "reid_scores" table whose SCORE is the edit distance between the
#'   RAW and ANON values of `target` (a distance: smaller is a better match).
#'
#' @seealso [score_containment()] for generalised columns.
#'
#' @examples
#' raw  <- data.frame(ROW_NUMBER = 1:3, NAME = c("aaa", "bbb", "ccc"))
#' anon <- data.frame(ROW_NUMBER = 1:3, NAME = c("aax", "bbb", "ccd"))
#' d <- join_raw_anon_data(raw, anon)
#' score_char(d, "NAME")
#'
#' # a generalised ANON column is refused rather than scored (Issue #40)
#' g_raw  <- data.frame(ROW_NUMBER = 1:3, AGE = c("31", "37", "46"))
#' g_anon <- data.frame(ROW_NUMBER = 1:3, AGE = c("30s", "30s", "40s"))
#' try(score_char(join_raw_anon_data(g_raw, g_anon), "AGE"))
#'
#' @importFrom utils adist
#' @export
score_char <- function(dat_raw_anon, target, row_number = "ROW_NUMBER",
                       generalized = c("stop", "warn", "ignore"),
                       .fn_name = "score_char") {
  cols <- reid_prefixed_columns(dat_raw_anon, target, row_number, .fn_name)
  check_generalized_target(dat_raw_anon, cols, target, generalized, .fn_name)

  raw_target_col <- as.character(dat_raw_anon[[cols$raw_target]])
  anon_target_col <- as.character(dat_raw_anon[[cols$anon_target]])

  vec_distance <- mapply(
    FUN = function(x, y) {
      return(adist(x, y)[[1]])
    },
    anon_target_col, raw_target_col
  )

  new_reid_scores(
    raw_row_number = dat_raw_anon[[cols$raw_row_number]],
    anon_row_number = dat_raw_anon[[cols$anon_row_number]],
    score = vec_distance
  )
}

#' score a distribution column ("A:B:C") by quantile-vector distance
#'
#' @inheritParams score_num
#' @param split character separating the elements of the distribution column
#'   (default ":"). Treated as a **literal string**, never as a regular
#'   expression, so metacharacters such as `"|"`, `"."` or `"$"` are safe to
#'   use as separators. Must be a single non-empty string.
#' @param generalized what to do when `target` turns out to hold generalised
#'   values on the ANON side: `"stop"` (default), `"warn"` or `"ignore"`. A
#'   generalised value has no distribution to compare, so this normally only
#'   replaces the coercion error raised further down with one that names the
#'   score to use instead (Issue #40).
#'
#' @return a "reid_scores" table whose SCORE is [distribution_distance()]
#'   between the RAW and ANON distributions (a distance: smaller is a better
#'   match).
#'
#' @examples
#' # each record carries a whole distribution, written as "A:B:C"
#' raw  <- data.frame(ROW_NUMBER = 1:3,
#'                    SPEND = c("1:2:3", "10:11:12", "20:21:22"))
#' anon <- data.frame(ROW_NUMBER = 1:3,
#'                    SPEND = c("1:2:4", "10:12:13", "19:21:23"))
#' d <- join_raw_anon_data(raw, anon)
#' score_dist(d, "SPEND")
#'
#' @export
score_dist <- function(dat_raw_anon, target, row_number = "ROW_NUMBER",
                       split = ":",
                       generalized = c("stop", "warn", "ignore"),
                       .fn_name = "score_dist") {
  cols <- reid_prefixed_columns(dat_raw_anon, target, row_number, .fn_name)
  check_generalized_target(dat_raw_anon, cols, target, generalized, .fn_name)

  raw_target_col <- as.character(dat_raw_anon[[cols$raw_target]])
  anon_target_col <- as.character(dat_raw_anon[[cols$anon_target]])

  distance <- mapply(
    FUN = function(x, y) distribution_distance(x, y, split = split),
    raw_target_col, anon_target_col
  )

  new_reid_scores(
    raw_row_number = dat_raw_anon[[cols$raw_row_number]],
    anon_row_number = dat_raw_anon[[cols$anon_row_number]],
    score = distance
  )
}

#' rank a numeric column within RAW and within ANON, and score by rank gap
#'
#' Rank ties are resolved with
#' `ties.method = "min"`: genuinely tied values are indistinguishable in the
#' data, so they collapse to the same rank instead of being split into a fake
#' total order by incidental row position.
#'
#' **The target column must be numeric.** `rank()` accepts a character column
#' and orders it lexicographically, so a generalised or categorical column used
#' to come back as a full set of plausible rank gaps with no error at all --
#' the same silent under-report as [score_char()] (Issue #40).
#'
#' @inheritParams score_num
#' @param generalized what to do when `target` turns out to hold generalised
#'   values on the ANON side: `"stop"` (default), `"warn"` or `"ignore"`. A
#'   generalised column is also non-numeric, so this normally only decides
#'   which of the two errors is raised.
#'
#' @return a "reid_scores" table whose SCORE is the absolute difference
#'   between the ANON-side and RAW-side ranks of `target` (a distance:
#'   smaller is a better match).
#'
#' @examples
#' # the published values are on a different scale, but the order survives,
#' # and the order is enough: every rank gap on the diagonal is 0
#' raw  <- data.frame(ROW_NUMBER = 1:5, V = c(10, 20, 30, 40, 50))
#' anon <- data.frame(ROW_NUMBER = 1:5, V = c(1.2, 2.5, 3.1, 4.4, 5.0))
#' d <- join_raw_anon_data(raw, anon)
#' score_num_rank(d, "V")
#' match_greedy(score_num_rank(d, "V"))
#'
#' @export
score_num_rank <- function(dat_raw_anon, target, row_number = "ROW_NUMBER",
                           generalized = c("stop", "warn", "ignore"),
                           .fn_name = "score_num_rank") {
  ranks <- compute_num_ranks(dat_raw_anon, target, row_number, .fn_name,
                             generalized = generalized)

  new_reid_scores(
    raw_row_number = ranks$raw_row_number,
    anon_row_number = ranks$anon_row_number,
    score = abs(ranks$anon_rank - ranks$raw_rank)
  )
}

#' compute the per-row RAW/ANON ranks used by score_num_rank()
#'
#' Split out from [score_num_rank()] so that a caller which wants the two rank
#' columns themselves does not have to recompute them.
#'
#' Ranks are computed over the *distinct* (row number, target) pairs on each
#' side -- i.e. once per record, not once per candidate pair -- and then
#' broadcast back onto every candidate row.
#'
#' @inheritParams score_num
#' @param fn_name name used in error messages
#' @param generalized `"stop"`, `"warn"` or `"ignore"`; see [score_num_rank()]
#'
#' @return a list with the per-candidate-row vectors `raw_row_number`,
#'   `anon_row_number`, `raw_rank`, `anon_rank`, plus the resolved column
#'   names in `cols`.
#'
#' @keywords internal
compute_num_ranks <- function(dat_raw_anon, target, row_number, fn_name,
                              generalized = c("stop", "warn", "ignore")) {
  cols <- reid_prefixed_columns(dat_raw_anon, target, row_number, fn_name)
  check_generalized_target(dat_raw_anon, cols, target, generalized, fn_name)

  ## rank() happily orders a character column lexicographically, so a
  ## generalised or categorical target used to produce a complete set of
  ## plausible rank gaps and no error -- "30代" simply sorted before "40代".
  ## The gaps are then edit-distance-like noise, not evidence, and the reported
  ## success rate lands far below the real one (Issue #40). The structural
  ## generalisation check above cannot see a categorical generalisation, so the
  ## type check below is what actually closes that hole.
  is_text <- function(v) is.character(v) || is.factor(v)
  if (is_text(dat_raw_anon[[cols$raw_target]]) ||
        is_text(dat_raw_anon[[cols$anon_target]])) {
    stop(non_numeric_target_message(
      dat_raw_anon, cols, target, fn_name,
      alternative = paste0("rank() would order it lexicographically, which is ",
                           "not a distance between records. Use score_char() ",
                           "or score_idf() for a categorical column, or ",
                           "convert \"", target, "\" to numeric.")
    ), call. = FALSE)
  }

  ## rank(..., na.last = TRUE) (the default) does NOT propagate NA: it
  ## silently assigns missing values a real, high rank instead of erroring
  ## or returning NA, which would let an ANON/RAW record with a genuinely
  ## missing target value be reported as a confident (even SCORE == 0)
  ## reidentification match. Stop instead of letting that happen silently.
  if (anyNA(dat_raw_anon[[cols$anon_target]]) || anyNA(dat_raw_anon[[cols$raw_target]])) {
    stop(
      fn_name, "(): target column \"", target, "\" contains NA/missing ",
      "values in RAW and/or ANON. rank(..., na.last = TRUE) would silently ",
      "assign missing values a real rank instead of erroring, which could ",
      "report a false reidentification match. Remove or explicitly handle ",
      "missing values before calling ", fn_name, "().",
      call. = FALSE
    )
  }

  side_rank <- function(row_col, target_col) {
    key <- dat_raw_anon[[row_col]]
    val <- dat_raw_anon[[target_col]]
    keep <- !duplicated(key)
    r <- rank(val[keep], ties.method = "min")
    r[match(key, key[keep])]
  }

  list(
    cols = cols,
    raw_row_number = dat_raw_anon[[cols$raw_row_number]],
    anon_row_number = dat_raw_anon[[cols$anon_row_number]],
    raw_rank = side_rank(cols$raw_row_number, cols$raw_target),
    anon_rank = side_rank(cols$anon_row_number, cols$anon_target)
  )
}
