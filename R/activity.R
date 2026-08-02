## ---------------------------------------------------------------------------
## Activity profile scores (Issue #22)
##
## transform_transaction_to_master() has computed a ROWCOUNT column since long
## before any of this, and no attack has ever looked at it. How often somebody
## shows up is one of the most stable things about them, and it is published in
## the clear.
##
## This file is also where the count difference that Issue #5 removed from
## distribution_distance() belongs. That fix was right -- a distribution
## distance should measure the *shape* of a distribution, and letting the
## number of observations leak into it meant two people with the same habits
## and different activity levels were scored as having different habits. But
## the count itself is real evidence, and deleting it from one place without
## putting it anywhere else would have thrown it away. The split here is
## deliberate and is what makes the two axes safe to add together in
## score_multi():
##
##   score_count()   -- how much activity there is, and nothing else
##   score_profile() -- what the activity looks like, scale-free by
##                      construction (doubling every event leaves it unchanged)
##   score_span()    -- how long the activity stretches over
##
## Each measures something the others cannot see, so combining them counts
## each piece of evidence exactly once.
##
## SEPARATORS ARE LITERAL HERE. The collapsed columns are produced by
## paste(collapse = ...), which is a literal string, so they are split with
## fixed = TRUE. The older parse_dist_values() path splits on a regular
## expression, so a separator like "|" or "." behaves differently there; that
## inconsistency is Issue #32's, and these functions take the literal reading
## because it is the one that matches how the columns are written.
## ---------------------------------------------------------------------------

#' split a collapsed column into per-record vectors
#'
#' @param values character vector of collapsed values, one per record
#' @param split literal separator
#' @param target column name, for error messages
#' @param fn_name calling function name, for error messages
#' @param numeric whether the elements must be convertible to numeric
#'
#' @return a list of vectors, one per element of `values`
#'
#' @keywords internal
split_collapsed <- function(values, split, target, fn_name, numeric = FALSE) {
  if (anyNA(values)) {
    stop(fn_name, "(): column \"", target, "\" contains NA. An absent activity ",
         "profile is not the same as an empty one, and guessing which was ",
         "meant would change the reported risk.", call. = FALSE)
  }
  parts <- strsplit(as.character(values), split = split, fixed = TRUE)

  if (!numeric) {
    return(parts)
  }

  out <- lapply(parts, function(p) suppressWarnings(as.numeric(p)))
  bad <- vapply(seq_along(out), function(i) anyNA(out[[i]]), logical(1))
  if (any(bad)) {
    stop(fn_name, "(): could not convert column \"", target, "\" to numeric ",
         "after splitting on \"", split, "\" (e.g. \"", values[which(bad)[1]],
         "\"). This score needs a numeric collapsed column such as ",
         "\"3:8:12\".", call. = FALSE)
  }
  out
}

#' turn a per-pair count comparison into a score
#'
#' @param raw,anon numeric vectors of the same length
#' @param method comparison method
#'
#' @return numeric vector of non-negative distances
#'
#' @keywords internal
compare_magnitudes <- function(raw, anon,
                               method = c("log_ratio", "absolute", "relative")) {
  method <- match.arg(method)
  switch(
    method,
    ## log1p, not log: a count of 0 is a legitimate value, and log(0) would
    ## make every zero-activity record infinitely far from everything.
    log_ratio = abs(log1p(raw) - log1p(anon)),
    absolute = abs(raw - anon),
    relative = abs(raw - anon) / pmax(raw, anon, 1)
  )
}

#' score how much activity two records show
#'
#' Compares a per-record count -- by default the `ROWCOUNT` column
#' [transform_transaction_to_master()] produces -- between the RAW and ANON
#' sides.
#'
#' The default `method = "log_ratio"` compares `log1p` of the two counts rather
#' than the counts themselves, because activity counts are heavy tailed: the
#' gap between 2 visits and 7 is strong evidence that these are different
#' people, while the same gap between 200 and 205 is nothing. An absolute
#' difference scores those two situations identically and so spends most of its
#' discriminating power on the few busiest records.
#'
#' @inheritParams score_num
#' @param target name of the count column (default `"ROWCOUNT"`)
#' @param method `"log_ratio"` (default), `"absolute"` or `"relative"`
#'   (`|a - b| / max(a, b, 1)`, bounded in \[0, 1\])
#' @param generalized what to do when `target` turns out to hold generalised
#'   values on the ANON side: `"stop"` (default), `"warn"` or `"ignore"`. A
#'   count published as a band (`"[10,20)"`) is not a number, so this only
#'   decides which of the two errors is raised.
#'
#' @return a "reid_scores" table (a distance: smaller is a better match)
#'
#' @examples
#' tran <- create_dummy_transaction_data(people = 20, size = 4)
#' master <- transform_transaction_to_master(
#'   tran, DYNAMIC_NUM = "NUM_DYNAMIC", STATIC_NUM = "NUM_STATIC"
#' )
#' j <- join_raw_anon_data(master, master)
#' match_greedy(score_count(j, row_number = "ROW_NUMBER"))
#'
#' @export
score_count <- function(dat_raw_anon, target = "ROWCOUNT",
                        row_number = "ROW_NUMBER",
                        method = c("log_ratio", "absolute", "relative"),
                        generalized = c("stop", "warn", "ignore"),
                        .fn_name = "score_count") {
  method <- match.arg(method)
  cols <- reid_score_columns(dat_raw_anon, target, row_number, .fn_name,
                             generalized)

  as_count <- function(nm) {
    v <- dat_raw_anon[[nm]]
    if (!is.numeric(v)) {
      stop(.fn_name, "(): column \"", nm, "\" is ", class(v)[1],
           ", not numeric. score_count() expects a count column such as the ",
           "ROWCOUNT that transform_transaction_to_master() produces.",
           call. = FALSE)
    }
    if (anyNA(v)) {
      stop(.fn_name, "(): column \"", nm, "\" contains NA; a missing count ",
           "cannot be compared.", call. = FALSE)
    }
    if (any(v < 0)) {
      stop(.fn_name, "(): column \"", nm, "\" contains negative values, so it ",
           "is not a count.", call. = FALSE)
    }
    as.numeric(v)
  }

  new_reid_scores(
    raw_row_number = dat_raw_anon[[cols$raw_row_number]],
    anon_row_number = dat_raw_anon[[cols$anon_row_number]],
    score = compare_magnitudes(as_count(cols$raw_target),
                               as_count(cols$anon_target), method)
  )
}

#' build the per-record histogram of a collapsed column
#'
#' @param values character vector of collapsed values, one per record
#' @param bins character vector of bin labels
#' @param split literal separator
#' @param shape_only divide each row by its total
#' @param target,fn_name used in error messages
#'
#' @return a numeric matrix with one row per record and one column per bin
#'
#' @keywords internal
collapsed_histogram <- function(values, bins, split, shape_only, target, fn_name) {
  parts <- split_collapsed(values, split, target, fn_name)

  ## A record with no value in any bin has no shape. Under shape_only the row
  ## would be divided by a total of zero, and the branch that catches that has
  ## to hand *something* back: an all-zero row. Two such rows then differ by
  ## nothing, so score_profile() reports them as a perfect match -- the best
  ## possible score for two records it knows nothing about, and one that beats
  ## every genuine match in the table. Refuse instead, the same way
  ## split_collapsed() refuses NA: an absent profile is not an empty one, and
  ## guessing which was meant changes the reported risk.
  if (shape_only) {
    empty <- vapply(parts, function(p) !any(p %in% bins), logical(1))
    if (any(empty)) {
      i <- which(empty)[1]
      stop(fn_name, "(): value \"", values[i], "\" of column \"", target,
           "\" has nothing in any of the ", length(bins), " bin(s), so its ",
           "profile has no shape to compare and would score as a perfect ",
           "match against every other empty record. Widen `bins`, drop the ",
           "record, or pass shape_only = FALSE to compare raw counts.",
           call. = FALSE)
    }
  }

  out <- vapply(parts, function(p) {
    counts <- tabulate(match(p, bins), nbins = length(bins))
    ## the guard above is what makes sum(counts) safe to divide by here
    if (shape_only) counts / sum(counts) else as.numeric(counts)
  }, numeric(length(bins)))

  ## vapply gives bins-by-records; the rest of the file wants records-by-bins
  matrix(out, nrow = length(values), byrow = TRUE,
         dimnames = list(NULL, bins))
}

#' score the shape of an activity profile
#'
#' Reads a collapsed column such as the `<col>_DIST` that
#' [transform_transaction_to_master()] produces -- a day-of-week column, an
#' hour-of-day column, a category column -- turns each record's values into a
#' histogram over a fixed set of bins, and scores a candidate pair by the
#' distance between the two histograms.
#'
#' SCALE-FREE BY CONSTRUCTION. With the default `shape_only = TRUE` each
#' histogram is divided by its own total, so a record whose every event is
#' duplicated scores exactly as before. This is the point of splitting the
#' activity profile into three functions: how *much* activity there is belongs
#' to [score_count()], and if it leaked into this score as well, adding the two
#' together in [score_multi()] would count the same evidence twice. It is also
#' the property Issue #5 established that a distribution distance must have.
#'
#' Unlike [score_dist()], nothing here is treated as a number: the bins are
#' compared as labels, so an hour-of-day or day-of-week profile is scored
#' correctly rather than being run through numeric quantiles that assume
#' hour 23 and hour 0 are as far apart as possible.
#'
#' @inheritParams score_num
#' @param split literal separator between the elements of the collapsed column
#'   (default ":")
#' @param bins character vector of bin labels, in order. `NULL` (the default)
#'   uses every value that occurs anywhere in either side, so a caller who
#'   knows the full support -- all seven weekdays, all 24 hours -- should pass
#'   it explicitly, otherwise a bin nobody happens to use simply does not
#'   exist.
#' @param shape_only divide each histogram by its total, making the score
#'   independent of activity volume (default TRUE). A record with no value in
#'   any bin then has no shape at all and is an error rather than an all-zero
#'   profile, which would match every other empty record perfectly; pass
#'   `FALSE` to compare raw counts, where an empty record is a meaningful zero.
#' @param metric `"l1"` (default; total variation when `shape_only = TRUE`) or
#'   `"l2"` (squared Euclidean)
#' @param generalized what to do when `target` turns out to hold generalised
#'   values on the ANON side: `"stop"` (default), `"warn"` or `"ignore"`.
#'
#' @section Generalised columns are refused:
#'
#' The bins are labels, and a published region is a label like any other, so a
#' generalised column bins cleanly into a histogram with one occupied bin per
#' record and produces a complete, plausible score table -- silently. Measured
#' on a fully generalised age column it reported a fifth of the success rate
#' [score_containment()] reports on the same data (Issue #100). Both this and
#' [score_idf()] were missed by the Issue #40 fix because that fix was attached
#' to a list of functions rather than to the act of reading a target column.
#'
#' @return a "reid_scores" table (a distance: smaller is a better match)
#'
#' @seealso [score_containment()] for generalised columns.
#'
#' @examples
#' raw <- data.frame(
#'   ROW_NUMBER = 1:4,
#'   DOW = c("Mon:Mon:Tue", "Sat:Sun", "Wed:Wed:Wed", "Mon:Fri:Fri")
#' )
#' match_greedy(score_profile(join_raw_anon_data(raw, raw), "DOW"))
#'
#' @export
score_profile <- function(dat_raw_anon, target, row_number = "ROW_NUMBER",
                          split = ":", bins = NULL, shape_only = TRUE,
                          metric = c("l1", "l2"),
                          generalized = c("stop", "warn", "ignore"),
                          .fn_name = "score_profile") {
  metric <- match.arg(metric)
  cols <- reid_score_columns(dat_raw_anon, target, row_number, .fn_name,
                             generalized)

  raw_key <- dat_raw_anon[[cols$raw_row_number]]
  anon_key <- dat_raw_anon[[cols$anon_row_number]]

  ## One histogram per *record*, not per candidate pair: a cross join repeats
  ## each record once per candidate, and parsing it that many times is both
  ## wasteful and, for the automatic bin set, misleading.
  raw_keep <- !duplicated(raw_key)
  anon_keep <- !duplicated(anon_key)
  raw_values <- as.character(dat_raw_anon[[cols$raw_target]][raw_keep])
  anon_values <- as.character(dat_raw_anon[[cols$anon_target]][anon_keep])

  if (is.null(bins)) {
    seen <- c(
      unlist(split_collapsed(raw_values, split, target, .fn_name)),
      unlist(split_collapsed(anon_values, split, target, .fn_name))
    )
    bins <- sort(unique(seen))
  } else {
    bins <- as.character(bins)
    if (length(bins) == 0 || anyDuplicated(bins) > 0) {
      stop(.fn_name, "(): `bins` must be a non-empty vector of distinct bin ",
           "labels.", call. = FALSE)
    }
  }
  if (length(bins) == 0) {
    stop(.fn_name, "(): column \"", target, "\" produced no values to bin.",
         call. = FALSE)
  }

  raw_hist <- collapsed_histogram(raw_values, bins, split, shape_only, target, .fn_name)
  anon_hist <- collapsed_histogram(anon_values, bins, split, shape_only, target, .fn_name)

  ## broadcast the per-record histograms back onto every candidate pair
  delta <- raw_hist[match(raw_key, raw_key[raw_keep]), , drop = FALSE] -
    anon_hist[match(anon_key, anon_key[anon_keep]), , drop = FALSE]

  score <- if (identical(metric, "l1")) {
    rowSums(abs(delta))
  } else {
    rowSums(delta^2)
  }

  new_reid_scores(
    raw_row_number = raw_key,
    anon_row_number = anon_key,
    score = unname(score)
  )
}

#' score how long a record's activity stretches over
#'
#' Reads a collapsed column of numeric timestamps (or day numbers, or any
#' ordered numeric marker) and compares `max - min` -- how long the record was
#' active for -- between the two sides.
#'
#' The span is a different fact from both the number of events and their shape:
#' twenty purchases in one week and twenty spread over two years describe
#' different people. Comparing spans with `method = "log_ratio"` for the same
#' reason [score_count()] does: the difference between a one-day and a
#' one-month span matters far more than between two and three years.
#'
#' @inheritParams score_profile
#' @param method passed to the same comparison [score_count()] uses
#'
#' @return a "reid_scores" table (a distance: smaller is a better match)
#'
#' @seealso [score_containment()] for generalised columns.
#'
#' @examples
#' raw <- data.frame(ROW_NUMBER = 1:3, T = c("1:2:3", "1:40", "5:6:7:80"))
#' match_greedy(score_span(join_raw_anon_data(raw, raw), "T"))
#'
#' @export
score_span <- function(dat_raw_anon, target, row_number = "ROW_NUMBER",
                       split = ":",
                       method = c("log_ratio", "absolute", "relative"),
                       generalized = c("stop", "warn", "ignore"),
                       .fn_name = "score_span") {
  method <- match.arg(method)
  cols <- reid_score_columns(dat_raw_anon, target, row_number, .fn_name,
                             generalized)

  raw_key <- dat_raw_anon[[cols$raw_row_number]]
  anon_key <- dat_raw_anon[[cols$anon_row_number]]

  span_of <- function(values) {
    parsed <- split_collapsed(values, split, target, .fn_name, numeric = TRUE)
    vapply(parsed, function(v) if (length(v) == 0) 0 else max(v) - min(v), numeric(1))
  }

  raw_keep <- !duplicated(raw_key)
  anon_keep <- !duplicated(anon_key)
  raw_span <- span_of(as.character(dat_raw_anon[[cols$raw_target]][raw_keep]))
  anon_span <- span_of(as.character(dat_raw_anon[[cols$anon_target]][anon_keep]))

  new_reid_scores(
    raw_row_number = raw_key,
    anon_row_number = anon_key,
    score = compare_magnitudes(
      raw_span[match(raw_key, raw_key[raw_keep])],
      anon_span[match(anon_key, anon_key[anon_keep])],
      method
    )
  )
}
