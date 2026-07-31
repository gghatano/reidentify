## ---------------------------------------------------------------------------
## Candidate generation / blocking (Issue #36)
##
## join_raw_anon_data() materialises every RAW x ANON pair. That is n^2 rows,
## and the wall it hits is memory, not solver speed: 10^4 people is already
## 10^8 pairs, 10^5 people is 10^10. No assignment algorithm can be made fast
## enough to cross that, because the candidate table does not fit before the
## solver is ever called.
##
## Blocking replaces the cross join with "only the pairs that agree on some
## cheap key". The cost is that it is *lossy*: if the true counterpart of an
## ANON record is not in the candidate set, that record can never be
## reidentified and the reported success rate falls.
##
## THAT IS THE DANGEROUS DIRECTION. docs/lessons-learned.md section 2: a
## safety-evaluation tool that breaks towards "looks safe" does not get
## questioned. So everything here reports **recall** -- the fraction of true
## pairs that survived -- next to the reduction, every generator attaches a
## `reid_blocking` record of what it discarded, warns when recall < 1, and
## reid_evaluate() independently detects a candidate table that is not a full
## cross join and says so in its printed output. The user has to work to *not*
## see it.
##
## Recall is computable exactly, and for free: this package's ground truth is
## "same ROW_NUMBER on both sides" (that is what reid_per_anon() scores
## against), so the generator already knows which pairs are the true ones.
## ---------------------------------------------------------------------------

#' record of what a blocking step kept and what it threw away
#'
#' @param method short name of the blocking method
#' @param n_raw,n_anon number of records on each side
#' @param n_pairs_full number of pairs before blocking
#' @param n_pairs_kept number of pairs after blocking
#' @param n_true_pairs number of true (same ROW_NUMBER) pairs available before
#'   blocking
#' @param n_true_pairs_kept how many of those survived
#' @param n_anon_without_candidate ANON records left with no candidate at all
#' @param extra named list of method-specific fields
#'
#' @return an object of class "reid_blocking"
#'
#' @keywords internal
new_blocking_info <- function(method, n_raw, n_anon, n_pairs_full, n_pairs_kept,
                              n_true_pairs, n_true_pairs_kept,
                              n_anon_without_candidate, extra = list()) {
  out <- c(
    list(
      method = method,
      n_raw = n_raw,
      n_anon = n_anon,
      n_pairs_full = n_pairs_full,
      n_pairs_kept = n_pairs_kept,
      kept_fraction = if (n_pairs_full > 0) n_pairs_kept / n_pairs_full else NA_real_,
      reduction = if (n_pairs_full > 0) 1 - n_pairs_kept / n_pairs_full else NA_real_,
      n_true_pairs = n_true_pairs,
      n_true_pairs_kept = n_true_pairs_kept,
      ## NA rather than 1 when there is no ground truth to measure against:
      ## "recall 1.0" on zero true pairs would be a reassuring number that
      ## means nothing.
      recall = if (n_true_pairs > 0) n_true_pairs_kept / n_true_pairs else NA_real_,
      n_anon_without_candidate = n_anon_without_candidate
    ),
    extra
  )
  class(out) <- "reid_blocking"
  out
}

#' print a blocking record
#'
#' @param x a "reid_blocking" object
#' @param ... ignored
#'
#' @return `x`, invisibly
#'
#' @export
print.reid_blocking <- function(x, ...) {
  cat(sprintf(
    "blocking (%s): %s of %s pair(s) kept (%.4g%% of the full %d x %d join)\n",
    x$method, format(x$n_pairs_kept, big.mark = ","),
    format(x$n_pairs_full, big.mark = ","),
    100 * x$kept_fraction, x$n_anon, x$n_raw
  ))
  if (is.na(x$recall)) {
    cat("  recall       : not measurable (no RAW/ANON record shares a ROW_NUMBER)\n")
  } else {
    cat(sprintf(
      "  recall       : %.4f  (%d of %d true pair(s) retained)\n",
      x$recall, x$n_true_pairs_kept, x$n_true_pairs
    ))
  }
  cat(sprintf(
    "  ANON records with no candidate at all: %d\n", x$n_anon_without_candidate
  ))
  if (!is.na(x$recall) && x$recall < 1) {
    cat(sprintf(
      "  ! %d true pair(s) were discarded. A reidentification rate measured on\n",
      x$n_true_pairs - x$n_true_pairs_kept
    ))
    cat("    this candidate set is a LOWER bound: those records cannot be found.\n")
  }
  known <- c("method", "n_raw", "n_anon", "n_pairs_full", "n_pairs_kept",
             "kept_fraction", "reduction", "n_true_pairs", "n_true_pairs_kept",
             "recall", "n_anon_without_candidate")
  extra <- setdiff(names(x), known)
  if (length(extra) > 0) {
    cat("  settings     : ",
        paste(vapply(extra, function(nm) {
          paste0(nm, " = ", paste(format(x[[nm]]), collapse = ","))
        }, character(1)), collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

#' warn when a blocking step lost true pairs
#'
#' Printing the record only helps a reader who prints it. This fires whether or
#' not anybody looks, because the failure it announces is invisible downstream:
#' the assessment simply returns a smaller number.
#'
#' @param info a "reid_blocking" object
#' @param fn_name calling function, for the message
#'
#' @return `info`, invisibly
#'
#' @keywords internal
warn_blocking_loss <- function(info, fn_name) {
  if (!is.na(info$recall) && info$recall < 1) {
    warning(
      fn_name, "(): blocking discarded ",
      info$n_true_pairs - info$n_true_pairs_kept, " of ", info$n_true_pairs,
      " true pair(s) (recall ", format(round(info$recall, 4)), "). Any ",
      "reidentification rate measured on this candidate set is a LOWER bound. ",
      "See attr(x, \"blocking\").",
      call. = FALSE
    )
  }
  invisible(info)
}

#' count how many true pairs a list of candidate index pairs retained
#'
#' @param raw_key,anon_key the row-number vectors of the two input tables
#' @param ri,ai integer index vectors into `raw_key` / `anon_key`
#'
#' @return a list with `n_true_pairs` (true pairs available in the full join)
#'   and `n_true_pairs_kept`
#'
#' @keywords internal
count_true_pairs <- function(raw_key, anon_key, ri, ai) {
  ## A true pair exists in the full join for every ANON record whose row number
  ## also occurs in RAW. Duplicated row numbers would make several, so this
  ## counts pairs, not records.
  tab_raw <- table(raw_key)
  per_anon <- as.numeric(tab_raw[match(as.character(anon_key), names(tab_raw))])
  per_anon[is.na(per_anon)] <- 0
  n_true <- sum(per_anon)

  kept <- if (length(ri) == 0) 0 else sum(raw_key[ri] == anon_key[ai])

  list(n_true_pairs = n_true, n_true_pairs_kept = kept)
}

#' glue two sides of a candidate set into a raw_anon table
#'
#' @param raw,anon the input data frames
#' @param ri,ai index vectors of equal length
#' @param raw_header,anon_header column prefixes
#'
#' @return a data frame in raw_anon form
#'
#' @keywords internal
assemble_candidates <- function(raw, anon, ri, ai, raw_header, anon_header) {
  out_raw <- raw[ri, , drop = FALSE]
  out_anon <- anon[ai, , drop = FALSE]
  names(out_raw) <- paste0(raw_header, names(out_raw))
  names(out_anon) <- paste0(anon_header, names(out_anon))
  out <- cbind(out_raw, out_anon)
  rownames(out) <- NULL
  out
}

#' build the blocking keys of one pass, for both sides at once
#'
#' Both sides are coded together and only then split apart, because the codes
#' have to mean the same thing on each: a key is only useful here if
#' `key_raw[i] == key_anon[j]` says the two records agree, and per-frame codes
#' would make that comparison meaningless.
#'
#' This uses [reid_value_key()] rather than pasting the values themselves
#' (Issue #70). The old form separated the parts with `"\\r"` on the assumption
#' that no column value contains one -- an assumption about the *user's* data,
#' not about this package's output, and false for anything read out of a
#' CRLF-quoted CSV field. Two records that disagreed could then land in one
#' block, which is not the direction that loses true pairs, but it does make
#' the reported reduction describe a different blocking than the one asked for.
#'
#' @param raw,anon the two data frames
#' @param cols column names making up the key
#' @param transform named list of functions applied before comparison
#'
#' @return a list with `raw` and `anon` character vectors, one key per row
#'
#' @keywords internal
blocking_pass_keys <- function(raw, anon, cols, transform) {
  n_raw <- nrow(raw)
  codes <- lapply(cols, function(cl) {
    fn <- transform[[cl]]
    vr <- raw[[cl]]
    va <- anon[[cl]]
    if (!is.null(fn)) {
      vr <- fn(vr)
      va <- fn(va)
    }
    ## as.character() first: the two sides are supplied independently and may
    ## disagree on storage type (factor against character, integer against
    ## double), and blocking has always compared the value as written. Coding
    ## the two sides jointly is what makes the codes comparable.
    reid_class_codes(c(as.character(vr), as.character(va)))
  })
  key <- reid_value_key(codes)
  list(raw = key[seq_len(n_raw)], anon = key[n_raw + seq_len(nrow(anon))])
}

#' build a reduced RAW/ANON candidate table by deterministic blocking
#'
#' A drop-in replacement for [join_raw_anon_data()] that keeps only the pairs
#' agreeing on a **blocking key** -- the oldest and cheapest candidate
#' reduction there is, and the one that makes a hundred-thousand-record
#' assessment possible at all: the full join of 100,000 x 100,000 is 10^10
#' pairs.
#'
#' `keys` names the columns that must agree. Give several passes to take their
#' **union**, which is the standard way to buy recall back: a record whose ZIP
#' was perturbed is still reachable through the (AGE, SEX) pass. `transform`
#' coarsens a column before comparison, so blocking on a *generalisation* of a
#' quasi-identifier -- decade of age, first three digits of a postcode -- does
#' not need a second column in the data.
#'
#' **THIS UNDER-REPORTS RISK WHEN IT LOSES A TRUE PAIR.** If the true
#' counterpart of an ANON record does not agree on any key, it is not a
#' candidate, the record can never be reidentified, and the measured
#' reidentification rate goes *down*. That is the direction a safety tool must
#' not fail in quietly (`docs/lessons-learned.md` section 2), so this function
#' measures its own recall, records it in the `blocking` attribute of the
#' result, and warns when it is below 1. Recall is exact, not estimated: the
#' true pairs are the ones sharing a `row_number`, which is the same ground
#' truth [reid_evaluate()] scores against.
#'
#' Blocking is only worth it when the key is *stable* under the anonymisation
#' being assessed. A key the release perturbs (a noised age, a suppressed
#' postcode) drops true pairs by construction; the recall figure will say so.
#'
#' @param raw,anon data frames, as for [join_raw_anon_data()]
#' @param keys the blocking key. Either a character vector of column names --
#'   all of which must agree -- or a list of such vectors, whose passes are
#'   unioned. Columns must exist in both `raw` and `anon`.
#' @param transform optional named list of functions, applied to the column of
#'   that name on **both** sides before the keys are compared. Use it to block
#'   on a coarsened value, e.g. `list(AGE = function(x) x \%/\% 10)`.
#' @param row_number name of the row-number column (default "ROW_NUMBER"),
#'   used to identify the true pairs when measuring recall
#' @param max_pairs safety valve: stop rather than materialise more than this
#'   many candidate pairs (default 1e7). A key that barely discriminates
#'   produces one enormous block and no saving at all; failing loudly is
#'   better than exhausting memory.
#' @param raw_header,anon_header column prefixes, as for [join_raw_anon_data()]
#'
#' @return a data frame in raw_anon form holding a *subset* of the pairs
#'   [join_raw_anon_data()] would produce, carrying a `blocking` attribute (a
#'   [print()]able "reid_blocking" record of the reduction and the recall).
#'
#' @seealso [lsh_candidates()] for set-valued columns, [top_k_candidates()] for
#'   pruning a score table, [blocking_recall()] to measure a candidate set that
#'   was built some other way.
#'
#' @examples
#' raw <- data.frame(ROW_NUMBER = 1:6, ZIP = c("A", "A", "B", "B", "C", "C"),
#'                   AGE = c(31, 42, 33, 44, 35, 46))
#' cand <- block_candidates(raw, raw, keys = "ZIP")
#' nrow(cand)
#' attr(cand, "blocking")
#'
#' @export
block_candidates <- function(raw, anon, keys, transform = NULL,
                             row_number = "ROW_NUMBER",
                             max_pairs = 1e7,
                             raw_header = "RAW_", anon_header = "ANON_") {
  if (!is.data.frame(raw) || !is.data.frame(anon)) {
    stop("block_candidates(): `raw` and `anon` must both be data frames.",
         call. = FALSE)
  }
  if (is.character(keys)) {
    keys <- list(keys)
  }
  if (!is.list(keys) || length(keys) == 0 ||
        !all(vapply(keys, function(k) is.character(k) && length(k) > 0, logical(1)))) {
    stop("block_candidates(): `keys` must be a character vector of column ",
         "names, or a list of such vectors (one per blocking pass).",
         call. = FALSE)
  }
  if (is.null(transform)) {
    transform <- list()
  }
  if (!is.list(transform) ||
        (length(transform) > 0 && is.null(names(transform)))) {
    stop("block_candidates(): `transform` must be a named list of functions.",
         call. = FALSE)
  }
  for (nm in names(transform)) {
    if (!is.function(transform[[nm]])) {
      stop("block_candidates(): `transform$", nm, "` is not a function.",
           call. = FALSE)
    }
  }
  needed <- unique(c(unlist(keys, use.names = FALSE), row_number))
  for (nm in needed) {
    missing_in <- c(
      if (!nm %in% names(raw)) "raw",
      if (!nm %in% names(anon)) "anon"
    )
    if (length(missing_in) > 0) {
      stop("block_candidates(): column \"", nm, "\" not found in ",
           paste(missing_in, collapse = " and "), ".", call. = FALSE)
    }
  }
  if (!is.numeric(max_pairs) || length(max_pairs) != 1L || is.na(max_pairs) ||
        max_pairs < 1) {
    stop("block_candidates(): `max_pairs` must be a single positive number.",
         call. = FALSE)
  }

  n_raw <- nrow(raw)
  n_anon <- nrow(anon)

  ri <- integer(0)
  ai <- integer(0)
  for (pass in keys) {
    pk <- blocking_pass_keys(raw, anon, pass, transform)

    by_raw <- split(seq_len(n_raw), pk$raw)
    by_anon <- split(seq_len(n_anon), pk$anon)
    common <- intersect(names(by_raw), names(by_anon))
    if (length(common) == 0) {
      next
    }

    ## Size the pass before building it. expand.grid() on a block that turned
    ## out to hold everybody is exactly the n^2 allocation this function exists
    ## to avoid.
    n_new <- sum(vapply(common, function(k) {
      as.numeric(length(by_raw[[k]])) * as.numeric(length(by_anon[[k]]))
    }, numeric(1)))
    if (length(ri) + n_new > max_pairs) {
      stop("block_candidates(): the key (", paste(pass, collapse = ", "),
           ") would produce ", format(length(ri) + n_new, scientific = FALSE),
           " candidate pair(s), above max_pairs = ",
           format(max_pairs, scientific = FALSE),
           ". The key is probably too coarse -- add a column, coarsen less, or ",
           "raise max_pairs deliberately.", call. = FALSE)
    }

    ## Accumulate per block and concatenate once. Growing `ri` with c() inside
    ## the loop is quadratic in the number of blocks, and the number of blocks
    ## is what grows with n -- which would have put an n^2 back into the
    ## function whose whole purpose is to remove one.
    chunk_r <- vector("list", length(common))
    chunk_a <- vector("list", length(common))
    for (i in seq_along(common)) {
      r <- by_raw[[common[i]]]
      a <- by_anon[[common[i]]]
      chunk_r[[i]] <- rep(r, times = length(a))
      chunk_a[[i]] <- rep(a, each = length(r))
    }
    ri <- c(ri, unlist(chunk_r, use.names = FALSE))
    ai <- c(ai, unlist(chunk_a, use.names = FALSE))
  }

  if (length(ri) > 0 && length(keys) > 1) {
    ## Passes overlap; a pair must appear once or combine_scores() would reject
    ## the duplicated candidate rows downstream.
    code <- (as.numeric(ri) - 1) * n_anon + as.numeric(ai)
    keep <- !duplicated(code)
    ri <- ri[keep]
    ai <- ai[keep]
  }
  if (length(ri) > 0) {
    ord <- order(ai, ri)
    ri <- ri[ord]
    ai <- ai[ord]
  }

  out <- assemble_candidates(raw, anon, ri, ai, raw_header, anon_header)

  truth <- count_true_pairs(raw[[row_number]], anon[[row_number]], ri, ai)
  info <- new_blocking_info(
    method = "deterministic",
    n_raw = n_raw, n_anon = n_anon,
    n_pairs_full = as.numeric(n_raw) * as.numeric(n_anon),
    n_pairs_kept = length(ri),
    n_true_pairs = truth$n_true_pairs,
    n_true_pairs_kept = truth$n_true_pairs_kept,
    n_anon_without_candidate = n_anon - length(unique(ai)),
    extra = c(
      list(keys = vapply(keys, paste, character(1), collapse = "+")),
      if (length(transform) > 0) list(transform = names(transform))
    )
  )
  attr(out, "blocking") <- info
  warn_blocking_loss(info, "block_candidates")
  out
}

#' keep only the k best-scoring RAW candidates per ANON record
#'
#' Blocking on a key reduces the candidate table before any score is computed.
#' This reduces it *after*: given a score table, it keeps each ANON record's k
#' best candidates and drops the rest. That is useful as a second stage -- a
#' cheap score over a blocked candidate set, pruned to k, then an expensive
#' score only on what survived -- and it bounds the memory of everything
#' downstream at `k * n_anon` rows regardless of how many candidates the first
#' stage produced.
#'
#' **Ties are kept, not cut.** If the k-th and (k+1)-th candidates score
#' equally there is no evidence to choose between them, and cutting on row
#' order would drop true pairs for no reason -- silently lowering the reported
#' rate. So `ties = "keep"` (the default) returns *at least* k candidates per
#' record, more where the score is flat. `ties = "random"` caps hard at k, and
#' needs a `seed` for the same reason [match_greedy()] does.
#'
#' Recall is reported the same way as for [block_candidates()]: it is below 1
#' whenever the true RAW record was not among the k best, which is not rare --
#' that is exactly the "not identified at rank 1 but identified at rank 7" case
#' the top-k hit rate of [reid_evaluate()] measures.
#'
#' @param scores a score table (see [score_num()])
#' @param k number of candidates to keep per ANON record
#' @param ties `"keep"` (default) to keep every candidate tied with the k-th,
#'   or `"random"` to cut at exactly k, breaking ties at random
#' @param seed integer seed used when `ties = "random"`, or NULL for the
#'   ambient RNG stream
#'
#' @return the score table restricted to the kept pairs, carrying a `blocking`
#'   attribute (a "reid_blocking" record).
#'
#' @examples
#' raw <- data.frame(ROW_NUMBER = 1:6, V = c(1, 2, 3, 4, 5, 6))
#' s <- score_num(join_raw_anon_data(raw, raw), "V")
#' pruned <- top_k_candidates(s, k = 2)
#' nrow(pruned)
#' attr(pruned, "blocking")$recall
#'
#' @export
top_k_candidates <- function(scores, k = 10, ties = c("keep", "random"),
                             seed = NULL) {
  ties <- match.arg(ties)
  score_type <- validate_reid_scores(scores, "scores")
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 1) {
    stop("top_k_candidates(): `k` must be a single positive number.",
         call. = FALSE)
  }
  k <- as.integer(k)
  if (anyNA(scores$SCORE)) {
    stop("top_k_candidates(): `scores$SCORE` contains NA; a missing score ",
         "cannot be ranked, and treating it as worst would silently drop the ",
         "record's candidates.", call. = FALSE)
  }

  value <- if (identical(score_type, "similarity")) -scores$SCORE else scores$SCORE
  anon <- scores$ANON_ROW_NUMBER

  idx <- seq_len(nrow(scores))
  if (identical(ties, "random")) {
    ## order() is stable, so shuffling first makes the tie order random rather
    ## than "whatever the join happened to produce".
    idx <- with_local_seed(seed, sample(idx))
  }
  ord <- idx[order(anon[idx], value[idx])]
  a <- anon[ord]
  v <- value[ord]

  runs <- rle(as.character(a))$lengths
  starts <- cumsum(c(0L, utils::head(runs, -1L)))
  rank_in <- sequence(runs)

  if (identical(ties, "random")) {
    keep_sorted <- rank_in <= k
  } else {
    ## the k-th best value of each group, broadcast over the group
    kth <- v[starts + pmin(k, runs)]
    keep_sorted <- v <= rep(kth, times = runs)
  }

  kept_rows <- sort(ord[keep_sorted])
  out <- scores[kept_rows, , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "score_type") <- score_type
  class(out) <- unique(c("reid_scores", class(out)))

  ## The denominator is the true pairs *present in the input*, not the true
  ## pairs that exist in the world: top-k cannot recover a pair an earlier
  ## stage already dropped, and charging it for that would hide the earlier
  ## loss inside this one. Chain the records instead of merging them.
  n_true_in <- sum(scores$RAW_ROW_NUMBER == scores$ANON_ROW_NUMBER)
  n_true_kept <- sum(out$RAW_ROW_NUMBER == out$ANON_ROW_NUMBER)

  info <- new_blocking_info(
    method = "top-k",
    n_raw = length(unique(scores$RAW_ROW_NUMBER)),
    n_anon = length(unique(anon)),
    n_pairs_full = nrow(scores),
    n_pairs_kept = nrow(out),
    n_true_pairs = n_true_in,
    n_true_pairs_kept = n_true_kept,
    n_anon_without_candidate = length(unique(anon)) -
      length(unique(out$ANON_ROW_NUMBER)),
    extra = list(k = k, ties = ties)
  )
  attr(out, "blocking") <- info
  warn_blocking_loss(info, "top_k_candidates")
  out
}

#' measure the reduction and the recall of any candidate set
#'
#' [block_candidates()], [lsh_candidates()] and [top_k_candidates()] already
#' attach this record to their own output. Use this when the candidate set came
#' from somewhere else -- a hand-written filter, a database query, a subset
#' taken for speed -- because a candidate set with no recall figure attached is
#' a reidentification rate with an unknown downward bias.
#'
#' @param candidates a candidate table in raw_anon form (as from
#'   [join_raw_anon_data()]) or a score table (as from [score_num()])
#' @param raw,anon the data frames the candidates were drawn from. Optional,
#'   but without them the totals have to be inferred from the candidate table
#'   itself, which **cannot see records that were dropped entirely** -- pass
#'   them whenever you have them.
#' @param row_number name of the row-number column, before RAW_/ANON_
#'   prefixing (default "ROW_NUMBER")
#'
#' @return a "reid_blocking" object; see [block_candidates()].
#'
#' @examples
#' raw <- data.frame(ROW_NUMBER = 1:6, ZIP = c("A", "A", "B", "B", "C", "C"))
#' cand <- block_candidates(raw, raw, keys = "ZIP")
#' blocking_recall(cand, raw, raw)
#'
#' @export
blocking_recall <- function(candidates, raw = NULL, anon = NULL,
                            row_number = "ROW_NUMBER") {
  if (!is.data.frame(candidates)) {
    stop("blocking_recall(): `candidates` must be a data frame.", call. = FALSE)
  }

  raw_col <- paste0("RAW_", row_number)
  anon_col <- paste0("ANON_", row_number)
  if (all(c("RAW_ROW_NUMBER", "ANON_ROW_NUMBER") %in% names(candidates)) &&
        !all(c(raw_col, anon_col) %in% names(candidates))) {
    raw_col <- "RAW_ROW_NUMBER"
    anon_col <- "ANON_ROW_NUMBER"
  }
  if (!all(c(raw_col, anon_col) %in% names(candidates))) {
    stop("blocking_recall(): `candidates` has neither \"", raw_col, "\"/\"",
         anon_col, "\" nor the score-layer RAW_ROW_NUMBER/ANON_ROW_NUMBER ",
         "columns.", call. = FALSE)
  }

  raw_key <- if (is.null(raw)) {
    unique(candidates[[raw_col]])
  } else {
    if (!row_number %in% names(raw)) {
      stop("blocking_recall(): column \"", row_number, "\" not found in `raw`.",
           call. = FALSE)
    }
    raw[[row_number]]
  }
  anon_key <- if (is.null(anon)) {
    unique(candidates[[anon_col]])
  } else {
    if (!row_number %in% names(anon)) {
      stop("blocking_recall(): column \"", row_number, "\" not found in `anon`.",
           call. = FALSE)
    }
    anon[[row_number]]
  }

  tab_raw <- table(raw_key)
  per_anon <- as.numeric(tab_raw[match(as.character(anon_key), names(tab_raw))])
  per_anon[is.na(per_anon)] <- 0

  info <- new_blocking_info(
    method = if (is.null(raw) && is.null(anon)) "measured (totals inferred)" else "measured",
    n_raw = length(raw_key),
    n_anon = length(anon_key),
    n_pairs_full = as.numeric(length(raw_key)) * as.numeric(length(anon_key)),
    n_pairs_kept = nrow(candidates),
    n_true_pairs = sum(per_anon),
    n_true_pairs_kept = sum(candidates[[raw_col]] == candidates[[anon_col]]),
    n_anon_without_candidate =
      length(anon_key) - length(unique(candidates[[anon_col]]))
  )
  info
}
