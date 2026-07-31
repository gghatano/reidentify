## ---------------------------------------------------------------------------
## Evaluation metrics (Issue #12)
##
## reid_result() reports "success / trial" and nothing else. Two things are
## missing from that number, and both of them matter more than the number
## itself:
##
##  1. There is no baseline. A success rate of 0.05 is a catastrophe on 20
##     records (random guessing gives 0.05) and a strong result on 10,000.
##     Without the comparison the figure cannot be read at all.
##  2. It is an average over every ANON record, so it hides the shape of the
##     risk. "10% of records identified with 95% precision" is a far worse
##     outcome than "30% of records identified with 30% precision", and the
##     mean cannot tell the two apart.
##
## Everything here is computed analytically from the score table wherever an
## exact value exists, and cross-checked against a seed-driven simulation.
## The two agreeing is the "would I notice if this broke" check that
## docs/lessons-learned.md section 2 argues an evaluation tool needs.
## ---------------------------------------------------------------------------

#' per-ANON-record view of a score table
#'
#' Reduces the long score table to one row per ANON record, recording where
#' the *true* RAW record sits in that record's ranking and how many other RAW
#' records are indistinguishable from it.
#'
#' @param scores a score table (see [score_num()])
#' @param confidence which confidence measure to put in the CONFIDENCE
#'   column, `"margin"` (default since Issue #44) or `"tie"`. See
#'   [reid_confidence()].
#' @param tolerance relative tie tolerance (Issue #61), see
#'   [reid_confidence()]. `N_BETTER`, `TRUE_TIE_SIZE`, `TRUE_RANK` and
#'   `BEST_TIE_SIZE` are all counts of "how many candidates are as good as, or
#'   better than, this one", so all four depend on it.
#'
#' @return a data frame with one row per ANON record and columns
#'   ANON_ROW_NUMBER, N_CANDIDATES, BEST_SCORE, BEST_TIE_SIZE, CONFIDENCE,
#'   MARGIN, ECCENTRICITY, TRUE_SCORE, N_BETTER, TRUE_TIE_SIZE and TRUE_RANK.
#'
#' @keywords internal
reid_per_anon <- function(scores, confidence = c("margin", "tie"),
                          tolerance = reid_tie_tolerance()) {
  confidence <- match.arg(confidence)
  score_type <- validate_reid_scores(scores, "scores")
  validate_tie_tolerance(tolerance, "reid_per_anon")

  value <- if (identical(score_type, "similarity")) -scores$SCORE else scores$SCORE
  if (anyNA(value)) {
    stop("`scores$SCORE` contains NA; every candidate pair must have a score ",
         "before it can be evaluated.", call. = FALSE)
  }

  anon <- scores$ANON_ROW_NUMBER
  raw <- scores$RAW_ROW_NUMBER
  levels_anon <- sort(unique(anon))
  groups <- split(seq_along(anon), factor(anon, levels = levels_anon))

  rows <- lapply(seq_along(groups), function(i) {
    idx <- groups[[i]]
    ## Issue #61: every comparison below is a tie test ("as good as", "better
    ## than"), so they all run on tie-snapped values. snap_tied_values()
    ## preserves min(), so BEST_SCORE is unchanged.
    v <- snap_tied_values(value[idx], tolerance)
    this_anon <- levels_anon[i]

    best <- min(v)
    best_tie <- sum(v == best)

    is_true <- raw[idx] == this_anon
    if (any(is_true)) {
      true_score <- v[is_true][1]
      n_better <- sum(v < true_score)
      true_tie <- sum(v == true_score)
      ## "min" ranking: the best rank the true record could be given
      true_rank <- n_better + 1
    } else {
      true_score <- NA_real_
      n_better <- NA_real_
      true_tie <- NA_real_
      true_rank <- NA_real_
    }

    data.frame(
      ANON_ROW_NUMBER = this_anon,
      N_CANDIDATES = length(idx),
      BEST_SCORE = best,
      BEST_TIE_SIZE = best_tie,
      CONFIDENCE = 1 / best_tie,
      TRUE_SCORE = true_score,
      N_BETTER = n_better,
      TRUE_TIE_SIZE = true_tie,
      TRUE_RANK = true_rank,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)

  ## MARGIN / ECCENTRICITY are reported whichever measure was asked for, so a
  ## reader can see why the threshold sweep has the resolution it has without
  ## rerunning anything (Issue #16).
  conf <- reid_confidence(scores, method = confidence, tolerance = tolerance)
  ord <- match(out$ANON_ROW_NUMBER, conf$ANON_ROW_NUMBER)
  out$MARGIN <- conf$MARGIN[ord]
  out$ECCENTRICITY <- conf$ECCENTRICITY[ord]
  if (identical(confidence, "margin")) {
    out$CONFIDENCE <- conf$CONFIDENCE[ord]
  }

  out
}

#' probability that the true RAW record is inside an attacker's k best guesses
#'
#' An attacker who outputs `k` candidates takes every RAW record that is
#' strictly better than the tie group the true record sits in, then fills the
#' remaining slots from that tie group at random. Writing `b` for the number
#' of strictly better candidates and `m` for the size of the true record's tie
#' group, the true record is returned
#'
#' * never, if `b >= k` (the slots run out before the tie group is reached);
#' * always, if `b + m <= k` (the whole tie group fits);
#' * with probability `(k - b) / m` otherwise.
#'
#' At `k = 1` this reduces to `1 / m` when the true record is among the best
#' candidates and 0 otherwise -- exactly the `CONFIDENCE` that
#' [match_greedy()] reports, which is what makes the top-k curve and the
#' single-guess success rate consistent with each other by construction.
#'
#' @param n_better number of RAW records scoring strictly better than the true
#'   one (NA when the true record is absent from the candidate set)
#' @param tie_size size of the true record's tie group
#' @param k number of guesses
#'
#' @return numeric vector of probabilities in \[0, 1\]
#'
#' @keywords internal
top_k_probability <- function(n_better, tie_size, k) {
  out <- ifelse(
    is.na(n_better), 0,
    ifelse(
      n_better >= k, 0,
      ifelse(n_better + tie_size <= k, 1, (k - n_better) / tie_size)
    )
  )
  as.numeric(out)
}

#' evaluate a reidentification attack: baselines, precision-recall, top-k and
#' per-record risk
#'
#' Takes the output of the score layer and reports how dangerous the attack
#' actually is, in a form that can be read without already knowing the answer.
#'
#' **Baselines.** Two attacks that use no information are evaluated on the same
#' data: `random`, which assigns each ANON record a RAW record uniformly at
#' random, and `mode`, which ignores the ANON record entirely and always names
#' the single RAW record that comes out best most often. A measured success
#' rate must beat both before it means anything; `lift` is the ratio to the
#' random baseline.
#'
#' **Precision-recall.** The attacker can see `CONFIDENCE` but not the answer,
#' so they can choose to attack only the records they are confident about.
#' Sweeping a threshold over `CONFIDENCE` gives, at each coverage level, the
#' precision they would achieve. This is what makes "10% of records at 95%
#' precision" visible; the plain success rate averages it away.
#'
#' **Variance.** The success rate is also simulated over `seeds` tie-break
#' draws, so the point estimate comes with an sd and a range. A single run is
#' one draw from a distribution, not a property of the data: on a tie-heavy
#' 50-person fixture the rate ranges over \[0.02, 0.14\] depending only on the
#' draw. `success_analytic` is the exact expectation of that same quantity;
#' it agreeing with `success_mean` is a self-check on the implementation.
#'
#' @param scores a score table produced by a `score_*()` function or
#'   [combine_scores()]
#' @param seeds integer vector of tie-break seeds used for the simulated
#'   success rate (default 1:20). Must contain at least 2 distinct values.
#' @param top_k integer vector of guess budgets for the top-k hit rate
#'   (default `c(1, 5, 10)`); values larger than the number of candidates are
#'   dropped.
#' @param confidence which attacker-visible confidence the precision-recall
#'   sweep should threshold on: `"margin"` (default since Issue #44,
#'   eccentricity) or `"tie"` (`1 / tie size`). `"tie"` is a calibrated
#'   probability but has almost no resolution on continuous scores -- every
#'   record with a unique best candidate lands on 1, so the sweep collapses to
#'   a single point equal to the overall success rate. `"margin"` gives a
#'   distinct threshold per record and is what makes "attack the top 10% and
#'   be right most of the time" visible. It is an ordering, not a probability
#'   (Issue #16), and its scale does not carry between data sets.
#' @param tolerance relative tolerance for deciding that two candidate scores
#'   are tied, default `sqrt(.Machine$double.eps)` (Issue #61). Every risk
#'   figure here is built out of "how many candidates are at least as good as
#'   the true one", so an exact `==` made them depend on the units the input
#'   happened to use: the same 200-record data set expressed in 1/10 units
#'   moved `max_risk` from 0.5 to 1.0 and grew `precision_recall` from 3 rows
#'   to 93. Pass `tolerance = 0` for the pre-#61 behaviour; see
#'   `docs/default-changes.md`.
#'
#' @section Changed defaults:
#'
#' `confidence` defaulted to `"tie"` before Issue #44 and to `"margin"` from
#' #44 onwards, so `precision_recall` and the CONFIDENCE column of
#' `per_record` **differ from what earlier versions reported for the same
#' input.** On a 150-record continuous fixture the sweep went from 1 row
#' (threshold 1, attack 150/150, precision 0.2467) to 150 rows (top row:
#' threshold 0.3941, attack 1/150, precision 1.0000). `success_analytic`,
#' `success_mean`, `baseline`, `lift`, `top_k`, `RISK` and `max_risk` are
#' **unchanged** -- the confidence measure only reorders records, it does not
#' alter the risk. Pass `confidence = "tie"` to reproduce old numbers. See
#' `docs/default-changes.md`.
#'
#' @section Blocked candidate sets:
#'
#' A full cross join has exactly `n_anon * n_raw` rows. Anything smaller was
#' filtered -- by [block_candidates()], [lsh_candidates()],
#' [top_k_candidates()] or by hand -- and a filtered candidate set can only
#' lower the measured rate, never raise it. That is checked here from the score
#' table itself rather than from an attribute the caller has to remember to
#' pass on, and reported as `blocked` / `candidate_coverage` /
#' `n_true_missing`, which the print method shows above the success rate
#' (Issue #36).
#'
#' The row-count test alone is not enough, because `n_anon` and `n_raw` are
#' counted from the score table and so only see the records that survived. When
#' every surviving ANON record was offered every surviving RAW record the table
#' is a **complete rectangle over a subset**, `nrow(scores) == n_anon * n_raw`
#' holds, and the shape test cannot fire -- which is what a release published as
#' a single region, year or category produces. So `n_true_missing` (and
#' `truth_coverage`) is measured against the ground truth, is treated as
#' independent evidence of a filtered candidate set, and is printed whether or
#' not the shape test fired (Issue #56).
#'
#' When `n_true_missing == n_anon` **nothing at all was measured**: no ANON
#' record could have been reidentified, so every rate is 0 by construction. That
#' prints identically to a genuinely safe release, so this warns.
#'
#' @return an object of class "reid_evaluation": a list with
#'   \describe{
#'     \item{n_anon, n_raw, n_pairs}{size of the problem}
#'     \item{n_pairs_full, candidate_coverage}{the size of the full cross join
#'       **over the records present in the score table**, and what fraction of
#'       it is present. A record dropped entirely is invisible to these two,
#'       which is why the next three exist beside them}
#'     \item{n_true_missing, truth_coverage, truth_measurable}{for how many ANON
#'       records the true RAW record is not a candidate at all, the complementary
#'       fraction, and whether at least one ANON record could have been
#'       reidentified}
#'     \item{blocked}{TRUE when the candidate set is not the full cross join,
#'       by either test: fewer rows than `n_anon * n_raw`, **or**
#'       `n_true_missing > 0`}
#'     \item{confidence}{which confidence measure the sweep thresholded on}
#'     \item{success_analytic}{exact expected single-guess success rate}
#'     \item{success_mean, success_sd, success_min, success_max, n_seeds}{the
#'       same quantity simulated over `seeds`}
#'     \item{per_seed}{data frame of seed / success / trial / rate}
#'     \item{baseline}{data frame of method / rate for the random and mode
#'       baselines}
#'     \item{lift}{`success_analytic` divided by the random baseline}
#'     \item{top_k}{data frame of k / hit_rate}
#'     \item{precision_recall}{data frame of threshold / n_attacked /
#'       coverage / precision / recall}
#'     \item{per_record}{data frame of per-ANON-record risk, ordered by
#'       decreasing RISK}
#'     \item{max_risk}{the largest per-record risk}
#'   }
#'
#' @examples
#' raw <- data.frame(ROW_NUMBER = 1:6, V = c(1, 1, 2, 2, 3, 3))
#' d <- join_raw_anon_data(raw, raw)
#' reid_evaluate(score_num(d, "V"), seeds = 1:10, top_k = c(1, 2))
#'
#' @importFrom stats sd
#' @export
reid_evaluate <- function(scores, seeds = 1:20, top_k = c(1, 5, 10),
                          confidence = c("margin", "tie"),
                          tolerance = reid_tie_tolerance()) {
  confidence <- match.arg(confidence)
  validate_reid_scores(scores, "scores")
  validate_tie_tolerance(tolerance, "reid_evaluate")
  ## Issue #60: this has to be checked here and not left to the cross-check
  ## between success_analytic and success_mean, because a duplicated candidate
  ## pair moves both of them the same way. See
  ## validate_unique_candidate_pairs().
  validate_unique_candidate_pairs(scores, "reid_evaluate")

  if (length(seeds) < 2) {
    stop("reid_evaluate(): need at least 2 seeds to report a standard ",
         "deviation; got ", length(seeds), ".", call. = FALSE)
  }
  if (anyDuplicated(seeds) > 0) {
    stop("reid_evaluate(): `seeds` must not contain duplicates.", call. = FALSE)
  }

  per_anon <- reid_per_anon(scores, confidence = confidence,
                            tolerance = tolerance)
  n_anon <- nrow(per_anon)
  n_raw <- length(unique(scores$RAW_ROW_NUMBER))

  ## ---- exact per-record risk and overall expectation ----------------------
  risk <- top_k_probability(per_anon$N_BETTER, per_anon$TRUE_TIE_SIZE, 1)
  success_analytic <- mean(risk)

  ## ---- simulated success rate, for the variance and as a cross-check ------
  per_seed <- do.call(rbind, lapply(seeds, function(s) {
    m <- match_greedy(scores, seed = s, tolerance = tolerance)
    data.frame(seed = s, success = sum(m$RESULT), trial = nrow(m))
  }))
  per_seed$rate <- per_seed$success / per_seed$trial

  ## ---- baselines ----------------------------------------------------------
  ## Random assignment: for each ANON record, guess uniformly among the RAW
  ## records it was offered. Correct with probability 1/N_CANDIDATES, and only
  ## when the true record is in the candidate set at all.
  has_true <- !is.na(per_anon$TRUE_RANK)
  baseline_random <- mean(ifelse(has_true, 1 / per_anon$N_CANDIDATES, 0))

  ## Mode assignment: name the same RAW record for every ANON record -- the one
  ## that is among the best candidates most often. This is the strongest attack
  ## that ignores which ANON record it is looking at.
  ## BEST_SCORE is on the internal minimised scale, so compare against that.
  value <- if (identical(attr(scores, "score_type"), "similarity")) {
    -scores$SCORE
  } else {
    scores$SCORE
  }
  ## Snapped for the same reason as everywhere else (Issue #61): "is this
  ## candidate one of the best ones" is a tie test, and the mode baseline is
  ## what the measured rate has to beat, so it must not move with the units.
  value <- snap_tied_values_by_group(value, scores$ANON_ROW_NUMBER, tolerance)
  best_mask <- value == per_anon$BEST_SCORE[
    match(scores$ANON_ROW_NUMBER, per_anon$ANON_ROW_NUMBER)
  ]
  best_counts <- table(scores$RAW_ROW_NUMBER[best_mask])
  if (length(best_counts) == 0) {
    baseline_mode <- 0
  } else {
    modal_raw <- names(best_counts)[which.max(best_counts)]
    baseline_mode <- sum(as.character(per_anon$ANON_ROW_NUMBER) == modal_raw) / n_anon
  }

  ## ---- top-k --------------------------------------------------------------
  top_k <- sort(unique(as.numeric(top_k)))
  top_k <- top_k[top_k >= 1 & top_k <= max(per_anon$N_CANDIDATES)]
  top_k_df <- data.frame(
    k = top_k,
    hit_rate = vapply(
      top_k,
      function(k) mean(top_k_probability(per_anon$N_BETTER, per_anon$TRUE_TIE_SIZE, k)),
      numeric(1)
    )
  )

  ## ---- precision-recall over the attacker-visible confidence --------------
  ## The attacker can rank records by CONFIDENCE but cannot see RISK, so the
  ## threshold sweeps CONFIDENCE and the yield is measured with RISK.
  thresholds <- sort(unique(per_anon$CONFIDENCE), decreasing = TRUE)
  pr <- do.call(rbind, lapply(thresholds, function(t) {
    sel <- per_anon$CONFIDENCE >= t
    n_attacked <- sum(sel)
    expected_correct <- sum(risk[sel])
    data.frame(
      threshold = t,
      n_attacked = n_attacked,
      coverage = n_attacked / n_anon,
      precision = expected_correct / n_attacked,
      recall = expected_correct / n_anon
    )
  }))

  ## ---- per-record risk ----------------------------------------------------
  ## The empirical column is deliberately kept next to the analytic one: they
  ## are computed by completely different routes, so a disagreement is a bug
  ## signal rather than something to be talked around.
  hit_counts <- rep(0, n_anon)
  names(hit_counts) <- as.character(per_anon$ANON_ROW_NUMBER)
  for (s in seeds) {
    m <- match_greedy(scores, seed = s, tolerance = tolerance)
    hit_counts[as.character(m$ANON_ROW_NUMBER)] <-
      hit_counts[as.character(m$ANON_ROW_NUMBER)] + as.integer(m$RESULT)
  }

  per_record <- data.frame(
    ANON_ROW_NUMBER = per_anon$ANON_ROW_NUMBER,
    N_CANDIDATES = per_anon$N_CANDIDATES,
    TRUE_RANK = per_anon$TRUE_RANK,
    TIE_SIZE = per_anon$TRUE_TIE_SIZE,
    CONFIDENCE = per_anon$CONFIDENCE,
    MARGIN = per_anon$MARGIN,
    ECCENTRICITY = per_anon$ECCENTRICITY,
    RISK = risk,
    EMPIRICAL_RATE = as.numeric(hit_counts) / length(seeds),
    stringsAsFactors = FALSE
  )
  per_record <- per_record[order(-per_record$RISK, per_record$ANON_ROW_NUMBER), , drop = FALSE]
  rownames(per_record) <- NULL

  ## ---- is this a full cross join, or a blocked candidate set? -------------
  ## Read off the score table itself rather than from an attribute a caller
  ## would have to remember to pass: a blocked candidate set that is *not*
  ## flagged reports a rate biased downwards, and a low number is the one
  ## nobody questions (docs/lessons-learned.md section 2, Issue #36). A full
  ## join has exactly n_anon * n_raw rows, so anything smaller was filtered --
  ## by lsh_candidates(), block_candidates(), top_k_candidates() or by hand.
  ##
  ## That test has a blind spot, and Issue #56 is it. n_anon and n_raw are
  ## counted from the score table, so they only see the records that survived.
  ## If every surviving ANON record was offered every surviving RAW record --
  ## which is exactly what blocking on a key the release collapsed produces,
  ## e.g. a file published as one prefecture -- the candidate table is a
  ## *complete rectangle over a subset*, the equality holds, and the shape test
  ## says nothing. The number of ANON records whose true RAW record is not a
  ## candidate at all is measured against the ground truth instead, does not
  ## share that blind spot, and is therefore treated as independent evidence.
  n_pairs_full <- as.numeric(n_anon) * as.numeric(n_raw)
  n_true_missing <- sum(is.na(per_anon$TRUE_RANK))
  truth_coverage <- if (n_anon > 0) 1 - n_true_missing / n_anon else NA_real_

  ## "Nothing was found" and "nothing could have been found" print identically
  ## -- 0.0000 everywhere -- and only one of them is good news. Say which one
  ## it is before the reader reads the zeros (Issue #56). block_candidates()
  ## and axis_informativeness() already say "not measurable" in this
  ## situation; reid_evaluate() used to be the one that stayed quiet.
  if (n_anon > 0 && n_true_missing == n_anon) {
    warning(
      "reid_evaluate(): the true RAW record is absent from the candidate set ",
      "of every one of the ", n_anon, " ANON record(s), so every rate ",
      "reported below is 0 by construction. This is the ABSENCE OF A ",
      "MEASUREMENT, not evidence that the release is safe. Check that RAW and ",
      "ANON share a `row_number` column with matching values, and that ",
      "blocking did not discard every true pair.",
      call. = FALSE
    )
  }

  structure(
    list(
      n_anon = n_anon,
      n_raw = n_raw,
      n_pairs = nrow(scores),
      n_pairs_full = n_pairs_full,
      ## Relative to the records *present in the score table*: a record that was
      ## dropped entirely is invisible here, which is why truth_coverage exists
      ## next to it rather than instead of it.
      candidate_coverage = if (n_pairs_full > 0) nrow(scores) / n_pairs_full else NA_real_,
      n_true_missing = n_true_missing,
      truth_coverage = truth_coverage,
      truth_measurable = n_anon > 0 && n_true_missing < n_anon,
      blocked = nrow(scores) < n_pairs_full || n_true_missing > 0,
      confidence = confidence,
      success_analytic = success_analytic,
      success_mean = mean(per_seed$rate),
      success_sd = stats::sd(per_seed$rate),
      success_min = min(per_seed$rate),
      success_max = max(per_seed$rate),
      n_seeds = length(seeds),
      per_seed = per_seed,
      baseline = data.frame(
        method = c("random", "mode"),
        rate = c(baseline_random, baseline_mode),
        stringsAsFactors = FALSE
      ),
      lift = if (baseline_random > 0) success_analytic / baseline_random else NA_real_,
      top_k = top_k_df,
      precision_recall = pr,
      per_record = per_record,
      max_risk = max(risk)
    ),
    class = "reid_evaluation"
  )
}

#' print a reidentification evaluation
#'
#' @param x a "reid_evaluation" object
#' @param ... ignored
#'
#' @return `x`, invisibly
#'
#' @export
print.reid_evaluation <- function(x, ...) {
  cat(sprintf(
    "reid evaluation: %d ANON x %d RAW record(s), %d candidate pair(s)\n",
    x$n_anon, x$n_raw, x$n_pairs
  ))
  ## Only printed when something is wrong, so the ordinary full-join output is
  ## unchanged -- but when something is wrong, it is printed before the success
  ## rate, not after it (Issue #36).
  ##
  ## Two independent symptoms, reported independently (Issue #56): the *shape*
  ## of the candidate table, and the *ground truth* missing from it. The shape
  ## test is blind to a complete rectangle over a subset; the missing-truth
  ## count is not. Gating the second on the first meant that in exactly the
  ## case the shape test cannot see, nothing at all was printed -- while the
  ## count sat in the object, computed and unshown.
  n_missing <- x$n_true_missing %||% 0L
  if (length(n_missing) != 1L || is.na(n_missing)) {
    n_missing <- 0L
  }
  shape_blocked <- isTRUE(x$n_pairs < x$n_pairs_full)

  if (shape_blocked) {
    cat(sprintf(
      "  candidate set  : BLOCKED -- %.4g%% of the full %.0f-pair join kept\n",
      100 * x$candidate_coverage, x$n_pairs_full
    ))
    cat(sprintf(
      "    true RAW record absent from the candidates of %d/%d ANON record(s)%s\n",
      n_missing, x$n_anon,
      if (n_missing > 0) "" else " (recall 1.0 on the records shown)"
    ))
    if (n_missing > 0) {
      cat("    -> the success rate below is a LOWER bound. ANON records that ",
          "were left\n       with no candidate at all are not counted here at ",
          "all.\n", sep = "")
    }
  } else if (n_missing > 0) {
    cat(sprintf(
      "  ground truth   : true RAW record absent from the candidates of %d/%d ANON record(s)\n",
      n_missing, x$n_anon
    ))
    cat("    -> the candidate table has exactly n_anon x n_raw rows, so the ",
        "row-count\n       test cannot see this. Those records can never be ",
        "reidentified: the\n       success rate below is a LOWER bound.\n", sep = "")
  }
  if (n_missing > 0 && n_missing >= x$n_anon) {
    cat("  ! NOT MEASURABLE: no ANON record has its true RAW record among its ",
        "candidates.\n    Every rate below is 0 by construction. That is the ",
        "absence of a measurement,\n    not evidence that the release is safe. ",
        "Check that RAW and ANON share a\n    matching row-number column, and ",
        "that blocking did not discard every true pair.\n", sep = "")
  }
  cat(sprintf(
    "  success rate   : %.4f exact | simulated mean %.4f sd %.4f range [%.4f, %.4f] over %d seeds\n",
    x$success_analytic, x$success_mean, x$success_sd,
    x$success_min, x$success_max, x$n_seeds
  ))
  cat(sprintf(
    "  baseline       : random %.4f | mode %.4f%s\n",
    x$baseline$rate[x$baseline$method == "random"],
    x$baseline$rate[x$baseline$method == "mode"],
    if (is.na(x$lift)) "" else sprintf("   (lift vs random: %.2fx)", x$lift)
  ))
  cat(sprintf(
    "  top-k hit rate : %s\n",
    paste(sprintf("k=%g %.4f", x$top_k$k, x$top_k$hit_rate), collapse = "  ")
  ))
  cat(sprintf("  max per-record risk: %.4f\n", x$max_risk))

  cat(sprintf(
    "  precision-recall (threshold on attacker-visible CONFIDENCE, %s):\n",
    x$confidence %||% "tie"
  ))
  ## The %||% fallback above stays "tie", not the new "margin" default: an
  ## object without a `confidence` field predates Issue #16, and back then the
  ## sweep really was thresholding on 1 / tie size.
  pr <- x$precision_recall
  show <- utils::head(pr, 5L)
  for (i in seq_len(nrow(show))) {
    cat(sprintf(
      "    conf >= %.4f : attack %d/%d (%.1f%%)  precision %.4f  recall %.4f\n",
      show$threshold[i], show$n_attacked[i], x$n_anon,
      100 * show$coverage[i], show$precision[i], show$recall[i]
    ))
  }
  if (nrow(pr) > nrow(show)) {
    cat(sprintf("    ... %d more threshold(s)\n", nrow(pr) - nrow(show)))
  }

  invisible(x)
}
