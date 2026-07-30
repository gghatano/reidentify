## ---------------------------------------------------------------------------
## Assignment layer (Issue #11)
##
## Takes a score table from the score layer (R/score.R) or the integration
## layer (R/combine.R) and decides, for each ANON record, which RAW record the
## attacker would guess.
##
## Two assignment rules live here:
##
##   match_greedy()  -- independent per-ANON argmin, no one-to-one constraint
##   match_optimal() -- globally optimal one-to-one assignment (Issue #15)
##
## Margin/eccentricity-based confidence is Issue #16.
## ---------------------------------------------------------------------------

#' assign each ANON record to its best-scoring RAW record, independently
#'
#' For every ANON record this picks the RAW record with the best (by default:
#' smallest) SCORE. Assignments are made independently per ANON record, so the
#' same RAW record may be claimed by several ANON records -- that is what
#' "greedy" means here. Enforcing a one-to-one assignment is [match_optimal()].
#'
#' Ties are broken uniformly at random via [resolve_min_distance_ties()], so a
#' record that is genuinely indistinguishable from `k` others is credited with
#' a `1/k` chance rather than being deterministically awarded to whichever
#' candidate happened to sort first (Issue #3).
#'
#' `CONFIDENCE` is `1 / (number of RAW records tied at the best score)`: the
#' probability that this particular draw picked the right record, *given* that
#' the right record is among the tied best candidates. It is 1 exactly when the
#' best candidate is unique. It deliberately says nothing about how far ahead
#' of the runners-up the winner is -- margin / eccentricity confidence is
#' Issue #16.
#'
#' @param scores a score table: a data frame with columns RAW_ROW_NUMBER,
#'   ANON_ROW_NUMBER and SCORE, normally produced by a `score_*()` function or
#'   by [combine_scores()]. Its `score_type` attribute decides whether the best
#'   score is the smallest ("distance", the default) or the largest
#'   ("similarity").
#' @param seed integer seed for the random tie-break (default 0L, so a plain
#'   call is reproducible). NULL uses the ambient RNG stream instead.
#'
#' @return a data frame with exactly one row per ANON record, ordered by
#'   ANON_ROW_NUMBER, with columns ANON_ROW_NUMBER, RAW_ROW_NUMBER,
#'   CONFIDENCE (numeric, in (0, 1]) and RESULT (logical: whether the guessed
#'   RAW record is in fact the one the ANON record came from).
#'
#' @examples
#' raw <- data.frame(ROW_NUMBER = 1:5, V = c(10, 20, 30, 40, 50))
#' d <- join_raw_anon_data(raw, raw)
#' match_greedy(score_num(d, "V"))
#'
#' @export
match_greedy <- function(scores, seed = 0L) {
  score_type <- validate_reid_scores(scores, "scores")

  ## Internally everything is minimised. A similarity is negated rather than
  ## inverted so the transformation is monotone and never divides by zero.
  distance <- if (identical(score_type, "similarity")) -scores$SCORE else scores$SCORE

  work <- data.frame(
    RAW_ROW_NUMBER = scores$RAW_ROW_NUMBER,
    ANON_ROW_NUMBER = scores$ANON_ROW_NUMBER,
    DISTANCE = distance,
    SCORE_ROW = seq_len(nrow(scores)),
    stringsAsFactors = FALSE
  )

  picked <- resolve_min_distance_ties(work, seed = seed)

  ## size of the tie group each winner was drawn from, i.e. how many RAW
  ## records were indistinguishable from it under this score
  n_tied <- tapply(
    work$DISTANCE, work$ANON_ROW_NUMBER,
    function(v) sum(v == min(v))
  )
  tie_size <- as.numeric(n_tied[as.character(picked$ANON_ROW_NUMBER)])

  out <- data.frame(
    ANON_ROW_NUMBER = picked$ANON_ROW_NUMBER,
    RAW_ROW_NUMBER = picked$RAW_ROW_NUMBER,
    CONFIDENCE = 1 / tie_size,
    RESULT = (picked$ANON_ROW_NUMBER == picked$RAW_ROW_NUMBER),
    stringsAsFactors = FALSE
  )

  ## Which row of `scores` each winner came from. The reid_by_*() wrappers use
  ## this to recover the per-pair detail columns they have always reported;
  ## it is an implementation detail, not part of the documented return value.
  attr(out, "score_row") <- picked$SCORE_ROW

  out
}

## ---------------------------------------------------------------------------
## Globally optimal one-to-one assignment (Issue #15)
## ---------------------------------------------------------------------------

#' available linear-assignment solver backends
#'
#' The assignment layer is written against a one-function interface so the
#' solver can be swapped without touching [match_optimal()]: a backend takes a
#' non-negative cost matrix with `nrow <= ncol` and returns an integer vector
#' of length `nrow` giving the column chosen for each row.
#'
#' Only "clue" ([clue::solve_LSAP()], Hungarian algorithm) ships today.
#' `RcppHungarian` and Jonker-Volgenant implementations are faster on large
#' instances and can be added here without any change to the caller.
#'
#' @return a named list of solver functions
#'
#' @keywords internal
#'
#' @importFrom clue solve_LSAP
reid_lsap_solvers <- function() {
  list(
    clue = function(cost) as.integer(clue::solve_LSAP(cost))
  )
}

#' assign ANON records to RAW records under a global one-to-one constraint
#'
#' [match_greedy()] lets every ANON record grab its own best RAW record
#' independently, so one popular RAW record can be claimed by many ANON
#' records. If the attacker knows that the two data sets describe the *same*
#' people -- one row each -- that is a wasted constraint. `match_optimal()`
#' spends it: it chooses the assignment minimising the *total* score over all
#' ANON records at once, subject to no RAW record being used twice. This is
#' the linear sum assignment problem, solved exactly by the Hungarian
#' algorithm.
#'
#' Because the constraint is real information, the resulting success rate is a
#' better upper bound on attacker capability than the greedy one -- *but only
#' while the one-to-one premise actually holds*. Read the next section before
#' reporting a number from this function.
#'
#' @section When the one-to-one premise does not hold -- measured:
#'
#' If only part of the population appears on both sides -- the release is a
#' sample, or the attacker's background knowledge covers only some people --
#' then some ANON records have no correct answer available at all. Forcing
#' *those* records to take a RAW record anyway does not merely waste a guess:
#' the assignment is exclusive, so a record with no true match occupies the
#' RAW record that some other ANON record needed, and the damage propagates.
#'
#' This was measured (150 RAW x 150 ANON, two numeric attributes, Gaussian
#' noise sd 3, 20 seeds; script in the Issue #15 verification log):
#'
#' \tabular{lrrr}{
#'   overlap \tab greedy \tab optimal, no padding \tab optimal, padded \cr
#'   150/150 \tab 0.571 \tab 0.656 \tab 0.656 \cr
#'   120/150 \tab 0.437 \tab 0.357 \tab 0.401 \cr
#'    90/150 \tab 0.329 \tab 0.225 \tab 0.237 \cr
#'    60/150 \tab 0.227 \tab 0.156 \tab 0.124 \cr
#'    30/150 \tab 0.117 \tab 0.070 \tab 0.038
#' }
#'
#' Two things to take from that. First, a false one-to-one premise is *worse
#' than useless*: from 120/150 downwards `match_optimal()` reports a lower
#' success rate than [match_greedy()] on the same data. A tool whose job is to
#' find risk must not be run in a configuration where it under-reports, so
#' **use [match_greedy()] as the reference whenever the overlap is partial or
#' unknown** (docs/lessons-learned.md section 2). Second, padding does *not*
#' restore the raw success rate to the greedy level -- that was the expected
#' outcome and the measurement rejected it. What padding buys is precision:
#' at 90/150 overlap it lifts precision among the records actually guessed
#' from 0.228 to about 0.39, at a coverage of `sampling_rate`.
#'
#' `sampling_rate` is how a partial overlap is declared. It is the fraction of
#' ANON records believed to have their true RAW counterpart present in
#' `scores`. The cost matrix is padded with that many dummy "no guess"
#' columns, priced at `dummy_cost`, and an ANON record whose best real
#' candidate is worse than `dummy_cost` takes the dummy instead: it is
#' reported with `RAW_ROW_NUMBER = NA`, `CONFIDENCE = 0` and
#' `RESULT = FALSE`. With the default `dummy_cost` the fraction of records
#' actually guessed comes out at `sampling_rate`.
#'
#' Dummy columns are also added whenever there are fewer RAW records than ANON
#' records, since a one-to-one assignment is otherwise infeasible.
#'
#' The premise *does* hold when ANON is a strict subsample of RAW (every ANON
#' record has its counterpart in the attacker's knowledge). There
#' `match_optimal()` never loses, but its advantage shrinks as the subsample
#' does, because a smaller ANON side leaves more spare RAW records and the
#' exclusivity constraint binds less. Measured deltas over greedy, 40 seeds:
#' +0.089 at 150 of 150, +0.031 at 100 of 150, +0.014 at 60 of 150 and
#' -0.003 (not distinguishable from zero) at 30 of 150.
#'
#' The size of the gain also depends on how much signal the score carries. At
#' full overlap with 150 people and two numeric attributes, the advantage was
#' +0.039 at noise sd 1, +0.097 at sd 3, and indistinguishable from zero from
#' sd 8 upwards -- once almost nobody is identifiable, there is no structure
#' for the constraint to exploit.
#'
#' @section Cost and practical limits:
#'
#' Two separate walls, and only one of them is the solver's fault.
#'
#' * **Time.** The Hungarian algorithm is `O(n^3)`. Measured on dense random
#'   matrices with `clue::solve_LSAP()`: n = 500 about 0.2 s, n = 1000 about
#'   1.7 s, n = 2000 about 17 s, n = 3000 about 63 s. Doubling `n` costs
#'   roughly ten times as much. `match_optimal()` warns above `warn_size`
#'   (default 1000) and refuses above `max_size` (default 5000, which would
#'   run for many minutes).
#' * **Memory.** The cost matrix is `n_anon x n_raw` dense, and so is the
#'   candidate table feeding it: 10,000 people is 1e8 pairs (about 1.5 GB),
#'   100,000 people is 1e10 (about 149 GB). No solver fixes that; only
#'   candidate blocking does, and that is out of scope here.
#'
#' `block` is the escape hatch in the meantime: it splits the problem into
#' independent sub-problems. Splitting is not free -- the one-to-one
#' constraint then only holds *within* a block, so the same RAW record may be
#' used in two blocks and the result is optimal per block, not globally. Pass
#' blocks that genuinely cannot contain each other's true matches (same
#' region, same birth year, ...) and the loss is nil; pass arbitrary ones and
#' it is an approximation. Either way the choice is the caller's and visible.
#'
#' @param scores a score table with columns RAW_ROW_NUMBER, ANON_ROW_NUMBER
#'   and SCORE, as produced by a `score_*()` function or [combine_scores()].
#'   Its `score_type` attribute decides the orientation. Candidate pairs
#'   absent from the table are treated as forbidden, never as free.
#' @param sampling_rate fraction of ANON records assumed to have their true
#'   RAW counterpart present in `scores`, in (0, 1]. The default 1 is the
#'   classic "same people on both sides" assumption. Lower it when only part
#'   of the population overlaps.
#' @param seed integer seed (default 0L) used to shuffle rows and columns
#'   before solving, so that the choice among equal-cost optimal assignments
#'   depends on the seed rather than on incidental row order. NULL uses the
#'   ambient RNG stream.
#' @param dummy_cost cost of declining to guess. NULL (default) derives it
#'   from the data as the `n_real / n_anon` quantile of the per-ANON best
#'   score, so that roughly the expected number of records is matched. Give a
#'   number to set the rejection threshold explicitly; it is interpreted on
#'   the minimised score scale, shifted so the smallest score in `scores` is 0.
#' @param solver name of the linear-assignment backend (see
#'   [reid_lsap_solvers()]). Only "clue" is available today.
#' @param block optional vector of length `nrow(scores)` splitting the
#'   candidate pairs into independent sub-problems. Every candidate pair of a
#'   given ANON record must fall in the same block.
#' @param warn_size problem size (the larger matrix dimension) above which a
#'   runtime warning is issued (default 1000). NULL disables it.
#' @param max_size problem size above which this stops with an error instead
#'   of running for many minutes (default 5000). NULL disables the guard.
#'
#' @return a data frame with the same four columns as [match_greedy()] --
#'   ANON_ROW_NUMBER, RAW_ROW_NUMBER, CONFIDENCE, RESULT -- one row per ANON
#'   record, ordered by ANON_ROW_NUMBER. `RAW_ROW_NUMBER` is NA for a record
#'   the assignment declined to guess.
#'
#' @seealso [match_greedy()] for the unconstrained rule.
#'
#' @examples
#' raw <- data.frame(ROW_NUMBER = 1:5, V = c(10, 20, 30, 40, 50))
#' d <- join_raw_anon_data(raw, raw)
#' match_optimal(score_num(d, "V"))
#'
#' @importFrom stats quantile
#' @export
match_optimal <- function(scores, sampling_rate = 1, seed = 0L,
                          dummy_cost = NULL, solver = "clue", block = NULL,
                          warn_size = 1000L, max_size = 5000L) {
  score_type <- validate_reid_scores(scores, "scores")

  if (nrow(scores) == 0) {
    stop("match_optimal(): `scores` has no rows; there is nothing to assign.",
         call. = FALSE)
  }
  if (!is.numeric(sampling_rate) || length(sampling_rate) != 1 ||
      is.na(sampling_rate) || sampling_rate <= 0 || sampling_rate > 1) {
    stop("match_optimal(): `sampling_rate` must be a single number in (0, 1]; ",
         "got ", paste(format(sampling_rate), collapse = " "), ".",
         call. = FALSE)
  }
  if (!is.null(dummy_cost) &&
      (!is.numeric(dummy_cost) || length(dummy_cost) != 1 || is.na(dummy_cost))) {
    stop("match_optimal(): `dummy_cost` must be NULL or a single number.",
         call. = FALSE)
  }

  solvers <- reid_lsap_solvers()
  if (!is.character(solver) || length(solver) != 1 ||
      !(solver %in% names(solvers))) {
    stop("match_optimal(): unknown `solver` ",
         paste(format(solver), collapse = " "), ". Available: ",
         paste(names(solvers), collapse = ", "), ".", call. = FALSE)
  }
  solve_fn <- solvers[[solver]]

  ## Internally everything is minimised, exactly as in match_greedy().
  value <- if (identical(score_type, "similarity")) -scores$SCORE else scores$SCORE
  if (anyNA(value)) {
    stop("match_optimal(): `scores$SCORE` contains NA. A missing score cannot ",
         "be told apart from a forbidden pair, and guessing either way would ",
         "silently change the reported reidentification rate.", call. = FALSE)
  }

  key <- paste(scores$ANON_ROW_NUMBER, scores$RAW_ROW_NUMBER, sep = "\r")
  if (anyDuplicated(key) > 0) {
    stop("match_optimal(): `scores` contains duplicated (ANON_ROW_NUMBER, ",
         "RAW_ROW_NUMBER) pairs; each candidate pair must appear exactly once.",
         call. = FALSE)
  }

  if (is.null(block)) {
    parts <- list(seq_len(nrow(scores)))
  } else {
    if (length(block) != nrow(scores)) {
      stop("match_optimal(): `block` must have one entry per row of `scores` (",
           nrow(scores), " expected, got ", length(block), ").", call. = FALSE)
    }
    if (anyNA(block)) {
      stop("match_optimal(): `block` must not contain NA.", call. = FALSE)
    }
    parts <- unname(split(seq_len(nrow(scores)), factor(block)))
    ## An ANON record split across blocks would be assigned once per block and
    ## appear twice in the output, quietly inflating the trial count.
    block_of_row <- integer(nrow(scores))
    for (i in seq_along(parts)) block_of_row[parts[[i]]] <- i
    spread <- tapply(block_of_row, scores$ANON_ROW_NUMBER,
                     function(b) length(unique(b)))
    if (any(spread > 1)) {
      stop("match_optimal(): every candidate pair of an ANON record must be ",
           "in the same `block`, but ", sum(spread > 1), " ANON record(s) are ",
           "split across blocks.", call. = FALSE)
    }
  }

  ## Size guard, checked once for the whole call rather than inside the block
  ## loop, so a split problem does not emit one identical warning per block.
  ## The relevant size is the largest cost-matrix dimension any block will
  ## build: blocking by ANON alone does not shrink the RAW side, so a caller
  ## who wants a real speed-up has to drop cross-block candidate pairs too.
  dims <- vapply(parts, function(idx) {
    n_a <- length(unique(scores$ANON_ROW_NUMBER[idx]))
    n_r <- length(unique(scores$RAW_ROW_NUMBER[idx]))
    max(n_a, n_r + max(0L, n_a - min(n_r, as.integer(round(sampling_rate * n_a)))))
  }, numeric(1))
  size <- max(dims)
  if (!is.null(max_size) && size > max_size) {
    stop("match_optimal(): problem size ", size, " exceeds `max_size` (",
         max_size, "). The Hungarian algorithm is O(n^3) -- n = 3000 already ",
         "takes about a minute -- and the cost matrix is dense, so this would ",
         "run for a very long time or exhaust memory. Split the problem with ",
         "`block`, or raise `max_size` deliberately.", call. = FALSE)
  }
  if (!is.null(warn_size) && size > warn_size) {
    warning("match_optimal(): largest sub-problem is ", size,
            " x ", size, "; clue::solve_LSAP() takes about 1.7 s at n = 1000, ",
            "17 s at n = 2000 and 63 s at n = 3000. Consider `block` (and ",
            "dropping cross-block candidate pairs, which shrinks the RAW ",
            "side too).", call. = FALSE)
  }

  res <- with_local_seed(seed, {
    do.call(rbind, lapply(parts, function(idx) {
      match_optimal_one(
        raw = scores$RAW_ROW_NUMBER[idx],
        anon = scores$ANON_ROW_NUMBER[idx],
        value = value[idx],
        sampling_rate = sampling_rate,
        dummy_cost = dummy_cost,
        solve_fn = solve_fn
      )
    }))
  })

  res <- res[order(res$ANON_ROW_NUMBER), , drop = FALSE]
  rownames(res) <- NULL
  res
}

#' solve one linear assignment sub-problem
#'
#' The body of [match_optimal()], applied to a single block. Split out so the
#' blocking loop stays readable and so the padding arithmetic has one home.
#'
#' @param raw RAW record identifiers of this block's candidate pairs
#' @param anon ANON record identifiers of this block's candidate pairs
#' @param value scores of this block's candidate pairs, already on the
#'   minimised scale
#' @param sampling_rate see [match_optimal()]
#' @param dummy_cost see [match_optimal()]
#' @param solve_fn a solver function from [reid_lsap_solvers()]
#'
#' @return a data frame of ANON_ROW_NUMBER, RAW_ROW_NUMBER, CONFIDENCE, RESULT
#'
#' @keywords internal
match_optimal_one <- function(raw, anon, value, sampling_rate, dummy_cost,
                              solve_fn) {
  anon_levels <- sort(unique(anon))
  raw_levels <- sort(unique(raw))
  n_anon <- length(anon_levels)
  n_raw <- length(raw_levels)

  ## How many ANON records we expect to have a genuine counterpart available.
  n_real <- min(n_raw, as.integer(round(sampling_rate * n_anon)))
  n_dummy <- max(0L, n_anon - n_real)
  n_col <- n_raw + n_dummy

  ## Shift to a non-negative scale (solve_LSAP requires it). Everything below,
  ## including dummy_cost, is expressed on this shifted scale, so the shift
  ## cannot change which assignment is optimal.
  cost <- value - min(value)
  max_cost <- max(cost)

  ## Rows and columns are shuffled so that the tie-break among equal-cost
  ## optimal assignments depends on the seed rather than on the row order of
  ## the score table (docs/lessons-learned.md section 1). This does not make
  ## the draw uniform over optima -- the algorithm still has its own
  ## preferences -- but it removes the fixed positional bias.
  anon_shuffled <- anon_levels[sample.int(n_anon)]
  raw_shuffled <- raw_levels[sample.int(n_raw)]

  if (is.null(dummy_cost)) {
    frac_real <- n_real / n_anon
    if (frac_real >= 1) {
      ## frac_real >= 1 implies n_dummy == 0, so this value is never used; it
      ## only has to be a number.
      dummy_cost <- max_cost + 1
    } else {
      best_per_anon <- tapply(cost, match(anon, anon_levels), min)
      dummy_cost <- as.numeric(stats::quantile(
        best_per_anon, probs = frac_real, names = FALSE, type = 7
      ))
    }
  }

  ## A pair absent from the score table is forbidden, not free. Pricing it at
  ## "a bit more than the worst real pair" is NOT enough: the solver minimises
  ## a *sum*, so one expensive forbidden cell can still be bought with savings
  ## spread over the other rows, and the result is a confident-looking match
  ## on a pair the score layer never offered. The bound has to dominate the
  ## whole assignment: no allowed assignment can cost more than
  ## n_anon * max(worst real cost, dummy cost), so anything above that can
  ## never be part of an optimum while an allowed alternative exists.
  forbidden <- n_anon * max(max_cost, dummy_cost) + 1

  m <- matrix(forbidden, nrow = n_anon, ncol = n_col)
  m[cbind(match(anon, anon_shuffled), match(raw, raw_shuffled))] <- cost
  if (n_dummy > 0) {
    m[, (n_raw + 1L):n_col] <- dummy_cost
  }

  picked <- solve_fn(m)

  picked_cost <- m[cbind(seq_len(n_anon), picked)]
  declined <- picked > n_raw | picked_cost >= forbidden
  raw_pick <- raw_shuffled[rep(NA_integer_, n_anon)]
  raw_pick[!declined] <- raw_shuffled[picked[!declined]]

  ## CONFIDENCE: 1 / (number of candidates at least as good as the one
  ## actually assigned). For an unconstrained unique argmin this is 1, for a
  ## k-way tie at the best score it is 1/k -- the same number match_greedy()
  ## reports -- and it degrades when the one-to-one constraint pushed this
  ## record off its own first choice. Margin-based confidence is Issue #16.
  anon_idx <- match(anon, anon_shuffled)
  conf <- rep(0, n_anon)
  ok <- which(!declined)
  if (length(ok) > 0) {
    by_anon <- split(cost, factor(anon_idx, levels = seq_len(n_anon)))
    conf[ok] <- 1 / vapply(
      ok, function(i) sum(by_anon[[i]] <= picked_cost[i]), numeric(1)
    )
  }

  out <- data.frame(
    ANON_ROW_NUMBER = anon_shuffled,
    RAW_ROW_NUMBER = raw_pick,
    CONFIDENCE = conf,
    RESULT = !is.na(raw_pick) & raw_pick == anon_shuffled,
    stringsAsFactors = FALSE
  )
  out$RESULT[is.na(out$RESULT)] <- FALSE
  out
}
