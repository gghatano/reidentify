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

#' the package-wide default tolerance for deciding that two scores are tied
#'
#' `sqrt(.Machine$double.eps)`, about 1.5e-8, used as a **relative** tolerance:
#' two scores count as tied when they agree to roughly the first 8 significant
#' digits. That is `all.equal()`'s default and it deliberately spends half of a
#' double's 16 digits, because the last 8 are where representation noise lives.
#'
#' @return a single positive number
#'
#' @keywords internal
reid_tie_tolerance <- function() sqrt(.Machine$double.eps)

#' collapse near-equal scores onto a common value, so `==` can be used on them
#'
#' Issue #61. `score_num()` is `|raw - anon|`, so in exact arithmetic scaling
#' every value by `c > 0` scales every score by `c` and leaves the tie structure
#' -- and therefore every risk figure -- unchanged. In doubles it does not:
#'
#' ```
#' 42.3 - 41.2 = 1.0999999999999943
#' 43.4 - 42.3 = 1.1000000000000014
#' ```
#'
#' Equal as reals, 7.1e-15 apart as doubles. Every tie test in this package was
#' an exact `==`, so re-expressing the same data in 1/10 units turned ties into
#' strict orderings. Measured on a 200-record fixture built so every ANON record
#' has exactly one symmetric decoy (`docs/adversarial/adv2-02-probe.R` C3): the
#' max per-record risk moved from 0.5 to 1.0, 43 records with a true risk of 0.5
#' were reported as 0 and 47 as 1, and the precision-recall table grew from 3
#' rows to 93 -- 90 of them thresholds separated by ~1e-14 of rounding noise.
#'
#' Rather than scatter tolerant comparisons through every tie test, this
#' rewrites the values once: within one ANON record's candidate list, scores
#' that are within `tolerance` of each other are replaced by their group
#' minimum. All the existing `==`, `<` and `min()` tests then work unchanged and
#' remain exact, transitive and order-preserving -- which a pairwise
#' `abs(a - b) < tol` is not.
#'
#' Grouping is single-linkage over the sorted values: a gap wider than
#' `tolerance * max(1, |a|, |b|)` starts a new group. Chaining is therefore
#' possible in principle (a ladder of values each within tolerance of the next
#' fuses end to end), but at 1.5e-8 relative it takes on the order of 10^8
#' candidates in one ANON record's list to span a factor of two.
#'
#' `min()` is preserved exactly -- the representative of a group is its
#' smallest member -- so `BEST_SCORE` and the identity of the winning candidate
#' do not move. What moves is only how many candidates count as tied with it.
#'
#' @param v numeric vector of scores for one ANON record, on the internally
#'   minimised scale
#' @param tolerance relative tolerance; 0 restores exact `==` comparison
#'
#' @return `v` with near-equal entries replaced by their group minimum
#'
#' @keywords internal
snap_tied_values <- function(v, tolerance = reid_tie_tolerance()) {
  if (tolerance == 0 || length(v) < 2L) {
    return(v)
  }
  ord <- order(v)
  s <- v[ord]
  a <- s[-length(s)]
  b <- s[-1L]
  ## a == b catches Inf == Inf, which the subtraction cannot (Inf - Inf is
  ## NaN); the is.finite() guard keeps Inf from being fused with a finite
  ## score, which the scaled comparison would otherwise do (Inf <= Inf).
  same <- (a == b) |
    (is.finite(a) & is.finite(b) & (b - a) <= tolerance * pmax(1, abs(a), abs(b)))
  same[is.na(same)] <- FALSE
  gid <- cumsum(c(TRUE, !same))
  out <- v
  out[ord] <- s[!duplicated(gid)][gid]
  out
}

#' apply [snap_tied_values()] within each ANON record's candidate list
#'
#' Ties only ever mean anything *within* one ANON record's candidates, so the
#' snapping is per group; comparing scores across ANON records would fuse
#' values that are never compared with each other.
#'
#' @param v numeric vector of scores, on the internally minimised scale
#' @param group ANON record identifier, same length as `v`
#' @param tolerance relative tolerance; 0 restores exact `==` comparison
#'
#' @return `v`, snapped within each group
#'
#' @keywords internal
snap_tied_values_by_group <- function(v, group, tolerance = reid_tie_tolerance()) {
  if (tolerance == 0 || length(v) < 2L) {
    return(v)
  }
  out <- v
  for (idx in split(seq_along(v), group)) {
    if (length(idx) > 1L) {
      out[idx] <- snap_tied_values(v[idx], tolerance)
    }
  }
  out
}

#' validate a tie tolerance
#'
#' @param tolerance the value to check
#' @param fn_name calling function, for the message
#'
#' @return `tolerance`, invisibly
#'
#' @keywords internal
validate_tie_tolerance <- function(tolerance, fn_name) {
  if (!is.numeric(tolerance) || length(tolerance) != 1L || is.na(tolerance) ||
        tolerance < 0 || is.infinite(tolerance)) {
    stop(fn_name, "(): `tolerance` must be a single finite non-negative ",
         "number (0 restores exact == comparison of scores).", call. = FALSE)
  }
  invisible(tolerance)
}

#' reject a score table that lists the same candidate pair more than once
#'
#' An attacker's candidate list is a **set**. If (ANON 1, RAW 2) appears twice,
#' RAW 2 is still one guess, and a uniform draw among the tied best candidates
#' must still give it probability `1 / (number of distinct candidates)`. Both
#' tie paths in this package count *rows* instead: `resolve_min_distance_ties()`
#' shuffles every row of the tie group, and `reid_per_anon()` computes
#' `sum(v == true_score)` over rows. A duplicated wrong candidate therefore
#' takes a share of the draw it is not entitled to, and the reported risk falls.
#'
#' The reason this needs an explicit guard rather than being caught downstream
#' is Issue #60: the analytic value and the simulated value are wrong **in the
#' same direction and by the same amount**, because they read the same inflated
#' multiset. The random baseline is `1 / N_CANDIDATES`, which is inflated
#' identically, so `lift` does not move either. The "would I notice if this
#' broke" cross-check that `docs/lessons-learned.md` section 2 asks for is
#' precisely what fails here, and it fails silently, downwards.
#'
#' `match_optimal()`, `combine_scores()` and `reid_result()` already refused
#' duplicated input. This is the same test, so that `match_greedy()` and
#' `reid_evaluate()` -- the two entry points that were still permissive -- hold
#' to the same contract.
#'
#' @param scores a score table, already through [validate_reid_scores()]
#' @param fn_name calling function, for the message
#'
#' @return `scores`, invisibly
#'
#' @keywords internal
validate_unique_candidate_pairs <- function(scores, fn_name) {
  key <- paste(scores$ANON_ROW_NUMBER, scores$RAW_ROW_NUMBER, sep = "\r")
  if (anyDuplicated(key) > 0) {
    dup <- duplicated(key)
    first <- which(dup)[1]
    stop(fn_name, "(): `scores` contains duplicated (ANON_ROW_NUMBER, ",
         "RAW_ROW_NUMBER) pairs; each candidate pair must appear exactly once. ",
         sum(dup), " repeated row(s); e.g. (ANON ",
         format(scores$ANON_ROW_NUMBER[first]), ", RAW ",
         format(scores$RAW_ROW_NUMBER[first]), ") appears ",
         sum(key == key[first]), " times. An attacker's candidates are a SET, ",
         "so a pair listed twice must not take twice the share of the ",
         "tie-break -- and when it does, the analytic rate, the simulated ",
         "rate and the random baseline are all wrong in the same direction, ",
         "so their agreement does not reveal it (Issue #60). Repeated row ",
         "numbers in `raw` or `anon` produce this: deduplicate before ",
         "join_raw_anon_data(). So do unioned candidate passes: drop the ",
         "pairs the passes share.",
         call. = FALSE)
  }
  invisible(scores)
}

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
#' candidate happened to sort first (Issue #3). Two scores count as tied when
#' they agree to within `tolerance` **relatively**, not only when they are
#' bit-identical; see `tolerance` below and `docs/default-changes.md`.
#'
#' By default `CONFIDENCE` is the **eccentricity** (`confidence = "margin"`):
#' how far ahead of the runner-up the winner is, in units of the spread of
#' that record's own candidate scores. `confidence = "tie"` gives the older
#' measure, `1 / (number of RAW records tied at the best score)` -- a
#' calibrated probability, but 1 for every record whose best candidate is
#' unique, which on a continuous score is nearly always. See
#' [reid_confidence()] for both, and **Changed defaults** below.
#'
#' @section Changed defaults:
#'
#' `confidence` defaulted to `"tie"` up to and including the release that
#' introduced it (Issue #16), and defaults to `"margin"` from Issue #44
#' onwards. The same call therefore reports **different CONFIDENCE values than
#' it used to**: `"tie"` values live in `(0, 1]` and are mostly exactly 1,
#' while `"margin"` values are non-negative, unbounded and almost all distinct.
#' Nothing else about the assignment changes -- the guessed
#' `RAW_ROW_NUMBER` and `RESULT` are untouched -- but any code comparing
#' `CONFIDENCE` against a literal, or `min_confidence` tuned against the old
#' scale, has to be revisited. Pass `confidence = "tie"` to get the old
#' numbers back. See `docs/default-changes.md`.
#'
#' @param scores a score table: a data frame with columns RAW_ROW_NUMBER,
#'   ANON_ROW_NUMBER and SCORE, normally produced by a `score_*()` function or
#'   by [combine_scores()]. Its `score_type` attribute decides whether the best
#'   score is the smallest ("distance", the default) or the largest
#'   ("similarity").
#' @param seed integer seed for the random tie-break (default 0L, so a plain
#'   call is reproducible). NULL uses the ambient RNG stream instead.
#' @param confidence how to fill the CONFIDENCE column: `"margin"` (default
#'   since Issue #44: eccentricity, a fine-grained ranking but not a
#'   probability and with **no scale that carries between data sets**) or
#'   `"tie"` (`1 / tie size`, a calibrated probability with almost no
#'   resolution). See [reid_confidence()].
#' @param min_confidence decline to guess for any ANON record whose
#'   CONFIDENCE falls below this (default 0, i.e. always guess). A declined
#'   record keeps its row but is reported with `RAW_ROW_NUMBER = NA` and
#'   `RESULT = FALSE`, so the trial count is unchanged and the reported rate
#'   cannot be inflated by simply attacking less.
#' @param tolerance relative tolerance for deciding that two candidate scores
#'   are tied, default `sqrt(.Machine$double.eps)` (Issue #61). Scores agreeing
#'   to about the first 8 significant digits are treated as indistinguishable,
#'   so re-expressing the same data in different units does not turn a tie into
#'   a strict ordering. Pass `tolerance = 0` for the exact `==` comparison used
#'   before #61; see `docs/default-changes.md`.
#'
#' @return a data frame with exactly one row per ANON record, ordered by
#'   ANON_ROW_NUMBER, with columns ANON_ROW_NUMBER, RAW_ROW_NUMBER,
#'   CONFIDENCE (numeric; in (0, 1] for `confidence = "tie"`, non-negative and
#'   possibly unbounded for `"margin"`) and RESULT (logical: whether the
#'   guessed RAW record is in fact the one the ANON record came from).
#'
#' @examples
#' raw <- data.frame(ROW_NUMBER = 1:5, V = c(10, 20, 30, 40, 50))
#' d <- join_raw_anon_data(raw, raw)
#' match_greedy(score_num(d, "V"))
#'
#' @export
match_greedy <- function(scores, seed = 0L, confidence = c("margin", "tie"),
                         min_confidence = 0,
                         tolerance = reid_tie_tolerance()) {
  confidence <- match.arg(confidence)
  score_type <- validate_reid_scores(scores, "scores")
  validate_unique_candidate_pairs(scores, "match_greedy")
  validate_tie_tolerance(tolerance, "match_greedy")

  ## Internally everything is minimised. A similarity is negated rather than
  ## inverted so the transformation is monotone and never divides by zero.
  distance <- if (identical(score_type, "similarity")) -scores$SCORE else scores$SCORE

  ## Issue #61: collapse near-equal candidate scores onto one value *before*
  ## any tie test, so both the "which rows are minimal" filter inside
  ## resolve_min_distance_ties() and the tie-size count below see the same,
  ## unit-invariant notion of "tied". DISTANCE is only used for those two
  ## comparisons, never reported, so snapping it changes no output value --
  ## only which candidates count as indistinguishable.
  distance <- snap_tied_values_by_group(distance, scores$ANON_ROW_NUMBER,
                                        tolerance)

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

  score_row <- picked$SCORE_ROW
  out <- apply_confidence(out, scores, confidence, min_confidence, tolerance)

  ## Which row of `scores` each winner came from. The reid_by_*() wrappers use
  ## this to recover the per-pair detail columns they have always reported;
  ## it is an implementation detail, not part of the documented return value.
  ## It still points at the argmin row even for a declined record, so the
  ## wrappers keep reporting the pair the attacker looked at.
  attr(out, "score_row") <- score_row

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
#' @param confidence how to fill the CONFIDENCE column: `"margin"` (default
#'   since Issue #44, eccentricity) or `"tie"` (`1 / tie size`). See
#'   [reid_confidence()] and the "Changed defaults" section of
#'   [match_greedy()]. Note that this is a
#'   know about the one-to-one constraint; a record the constraint pushed off
#'   its first choice still reports the confidence of that first choice under
#'   `"margin"`.
#' @param min_confidence decline to guess below this confidence (default 0).
#'   Applied on top of any declining the padding already did.
#' @param tolerance relative tolerance for tie detection in the reported
#'   CONFIDENCE, default `sqrt(.Machine$double.eps)` (Issue #61), so that
#'   `match_optimal()` and [match_greedy()] agree on what "tied" means. The
#'   **assignment** is unaffected: the solver minimises the raw costs, and
#'   perturbing them to make the report unit-invariant could change which
#'   assignment is chosen. Pass 0 for the exact comparison used before #61.
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
                          warn_size = 1000L, max_size = 5000L,
                          confidence = c("margin", "tie"),
                          min_confidence = 0,
                          tolerance = reid_tie_tolerance()) {
  confidence <- match.arg(confidence)
  score_type <- validate_reid_scores(scores, "scores")
  validate_tie_tolerance(tolerance, "match_optimal")

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

  validate_unique_candidate_pairs(scores, "match_optimal")

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
        solve_fn = solve_fn,
        tolerance = tolerance
      )
    }))
  })

  res <- res[order(res$ANON_ROW_NUMBER), , drop = FALSE]
  rownames(res) <- NULL

  ## A record the padding already declined keeps CONFIDENCE 0 whichever
  ## measure is asked for: it made no claim, so there is nothing to be
  ## confident about.
  declined_by_padding <- is.na(res$RAW_ROW_NUMBER)
  res <- apply_confidence(res, scores, confidence, min_confidence, tolerance)
  res$CONFIDENCE[declined_by_padding] <- 0

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
#' @param tolerance relative tie tolerance for the CONFIDENCE count only, see
#'   [match_optimal()]
#'
#' @return a data frame of ANON_ROW_NUMBER, RAW_ROW_NUMBER, CONFIDENCE, RESULT
#'
#' @keywords internal
match_optimal_one <- function(raw, anon, value, sampling_rate, dummy_cost,
                              solve_fn, tolerance = reid_tie_tolerance()) {
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
  ##
  ## The count is taken on tie-snapped costs (Issue #61) so that "at least as
  ## good as" means the same here as in match_greedy(): otherwise the same
  ## data in different units would give the two functions different
  ## CONFIDENCE for an identical assignment. The *assignment* above is still
  ## solved on the raw costs -- perturbing the solver's input to make a report
  ## unit-invariant could change which assignment is optimal, which is a much
  ## bigger claim than this fix is making.
  anon_idx <- match(anon, anon_shuffled)
  conf <- rep(0, n_anon)
  ok <- which(!declined)
  if (length(ok) > 0) {
    snapped <- snap_tied_values_by_group(cost, anon_idx, tolerance)
    ## picked_cost was read off the raw matrix, so it has to be mapped onto the
    ## snapped scale too, or a winner sitting a rounding error above its own
    ## group representative would count itself out.
    snapped_pick <- picked_cost
    by_raw <- split(cost, factor(anon_idx, levels = seq_len(n_anon)))
    by_anon <- split(snapped, factor(anon_idx, levels = seq_len(n_anon)))
    for (i in ok) {
      hit <- which(by_raw[[i]] == picked_cost[i])
      if (length(hit) > 0) {
        snapped_pick[i] <- by_anon[[i]][hit[1]]
      }
    }
    conf[ok] <- 1 / vapply(
      ok, function(i) sum(by_anon[[i]] <= snapped_pick[i]), numeric(1)
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
