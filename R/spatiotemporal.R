## ---------------------------------------------------------------------------
## Spatio-temporal unicity (Issue #24)
##
## de Montjoye et al., "Unique in the Crowd", measures something the rest of
## this package cannot: not how identifying a person's *attributes* are, but
## how identifying a handful of points of their *trace* is. Four
## approximate (place, time) points -- the level of detail a few social media
## posts or dated receipts leak -- pinned down 95% of the individuals in a
## fifteen-month mobility data set.
##
## unicity() (Issue #21) cannot express this. It asks whether a record is
## unique on a *fixed set of columns*, one row per individual. Here every
## individual carries a variable-length set of points, the attacker knows an
## arbitrary k of them, and the question is whether that subset narrows the
## population to one. The sweep is over k and over how coarsely place and time
## are recorded, because coarsening is the defence being evaluated.
##
## WHAT THIS BOUNDS. Issue #21 established by measurement that unicity is a
## *lower* bound on attack success, not an upper one: an individual sharing
## their points with m - 1 others counts as 0 here, but an attacker picking
## among the m still wins with probability 1/m. That correction is reported
## alongside, as `expected_id_rate` -- the success rate of an attacker who
## guesses uniformly inside the anonymity set. `unicity <= expected_id_rate`
## always, and the gap is exactly what a unicity-only report would hide.
##
## COARSENING IS THE THING BEING MEASURED, so it has to be honest about its
## assumptions. Time is numeric and coarsens onto an absolute grid.
## Locations are codes, and merging them requires knowing which are adjacent;
## coarsen_place() takes sort order as the adjacency, which is right for mesh
## codes, geohashes and zero-padded antenna ids, and wrong for arbitrary
## labels. That assumption is stated rather than hidden, because a coarsening
## that merged unrelated locations would report a lower unicity than the data
## deserves -- the under-reporting direction of docs/lessons-learned.md
## section 2.
## ---------------------------------------------------------------------------

#' put timestamps onto a coarser grid
#'
#' @param x numeric timestamps, or anything [as.numeric()] turns into them
#'   (a POSIXct becomes seconds since the epoch, so `resolution` is then in
#'   seconds)
#' @param resolution width of one bin, in the units of `x`. `1` leaves `x`
#'   alone up to flooring; `24` on hourly data gives days.
#'
#' @return an integer-valued numeric vector of bin indices
#'
#' @examples
#' coarsen_time(c(0, 1, 5, 23, 24, 25), resolution = 12)
#'
#' @export
coarsen_time <- function(x, resolution = 1) {
  if (!is.numeric(resolution) || length(resolution) != 1 || is.na(resolution) ||
      resolution <= 0) {
    stop("`resolution` must be a single positive number.", call. = FALSE)
  }
  v <- suppressWarnings(as.numeric(x))
  if (anyNA(v) && !anyNA(x)) {
    stop("`x` could not be read as numeric time; convert it first (a POSIXct ",
         "works, an arbitrary character label does not).", call. = FALSE)
  }
  floor(v / resolution)
}

#' merge neighbouring location codes into a coarser grid
#'
#' Distinct values of `x` are put in sorted order and then grouped in blocks of
#' `resolution`, so `resolution = 1` keeps every location separate and
#' `resolution = 4` merges each run of four neighbouring codes into one cell.
#'
#' SORT ORDER IS TAKEN AS ADJACENCY. There is no geometry here, only labels, so
#' "next to" has to mean "next in sort order". That is correct for codes built
#' to have that property -- mesh codes, geohashes, zero-padded antenna or shop
#' ids -- and wrong for arbitrary names, where it would merge unrelated
#' locations and report a *lower* unicity than the data deserves. Pass an
#' already-coarsened column with `resolution = 1` if your locations do not sort
#' geographically.
#'
#' Grouping is relative to the values actually present, not to an absolute
#' grid, so two data sets with different location sets are not directly
#' comparable at the same `resolution`.
#'
#' @param x location codes: any vector that can be sorted
#' @param resolution number of neighbouring locations merged into one cell
#'
#' @return an integer vector of cell indices, one per element of `x`
#'
#' @examples
#' coarsen_place(c("P001", "P002", "P003", "P004"), resolution = 2)
#'
#' @export
coarsen_place <- function(x, resolution = 1) {
  if (!is.numeric(resolution) || length(resolution) != 1 || is.na(resolution) ||
      resolution < 1) {
    stop("`resolution` must be a single number >= 1.", call. = FALSE)
  }
  resolution <- as.integer(resolution)
  levels_x <- sort(unique(x))
  idx <- match(x, levels_x)
  ((idx - 1L) %/% resolution) + 1L
}

#' how many people are pinned down by k points of their trace
#'
#' The measurement from de Montjoye et al.'s *Unique in the Crowd*: give an
#' attacker `k` (place, time) points drawn from somebody's trace and ask how
#' often that narrows the population to a single individual. Sweeping `k` and
#' the recording resolution says how much coarsening it takes to make a
#' mobility-style data set safe -- and, in the original paper, the answer was
#' "more than you would think": four points identified 95% of people.
#'
#' @section What it bounds:
#'
#' `unicity` is the fraction pinned down with *certainty*, so it is a lower
#' bound on what an attack achieves, not an upper one (this was established by
#' measurement in Issue #21). An individual sharing their `k` points with
#' `m - 1` others counts 0 towards `unicity`, but an attacker guessing inside
#' that anonymity set still wins with probability `1/m`.
#' `expected_id_rate` reports exactly that, and is always at least `unicity`.
#' Read the pair, not `unicity` alone.
#'
#' @section Individuals with short traces:
#'
#' An attacker cannot hold `k` distinct points of somebody who only ever
#' visited `k - 1`, so at each `k` the measurement covers only the individuals
#' with at least `k` distinct points. `n_evaluated` reports how many that was:
#' when it falls well below `n_individuals` the row describes the frequent
#' visitors rather than the population, and those are the people a trace-based
#' attack works on anyway.
#'
#' @param dat a transaction-shaped data frame: one row per event, several rows
#'   per individual
#' @param id name of the column identifying the individual
#' @param place name of the location column
#' @param time name of the timestamp column
#' @param k number of known points to evaluate (default `1:4`, the range the
#'   original paper reports)
#' @param time_resolution numeric vector of time bin widths, in the units of
#'   `time`; every combination with `space_resolution` is evaluated
#' @param space_resolution numeric vector of location-merging factors; see
#'   [coarsen_place()]
#' @param n_samples maximum number of point subsets drawn per individual
#'   (default 20). Individuals for whom `choose(trace, k)` is no larger than
#'   this are enumerated exhaustively instead, which makes them exact.
#' @param seed integer seed for the subset sampling (default 0L, so a plain
#'   call is reproducible); NULL uses the ambient RNG stream
#'
#' @return a data frame with one row per `(k, time_resolution,
#'   space_resolution)` combination and columns
#'   \describe{
#'     \item{k, time_resolution, space_resolution}{the setting}
#'     \item{n_individuals}{individuals in `dat`}
#'     \item{n_evaluated}{those with at least `k` distinct points}
#'     \item{n_points}{distinct (place, time) points at this resolution}
#'     \item{exhaustive}{TRUE when every evaluated individual had all their
#'       point subsets enumerated, so the row is exact rather than sampled}
#'     \item{unicity}{fraction pinned down to exactly one individual}
#'     \item{expected_id_rate}{mean of `1 / anonymity set size` -- what an
#'       attacker guessing inside the set achieves}
#'     \item{mean_anonymity_set}{mean number of individuals matching the `k`
#'       points}
#'   }
#'
#' @examples
#' tran <- create_dummy_transaction_data(
#'   people = 40, size = 20, spatiotemporal = TRUE, seed = 1
#' )
#' spatiotemporal_unicity(tran, k = c(1, 2, 4), time_resolution = c(1, 24))
#'
#' @seealso [unicity()] for the fixed-attribute form, and [coarsen_place()] for
#'   what `space_resolution` assumes about location codes.
#'
#' @importFrom utils combn
#' @export
spatiotemporal_unicity <- function(dat, id = "ID", place = "PLACE",
                                   time = "TIME", k = 1:4,
                                   time_resolution = 1, space_resolution = 1,
                                   n_samples = 20, seed = 0L) {
  if (!is.data.frame(dat)) {
    stop("`dat` must be a data frame with one row per event.", call. = FALSE)
  }
  missing_cols <- setdiff(c(id, place, time), names(dat))
  if (length(missing_cols) > 0) {
    stop("column(s) not found in `dat`: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  if (nrow(dat) == 0) {
    stop("`dat` has no rows; there is no trace to measure.", call. = FALSE)
  }

  ## A missing place or time would be flattened into the literal point "NA",
  ## which every other row missing that field would also match. The anonymity
  ## sets would grow, the reported unicity would fall, and the data would look
  ## safer than it is -- the under-reporting direction of
  ## docs/lessons-learned.md section 2. Refuse rather than guess.
  for (cn in c(id, place, time)) {
    if (anyNA(dat[[cn]])) {
      stop("column \"", cn, "\" contains NA. A missing coordinate would match ",
           "every other missing one and report a lower unicity -- i.e. a safer ",
           "data set -- than the data supports. Drop or impute those rows ",
           "first.", call. = FALSE)
    }
  }

  k <- sort(unique(as.integer(k)))
  if (anyNA(k) || any(k < 1)) {
    stop("`k` must be positive whole numbers: the attacker holds at least one ",
         "point.", call. = FALSE)
  }
  check_resolution <- function(v, nm) {
    if (!is.numeric(v) || length(v) == 0 || anyNA(v) || any(v <= 0)) {
      stop("`", nm, "` must be a non-empty vector of positive numbers.",
           call. = FALSE)
    }
  }
  check_resolution(time_resolution, "time_resolution")
  check_resolution(space_resolution, "space_resolution")
  if (!is.numeric(n_samples) || length(n_samples) != 1 || is.na(n_samples) ||
      n_samples < 1) {
    stop("`n_samples` must be a single number >= 1.", call. = FALSE)
  }
  n_samples <- as.integer(n_samples)

  id_values <- as.character(dat[[id]])
  id_levels <- sort(unique(id_values))
  id_code <- match(id_values, id_levels)
  n_individuals <- length(id_levels)

  grid <- expand.grid(
    k = k,
    time_resolution = sort(unique(as.numeric(time_resolution))),
    space_resolution = sort(unique(as.numeric(space_resolution))),
    KEEP.OUT.ATTRS = FALSE
  )

  rows <- with_local_seed(seed, {
    ## The coarsening depends only on the two resolutions, so it is done once
    ## per resolution pair rather than once per grid row.
    lapply(split(grid, list(grid$time_resolution, grid$space_resolution),
                 drop = TRUE), function(block) {
      tr <- block$time_resolution[1]
      sr <- block$space_resolution[1]

      point <- paste(
        coarsen_place(dat[[place]], sr),
        coarsen_time(dat[[time]], tr),
        sep = "\r"
      )
      point_levels <- unique(point)
      point_code <- match(point, point_levels)
      n_points <- length(point_levels)

      ## point -> the individuals seen there, and individual -> their distinct
      ## points. Both as integer codes, so the subset intersection below is a
      ## handful of integer operations rather than a scan over the table.
      by_point <- lapply(
        split(id_code, factor(point_code, levels = seq_len(n_points))),
        unique
      )
      traces <- lapply(
        split(point_code, factor(id_code, levels = seq_len(n_individuals))),
        unique
      )

      do.call(rbind, lapply(block$k, function(this_k) {
        one_k_unicity(traces, by_point, this_k, n_samples, n_individuals,
                      n_points, tr, sr)
      }))
    })
  })

  out <- do.call(rbind, rows)
  out <- out[order(out$space_resolution, out$time_resolution, out$k), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' unicity at one k and one resolution pair
#'
#' @param traces list of distinct point codes per individual
#' @param by_point list of individual codes per point
#' @param this_k number of known points
#' @param n_samples maximum subsets drawn per individual
#' @param n_individuals number of individuals
#' @param n_points number of distinct points
#' @param tr,sr the resolutions, copied into the result row
#'
#' @return a one-row data frame
#'
#' @keywords internal
#'
#' @importFrom utils combn
one_k_unicity <- function(traces, by_point, this_k, n_samples, n_individuals,
                          n_points, tr, sr) {
  unique_hits <- 0
  expected <- 0
  set_total <- 0
  n_evaluated <- 0L
  exhaustive <- TRUE

  for (i in seq_len(n_individuals)) {
    trace <- traces[[i]]
    n_trace <- length(trace)
    ## An attacker cannot hold k distinct points of somebody who never visited
    ## k. Counting them as "not unique" would credit the data with a safety it
    ## does not have; they are excluded and reported as excluded.
    if (n_trace < this_k) {
      next
    }
    n_evaluated <- n_evaluated + 1L

    if (choose(n_trace, this_k) <= n_samples) {
      subsets <- as.list(as.data.frame(utils::combn(n_trace, this_k)))
    } else {
      subsets <- lapply(seq_len(n_samples),
                        function(d) sort(sample.int(n_trace, this_k)))
      ## Distinct subsets only, and sorted so that two draws of the same set in
      ## different orders count as one. This is the convention unicity()
      ## already follows; a repeated draw would silently weight it twice.
      subsets <- unique(subsets)
      exhaustive <- FALSE
    }

    sizes <- vapply(subsets, function(sel) {
      codes <- trace[sel]
      ## Everyone who was at all k of these points. The individual themself is
      ## always in here, so the intersection is never empty and 1/size is
      ## always defined.
      matched <- by_point[[codes[1]]]
      for (cc in codes[-1]) {
        matched <- intersect(matched, by_point[[cc]])
        if (length(matched) == 1L) {
          break
        }
      }
      length(matched)
    }, numeric(1))

    unique_hits <- unique_hits + mean(sizes == 1)
    expected <- expected + mean(1 / sizes)
    set_total <- set_total + mean(sizes)
  }

  data.frame(
    k = this_k,
    time_resolution = tr,
    space_resolution = sr,
    n_individuals = n_individuals,
    n_evaluated = n_evaluated,
    n_points = n_points,
    ## With nobody to evaluate there is no answer to be exact about, so
    ## `exhaustive` says NA rather than claiming an exhaustive TRUE.
    exhaustive = if (n_evaluated > 0) exhaustive else NA,
    unicity = if (n_evaluated > 0) unique_hits / n_evaluated else NA_real_,
    expected_id_rate = if (n_evaluated > 0) expected / n_evaluated else NA_real_,
    mean_anonymity_set = if (n_evaluated > 0) set_total / n_evaluated else NA_real_,
    stringsAsFactors = FALSE
  )
}
