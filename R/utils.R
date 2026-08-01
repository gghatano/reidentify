## ---------------------------------------------------------------------------
## Small helpers shared across the package.
##
## with_local_seed() used to live in R/reidentify.R next to the legacy
## reid_by_*() wrappers. Those wrappers were removed in 3.0.0, but the helper
## is used by six other files (blocking, create_dummy_data, match, setsim,
## spatiotemporal, unicity), so it moved here rather than being deleted with
## them.
## ---------------------------------------------------------------------------

#' evaluate `code` with the RNG seeded to `seed`, restoring the caller's
#' RNG state afterwards
#'
#' `seed = NULL` runs `code` against the ambient RNG stream unchanged, so a
#' caller can still get reproducibility with a plain `set.seed()` before the
#' call. Any other value makes the call self-contained and repeatable without
#' perturbing the caller's stream.
#'
#' @param seed integer seed, or NULL to use the ambient RNG stream
#' @param code expression to evaluate
#'
#' @return the value of `code`
#'
#' @keywords internal
with_local_seed <- function(seed, code) {
  if (is.null(seed)) {
    return(code)
  }

  has_old <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (has_old) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(
      suppressWarnings(rm(".Random.seed", envir = globalenv())),
      add = TRUE
    )
  }

  set.seed(seed)
  code
}
