## ---------------------------------------------------------------------------
## Issue #14 -- does multi-attribute integration actually help, and does
## Mahalanobis actually beat a plain weighted sum?
##
## Run with:  Rscript docs/investigation/multiattr-benchmark.R
##
## Two claims from the issue's "検証方法" are tested here, both by measurement
## rather than by argument (docs/lessons-learned.md section 1):
##
##   A. a multi-attribute attack beats every single-attribute attack
##   B. on data with correlated attributes, Mahalanobis beats the weighted sum
##
## Claim B is the one worth being careful about. Mahalanobis is *not*
## universally better: it is the right metric when the perturbation the
## anonymiser applies has roughly the same shape as the population spread. The
## scenarios below are deliberately chosen to include a case where it should
## help and a case where it should not, so that the conclusion says when to
## reach for it rather than "always".
## ---------------------------------------------------------------------------

suppressMessages(pkgload::load_all(".", quiet = TRUE))

SEEDS <- 1:20

success <- function(scores) reid_evaluate(scores, seeds = SEEDS)$success_analytic

## ---------------------------------------------------------------------------
## Scenario 1: REDUNDANT ATTRIBUTES, LATENT PERTURBATION
##
## A and B are two views of one latent quantity L (think height in cm and in
## inches, or "annual spend" and "monthly spend"). The anonymiser perturbs the
## latent quantity, so A and B move together. C is an independent attribute,
## perturbed on its own.
##
## A weighted sum counts A and B as two independent votes, so the redundant
## pair outvotes C two to one. Mahalanobis with the population covariance
## should collapse them back to one vote.
## ---------------------------------------------------------------------------
scenario_redundant <- function(n = 200, noise = 0.5, seed = 1) {
  set.seed(seed)
  latent <- rnorm(n)
  raw <- data.frame(
    ROW_NUMBER = seq_len(n),
    A = latent + rnorm(n, sd = 0.05),
    B = 3 * latent + rnorm(n, sd = 0.15),
    C = rnorm(n)
  )
  latent_shift <- rnorm(n, sd = noise)
  anon <- data.frame(
    ROW_NUMBER = seq_len(n),
    A = raw$A + latent_shift,
    B = raw$B + 3 * latent_shift,
    C = raw$C + rnorm(n, sd = noise)
  )
  join_raw_anon_data(raw, anon)
}

## ---------------------------------------------------------------------------
## Scenario 2: INDEPENDENT ATTRIBUTES, EQUAL-SCALE PERTURBATION
##
## The control. Nothing is correlated and every attribute is perturbed by the
## same isotropic noise, which is exactly the situation in which plain
## Euclidean distance is the optimal rule. Mahalanobis should NOT win here; if
## it did, the scenario 1 result would be measuring something else.
## ---------------------------------------------------------------------------
scenario_independent <- function(n = 200, noise = 0.5, seed = 1) {
  set.seed(seed)
  raw <- data.frame(
    ROW_NUMBER = seq_len(n),
    A = rnorm(n),
    B = rnorm(n),
    C = rnorm(n)
  )
  anon <- raw
  for (v in c("A", "B", "C")) anon[[v]] <- raw[[v]] + rnorm(n, sd = noise)
  join_raw_anon_data(raw, anon)
}

## ---------------------------------------------------------------------------
## Scenario 3: MISMATCHED UNITS
##
## The attributes are independent but measured on scales three orders of
## magnitude apart, and each is perturbed by the same *relative* amount. This
## is the case that motivates normalisation at all: without it the widest
## column decides everything on its own.
## ---------------------------------------------------------------------------
scenario_scale <- function(n = 200, noise = 0.3, seed = 1) {
  set.seed(seed)
  raw <- data.frame(
    ROW_NUMBER = seq_len(n),
    SMALL = rnorm(n, sd = 1),
    MEDIUM = rnorm(n, sd = 50),
    LARGE = rnorm(n, sd = 5000)
  )
  anon <- raw
  for (v in c("SMALL", "MEDIUM", "LARGE")) {
    anon[[v]] <- raw[[v]] + rnorm(n, sd = noise * sd(raw[[v]]))
  }
  join_raw_anon_data(raw, anon)
}

report <- function(label, j, cols) {
  cat("\n== ", label, " ==\n", sep = "")

  singles <- vapply(cols, function(v) success(score_num(j, v)), numeric(1))
  for (v in cols) cat(sprintf("  single  %-8s          : %.4f\n", v, singles[[v]]))
  cat(sprintf("  --- best single           : %.4f\n", max(singles)))

  spec <- setNames(rep("num", length(cols)), cols)
  variants <- list(
    "multi none"        = score_multi(j, spec, normalize = "none"),
    "multi range"       = score_multi(j, spec, normalize = "range"),
    "multi zscore"      = score_multi(j, spec, normalize = "zscore"),
    "multi rank"        = score_multi(j, spec, normalize = "rank"),
    "mahalanobis range" = score_multi(j, spec, normalize = "range", method = "mahalanobis"),
    "mahalanobis none"  = score_multi(j, spec, normalize = "none", method = "mahalanobis")
  )
  res <- vapply(variants, success, numeric(1))
  for (nm in names(res)) cat(sprintf("  %-24s : %.4f\n", nm, res[[nm]]))

  invisible(c(best_single = max(singles), res))
}

cat("Issue #14 multi-attribute benchmark\n")
cat("success_analytic (exact expected single-guess success rate), n = 200\n")

r1 <- report("scenario 1: redundant A/B + independent C, latent perturbation",
             scenario_redundant(), c("A", "B", "C"))
r2 <- report("scenario 2 (control): independent, isotropic perturbation",
             scenario_independent(), c("A", "B", "C"))
r3 <- report("scenario 3: independent, units 3 orders of magnitude apart",
             scenario_scale(), c("SMALL", "MEDIUM", "LARGE"))

## ---------------------------------------------------------------------------
## Scenario 1 across seeds: one draw is not evidence.
## ---------------------------------------------------------------------------
cat("\n== scenario 1 across 10 data seeds ==\n")
tab <- do.call(rbind, lapply(1:10, function(s) {
  j <- scenario_redundant(seed = s)
  spec <- c(A = "num", B = "num", C = "num")
  data.frame(
    seed = s,
    weighted = success(score_multi(j, spec, normalize = "range")),
    mahalanobis = success(score_multi(j, spec, normalize = "range", method = "mahalanobis"))
  )
}))
tab$delta <- tab$mahalanobis - tab$weighted
print(tab, row.names = FALSE)
cat(sprintf("\n  mean delta = %+.4f, mahalanobis wins in %d/10 seeds\n",
            mean(tab$delta), sum(tab$delta > 0)))

cat("\n== scenario 2 (control) across 10 data seeds ==\n")
tab2 <- do.call(rbind, lapply(1:10, function(s) {
  j <- scenario_independent(seed = s)
  spec <- c(A = "num", B = "num", C = "num")
  data.frame(
    seed = s,
    weighted = success(score_multi(j, spec, normalize = "range")),
    mahalanobis = success(score_multi(j, spec, normalize = "range", method = "mahalanobis"))
  )
}))
tab2$delta <- tab2$mahalanobis - tab2$weighted
print(tab2, row.names = FALSE)
cat(sprintf("\n  mean delta = %+.4f, mahalanobis wins in %d/10 seeds\n",
            mean(tab2$delta), sum(tab2$delta > 0)))

## ---------------------------------------------------------------------------
## The W / M / S curve, which #13 could only make work with the stopgap
## normalisation. Check that every normalisation choice keeps it monotone.
## ---------------------------------------------------------------------------
cat("\n== W/M/S knowledge curve under each normalisation ==\n")
generalize_qi <- function(x) {
  x$AGE <- (x$AGE %/% 10) * 10
  x$ZIP <- substr(x$ZIP, 1, 3)
  x$VISIT_COUNT <- (x$VISIT_COUNT %/% 5) * 5
  x$SPEND_MEAN <- round(x$SPEND_MEAN / 25) * 25
  x$SPEND_DIST <- vapply(
    strsplit(x$SPEND_DIST, ":", fixed = TRUE),
    function(v) paste(round(as.numeric(v) / 20) * 20, collapse = ":"),
    character(1)
  )
  x
}
d <- create_dummy_qi_data(people = 60, seed = 7)
j <- join_raw_anon_data(d, generalize_qi(d))
qi_args <- list(
  quasi_identifiers = c(ZIP = "char", AGE = "num", SEX = "char"),
  behavior = c(VISIT_COUNT = "num", SPEND_MEAN = "num", SPEND_DIST = "dist"),
  identifiers = c(FINGERPRINT = "num"),
  weak_subset = "ZIP"
)
for (nrm in c("none", "range", "zscore", "rank")) {
  curve <- do.call(reid_knowledge_curve,
                   c(list(j, seeds = 1:20, normalize = nrm), qi_args))
  cat(sprintf("  normalize = %-7s : W %.4f  M %.4f  S %.4f   %s\n",
              nrm, curve$success_analytic[1], curve$success_analytic[2],
              curve$success_analytic[3],
              if (all(diff(curve$success_analytic) > 0)) "strictly increasing"
              else "NOT strictly increasing"))
}
