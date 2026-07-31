## ---------------------------------------------------------------------------
## Adversarial probe 6: LOCAL generalisation slips under the detector.
##
## Issue #40 measured GLOBAL generalisation (100% of published AGE values are
## regions) and score_char() now stops on it. But the detector needs
## GENERALIZATION_SHARE_THRESHOLD = 0.2 of the published values to be
## region-shaped.
##
## Local generalisation / local suppression -- generalise only the records that
## sit in small equivalence classes, leave the rest raw -- is the standard
## practical technique and typically touches 5-20% of records. Below the
## threshold score_char()/score_num_rank() proceed silently.
## ---------------------------------------------------------------------------
suppressMessages(pkgload::load_all(".", quiet = TRUE))

set.seed(20260731)
n <- 200

## RAW: age + area, one row per person
raw <- data.frame(
  ROW_NUMBER = 1:n,
  AGE = sample(20:69, n, replace = TRUE),
  stringsAsFactors = FALSE
)

## generalisation hierarchy: 10-year age bands
h <- generalization_hierarchy(data.frame(
  attribute = "AGE",
  value = as.character(20:69),
  parent = paste0("[", (20:69) %/% 10 * 10, ",", (20:69) %/% 10 * 10 + 10, ")"),
  stringsAsFactors = FALSE
))

rate <- function(s) reid_evaluate(s, seeds = 1:20)$success_analytic

probe <- function(share) {
  anon <- raw
  ## the `share` rarest ages are the ones a local scheme would generalise
  k <- round(share * n)
  idx <- if (k > 0) sample.int(n, k) else integer(0)
  anon$AGE <- as.character(anon$AGE)
  if (k > 0) {
    anon$AGE[idx] <- generalize_value(raw$AGE[idx], "AGE", h, levels = 1)
  }
  p <- join_raw_anon_data(raw, anon)

  warns <- character(0)
  ch <- tryCatch(
    withCallingHandlers(
      rate(score_char(p, "AGE")),
      warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) NA_real_
  )
  stopped <- is.na(ch)
  ct <- rate(score_containment(p, "AGE", hierarchy = h))

  cat(sprintf(
    "  share %5.1f%%  score_char %s  score_containment %.4f  %s\n",
    100 * share,
    if (stopped) "  STOPPED " else sprintf("%9.4f", ch),
    ct,
    if (stopped) "(detector fired)"
    else if (length(warns)) "(warned)"
    else sprintf("(SILENT; under-reports %.2fx)", ct / max(ch, 1e-9))
  ))
  invisible(NULL)
}

cat("== score_char() vs score_containment() as the generalised share grows ==\n")
for (s in c(0.00, 0.05, 0.10, 0.15, 0.19, 0.20, 0.25, 0.50, 1.00)) probe(s)

cat("\n== the same, for score_num_rank() (also documented as protected) ==\n")
probe_rank <- function(share) {
  anon <- raw
  k <- round(share * n)
  idx <- if (k > 0) sample.int(n, k) else integer(0)
  anon$AGE <- as.character(anon$AGE)
  if (k > 0) anon$AGE[idx] <- generalize_value(raw$AGE[idx], "AGE", h, levels = 1)
  p <- join_raw_anon_data(raw, anon)
  r <- tryCatch(rate(score_num_rank(p, "AGE")), error = function(e) NA_real_)
  cat(sprintf("  share %5.1f%%  score_num_rank %s\n", 100 * share,
              if (is.na(r)) "STOPPED" else sprintf("%.4f", r)))
}
for (s in c(0.10, 0.15, 0.30)) probe_rank(s)

cat("\n== full printed evaluation at share = 15% (nothing looks wrong) ==\n")
set.seed(20260731); invisible(sample.int(n, 30))
anon <- raw
idx <- sample.int(n, 30)
anon$AGE <- as.character(anon$AGE)
anon$AGE[idx] <- generalize_value(raw$AGE[idx], "AGE", h, levels = 1)
p <- join_raw_anon_data(raw, anon)
cat("--- score_char (what a user gets with no error) ---\n")
print(reid_evaluate(score_char(p, "AGE"), seeds = 1:20))
cat("\n--- score_containment (the correct method) ---\n")
print(reid_evaluate(score_containment(p, "AGE", hierarchy = h), seeds = 1:20))
