## Issue #20 verification: does containment matching narrow the candidate set
## correctly on generalised data, and does it work for a weak attacker?
##
## Run with:
##   Rscript docs/investigation/generalization-benchmark.R
##
## The issue's acceptance criterion is "on synthetic generalised data, the
## containment test narrows the candidates correctly". "Correctly" has two
## halves, and both are checked here:
##
##   1. the true counterpart of every published record survives its own
##      published region (TRUTH_CONTAINED) -- if it does not, the release and
##      the raw file disagree and every number below is meaningless;
##   2. the surviving candidate count k matches the number of raw records that
##      actually fall in that region, computed independently.
##
## It then measures what the existing scores do on the same data, because the
## headline claim of #20 is that they cannot be used at all.

suppressWarnings(suppressMessages(pkgload::load_all(".", quiet = TRUE)))

set.seed(20260730)

hier <- read_generalization_hierarchy(
  file.path("inst", "extdata", "generalization-jp.csv")
)

cat("R:", R.version.string, "\n")
cat("date:", format(Sys.time()), "\n\n")
print(hier)

## ---------------------------------------------------------------------------
## fixture: a raw file, and the generalised release built from it with the
## declared hierarchy. Because the release is *generated* by generalize_value()
## the ground truth is exact -- we know which raw record each published record
## came from and at which level it was coarsened.
## ---------------------------------------------------------------------------
area_leaves <- local({
  e <- hier$edges
  e$value[e$attribute == "AREA" & e$level == 1L]
})

make_release <- function(people, age_levels, area_levels) {
  raw <- data.frame(
    ROW_NUMBER = seq_len(people),
    AGE = sample(20:79, people, replace = TRUE),
    AREA = sample(area_leaves, people, replace = TRUE),
    SEX = sample(c("M", "F"), people, replace = TRUE),
    stringsAsFactors = FALSE
  )
  anon <- data.frame(
    ROW_NUMBER = raw$ROW_NUMBER,
    AGE = generalize_value(raw$AGE, "AGE", hier, levels = age_levels),
    AREA = generalize_value(raw$AREA, "AREA", hier, levels = area_levels),
    SEX = raw$SEX,
    stringsAsFactors = FALSE
  )
  list(raw = raw, anon = anon)
}

## ---------------------------------------------------------------------------
## 1. is the narrowing correct?
## ---------------------------------------------------------------------------
cat("\n== narrowing, checked against an independent count ==\n")
fx <- make_release(200, age_levels = 1, area_levels = 1)
d <- join_raw_anon_data(fx$raw, fx$anon)
cc <- containment_counts(d, c("AGE", "AREA", "SEX"), hierarchy = hier)

## independent recomputation: for each published record, count the raw records
## whose *generalised* value equals the published one, attribute by attribute.
raw_gen <- data.frame(
  AGE = generalize_value(fx$raw$AGE, "AGE", hier, levels = 1),
  AREA = generalize_value(fx$raw$AREA, "AREA", hier, levels = 1),
  SEX = fx$raw$SEX,
  stringsAsFactors = FALSE
)
independent <- vapply(seq_len(nrow(fx$anon)), function(i) {
  sum(raw_gen$AGE == fx$anon$AGE[i] &
        raw_gen$AREA == fx$anon$AREA[i] &
        raw_gen$SEX == fx$anon$SEX[i])
}, numeric(1))

cat("  every record's own counterpart survives: ", all(cc$TRUTH_CONTAINED), "\n")
cat("  k agrees with the independent count:     ",
    identical(as.numeric(cc$N_CONTAINED[order(cc$ANON_ROW_NUMBER)]),
              as.numeric(independent)), "\n")
cat(sprintf("  candidates offered %d -> surviving median %g, mean %.2f, max %g\n",
            cc$N_CANDIDATES[1], stats::median(cc$N_CONTAINED),
            mean(cc$N_CONTAINED), max(cc$N_CONTAINED)))
cat(sprintf("  uniquely identified: %.1f%% of records (k = 1)\n",
            100 * mean(cc$N_CONTAINED == 1)))

## ---------------------------------------------------------------------------
## 2. the weak attacker: one attribute at a time
## ---------------------------------------------------------------------------
cat("\n== narrowing per attacker knowledge subset (200 people) ==\n")
subsets <- list("SEX", "AGE", "AREA", c("AGE", "SEX"), c("AGE", "AREA"),
                c("AGE", "AREA", "SEX"))
for (s in subsets) {
  cc <- containment_counts(d, s, hierarchy = hier)
  ev <- reid_evaluate(score_containment(d, s, hierarchy = hier),
                      seeds = 1:10, top_k = 1)
  cat(sprintf("  %-22s median k = %5.1f   unique = %5.1f%%   success = %.4f (random %.4f)\n",
              paste(s, collapse = "+"), stats::median(cc$N_CONTAINED),
              100 * mean(cc$N_CONTAINED == 1), ev$success_analytic,
              ev$baseline$rate[ev$baseline$method == "random"]))
}

## ---------------------------------------------------------------------------
## 3. coarser generalisation must narrow less
## ---------------------------------------------------------------------------
cat("\n== effect of the generalisation level ==\n")
cat("  AGE  level 0 = 5-year bins, 1 = 10-year bins (2 = same, no coarser node)\n")
cat("  AREA level 0 = municipality, 1 = prefecture, 2 = region\n")
for (lv in list(c(0, 0), c(1, 0), c(1, 1), c(2, 2))) {
  fx2 <- make_release(200, age_levels = lv[1], area_levels = lv[2])
  d2 <- join_raw_anon_data(fx2$raw, fx2$anon)
  cc2 <- containment_counts(d2, c("AGE", "AREA", "SEX"), hierarchy = hier)
  ev2 <- reid_evaluate(score_containment(d2, c("AGE", "AREA", "SEX"),
                                         hierarchy = hier),
                       seeds = 1:10, top_k = 1)
  cat(sprintf("  AGE level %d, AREA level %d:  median k = %5.1f  unique = %5.1f%%  success = %.4f  truth kept = %s\n",
              lv[1], lv[2], stats::median(cc2$N_CONTAINED),
              100 * mean(cc2$N_CONTAINED == 1), ev2$success_analytic,
              all(cc2$TRUTH_CONTAINED)))
}

## ---------------------------------------------------------------------------
## 4. what the existing scores do on the same generalised column
## ---------------------------------------------------------------------------
cat("\n== existing scores on the generalised AGE column ==\n")
fx3 <- make_release(200, age_levels = 1, area_levels = 1)
d3 <- join_raw_anon_data(fx3$raw, fx3$anon)

show <- function(label, expr) {
  res <- tryCatch({
    ev <- reid_evaluate(expr, seeds = 1:10, top_k = 1)
    sprintf("success = %.4f", ev$success_analytic)
  }, error = function(e) paste0("ERROR: ", sub("\n.*", " ...", conditionMessage(e))))
  cat(sprintf("  %-24s %s\n", label, res))
}

show("score_num(AGE)", suppressWarnings(score_num(d3, "AGE")))
show("score_char(AGE)", score_char(d3, "AGE"))
show("score_containment(AGE)", score_containment(d3, "AGE", hierarchy = hier))

cat("\n== all three attributes ==\n")
show("score_char x3 (combined)",
     combine_scores(lapply(c("AGE", "AREA", "SEX"), function(t) score_char(d3, t))))
show("score_containment x3",
     score_containment(d3, c("AGE", "AREA", "SEX"), hierarchy = hier))

cat("\n  score_num() on the generalised column stops with:\n   ",
    tryCatch(score_num(d3, "AGE"), error = function(e) conditionMessage(e)), "\n")

## ---------------------------------------------------------------------------
## 4b. Issue #40: score_char() used to run here. It does not any more.
##
## The two lines above are the record of the defect: score_char() returned edit
## distances between a raw number and a bracket string, which look like scores
## and mean nothing. The numbers below are what that misuse reported, and are
## now reachable only by asking for them with generalized = "ignore".
## ---------------------------------------------------------------------------
cat("\n  score_char() now stops with:\n   ",
    tryCatch(score_char(d3, "AGE"), error = function(e) conditionMessage(e)), "\n")
cat("\n  what it used to return silently (generalized = \"ignore\"):\n")
i <- which(d3$RAW_ROW_NUMBER == d3$ANON_ROW_NUMBER)[1:3]
cat(sprintf("    RAW %-4s vs ANON %-10s -> score_char = %g\n",
            d3$RAW_AGE[i], d3$ANON_AGE[i],
            score_char(d3, "AGE", generalized = "ignore")$SCORE[i]),
    sep = "")
show("score_char x3, generalized='ignore'",
     combine_scores(lapply(c("AGE", "AREA", "SEX"), function(t) {
       score_char(d3, t, generalized = "ignore")
     })))

cat("\n  NOTE: AREA is generalised *categorically* (chiyoda -> tokyo). No\n",
    "  structural test can see that -- nothing about the string \"tokyo\" says\n",
    "  it contains \"chiyoda\" -- so score_char(AREA) is NOT stopped:\n")
show("score_char(AREA)", score_char(d3, "AREA"))
cat("  score_num_rank(AREA) is stopped, but by the type check, not the\n",
    "  generalisation check:\n   ",
    tryCatch(score_num_rank(d3, "AREA"), error = function(e) conditionMessage(e)),
    "\n")

## ---------------------------------------------------------------------------
## 5. a release the raw data does not agree with must be visible
## ---------------------------------------------------------------------------
cat("\n== a mis-declared release is reported, not hidden ==\n")
broken <- fx3
## the release says everybody is in their twenties, which is false
broken$anon$AGE <- "[20,30)"
db <- join_raw_anon_data(broken$raw, broken$anon)
ccb <- containment_counts(db, c("AGE", "AREA", "SEX"), hierarchy = hier)
cat(sprintf("  TRUTH_CONTAINED holds for %.1f%% of records (should be 100%%)\n",
            100 * mean(ccb$TRUTH_CONTAINED)))
cat(sprintf("  records with no surviving candidate at all: %d\n",
            sum(ccb$N_CONTAINED == 0)))
evb <- reid_evaluate(score_containment(db, c("AGE", "AREA", "SEX"), hierarchy = hier),
                     seeds = 1:10, top_k = 1)
cat(sprintf("  trials still counted: %d of %d anon records\n",
            evb$n_anon, nrow(broken$anon)))
