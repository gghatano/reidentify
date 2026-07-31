## Numbers shown on the public page (site/index.html), Issue #48.
##
## The page is read by people who will not run R, so every figure on it has to
## be traceable to a script that anyone can re-run. This is that script: one
## fixture, one seed, every number the page prints.
##
## Run with:
##   Rscript docs/investigation/site-figures.R
##
## It deliberately re-measures rather than quoting the other benchmark logs,
## because those report several fixtures drawn from the same seeded stream and
## mixing their numbers in one figure would be wrong even though each number
## is individually correct.

suppressWarnings(suppressMessages(pkgload::load_all(".", quiet = TRUE)))

cat("R:", R.version.string, "\n")
cat("date:", format(Sys.time()), "\n\n")

hier <- read_generalization_hierarchy(
  file.path("inst", "extdata", "generalization-jp.csv")
)

area_leaves <- local({
  e <- hier$edges
  e$value[e$attribute == "AREA" & e$level == 1L]
})

## ---------------------------------------------------------------------------
## fixture: 200 people, published with AGE in 10-year bins and AREA coarsened
## from municipality to prefecture -- an ordinary, rule-following release.
## ---------------------------------------------------------------------------
set.seed(20260731)
raw <- data.frame(
  ROW_NUMBER = 1:200,
  AGE  = sample(20:79, 200, replace = TRUE),
  AREA = sample(area_leaves, 200, replace = TRUE),
  SEX  = sample(c("M", "F"), 200, replace = TRUE),
  stringsAsFactors = FALSE
)
anon <- data.frame(
  ROW_NUMBER = raw$ROW_NUMBER,
  AGE  = generalize_value(raw$AGE, "AGE", hier, levels = 1),
  AREA = generalize_value(raw$AREA, "AREA", hier, levels = 1),
  SEX  = raw$SEX,
  stringsAsFactors = FALSE
)
d <- join_raw_anon_data(raw, anon)
targets <- c("AGE", "AREA", "SEX")

cc <- containment_counts(d, targets, hierarchy = hier)
stopifnot(all(cc$TRUTH_CONTAINED))   # the release and the raw file agree

cat("== figure 1: a rule-following generalisation still identifies people ==\n")
cat(sprintf("  candidates offered            : %d\n", cc$N_CANDIDATES[1]))
cat(sprintf("  surviving candidates          : median %g, mean %.2f, max %g\n",
            stats::median(cc$N_CONTAINED), mean(cc$N_CONTAINED),
            max(cc$N_CONTAINED)))
cat(sprintf("  records narrowed to 1 person  : %.1f%%\n",
            100 * mean(cc$N_CONTAINED == 1)))
cat(sprintf("  records narrowed to <= 3      : %.1f%%\n",
            100 * mean(cc$N_CONTAINED <= 3)))
cat(sprintf("  records narrowed to <= 5      : %.1f%%\n",
            100 * mean(cc$N_CONTAINED <= 5)))

right <- reid_evaluate(score_containment(d, targets, hierarchy = hier),
                       seeds = 1:10, top_k = 1)
base_rate <- right$baseline$rate[right$baseline$method == "random"]
cat(sprintf("  identified (correct method)   : %.4f\n", right$success_analytic))
cat(sprintf("  random-guess baseline         : %.4f  (lift %.0fx)\n",
            base_rate, right$success_analytic / base_rate))

## ---------------------------------------------------------------------------
## the same data, measured with the wrong tool: string distance between a raw
## number and a bracket string. This is what the package used to do silently
## before Issue #40; it is now reachable only by asking for it explicitly.
## ---------------------------------------------------------------------------
cat("\n== figure 2: the same data, measured the wrong way ==\n")
wrong <- reid_evaluate(
  combine_scores(lapply(targets, function(t)
    score_char(d, t, generalized = "ignore"))),
  seeds = 1:10, top_k = 1
)
cat(sprintf("  identified (wrong method)     : %.4f\n", wrong$success_analytic))
cat(sprintf("  fraction of the real figure   : %.2f\n",
            wrong$success_analytic / right$success_analytic))

## ---------------------------------------------------------------------------
## set-valued columns: the same release measured two ways (Issue #18).
## ---------------------------------------------------------------------------
cat("\n== figure 3: set-valued data, measured two ways ==\n")
set.seed(20260731)
pop <- 1 / seq_len(500)^1.1
items_raw <- lapply(1:200, function(i)
  sort(unique(sample.int(500, 8, replace = FALSE, prob = pop))))
items_anon <- lapply(items_raw, function(v) sort(sample(v, 4)))
s_raw <- data.frame(ROW_NUMBER = 1:200,
                    ITEMS = vapply(items_raw, paste, character(1), collapse = ":"),
                    stringsAsFactors = FALSE)
s_anon <- data.frame(ROW_NUMBER = 1:200,
                     ITEMS = vapply(items_anon, paste, character(1), collapse = ":"),
                     stringsAsFactors = FALSE)
sd_ <- join_raw_anon_data(s_raw, s_anon)
ev_dist <- reid_evaluate(score_dist(sd_, "ITEMS"), seeds = 1:10, top_k = 1)
ev_jac  <- reid_evaluate(score_jaccard(sd_, "ITEMS"), seeds = 1:10, top_k = 1)
cat(sprintf("  distribution distance         : %.4f\n", ev_dist$success_analytic))
cat(sprintf("  set overlap (Jaccard)         : %.4f\n", ev_jac$success_analytic))
cat(sprintf("  ratio                         : %.1fx\n",
            ev_jac$success_analytic / ev_dist$success_analytic))

cat("\n== package state quoted on the page ==\n")
cat(sprintf("  exported functions            : %d\n",
            length(getNamespaceExports("reidentify"))))
