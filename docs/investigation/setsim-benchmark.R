## Issue #18 verification: does set similarity beat score_dist() on a
## set-valued _DIST column?
##
## Run with:
##   Rscript docs/investigation/setsim-benchmark.R
##
## The issue's acceptance criterion is "compare the success rate with the
## current reid_by_dist() and beat it". score_dist() only accepts a *numeric*
## collapsed column, so the comparison has to be run on one -- a catalogue of
## items encoded as integers. That is the setting most favourable to
## score_dist(), because it is the only one where it runs at all; the
## categorical case (shop names, product codes) is not a comparison, it is a
## column score_dist() cannot read.

suppressWarnings(suppressMessages(pkgload::load_all(".", quiet = TRUE)))

set.seed(20260730)

## ---------------------------------------------------------------------------
## fixture: a purchase-basket style set column
##
## Each person draws `basket` items from a catalogue of `catalogue` codes with
## a Zipf-ish popularity, so a few items are common and most are rare. The
## anonymised release keeps a random `retain` fraction of each person's items
## -- suppression, the most common thing that actually happens to a long tail.
## ---------------------------------------------------------------------------
make_sets <- function(people, catalogue, basket, retain, collapse = ":") {
  pop <- 1 / seq_len(catalogue)^1.1
  pop <- pop / sum(pop)

  raw_items <- lapply(seq_len(people), function(i) {
    k <- max(2L, stats::rpois(1, basket))
    sort(unique(sample.int(catalogue, size = min(k, catalogue),
                           replace = FALSE, prob = pop)))
  })
  anon_items <- lapply(raw_items, function(v) {
    k <- max(1L, round(length(v) * retain))
    sort(sample(v, size = k))
  })

  list(
    raw = data.frame(
      ROW_NUMBER = seq_len(people),
      ITEMS = vapply(raw_items, paste, character(1), collapse = collapse),
      stringsAsFactors = FALSE
    ),
    anon = data.frame(
      ROW_NUMBER = seq_len(people),
      ITEMS = vapply(anon_items, paste, character(1), collapse = collapse),
      stringsAsFactors = FALSE
    )
  )
}

rate <- function(scores, seeds = 1:20) {
  ev <- reid_evaluate(scores, seeds = seeds, top_k = 1)
  c(analytic = ev$success_analytic,
    mean = ev$success_mean,
    sd = ev$success_sd,
    baseline = ev$baseline$rate[ev$baseline$method == "random"])
}

report <- function(label, people, catalogue, basket, retain, reps = 5) {
  rows <- list()
  for (r in seq_len(reps)) {
    fx <- make_sets(people, catalogue, basket, retain)
    d <- join_raw_anon_data(fx$raw, fx$anon)
    rows[[r]] <- rbind(
      data.frame(method = "score_dist",           t(rate(score_dist(d, "ITEMS")))),
      data.frame(method = "score_jaccard",        t(rate(score_jaccard(d, "ITEMS")))),
      data.frame(method = "score_jaccard(dice)",  t(rate(score_jaccard(d, "ITEMS", method = "dice")))),
      data.frame(method = "score_jaccard(overlap)", t(rate(score_jaccard(d, "ITEMS", method = "overlap")))),
      data.frame(method = "score_jaccard(tversky .5/2)",
                 t(rate(score_jaccard(d, "ITEMS", method = "tversky", alpha = 0.5, beta = 2)))),
      data.frame(method = "score_minhash(128)",   t(rate(score_minhash(d, "ITEMS", n_hash = 128))))
    )
  }
  all <- do.call(rbind, rows)
  agg <- aggregate(cbind(analytic, baseline) ~ method, data = all, FUN = mean)
  agg <- agg[order(-agg$analytic), ]

  cat("\n== ", label, " ==\n", sep = "")
  cat(sprintf("people=%d catalogue=%d basket=%d retain=%.2f reps=%d\n",
              people, catalogue, basket, retain, reps))
  for (i in seq_len(nrow(agg))) {
    cat(sprintf("  %-28s success=%.4f  random-baseline=%.4f\n",
                agg$method[i], agg$analytic[i], agg$baseline[i]))
  }
  invisible(agg)
}

cat("R:", R.version.string, "\n")
cat("date:", format(Sys.time()), "\n")

report("sparse baskets, heavy suppression", people = 200, catalogue = 500, basket = 8,  retain = 0.5)
report("larger baskets, light suppression", people = 200, catalogue = 500, basket = 20, retain = 0.8)
report("small catalogue (collisions)",      people = 200, catalogue = 60,  basket = 8,  retain = 0.5)

## ---------------------------------------------------------------------------
## the categorical case: score_dist() cannot run at all
## ---------------------------------------------------------------------------
cat("\n== categorical set column ==\n")
tran <- create_dummy_transaction_data(people = 60, size = 6)
master <- transform_transaction_to_master(
  tran, DYNAMIC_NUM = "NUM_DYNAMIC", DYNAMIC_CHAR = "CHAR"
)
d <- join_raw_anon_data(master, master)
cat("  score_dist(CHAR_DIST):    ",
    tryCatch({
      score_dist(d, "CHAR_DIST")
      "ran"
    }, error = function(e) paste0("ERROR: ", sub("\n.*", "", conditionMessage(e)))),
    "\n")
cat(sprintf("  score_jaccard(CHAR_DIST): success=%.4f\n",
            reid_evaluate(score_jaccard(d, "CHAR_DIST"), seeds = 1:10,
                          top_k = 1)$success_analytic))

## ---------------------------------------------------------------------------
## min-hash accuracy and LSH blocking cost/recall
## ---------------------------------------------------------------------------
cat("\n== min-hash accuracy vs exact Jaccard ==\n")
fx <- make_sets(120, 400, 12, 0.6)
d <- join_raw_anon_data(fx$raw, fx$anon)
ex <- score_jaccard(d, "ITEMS")$SCORE
for (nh in c(32, 128, 512)) {
  mh <- score_minhash(d, "ITEMS", n_hash = nh)$SCORE
  cat(sprintf("  n_hash=%3d  mean|err|=%.4f  max|err|=%.4f  cor=%.4f\n",
              nh, mean(abs(ex - mh)), max(abs(ex - mh)), stats::cor(ex, mh)))
}

cat("\n== LSH blocking ==\n")
for (bands in c(8, 16, 32, 64)) {
  blocked <- lsh_candidates(fx$raw, fx$anon, "ITEMS", n_hash = 128, bands = bands)
  info <- attr(blocked, "blocking")
  ## does the blocked candidate set still contain the true counterpart?
  truth_kept <- sum(blocked$RAW_ROW_NUMBER == blocked$ANON_ROW_NUMBER)
  cat(sprintf("  bands=%2d  kept=%.4f of pairs  anon without candidate=%d  true pairs retained=%d/%d\n",
              bands, info$kept_fraction, info$n_anon_without_candidate,
              truth_kept, nrow(fx$anon)))
}
