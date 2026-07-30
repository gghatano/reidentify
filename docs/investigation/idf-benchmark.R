## ---------------------------------------------------------------------------
## Issue #17 -- does rarity weighting beat unweighted exact matching, and which
## weighting formula should be the default?
##
## Run with:  Rscript docs/investigation/idf-benchmark.R
##
## The issue names `w = 1 / log(freq)` specifically. That formula is infinite
## at freq = 1 -- the singleton value, which is exactly the case the method
## exists for -- so it cannot be used as written; the implementation offers it
## as "inv_log" with the shift `1 / log(freq + 1)`. Whether it or the standard
## IDF `log(n / freq)` should be the default is decided here by measurement.
## ---------------------------------------------------------------------------

suppressMessages(pkgload::load_all(".", quiet = TRUE))

SEEDS <- 1:10
success <- function(scores) reid_evaluate(scores, seeds = SEEDS)$success_analytic

## ---------------------------------------------------------------------------
## Categorical data with skewed value frequencies.
##
## Each column draws from `k` values with Zipf-like probabilities, so a few
## values are very common and a long tail is nearly unique -- the situation
## rarity weighting is for. The anonymiser replaces each cell independently
## with a fresh draw with probability `corrupt`, so exact matching is
## informative but not decisive.
## ---------------------------------------------------------------------------
zipf_join <- function(n = 300, p = 6, k = 20, corrupt = 0.4, s = 1.2, seed = 1) {
  set.seed(seed)
  probs <- (seq_len(k))^(-s)
  probs <- probs / sum(probs)
  values <- sprintf("v%02d", seq_len(k))

  raw <- data.frame(ROW_NUMBER = seq_len(n), stringsAsFactors = FALSE)
  for (i in seq_len(p)) {
    raw[[paste0("X", i)]] <- sample(values, n, replace = TRUE, prob = probs)
  }

  anon <- raw
  for (i in seq_len(p)) {
    col <- paste0("X", i)
    hit <- runif(n) < corrupt
    anon[[col]][hit] <- sample(values, sum(hit), replace = TRUE, prob = probs)
  }
  join_raw_anon_data(raw, anon)
}

## A uniform control: with flat value frequencies every value is equally rare,
## so rarity weighting has nothing to work with and must not help.
uniform_join <- function(n = 300, p = 6, k = 20, corrupt = 0.4, seed = 1) {
  zipf_join(n = n, p = p, k = k, corrupt = corrupt, s = 0, seed = seed)
}

cols <- function(p = 6) paste0("X", seq_len(p))

cat("Issue #17 rarity-weighted exact matching\n")
cat("success_analytic, n = 300 records, 6 columns, 20 values/column\n")

compare <- function(label, gen) {
  cat("\n== ", label, " ==\n", sep = "")
  tab <- do.call(rbind, lapply(1:10, function(s) {
    j <- gen(seed = s)
    row <- lapply(c("none", "idf", "inv_log", "inv"), function(w) {
      success(score_idf_match(j, cols(), weight = w))
    })
    names(row) <- c("none", "idf", "inv_log", "inv")
    cbind(data.frame(seed = s), as.data.frame(row))
  }))
  print(tab, row.names = FALSE, digits = 3)
  cat("\n  mean:", sprintf("none %.4f  idf %.4f  inv_log %.4f  inv %.4f\n",
                           mean(tab$none), mean(tab$idf),
                           mean(tab$inv_log), mean(tab$inv)))
  for (w in c("idf", "inv_log", "inv")) {
    d <- tab[[w]] - tab$none
    cat(sprintf("  %-8s vs none: mean %+.4f, wins %d/10\n", w, mean(d), sum(d > 0)))
  }
  invisible(tab)
}

compare("skewed (Zipf s = 1.2) value frequencies", zipf_join)
compare("uniform value frequencies (control)", uniform_join)

## ---------------------------------------------------------------------------
## How much does the skew have to be before weighting pays?
## ---------------------------------------------------------------------------
cat("\n== effect of the frequency skew s ==\n")
for (s_par in c(0, 0.4, 0.8, 1.2, 1.6, 2.0)) {
  d <- vapply(1:5, function(sd_) {
    j <- zipf_join(s = s_par, seed = sd_)
    success(score_idf_match(j, cols(), weight = "idf")) -
      success(score_idf_match(j, cols(), weight = "none"))
  }, numeric(1))
  cat(sprintf("  s = %.1f : mean gain %+.4f (wins %d/5)\n", s_par, mean(d), sum(d > 0)))
}

## ---------------------------------------------------------------------------
## Where the frequencies are counted from. The default is the ANON side,
## because it needs no attacker knowledge; check that this costs nothing.
## ---------------------------------------------------------------------------
cat("\n== frequency source ==\n")
for (src in c("anon", "raw", "pooled")) {
  v <- vapply(1:5, function(s) {
    success(score_idf_match(zipf_join(seed = s), cols(), source = src))
  }, numeric(1))
  cat(sprintf("  source = %-6s : mean %.4f\n", src, mean(v)))
}

## ---------------------------------------------------------------------------
## Integration with #14: an IDF block alongside numeric attributes.
## ---------------------------------------------------------------------------
cat("\n== integrated into score_multi() alongside a numeric column ==\n")
mixed_join <- function(seed = 1, n = 300, k = 20, corrupt = 0.4) {
  set.seed(seed)
  probs <- (seq_len(k))^(-1.2)
  probs <- probs / sum(probs)
  values <- sprintf("v%02d", seq_len(k))
  raw <- data.frame(
    ROW_NUMBER = seq_len(n),
    A = sample(values, n, replace = TRUE, prob = probs),
    B = sample(values, n, replace = TRUE, prob = probs),
    AGE = sample(20:79, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  anon <- raw
  for (col in c("A", "B")) {
    hit <- runif(n) < corrupt
    anon[[col]][hit] <- sample(values, sum(hit), replace = TRUE, prob = probs)
  }
  anon$AGE <- (anon$AGE %/% 10) * 10
  join_raw_anon_data(raw, anon)
}

res <- do.call(rbind, lapply(1:5, function(s) {
  j <- mixed_join(seed = s)
  data.frame(
    seed = s,
    age_only = success(score_multi(j, c(AGE = "num"))),
    char_only = success(score_multi(j, c(A = "char", B = "char"))),
    idf_only = success(score_multi(j, c(A = "idf", B = "idf"))),
    char_plus_age = success(score_multi(j, c(A = "char", B = "char", AGE = "num"))),
    idf_plus_age = success(score_multi(j, c(A = "idf", B = "idf", AGE = "num")))
  )
}))
print(res, row.names = FALSE, digits = 3)
cat(sprintf("\n  mean: age %.4f | char %.4f | idf %.4f | char+age %.4f | idf+age %.4f\n",
            mean(res$age_only), mean(res$char_only), mean(res$idf_only),
            mean(res$char_plus_age), mean(res$idf_plus_age)))
