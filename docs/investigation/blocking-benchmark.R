## ---------------------------------------------------------------------------
## Blocking benchmark (Issue #36)
##
## Four questions, in the order they have to be answered:
##
##   1. Does a recall-1.0 deterministic block reproduce the full-join
##      reidentification rate? (If not, blocking is not a speed-up, it is a
##      different measurement.)
##   2. What does a block key that the anonymisation perturbs cost in recall,
##      and is that visible?
##   3. Does memory and time grow more slowly than n^2?
##   4. What does top-k pruning cost in recall?
##
## Run with:
##   Rscript docs/investigation/blocking-benchmark.R
## ---------------------------------------------------------------------------

suppressMessages(pkgload::load_all(".", quiet = TRUE))

fmt <- function(x, d = 4) formatC(x, format = "f", digits = d)
rule <- function(s) cat("\n", strrep("=", 74), "\n", s, "\n", strrep("=", 74), "\n", sep = "")

## Timed evaluation of one candidate table.
timed <- function(expr) {
  gc(reset = TRUE, full = TRUE)
  t <- system.time(value <- force(expr))
  list(value = value, elapsed = unname(t[["elapsed"]]),
       mem_mb = sum(gc()[, "max used"] * c(56, 8)) / 2^20)
}

rule("1. recall-1.0 deterministic blocking vs the full cross join")
cat("The block key must be among the attributes the attacker scores on;\n",
    "otherwise blocking is itself evidence and changes the measurement.\n", sep = "")

raw  <- create_dummy_qi_data(people = 400, seed = 11)
anon <- create_dummy_qi_data(people = 400, seed = 11)
set.seed(42)
anon$AGE <- anon$AGE + sample(c(-2, 0, 2), nrow(anon), replace = TRUE)

full    <- join_raw_anon_data(raw, anon)
blocked <- block_candidates(raw, anon, keys = "ZIP")
info    <- attr(blocked, "blocking")

targets <- c(AGE = "num", ZIP = "char", SEX = "char")
e_full <- reid_evaluate(score_multi(full, targets, screen = "none"), seeds = 1:10)
e_blk  <- reid_evaluate(score_multi(blocked, targets, screen = "none"), seeds = 1:10)

cat("\n  pairs  full ", format(nrow(full), big.mark = ","),
    " | blocked ", format(nrow(blocked), big.mark = ","),
    "  (", fmt(100 * info$kept_fraction, 2), "% kept)\n", sep = "")
cat("  recall ", fmt(info$recall), "\n", sep = "")
cat("  success_analytic  full ", fmt(e_full$success_analytic),
    " | blocked ", fmt(e_blk$success_analytic), "\n", sep = "")
cat("  max_risk          full ", fmt(e_full$max_risk),
    " | blocked ", fmt(e_blk$max_risk), "\n", sep = "")
cat("  top-1 hit rate    full ", fmt(e_full$top_k$hit_rate[1]),
    " | blocked ", fmt(e_blk$top_k$hit_rate[1]), "\n", sep = "")
cat("  baseline random   full ", fmt(e_full$baseline$rate[1]),
    " | blocked ", fmt(e_blk$baseline$rate[1]), "\n", sep = "")

cat("\n  And the same block key with a score that does NOT use it:\n")
targets2 <- c(AGE = "num", SEX = "char")
f2 <- reid_evaluate(score_multi(full, targets2, screen = "none"), seeds = 1:10)
b2 <- reid_evaluate(score_multi(blocked, targets2, screen = "none"), seeds = 1:10)
cat("  success_analytic  full ", fmt(f2$success_analytic),
    " | blocked ", fmt(b2$success_analytic),
    "   <- blocking added information the score did not have\n", sep = "")

rule("2. reduction and recall by block key")
keys_list <- list(
  list(label = "ZIP",                 keys = "ZIP",           tr = NULL),
  list(label = "ZIP + SEX",           keys = c("ZIP", "SEX"), tr = NULL),
  list(label = "AGE (exact)",         keys = "AGE",           tr = NULL),
  list(label = "AGE decade",          keys = "AGE",
       tr = list(AGE = function(x) x %/% 10)),
  list(label = "ZIP | AGE decade",    keys = list("ZIP", "AGE"),
       tr = list(AGE = function(x) x %/% 10))
)
cat(sprintf("%-18s %12s %10s %10s %8s\n",
            "key", "pairs kept", "kept %", "recall", "no cand."))
for (spec in keys_list) {
  cand <- suppressWarnings(
    block_candidates(raw, anon, keys = spec$keys, transform = spec$tr)
  )
  i <- attr(cand, "blocking")
  cat(sprintf("%-18s %12s %10s %10s %8d\n", spec$label,
              format(i$n_pairs_kept, big.mark = ","),
              fmt(100 * i$kept_fraction, 2), fmt(i$recall),
              i$n_anon_without_candidate))
}
cat("\n  AGE was perturbed by +/-2 in ANON, so an exact-AGE block loses true\n",
    "  pairs. The union pass buys them back at the cost of more pairs.\n", sep = "")

cat("\n  What a recall < 1 block does to the reported rate:\n")
lossy <- suppressWarnings(block_candidates(raw, anon, keys = "AGE"))
e_lossy <- reid_evaluate(score_multi(lossy, targets, screen = "none"), seeds = 1:10)
cat("  success_analytic  full ", fmt(e_full$success_analytic),
    " | AGE-blocked ", fmt(e_lossy$success_analytic), "\n", sep = "")
cat("\n  and this is what the user sees, without asking for it:\n\n")
print(e_lossy)

rule("3. scaling: full cross join vs deterministic blocking")
cat(sprintf("%7s %14s %10s %10s %14s %10s %10s %9s\n",
            "n", "full pairs", "full s", "full MB",
            "blocked pairs", "blocked s", "blocked MB", "recall"))

ns_full  <- c(500, 1000, 2000, 4000)
ns_block <- c(500, 1000, 2000, 4000, 8000, 16000, 32000)

results <- list()
for (n in ns_block) {
  r <- create_dummy_qi_data(people = n, seed = 7)
  a <- create_dummy_qi_data(people = n, seed = 7)

  fp <- NA; ft <- NA; fm <- NA
  if (n %in% ns_full) {
    tf <- timed(join_raw_anon_data(r, a))
    fp <- nrow(tf$value); ft <- tf$elapsed
    fm <- as.numeric(utils::object.size(tf$value)) / 2^20
    rm(tf); gc(FALSE)
  }

  tb <- timed(block_candidates(r, a, keys = "ZIP", max_pairs = 1e8))
  bp <- nrow(tb$value); bt <- tb$elapsed
  bm <- as.numeric(utils::object.size(tb$value)) / 2^20
  rec <- attr(tb$value, "blocking")$recall
  rm(tb); gc(FALSE)

  cat(sprintf("%7d %14s %10s %10s %14s %10s %10s %9s\n", n,
              if (is.na(fp)) "-" else format(fp, big.mark = ","),
              if (is.na(ft)) "-" else fmt(ft, 2),
              if (is.na(fm)) "-" else fmt(fm, 1),
              format(bp, big.mark = ","), fmt(bt, 2), fmt(bm, 1), fmt(rec)))
  results[[length(results) + 1L]] <- data.frame(
    n = n, full_pairs = fp, full_s = ft, full_mb = fm,
    blocked_pairs = bp, blocked_s = bt, blocked_mb = bm, recall = rec
  )
}
res <- do.call(rbind, results)

cat("\n  growth exponent (log-log slope of pairs against n):\n")
fitp <- function(x, y) {
  ok <- is.finite(x) & is.finite(y) & y > 0
  unname(stats::coef(stats::lm(log(y[ok]) ~ log(x[ok])))[2])
}
cat("    full join pairs : ", fmt(fitp(res$n, res$full_pairs), 2), "\n", sep = "")
cat("    blocked pairs   : ", fmt(fitp(res$n, res$blocked_pairs), 2), "\n", sep = "")
cat("    full join time  : ", fmt(fitp(res$n, res$full_s), 2), "\n", sep = "")
cat("    blocked time    : ", fmt(fitp(res$n, res$blocked_s), 2), "\n", sep = "")
cat("    full join MB    : ", fmt(fitp(res$n, res$full_mb), 2), "\n", sep = "")
cat("    blocked MB      : ", fmt(fitp(res$n, res$blocked_mb), 2), "\n", sep = "")

cat("\n  end-to-end at n = 32,000 (blocked candidates -> score -> evaluate):\n")
r <- create_dummy_qi_data(people = 32000, seed = 7)
a <- create_dummy_qi_data(people = 32000, seed = 7)
set.seed(42); a$AGE <- a$AGE + sample(c(-2, 0, 2), nrow(a), replace = TRUE)
t_all <- timed({
  cand <- block_candidates(r, a, keys = "ZIP", max_pairs = 1e8)
  reid_evaluate(score_multi(cand, targets, screen = "none"), seeds = 1:5)
})
cat("    elapsed ", fmt(t_all$elapsed, 2), " s\n", sep = "")
print(t_all$value)
cat("\n  The full join of that problem would be 1,024,000,000 pairs.\n")

rule("4. top-k pruning of a score table")
s_full <- score_multi(full, targets, screen = "none")
cat(sprintf("%6s %14s %10s %10s %18s\n",
            "k", "pairs kept", "kept %", "recall", "success_analytic"))
cat(sprintf("%6s %14s %10s %10s %18s\n", "-",
            format(nrow(s_full), big.mark = ","), "100.00", fmt(1),
            fmt(e_full$success_analytic)))
for (k in c(1, 2, 5, 10, 50)) {
  pk <- suppressWarnings(top_k_candidates(s_full, k = k))
  i <- attr(pk, "blocking")
  ev <- reid_evaluate(pk, seeds = 1:10, top_k = 1)
  cat(sprintf("%6d %14s %10s %10s %18s\n", k,
              format(i$n_pairs_kept, big.mark = ","),
              fmt(100 * i$kept_fraction, 2), fmt(i$recall),
              fmt(ev$success_analytic)))
}
cat("\n  top-k prunes the table but not the work that built it: the score has\n",
    "  to exist before it can be ranked. It is a second stage, not a first.\n", sep = "")

rule("5. LSH blocking now reports recall too")
set.seed(20260731)
pop    <- 1 / seq_len(500)^1.1
basket <- lapply(1:400, function(i) sort(sample.int(500, 8, prob = pop)))
kept   <- lapply(basket, function(v) sort(sample(v, 4)))
sr <- data.frame(ROW_NUMBER = 1:400,
                 ITEMS = vapply(basket, paste, character(1), collapse = ":"))
sa <- data.frame(ROW_NUMBER = 1:400,
                 ITEMS = vapply(kept, paste, character(1), collapse = ":"))
for (b in c(8, 16, 32, 64)) {
  cand <- suppressWarnings(lsh_candidates(sr, sa, "ITEMS", bands = b, seed = 1))
  i <- attr(cand, "blocking")
  cat(sprintf("  bands %3d : %8s pair(s) kept (%6s%%)  recall %s\n", b,
              format(i$n_pairs_kept, big.mark = ","),
              fmt(100 * i$kept_fraction, 2), fmt(i$recall)))
}

cat("\ndone.\n")
