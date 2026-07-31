## How much room does a "loose bound" assertion leave? Measure the actual
## value next to the threshold the test enforces, for the property tests in
## test-statistical-properties.R.
setwd(normalizePath(commandArgs(trailingOnly = TRUE)[1]))
suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = FALSE))

run <- function(f) mean(vapply(1:10, function(s) { set.seed(s); f(30) }, numeric(1)))

num <- run(function(n) {
  raw <- create_dummy_master_data(n); anon <- raw; anon$NUM <- runif(n)
  sum(reid_by_num(join_raw_anon_data(raw, anon), "NUM")$RESULT) })
chr <- run(function(n) {
  raw <- create_dummy_master_data(n); anon <- raw
  anon$CHAR <- stringi::stri_rand_strings(n, length = 2)
  sum(reid_by_char(join_raw_anon_data(raw, anon), "CHAR")$RESULT) })
rnk <- run(function(n) {
  raw <- create_dummy_master_data(n); anon <- raw; anon$NUM <- runif(n)
  sum(reid_by_num_rank(join_raw_anon_data(raw, anon), "NUM")$RESULT) })

cat(sprintf("%-18s actual %.2f   asserted < 5   slack %.1fx\n",
            c("reid_by_num", "reid_by_char", "reid_by_num_rank"),
            c(num, chr, rnk), 5 / c(num, chr, rnk)))

## monotonicity assertions: expect_gt(successes[1], successes[4])
mean_at <- function(fn, sigma, idx, n = 40, reps = 8)
  mean(vapply(seq_len(reps), function(i) {
    set.seed(idx * 1000L + i)
    raw <- create_dummy_master_data(n); anon <- raw
    anon$NUM <- raw$NUM + rnorm(n, sd = sigma)
    sum(fn(join_raw_anon_data(raw, anon), "NUM")$RESULT) }, numeric(1)))
sig <- c(1e-6, 1e-2, 1, 50)
v <- vapply(seq_along(sig), function(i) mean_at(reid_by_num, sig[i], i), numeric(1))
cat("\nreid_by_num monotonicity levels (sd = 1e-6, 1e-2, 1, 50):",
    paste(sprintf("%.2f", v), collapse = "  "),
    sprintf("\n  asserted only: level1 (%.2f) > level4 (%.2f)\n", v[1], v[length(v)]))
