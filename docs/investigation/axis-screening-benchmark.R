## ---------------------------------------------------------------------------
## Issue #35 -- does an uninformative axis really drag the combination down,
## and does screening it out fix that without throwing away live axes?
##
## Run with:  Rscript docs/investigation/axis-screening-benchmark.R
##
## The failure was found while implementing Issue #22 and reported in PR #34:
## under an anonymiser that publishes only 60% of each person's events, the
## record-count axis carries no information (every count shrinks by the same
## factor), and adding it to the combination at equal weight made the reported
## success rate *fall below* the static-attribute-only attack in 4 of 8 seeds.
## A safety tool reporting a smaller number after the attacker gained evidence
## is under-reporting, which is docs/lessons-learned.md section 2.
##
## Sections:
##   1. reproduce the failure on the PR #34 fixture
##   2. the detector: dead axes vs live axes, across four fixtures
##   3. does screen = "drop" repair the under-report
##   4. false positives: axes with real but weak signal
##   5. why the test is on ranks and not on the success rate
## ---------------------------------------------------------------------------

suppressMessages(pkgload::load_all(".", quiet = TRUE))

SEEDS <- 1:10
success <- function(scores) reid_evaluate(scores, seeds = SEEDS)$success_analytic
quiet <- function(expr) suppressWarnings(expr)

DOW <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

## The Issue #22 fixture, unchanged, so the two benchmarks are comparable.
##   "jitter"    every person's events are kept, the count moves by -1/0/+1
##   "subsample" only a `keep` fraction of each person's events is published,
##               so every count shrinks by roughly the same factor
activity_master <- function(n = 120, mode = c("jitter", "subsample"),
                            keep = 0.6, seed = 1) {
  mode <- match.arg(mode)
  set.seed(seed)

  rate <- rpois(n, lambda = 12) + 3
  favourite <- sample.int(7L, n, replace = TRUE)
  start <- sample.int(300L, n, replace = TRUE)
  window <- sample(c(10L, 30L, 90L, 300L), n, replace = TRUE)

  make_master <- function(counts) {
    rows <- lapply(seq_len(n), function(i) {
      k <- counts[i]
      p <- rep(1, 7)
      p[favourite[i]] <- 4
      data.frame(
        ID = i,
        DOW = DOW[sample.int(7L, k, replace = TRUE, prob = p)],
        DAY = start[i] + sample.int(window[i], k, replace = TRUE),
        stringsAsFactors = FALSE
      )
    })
    tran <- do.call(rbind, rows)
    tran$ROW_NUMBER <- seq_len(nrow(tran))
    m <- transform_transaction_to_master(
      tran, ROW_NUMBER = "ROW_NUMBER", ID = "ID",
      DYNAMIC_NUM = "DAY", DYNAMIC_CHAR = "DOW"
    )
    m$ROW_NUMBER <- m$ID
    as.data.frame(m)
  }

  anon_counts <- if (identical(mode, "jitter")) {
    pmax(1L, rate + sample(-1:1, n, replace = TRUE))
  } else {
    pmax(1L, rbinom(n, rate, keep))
  }

  join_raw_anon_data(make_master(rate), make_master(anon_counts))
}

ALL <- c(DAY_MEAN = "num", ROWCOUNT = "count", DOW_DIST = "profile",
         DAY_DIST = "span")
STATIC <- c(DAY_MEAN = "num")

## ---------------------------------------------------------------------------
cat("== 1. the failure, on the PR #34 fixture (120 people, 60% published) ==\n")
## ---------------------------------------------------------------------------
tab <- do.call(rbind, lapply(1:8, function(s) {
  j <- activity_master(mode = "subsample", seed = s)
  data.frame(
    seed = s,
    static = success(quiet(score_multi(j, STATIC, screen = "none"))),
    all_equal_weight = success(quiet(score_multi(j, ALL, screen = "none"))),
    screened = success(quiet(score_multi(j, ALL, screen = "drop")))
  )
}))
tab$drop_vs_static <- tab$all_equal_weight - tab$static
print(tab, row.names = FALSE, digits = 3)
cat(sprintf(
  "\n  mean: static %.4f | all axes at equal weight %.4f | screened %.4f\n",
  mean(tab$static), mean(tab$all_equal_weight), mean(tab$screened)))
cat(sprintf("  equal weight is BELOW static-only in %d/8 seeds\n",
            sum(tab$all_equal_weight < tab$static)))
cat(sprintf("  screened     is BELOW static-only in %d/8 seeds\n",
            sum(tab$screened < tab$static)))
cat(sprintf("  screened > equal weight in %d/8 seeds (mean gain %+.4f)\n",
            sum(tab$screened > tab$all_equal_weight),
            mean(tab$screened - tab$all_equal_weight)))

## ---------------------------------------------------------------------------
cat("\n== 2. the detector, axis by axis ==\n")
## ---------------------------------------------------------------------------
axes_of <- function(j) {
  list(
    "DAY_MEAN (static)" = score_num(j, "DAY_MEAN"),
    "ROWCOUNT (count)"  = score_count(j),
    "DOW_DIST (profile)" = score_profile(j, "DOW_DIST", bins = DOW),
    "DAY_DIST (span)"   = score_span(j, "DAY_DIST")
  )
}
for (mode in c("subsample", "jitter")) {
  cat(sprintf("\n-- anonymiser = %s\n", mode))
  rep_all <- do.call(rbind, lapply(1:8, function(s) {
    r <- axis_informativeness(axes_of(activity_master(mode = mode, seed = s)))
    r$seed <- s
    r
  }))
  for (ax in unique(rep_all$axis)) {
    r <- rep_all[rep_all$axis == ax, ]
    cat(sprintf("   %-20s z %6.2f .. %6.2f   rank %.3f   informative in %d/8 seeds\n",
                ax, min(r$z), max(r$z), mean(r$mean_rank_pct), sum(r$informative)))
  }
}

## ---------------------------------------------------------------------------
cat("\n== 3. controls: a column with no relation to identity at all ==\n")
## ---------------------------------------------------------------------------
set.seed(99)
n <- 120
raw <- data.frame(ROW_NUMBER = 1:n, SIGNAL = rnorm(n), NOISE = rnorm(n),
                  CONST = 1)
anon <- raw
anon$SIGNAL <- raw$SIGNAL + rnorm(n, sd = 0.1)
anon$NOISE <- rnorm(n)          # redrawn: no relation to the RAW value
j <- join_raw_anon_data(raw, anon)
print(axis_informativeness(list(
  SIGNAL = score_num(j, "SIGNAL"),
  NOISE = score_num(j, "NOISE"),
  CONST = score_num(j, "CONST")
)))

## ---------------------------------------------------------------------------
cat("\n== 4. false positives: axes with real but weak signal ==\n")
## ---------------------------------------------------------------------------
## Signal is buried under progressively more noise. The point at which the
## screen stops calling an axis informative should be the point at which the
## axis has stopped being usable, not well before it.
cat("  noise sd | success | baseline |  lift | rank  |     z | informative\n")
for (nz in c(0.1, 0.5, 1, 2, 4, 8, 16, 1e6)) {
  set.seed(7)
  raw <- data.frame(ROW_NUMBER = 1:120, V = rnorm(120))
  anon <- raw
  anon$V <- raw$V + rnorm(120, sd = nz)
  r <- axis_informativeness(list(V = score_num(join_raw_anon_data(raw, anon), "V")))
  cat(sprintf("  %8g | %7.4f | %8.4f | %5.2f | %.3f | %5.2f | %s\n",
              nz, r$success, r$baseline, r$lift, r$mean_rank_pct, r$z,
              r$informative))
}

## ---------------------------------------------------------------------------
cat("\n== 5. why the test is on ranks, not on the success rate ==\n")
## ---------------------------------------------------------------------------
## The success-rate test was implemented first and rejected. Its null expects
## about one hit in the whole table, so it has almost no power: on this
## package's own fixtures it called axes with unmistakable signal dead.
top1_p <- function(s, n_null = 999, seed = 0) {
  pa <- reidentify:::reid_per_anon(s)
  risk <- reidentify:::top_k_probability(pa$N_BETTER, pa$TRUE_TIE_SIZE, 1)
  ht <- !is.na(pa$TRUE_RANK)
  n_i <- as.numeric(pa$N_CANDIDATES)
  m_i <- as.numeric(pa$BEST_TIE_SIZE)
  S <- sum(risk)
  set.seed(seed)
  pm <- ifelse(ht, m_i / n_i, 0)
  vv <- ifelse(ht, 1 / m_i, 0)
  nd <- vapply(seq_len(n_null),
               function(k) sum((runif(length(pm)) < pm) * vv), numeric(1))
  (1 + sum(nd >= S - 1e-12)) / (n_null + 1)
}

cat("  fixture / axis                    | rank p   | success-rate p\n")
show_both <- function(label, s) {
  r <- axis_informativeness(list(x = s))
  cat(sprintf("  %-33s | %.6f | %.4f\n", label, r$p_value, top1_p(s)))
}
set.seed(1)
nn <- 60
L <- rnorm(nn)
raw <- data.frame(ROW_NUMBER = 1:nn, A = L + rnorm(nn, sd = 0.05),
                  B = 3 * L + rnorm(nn, sd = 0.15))
anon <- raw
Lp <- L + rnorm(nn, sd = 0.8)
anon$A <- Lp + rnorm(nn, sd = 0.05)
anon$B <- 3 * Lp + rnorm(nn, sd = 0.15)
jj <- join_raw_anon_data(raw, anon)
show_both("correlated fixture, A (n = 60)", score_num(jj, "A"))
show_both("correlated fixture, B (n = 60)", score_num(jj, "B"))
j4 <- activity_master(mode = "jitter", seed = 4)
show_both("activity jitter s4, DOW profile", score_profile(j4, "DOW_DIST", bins = DOW))
js <- activity_master(mode = "subsample", seed = 1)
show_both("activity subsample s1, ROWCOUNT (dead)", score_count(js))
