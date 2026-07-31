## ---------------------------------------------------------------------------
## Adversarial probe 8: score_mahalanobis() / score_multi(method =
## "mahalanobis") UNDER-REPORTS in exactly the situation the README recommends
## it for.
##
## README: "相関の強い数値列（身長と体重、購入回数と購入金額など）を等重みで
##          足すと、実質 1 列分の情報を 2 列分として数えてしまいます。
##          score_mahalanobis() は RAW 側の共分散でこの冗長さを打ち消します。"
##
## Mechanism: S^-1 amplifies the direction the population barely varies in. If
## the anonymisation's perturbation is comparable to (or larger than) the
## population spread along that direction -- which is what a near-collinear
## pair of columns guarantees -- the whitened distance is dominated by noise
## and the TRUE pair is pushed far away.
## ---------------------------------------------------------------------------
suppressMessages(pkgload::load_all(".", quiet = TRUE))
rate <- function(s) reid_evaluate(s, seeds = 1:20)$success_analytic

fixture <- function(n = 200, rho_noise = 1, noise = 2, seed = 5) {
  set.seed(seed)
  a <- rnorm(n, 50, 12)
  ## B = 2A + eps: the correlation is set by `rho_noise` (the sd of eps)
  b <- 2 * a + rnorm(n, sd = rho_noise)
  cc <- rnorm(n, 100, 20)
  raw <- data.frame(ROW_NUMBER = 1:n, A = a, B = b, C = cc)
  anon <- raw
  anon$A <- anon$A + rnorm(n, sd = noise)
  anon$B <- anon$B + rnorm(n, sd = 2 * noise)
  anon$C <- anon$C + rnorm(n, sd = 2.5 * noise)
  list(raw = raw, anon = anon,
       corr = cor(raw$A, raw$B))
}

cat("== sweep: how collinear A and B are, vs the noise the release adds ==\n")
cat(sprintf("%-8s %-7s %8s | %8s %12s %10s | %8s %8s\n",
            "eps_sd", "noise", "cor(A,B)", "weighted", "mahalanobis", "ratio",
            "A alone", "C alone"))
for (eps in c(0.5, 1, 3, 10, 30)) {
  for (noise in c(1, 2, 5)) {
    f <- fixture(rho_noise = eps, noise = noise)
    p <- join_raw_anon_data(f$raw, f$anon)
    tg <- c(A = "num", B = "num", C = "num")
    w <- rate(suppressWarnings(score_multi(p, tg, method = "weighted", screen = "none")))
    m <- rate(suppressWarnings(score_multi(p, tg, method = "mahalanobis", screen = "none")))
    cat(sprintf("%-8.1f %-7.1f %8.4f | %8.4f %12.4f %9.2fx | %8.4f %8.4f\n",
                eps, noise, f$corr, w, m, w / max(m, 1e-9),
                rate(score_num(p, "A")), rate(score_num(p, "C"))))
  }
}

cat("\n== does `ridge` rescue it? (the only knob the docs offer) ==\n")
f <- fixture(rho_noise = 1, noise = 2)
p <- join_raw_anon_data(f$raw, f$anon)
tg <- c(A = "num", B = "num", C = "num")
for (r in c(1e-6, 1e-3, 1e-2, 1e-1, 1, 10)) {
  m <- rate(suppressWarnings(score_multi(p, tg, method = "mahalanobis",
                                         ridge = r, screen = "none")))
  cat(sprintf("  ridge = %-8g  mahalanobis %.4f\n", r, m))
}
cat(sprintf("  (weighted, for comparison)  %.4f\n",
            rate(suppressWarnings(score_multi(p, tg, method = "weighted", screen = "none")))))

cat("\n== does anything warn / does axis screening notice? ==\n")
warns <- character(0)
s <- withCallingHandlers(
  score_multi(p, tg, method = "mahalanobis"),
  warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
cat(sprintf("  warnings: %s\n", if (length(warns)) paste(warns, collapse = " | ") else "NONE"))
print(axis_report(s))
cat("  (screening scores each column ALONE, so a metric that only fails when\n",
    "   the columns are combined is invisible to it.)\n")

cat("\n== bare score_mahalanobis(), i.e. the README's own call shape ==\n")
cat(sprintf("  score_mahalanobis(A, B)      %.4f\n", rate(score_mahalanobis(p, c("A", "B")))))
cat(sprintf("  score_multi weighted(A, B)   %.4f\n",
            rate(score_multi(p, c(A = "num", B = "num"), screen = "none"))))
cat(sprintf("  score_num(A) alone           %.4f\n", rate(score_num(p, "A"))))
cat(sprintf("  score_num(B) alone           %.4f\n", rate(score_num(p, "B"))))

cat("\n== cov_from = 'anon' / 'pooled' instead of the default 'raw' ==\n")
for (cf in c("raw", "anon", "pooled")) {
  cat(sprintf("  cov_from = %-7s  %.4f\n", cf,
              rate(suppressWarnings(score_multi(p, tg, method = "mahalanobis",
                                                cov_from = cf, screen = "none")))))
}

cat("\n== printed evaluation of the mahalanobis result (looks plausible) ==\n")
print(reid_evaluate(suppressWarnings(score_multi(p, tg, method = "mahalanobis",
                                                 screen = "none")), seeds = 1:20))
