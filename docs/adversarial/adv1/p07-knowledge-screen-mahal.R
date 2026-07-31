## ---------------------------------------------------------------------------
## Adversarial probe 7:
##   (a) reid_knowledge_curve() -- can a HIGHER knowledge level report LOWER
##       risk on a realistic release?
##   (b) screen = "drop" -- can the safety feature itself lower the number?
##   (c) method = "mahalanobis" -- the README's remedy for correlated columns.
## ---------------------------------------------------------------------------
suppressMessages(pkgload::load_all(".", quiet = TRUE))
rate <- function(s) reid_evaluate(s, seeds = 1:20)$success_analytic

## ---- a realistic release: noise on AGE, noise on every spend amount --------
mk <- function() {
  raw  <- create_dummy_qi_data(people = 200, seed = 1)
  anon <- create_dummy_qi_data(people = 200, seed = 1)
  set.seed(42)
  anon$AGE <- anon$AGE + sample(c(-2, 0, 2), nrow(anon), replace = TRUE)
  set.seed(43)
  anon$SPEND_DIST <- vapply(anon$SPEND_DIST, function(s) {
    v <- as.numeric(strsplit(s, ":", fixed = TRUE)[[1]])
    paste(round(pmax(0, v + rnorm(length(v), sd = 30)), 1), collapse = ":")
  }, character(1))
  anon$SPEND_MEAN <- anon$SPEND_MEAN + rnorm(nrow(anon), sd = 20)
  anon$VISIT_COUNT <- pmax(1, round(anon$VISIT_COUNT * 0.6))   # 60% subsampling
  anon$FINGERPRINT <- anon$FINGERPRINT + rnorm(nrow(anon), sd = 0.5)
  list(raw = raw, anon = anon)
}
d <- mk()
p <- join_raw_anon_data(d$raw, d$anon)

cat("###### (a) reid_knowledge_curve() on a noised release ######\n")
cat("--- default normalize = \"range\", screen = \"warn\" ---\n")
cur <- withCallingHandlers(
  reid_knowledge_curve(
    p,
    quasi_identifiers = c(AGE = "num", ZIP = "char", SEX = "char"),
    behavior = c(VISIT_COUNT = "num", SPEND_MEAN = "num", SPEND_DIST = "dist"),
    identifiers = c(FINGERPRINT = "num"),
    weak_subset = "ZIP",
    seeds = 1:20
  ),
  warning = function(w) { cat("  [warning]", substr(conditionMessage(w), 1, 130), "\n")
                          invokeRestart("muffleWarning") })
print(cur[, c("level", "n_visible", "success_analytic", "lift", "max_risk")])
cat(sprintf("  monotone in knowledge? W<=M<=S : %s\n",
            all(diff(cur$success_analytic) >= -1e-12)))

cat("\n--- same, normalize = \"rank\" ---\n")
cur2 <- suppressWarnings(reid_knowledge_curve(
  p,
  quasi_identifiers = c(AGE = "num", ZIP = "char", SEX = "char"),
  behavior = c(VISIT_COUNT = "num", SPEND_MEAN = "num", SPEND_DIST = "dist"),
  identifiers = c(FINGERPRINT = "num"),
  weak_subset = "ZIP", seeds = 1:20, normalize = "rank"))
print(cur2[, c("level", "n_visible", "success_analytic")])

cat("\n--- same, normalize = \"none\" (what plain combine_scores() does) ---\n")
cur3 <- suppressWarnings(reid_knowledge_curve(
  p,
  quasi_identifiers = c(AGE = "num", ZIP = "char", SEX = "char"),
  behavior = c(VISIT_COUNT = "num", SPEND_MEAN = "num", SPEND_DIST = "dist"),
  identifiers = c(FINGERPRINT = "num"),
  weak_subset = "ZIP", seeds = 1:20, normalize = "none"))
print(cur3[, c("level", "n_visible", "success_analytic")])

cat("\n###### (b) screen = \"drop\" vs \"none\" ######\n")
targets <- c(AGE = "num", ZIP = "char", SEX = "char", VISIT_COUNT = "num",
             SPEND_MEAN = "num", SPEND_DIST = "dist")
for (sc in c("none", "warn", "drop")) {
  s <- suppressWarnings(score_multi(p, targets, screen = sc))
  cat(sprintf("  screen = %-6s  success %.4f  axes kept: %s\n", sc, rate(s),
              if (is.null(axis_report(s))) "(not screened)"
              else paste(axis_report(s)$axis[axis_report(s)$kept], collapse = ",")))
}
cat("  axis report:\n")
print(suppressWarnings(axis_report(score_multi(p, targets, screen = "drop"))))

cat("\n###### (c) method = \"mahalanobis\" vs \"weighted\" ######\n")
num_only <- c(AGE = "num", VISIT_COUNT = "num", SPEND_MEAN = "num")
for (m in c("weighted", "mahalanobis")) {
  cat(sprintf("  numeric columns only, method = %-12s  %.4f\n", m,
              rate(suppressWarnings(score_multi(p, num_only, method = m, screen = "none")))))
}
mixed <- c(AGE = "num", VISIT_COUNT = "num", SPEND_MEAN = "num",
           ZIP = "char", SEX = "char")
for (m in c("weighted", "mahalanobis")) {
  cat(sprintf("  mixed columns,        method = %-12s  %.4f\n", m,
              rate(suppressWarnings(score_multi(p, mixed, method = m, screen = "none")))))
}
cat("\n  README's correlated-column example, both ways:\n")
cat(sprintf("    score_mahalanobis(AGE, VISIT_COUNT)  %.4f\n",
            rate(score_mahalanobis(p, c("AGE", "VISIT_COUNT")))))
cat(sprintf("    score_multi weighted(AGE,VISIT_COUNT) %.4f\n",
            rate(score_multi(p, c(AGE = "num", VISIT_COUNT = "num"), screen = "none"))))

cat("\n###### (d) strongly correlated pair, which mahalanobis exists for ######\n")
set.seed(5)
n <- 200
a <- rnorm(n, 50, 12)
raw2 <- data.frame(ROW_NUMBER = 1:n, A = a, B = 2 * a + rnorm(n, sd = 1),
                   C = rnorm(n, 100, 20))
anon2 <- raw2
anon2$A <- anon2$A + rnorm(n, sd = 2)
anon2$B <- anon2$B + rnorm(n, sd = 4)
anon2$C <- anon2$C + rnorm(n, sd = 5)
p2 <- join_raw_anon_data(raw2, anon2)
for (m in c("weighted", "mahalanobis")) {
  cat(sprintf("  A,B (corr .998) + C, method = %-12s  %.4f\n", m,
              rate(suppressWarnings(score_multi(p2, c(A = "num", B = "num", C = "num"),
                                                method = m, screen = "none")))))
}
cat(sprintf("  C alone %.4f | A alone %.4f\n",
            rate(score_num(p2, "C")), rate(score_num(p2, "A"))))
