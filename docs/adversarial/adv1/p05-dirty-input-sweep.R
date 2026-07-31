## ---------------------------------------------------------------------------
## Adversarial probe 5: dirty-input sweep.
##
## For each pathological input, record: does it error, warn, or silently return
## a plausible number -- and is that number below the truth?
## ---------------------------------------------------------------------------
suppressMessages(pkgload::load_all(".", quiet = TRUE))

outcome <- function(label, expr) {
  warns <- character(0)
  val <- tryCatch(
    withCallingHandlers(expr,
      warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) structure(conditionMessage(e), class = "probe_error")
  )
  if (inherits(val, "probe_error")) {
    cat(sprintf("  %-46s ERROR   %s\n", label, substr(val, 1, 90)))
    return(invisible(NA_real_))
  }
  cat(sprintf("  %-46s %s%s\n", label,
              if (is.numeric(val)) sprintf("%.4f", val) else "ok",
              if (length(warns)) paste0("   WARN: ", substr(warns[1], 1, 70)) else "   (silent)"))
  invisible(val)
}
rate <- function(s) reid_evaluate(s, seeds = 1:20)$success_analytic

base <- function() {
  raw  <- create_dummy_qi_data(people = 120, seed = 7)
  anon <- create_dummy_qi_data(people = 120, seed = 7)
  set.seed(11)
  anon$AGE <- anon$AGE + sample(c(-2, 0, 2), nrow(anon), replace = TRUE)
  list(raw = raw, anon = anon)
}

cat("########## 1. NA in a numeric target ##########\n")
d <- base()
cat(sprintf("  reference (no NA): score_num AGE = %.4f\n",
            rate(score_num(join_raw_anon_data(d$raw, d$anon), "AGE"))))
for (k in c(1, 5, 20)) {
  d2 <- base(); d2$anon$AGE[seq_len(k)] <- NA
  p <- join_raw_anon_data(d2$raw, d2$anon)
  outcome(sprintf("score_num + reid_evaluate, %2d NA in ANON", k),
          rate(score_num(p, "AGE")))
  outcome(sprintf("score_num + match_greedy,  %2d NA in ANON", k),
          mean(match_greedy(score_num(p, "AGE"), seed = 1)$RESULT))
  outcome(sprintf("reid_by_num,               %2d NA in ANON", k),
          mean(reid_by_num(p, "AGE", seed = 1)$RESULT))
  outcome(sprintf("score_multi(AGE,ZIP,SEX),  %2d NA in ANON", k),
          rate(score_multi(p, c(AGE = "num", ZIP = "char", SEX = "char"), screen = "none")))
}

cat("\n########## 2. Inf / NaN in a numeric target ##########\n")
for (nm in c("Inf", "NaN", "1e308")) {
  d2 <- base(); d2$anon$AGE[1] <- as.numeric(nm)
  p <- join_raw_anon_data(d2$raw, d2$anon)
  outcome(sprintf("score_num AGE, one %-6s in ANON", nm), rate(score_num(p, "AGE")))
  outcome(sprintf("score_multi,      one %-6s in ANON", nm),
          rate(score_multi(p, c(AGE = "num", ZIP = "char"), screen = "none")))
}

cat("\n########## 3. duplicated ROW_NUMBER ##########\n")
d2 <- base(); d2$anon$ROW_NUMBER[2] <- d2$anon$ROW_NUMBER[1]
p <- join_raw_anon_data(d2$raw, d2$anon)
outcome("ANON has one duplicated ROW_NUMBER (score_num)", rate(score_num(p, "AGE")))
outcome("ANON has one duplicated ROW_NUMBER (multi)",
        rate(score_multi(p, c(AGE = "num", ZIP = "char", SEX = "char"), screen = "none")))
d3 <- base(); d3$raw$ROW_NUMBER[2] <- d3$raw$ROW_NUMBER[1]
p3 <- join_raw_anon_data(d3$raw, d3$anon)
outcome("RAW has one duplicated ROW_NUMBER (score_num)", rate(score_num(p3, "AGE")))
d4 <- base()
d4$anon$ROW_NUMBER <- rep(seq_len(60), each = 2)   # 60 distinct, each twice
p4 <- join_raw_anon_data(d4$raw, d4$anon)
outcome("ANON ROW_NUMBER collapsed 120 -> 60", rate(score_num(p4, "AGE")))

cat("\n########## 4. factor / character type mixing ##########\n")
d2 <- base(); d2$anon$ZIP <- factor(d2$anon$ZIP)
p <- join_raw_anon_data(d2$raw, d2$anon)
outcome("score_char ZIP with ANON as factor", rate(score_char(p, "ZIP")))
d2 <- base(); d2$anon$ROW_NUMBER <- factor(d2$anon$ROW_NUMBER)
p <- join_raw_anon_data(d2$raw, d2$anon)
outcome("ROW_NUMBER as factor on ANON side", rate(score_num(p, "AGE")))
d2 <- base(); d2$anon$ROW_NUMBER <- as.character(d2$anon$ROW_NUMBER)
p <- join_raw_anon_data(d2$raw, d2$anon)
outcome("ROW_NUMBER as character on ANON side", rate(score_num(p, "AGE")))

cat("\n########## 5. suppression: empty strings / masks ##########\n")
d <- base()
cat(sprintf("  reference: score_char ZIP = %.4f\n",
            rate(score_char(join_raw_anon_data(d$raw, d$anon), "ZIP"))))
for (mask in c("", "*", "****", "SUPPRESSED")) {
  d2 <- base(); d2$anon$ZIP[1:40] <- mask
  p <- join_raw_anon_data(d2$raw, d2$anon)
  outcome(sprintf("40/120 ZIP suppressed as %-12s (char)", sQuote(mask)),
          rate(score_char(p, "ZIP")))
  outcome(sprintf("40/120 ZIP suppressed as %-12s (idf) ", sQuote(mask)),
          rate(score_idf(p, "ZIP")))
}

cat("\n########## 6. NA in unicity ##########\n")
d <- base()
cat(sprintf("  unicity_fraction(raw, AGE+ZIP)          = %.4f\n",
            unicity_fraction(d$raw, c("AGE", "ZIP"))))
d2 <- base(); d2$raw$ZIP[1:40] <- NA
cat(sprintf("  ... with 40/120 ZIP = NA (suppressed)   = %.4f\n",
            unicity_fraction(d2$raw, c("AGE", "ZIP"))))
d3 <- base(); d3$raw$ZIP[1:40] <- ""
cat(sprintf("  ... with 40/120 ZIP = \"\" (suppressed)   = %.4f\n",
            unicity_fraction(d3$raw, c("AGE", "ZIP"))))

cat("\n########## 7. numeric column handed to score_char ##########\n")
d <- base()
p <- join_raw_anon_data(d$raw, d$anon)
outcome("score_num  AGE (correct)", rate(score_num(p, "AGE")))
outcome("score_char AGE (misuse, no error?)", rate(score_char(p, "AGE")))
outcome("score_num  SPEND_MEAN (correct)", rate(score_num(p, "SPEND_MEAN")))
outcome("score_char SPEND_MEAN (misuse)", rate(score_char(p, "SPEND_MEAN")))
outcome("score_idf  AGE (misuse)", rate(score_idf(p, "AGE")))
