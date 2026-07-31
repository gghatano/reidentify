## Is the surviving top_k_candidates() mutant (head(runs,-1) -> tail(runs,-1))
## an EQUIVALENT mutant, or a genuine coverage hole?  It is equivalent exactly
## when every ANON record has the same number of candidates -- which is true of
## a full cross join and false of any blocked candidate set.
root <- normalizePath(commandArgs(trailingOnly = TRUE)[1], winslash = "/")
work <- file.path(tempdir(), "topk-demo")
unlink(work, recursive = TRUE); dir.create(work, recursive = TRUE)
for (d in c("R", "man", "inst")) file.copy(file.path(root, d), work, recursive = TRUE)
for (f in c("DESCRIPTION", "NAMESPACE")) file.copy(file.path(root, f), work, overwrite = TRUE)

mk <- function() {
  set.seed(7)
  n <- 60
  raw <- data.frame(ROW_NUMBER = 1:n,
                    ZIP = sprintf("Z%02d", c(rep(1, 30), rep(2, 20), rep(3, 10))),
                    V = runif(n), stringsAsFactors = FALSE)
  anon <- raw; anon$V <- raw$V + rnorm(n, sd = 0.01)
  list(raw = raw, anon = anon)
}

run <- function() {
  suppressMessages(pkgload::load_all(work, quiet = TRUE, export_all = FALSE))
  fx <- mk()
  ## blocked candidate set: group sizes 30x30, 20x20, 10x10 -> UNBALANCED
  cand <- suppressWarnings(block_candidates(fx$raw, fx$anon, keys = "ZIP"))
  s <- score_num(cand, "V")
  tk <- suppressWarnings(top_k_candidates(s, k = 3))
  b <- attr(tk, "blocking")
  list(kept = nrow(tk), recall = b$recall,
       nocand = b$n_anon_without_candidate,
       sizes = as.integer(table(table(s$ANON_ROW_NUMBER))))
}

cat("candidate-set group sizes present in this fixture (unbalanced):\n")
r1 <- run()
cat(sprintf("ORIGINAL  top_k_candidates(k=3): kept %d pair(s), recall %.4f, ANON with no candidate %d\n",
            r1$kept, r1$recall, r1$nocand))

p <- file.path(work, "R/blocking.R"); x <- readLines(p, warn = FALSE)
i <- grep("starts <- cumsum\\(c\\(0L, utils::head\\(runs, -1L\\)\\)\\)", x)
x[i] <- sub("utils::head", "utils::tail", x[i]); writeLines(x, p)
r2 <- run()
cat(sprintf("MUTANT    top_k_candidates(k=3): kept %d pair(s), recall %.4f, ANON with no candidate %d\n",
            r2$kept, r2$recall, r2$nocand))
cat("\nidentical? ", identical(r1[c("kept","recall","nocand")],
                              r2[c("kept","recall","nocand")]), "\n")

cat("\n-- for contrast, the shape every test and every README example uses --\n")
x[i] <- sub("utils::tail", "utils::head", x[i]); writeLines(x, p)
suppressMessages(pkgload::load_all(work, quiet = TRUE, export_all = FALSE))
fx <- mk(); full <- join_raw_anon_data(fx$raw, fx$anon); sf <- score_num(full, "V")
cat("full cross join: candidates per ANON record =",
    paste(unique(as.integer(table(sf$ANON_ROW_NUMBER))), collapse = ","),
    "(all equal -> head/tail agree)\n")
