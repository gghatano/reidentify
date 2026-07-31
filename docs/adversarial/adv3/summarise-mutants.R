setwd(normalizePath(commandArgs(trailingOnly = TRUE)[1]))
fs <- Sys.glob("docs/adversarial/mut-out-*.tsv")
fs <- fs[file.size(fs) > 0]
r <- do.call(rbind, lapply(fs, function(f)
  read.delim(f, header = FALSE, quote = "", stringsAsFactors = FALSE,
             col.names = c("id", "status", "by", "secs", "file", "line",
                           "kind", "tok", "new"))))
cat("mutants run:", nrow(r), "of", length(readLines("docs/adversarial/ids.txt")),
    "selected\n\n")
tb <- table(basename(r$file), r$status)
score <- round(100 * tb[, "KILLED"] / rowSums(tb), 1)
out <- data.frame(file = rownames(tb), killed = tb[, "KILLED"],
                  survived = tb[, "SURVIVED"], kill_pct = score)
print(out[order(out$kill_pct), ], row.names = FALSE)
cat("\noverall kill rate:",
    round(100 * sum(r$status == "KILLED") / nrow(r), 1), "%\n")
cat("\nby mutation kind:\n")
print(table(r$kind, r$status))
cat("\ntest files that did the killing (top 15):\n")
print(head(sort(table(r$by[r$status == "KILLED"]), decreasing = TRUE), 15))
cat("\ntest files that never killed anything so far:\n")
all_t <- basename(list.files("tests/testthat", pattern = "^test-"))
print(setdiff(all_t, unique(r$by)))
