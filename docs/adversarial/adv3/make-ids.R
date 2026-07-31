setwd(normalizePath(commandArgs(trailingOnly = TRUE)[1]))
d <- read.delim("docs/adversarial/mutants.tsv", stringsAsFactors = FALSE)
set.seed(1)
sel <- d$id[d$kind %in% c("relational/logical", "function swap")]
other <- d$id[d$kind %in% c("numeric+1", "bool flip")]
sel2 <- sort(sample(other, 200))
ids <- sort(c(sel, sel2))
## interleave so every worker gets a mix of files
writeLines(as.character(ids), "docs/adversarial/ids.txt")
cat("selected:", length(ids), "\n")
print(table(d$kind[d$id %in% ids]))
