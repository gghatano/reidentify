## Generate mutants by rewriting single tokens of R/*.R, using getParseData()
## so that comments and string literals are never touched.
## Output: docs/adversarial/mutants.rds + mutants.tsv
setwd(normalizePath(commandArgs(trailingOnly = TRUE)[1]))

files <- list.files("R", pattern = "[.]R$", full.names = TRUE)
files <- files[!grepl("reidentify-package[.]R$", files)]

## token-level swaps: token type -> named vector old text -> new text
swaps <- list(
  GT    = c(">"  = ">="),
  GE    = c(">=" = ">"),
  LT    = c("<"  = "<="),
  LE    = c("<=" = "<"),
  EQ    = c("==" = "!="),
  NE    = c("!=" = "=="),
  AND2  = c("&&" = "||"),
  OR2   = c("||" = "&&"),
  AND   = c("&"  = "|"),
  OR    = c("|"  = "&")
)

## function-name swaps (SYMBOL_FUNCTION_CALL)
fswap <- c(min = "max", max = "min", which.min = "which.max",
           which.max = "which.min", sum = "prod", any = "all", all = "any",
           mean = "median", median = "mean", ceiling = "floor",
           floor = "ceiling", sort = "rev", head = "tail", unique = "identity")

mut <- list()
add <- function(...) mut[[length(mut) + 1L]] <<- list(...)

for (f in files) {
  src <- readLines(f, warn = FALSE)
  p <- parse(f, keep.source = TRUE)
  pd <- utils::getParseData(p)
  pd <- pd[pd$terminal, ]
  for (i in seq_len(nrow(pd))) {
    r <- pd[i, ]
    if (r$line1 != r$line2) next
    tok <- r$text
    new <- NA_character_
    kind <- NA_character_
    if (r$token %in% names(swaps)) {
      s <- swaps[[r$token]]
      if (tok %in% names(s)) { new <- unname(s[tok]); kind <- "relational/logical" }
    } else if (r$token == "NUM_CONST") {
      v <- suppressWarnings(as.numeric(tok))
      if (!is.na(v) && !grepl("[LixX]", tok)) {
        new <- format(v + 1, scientific = FALSE); kind <- "numeric+1"
      }
    } else if (r$token == "SYMBOL_FUNCTION_CALL" && tok %in% names(fswap)) {
      new <- unname(fswap[tok]); kind <- "function swap"
    } else if (r$token == "NUM_CONST" ) {
    }
    if (is.na(new)) next
    ln <- r$line1
    line <- src[ln]
    ## rewrite by column position (1-based, byte cols from parse data)
    before <- substr(line, 1, r$col1 - 1L)
    after  <- substr(line, r$col2 + 1L, nchar(line))
    newline <- paste0(before, new, after)
    if (identical(newline, line)) next
    add(file = f, line = ln, kind = kind, tok = tok, new = new,
        orig_line = line, new_line = newline)
  }
  ## TRUE/FALSE constants
  for (i in seq_len(nrow(pd))) {
    r <- pd[i, ]
    if (r$line1 != r$line2) next
    if (!(r$token == "NUM_CONST" && r$text %in% c("TRUE", "FALSE"))) next
    new <- if (r$text == "TRUE") "FALSE" else "TRUE"
    line <- src[r$line1]
    newline <- paste0(substr(line, 1, r$col1 - 1L), new,
                      substr(line, r$col2 + 1L, nchar(line)))
    add(file = f, line = r$line1, kind = "bool flip", tok = r$text, new = new,
        orig_line = line, new_line = newline)
  }
}

df <- do.call(rbind, lapply(seq_along(mut), function(i) {
  m <- mut[[i]]
  data.frame(id = i, file = m$file, line = m$line, kind = m$kind,
             tok = m$tok, new = m$new, stringsAsFactors = FALSE)
}))
saveRDS(mut, "docs/adversarial/mutants.rds")
write.table(df, "docs/adversarial/mutants.tsv", sep = "\t",
            row.names = FALSE, quote = FALSE)
cat("total mutants:", length(mut), "\n")
print(table(df$kind))
print(table(basename(df$file)))
