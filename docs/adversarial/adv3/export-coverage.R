## For each exported function: how many times is it called directly from the
## test suite, and does its man page carry a runnable \examples block?
setwd(normalizePath(commandArgs(trailingOnly = TRUE)[1]))
ns <- readLines("NAMESPACE", warn = FALSE)
exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))

tfiles <- list.files("tests/testthat", pattern = "^test-", full.names = TRUE)
calls <- setNames(integer(length(exports)), exports)
where <- setNames(vector("list", length(exports)), exports)
for (f in tfiles) {
  p <- tryCatch(parse(f, keep.source = TRUE), error = function(e) NULL)
  if (is.null(p)) next
  pd <- utils::getParseData(p)
  ## SYMBOL too: do.call(reid_knowledge_curve, ...) is a SYMBOL, not a call
  fc <- pd$text[pd$token %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL")]
  tb <- table(fc)
  for (e in exports) if (!is.na(tb[e])) {
    calls[e] <- calls[e] + tb[e]
    where[[e]] <- c(where[[e]], basename(f))
  }
}

## man page examples
man_ex <- setNames(rep(NA_character_, length(exports)), exports)
for (e in exports) {
  rd <- file.path("man", paste0(e, ".Rd"))
  if (!file.exists(rd)) { man_ex[e] <- "NO .Rd"; next }
  txt <- paste(readLines(rd, warn = FALSE), collapse = "\n")
  man_ex[e] <- if (grepl("\\\\examples", txt)) {
    if (grepl("dontrun|donttest", txt)) "examples(dontrun)" else "examples"
  } else "no examples"
}

d <- data.frame(fn = exports, test_calls = as.integer(calls),
                n_files = vapply(where, function(x) length(unique(x)), 1L),
                man = man_ex, stringsAsFactors = FALSE)
d <- d[order(d$test_calls), ]
print(d, row.names = FALSE)
cat("\n--- exports with 0 direct test calls ---\n")
print(d$fn[d$test_calls == 0])
cat("\n--- exports with 1-3 direct test calls ---\n")
print(d$fn[d$test_calls > 0 & d$test_calls <= 3])
