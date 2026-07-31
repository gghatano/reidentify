# Does README mention function names that are NOT exported?
# The harness header (docs/verify-readme-examples.R, step 4) claims it checks
# "that the README names no function that is not exported" -- the code does not.
txt <- readLines("README.md", warn = FALSE, encoding = "UTF-8")
ns <- readLines("NAMESPACE", warn = FALSE)
exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
cand <- unique(unlist(regmatches(
  txt, gregexpr("[A-Za-z._][A-Za-z0-9._]*(?=\\()", txt, perl = TRUE))))
cand <- setdiff(cand, exports)
suppressMessages(pkgload::load_all(".", quiet = TRUE, export_all = FALSE))
known <- vapply(cand, function(f) exists(f, mode = "function"), logical(1))
rest <- cand[!known]
internal <- vapply(rest, function(f)
  exists(f, envir = asNamespace("reidentify"), mode = "function"), logical(1))
cat("=== README names used as f(...) that resolve NOWHERE ===\n")
print(unname(rest[!internal]))
cat("\n=== README names that are reidentify-INTERNAL (not exported) ===\n")
print(unname(rest[internal]))
