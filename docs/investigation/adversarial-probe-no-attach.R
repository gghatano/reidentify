## 敵対的検証 1: library(reidentify) だけで各関数が動くか
## (スモークテストは dplyr/magrittr を attach していたので疑わしい)

library(reidentify)

raw <- data.frame(ROW_NUMBER = 1:5, VAL = c(1, 2, 3, 4, 5), TXT = c("aa", "bb", "cc", "dd", "ee"),
                  D = c("1:2", "2:3", "3:4", "4:5", "5:6"), stringsAsFactors = FALSE)
anon <- data.frame(ROW_NUMBER = 1:5, VAL = c(1.1, 2.1, 3.1, 4.1, 5.1), TXT = c("aa", "bb", "cc", "dd", "ex"),
                   D = c("1:2.1", "2:3.1", "3:4.1", "4:5.1", "5:6.1"), stringsAsFactors = FALSE)

probe <- function(label, expr) {
  out <- tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)))
  if (is.character(out) && length(out) == 1 && grepl("^ERROR:", out)) {
    cat(sprintf("[FAIL] %-22s %s\n", label, out))
  } else {
    cat(sprintf("[ok  ] %-22s\n", label))
  }
  invisible(out)
}

dra <- probe("join_raw_anon_data", join_raw_anon_data(raw, anon))

probe("reid_by_num", reid_by_num(dra, "VAL"))
probe("reid_by_num_rank", reid_by_num_rank(dra, "VAL"))
probe("reid_by_char", reid_by_char(dra, "TXT"))
probe("reid_by_dist", reid_by_dist(dra, "D"))
probe("create_dummy_master_data", create_dummy_master_data(5))
probe("create_dummy_transaction_data", create_dummy_transaction_data(5, 2))
probe("transform_transaction_to_master", transform_transaction_to_master(
  create_dummy_transaction_data(5, 2), STATIC_NUM = "NUM_STATIC", DYNAMIC_NUM = "NUM_DYNAMIC", DYNAMIC_CHAR = "CHAR"))

## reid_result は上流が動いた前提が要るので dplyr を足して単体で
r <- suppressWarnings(tryCatch(reid_by_num(dra, "VAL"), error = function(e) NULL))
if (!is.null(r)) probe("reid_result", reid_result(r, method = "x"))
