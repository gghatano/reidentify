## Development scratch script -- a manual smoke test of the whole pipeline
## (dummy transaction data -> master -> join -> each reid_by_* method).
## Not part of the package: R/tmp/ is excluded by .Rbuildignore, so nothing
## here is built, installed or checked. Run it with
##
##   Rscript R/tmp/dev.R
##
## from the repository root.

## Load the package *from this working tree*, not from the library. A stale
## `reidentify` is very likely to be installed, and `library(reidentify)`
## would silently exercise that copy instead of the code being edited.
local({
  here <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NULL)
  root <- if (is.null(here)) "." else file.path(here, "..", "..")
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists(file.path(root, "DESCRIPTION"))) {
    pkgload::load_all(root, quiet = TRUE)
  } else {
    library(reidentify)
  }
})
library(dplyr)
library(magrittr)

set.seed(71)
dat_anon <- create_dummy_transaction_data(100)
dat_raw <- dat_anon %>%
  dplyr::mutate(ID = ID + 1) %>%
  dplyr::mutate(NUM_DYNAMIC = NUM_DYNAMIC + runif(nrow(.)) * 0.1)

dat_anon %>% head()
dat_raw %>% head()

## transform transaction to master test
dat <- create_dummy_transaction_data(100)
dat %<>% dplyr::mutate(NUM_STATIC_2 = NUM_STATIC + 1)
dat %<>% dplyr::mutate(NUM_DYNAMIC_2 = NUM_DYNAMIC + 1)
dat %<>% dplyr::mutate(CHAR_STATIC = paste("CHAR", ID, sep = ""))
dat %>% dim()
dat$ID %>%
  table() %>%
  length()
dat_master <- transform_transaction_to_master(dat,
  ROW_NUMBER = "ROW_NUMBER",
  STATIC_NUM = c("NUM_STATIC", "NUM_STATIC_2"),
  DYNAMIC_NUM = c("BIN", "NUM_DYNAMIC", "NUM_DYNAMIC_2"),
  STATIC_CHAR = c("CHAR_STATIC"),
  DYNAMIC_CHAR = c("CHAR")
)
dat_master %>% arrange(ID)
dat_master %>% dim()
dat_master$ID %>%
  table() %>%
  length()
dat_master %>%
  arrange(ID) %>%
  dplyr::select(ID, "CHAR_DIST") %>%
  distinct() %>%
  dim()

dat_master %>% dim()
dat_master %>%
  as.data.frame() %>%
  head()
## written under tempdir() so that running this script does not drop a file
## into the repository root
out_csv <- file.path(tempdir(), "dat_master.csv")
dat_master %>% write.csv(out_csv, quote = FALSE, row.names = FALSE)
cat("wrote", out_csv, "\n")

dat_master$ROW_NUMBER %>% table()
dat2_master <- dat_master %>%
  mutate(NUM_DYNAMIC_MEAN = NUM_DYNAMIC_MEAN + runif(nrow(.)) * 0.1) %>%
  mutate(NUM_STATIC_2 = NUM_STATIC_2 + runif(nrow(.)) * 1) %>%
  mutate(ID = paste("ID_", ID, sep = ""))

## NB: the function is join_raw_anon_data() -- this line used to say
## join_row_anon_data() ("row" for "raw"), so the script could not run at
## all past this point (Issue #27).
dat_raw_anon <- join_raw_anon_data(dat_master, dat2_master)

## 分布間の距離
## (was commented out because the line above errored; re-enabled and
## verified to run)
result_dist <- reid_by_dist(dat_raw_anon = dat_raw_anon, target = "NUM_DYNAMIC_DIST")
reid_result(dat_reid_result = result_dist, method = "dist") %>% print()

## 文字列の一致度合い
result_char <- reid_by_char(dat_raw_anon = dat_raw_anon, target = "CHAR_STATIC")
reid_result(dat_reid_result = result_char, method = "CHAR") %>% print()

## 順位でマッチング
result_num_rank <- reid_by_num_rank(dat_raw_anon = dat_raw_anon, target = "ID")
result_num_rank %>%
  reid_result(method = "rank of num static max") %>%
  print()
result_num_rank$ANON_RANK %>% table()

## 数値の距離
result_num <- reid_by_num(dat_raw_anon = dat_raw_anon, target = "NUM_DYNAMIC_MEAN")
result_num
reid_result(dat_reid_result = result_num, method = "NUM_DYNAMIC_MEAN") %>% print()
