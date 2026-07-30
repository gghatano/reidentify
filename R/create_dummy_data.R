## ダミーデータを作る(マスター)
#' create dummy master data
#'
#' @param people number of people
#'
#' @return a tibble with columns ROW_NUMBER, ID, NUM, BIN, CHAR containing
#'   `people` rows of randomly generated dummy master data.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr %>%
#' @importFrom stringi stri_rand_strings
#' @importFrom stats runif
#' @encoding UTF-8
#' @export
create_dummy_master_data <- function(people = 100) {
  ## error handling
  if (!is.numeric(people)) {
    stop("people is integer ( > 0)")
  } else if (people < 1) {
    stop("people is integer ( > 0)")
  }

  ROW_NUMBER <- 1:people
  RAW_ID <- ROW_NUMBER + 10000
  RAW_NUM <- runif(n = people)
  RAW_BIN <- sample(x = c(0, 1, 100), prob = c(20, 20, 1), size = people, replace = TRUE)
  RAW_CHAR <- stringi::stri_rand_strings(n = people, length = 2)

  dat_raw <- tibble::tibble(
    ROW_NUMBER = ROW_NUMBER,
    ID = RAW_ID,
    NUM = RAW_NUM,
    BIN = RAW_BIN,
    CHAR = RAW_CHAR
  )

  dat_raw %>% return()
}


#' create dummy transaction data
#'
#' @param people number of people
#' @param size mean record number
#'
#' @return a tibble with columns ROW_NUMBER, ID, NUM_STATIC, NUM_DYNAMIC,
#'   BIN, CHAR containing `people * size` rows of randomly generated dummy
#'   transaction data.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr %>%
#' @importFrom stringi stri_rand_strings
#' @importFrom stats runif
#' @examples
#' data_tran = create_dummy_transaction_data(people = 10, size = 4)
#' @export
#' @encoding UTF-8
create_dummy_transaction_data <- function(people = 100, size = 2) {

  if (!is.numeric(people)) {
    stop("people is integer ( > 0 )")
  } else if (!is.numeric(size)) {
    stop("size is integer ( > 0 )")
  } else if (people < 1) {
    stop("people is integer ( > 0 )")
  } else if (size <= 0) {
    stop("size is integer ( > 0 )")
  }

  row_num <- people * size

  ROW_NUMBER <- 1:row_num
  RAW_ID <- sample(x = 1:people, size = row_num, replace = TRUE)
  RAW_NUM_STATIC <- rep(10, row_num)
  RAW_NUM_DYNAMIC <- runif(n = row_num)
  RAW_BIN <- sample(x = c(0, 1, 100), prob = c(20, 20, 1), size = row_num, replace = TRUE)
  RAW_CHAR <- stringi::stri_rand_strings(n = row_num, length = 2)

  dat_raw <- tibble::tibble(
    ROW_NUMBER = ROW_NUMBER,
    ID = RAW_ID,
    NUM_STATIC = RAW_NUM_STATIC,
    NUM_DYNAMIC = RAW_NUM_DYNAMIC,
    BIN = RAW_BIN,
    CHAR = RAW_CHAR
  )

  dat_raw %>% return()
}


#' create dummy master data with an explicit quasi-identifier structure
#'
#' [create_dummy_master_data()] produces columns that are all equally
#' "identifying", so there is nothing for an attacker knowledge model
#' (Issue #13) to withhold. This generates data in which the columns fall into
#' the three groups the W / M / S levels distinguish, with deliberately
#' different discriminating power:
#'
#' \describe{
#'   \item{quasi-identifiers}{`ZIP` (very coarse -- roughly `people / 5`
#'     distinct values, so heavy collisions), `AGE` (moderately coarse) and
#'     `SEX` (2 values, almost no information on its own)}
#'   \item{behaviour}{`VISIT_COUNT`, `SPEND_MEAN` and the colon-joined
#'     `SPEND_DIST`, each a rounded summary rather than an exact value}
#'   \item{identifier}{`FINGERPRINT`, a continuous value that is unique with
#'     probability 1 -- i.e. the RAW record itself}
#' }
#'
#' A level-W attacker restricted to `ZIP` should therefore do only slightly
#' better than guessing, while a level-S attacker holding `FINGERPRINT`
#' should identify essentially everybody.
#'
#' @param people number of people
#' @param seed integer seed, or NULL to use the ambient RNG stream. Defaults
#'   to NULL so the function behaves like the other generators; pass a value
#'   to make a fixture self-contained.
#'
#' @return a tibble with columns ROW_NUMBER, ID, AGE, ZIP, SEX, VISIT_COUNT,
#'   SPEND_MEAN, SPEND_DIST and FINGERPRINT, one row per person.
#'
#' @seealso [dummy_qi_knowledge()], which declares the matching
#'   [attacker_knowledge()] specification for this data.
#'
#' @examples
#' create_dummy_qi_data(people = 10, seed = 1)
#'
#' @importFrom tibble tibble
#' @importFrom stats runif
#' @export
#' @encoding UTF-8
create_dummy_qi_data <- function(people = 100, seed = NULL) {
  if (!is.numeric(people) || length(people) != 1 || is.na(people) || people < 1) {
    stop("people is integer ( > 0)")
  }
  people <- as.integer(people)

  with_local_seed(seed, {
    n_zip <- max(2L, as.integer(ceiling(people / 5)))

    visit_count <- sample.int(20L, size = people, replace = TRUE)
    spend_dist <- vapply(
      visit_count,
      function(k) paste(round(runif(k, 0, 100)), collapse = ":"),
      character(1)
    )
    spend_mean <- vapply(
      strsplit(spend_dist, ":", fixed = TRUE),
      function(v) round(mean(as.numeric(v)), 1),
      numeric(1)
    )

    tibble::tibble(
      ROW_NUMBER = seq_len(people),
      ID = seq_len(people) + 10000L,
      AGE = sample(20:79, size = people, replace = TRUE),
      ZIP = sprintf("Z%03d", sample.int(n_zip, size = people, replace = TRUE)),
      SEX = sample(c("M", "F"), size = people, replace = TRUE),
      VISIT_COUNT = visit_count,
      SPEND_MEAN = spend_mean,
      SPEND_DIST = spend_dist,
      FINGERPRINT = runif(people)
    )
  })
}

#' the attacker knowledge specification that matches create_dummy_qi_data()
#'
#' @param level one of "W", "M", "S"
#' @param ... further arguments passed to [attacker_knowledge()]
#'
#' @return an [attacker_knowledge()] object for the columns
#'   [create_dummy_qi_data()] generates
#'
#' @examples
#' dummy_qi_knowledge("W")
#'
#' @export
dummy_qi_knowledge <- function(level = c("W", "M", "S"), ...) {
  attacker_knowledge(
    level,
    quasi_identifiers = c(ZIP = "char", AGE = "num", SEX = "char"),
    behavior = c(VISIT_COUNT = "num", SPEND_MEAN = "num", SPEND_DIST = "dist"),
    identifiers = c(FINGERPRINT = "num"),
    weak_subset = "ZIP",
    ...
  )
}

#' create reid-format data from raw and anon data frame
#'
#' @param raw  raw data frame
#' @param anon anonymized data frame
#' @param raw_header strings which is added for columns from raw data
#' @param anon_header strings which is added for columns from anon data
#'
#' @return a data frame: the cross join (every RAW row paired with every
#'   ANON row) of `raw` and `anon`, with their column names prefixed by
#'   `raw_header`/`anon_header` respectively.
#'
#' @importFrom dplyr %>%
#' @export
#' @encoding UTF-8
join_raw_anon_data <- function(raw, anon, raw_header = "RAW_", anon_header = "ANON_") {

  ## error handling
  if (is.data.frame(raw) + is.data.frame(anon) != 2) {
    stop("raw and anon are data frame")
  }

  ## convert column names
  names(raw) <- paste(raw_header, names(raw), sep = "")
  names(anon) <- paste(anon_header, names(anon), sep = "")

  ## cross join
  merge(raw, anon, all = TRUE) %>% return()
}
