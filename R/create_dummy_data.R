## ダミーデータを作る(マスター)
#' create dummy master data
#'
#' @param people number of people
#'
#' @return a tibble with columns ROW_NUMBER, ID, NUM, BIN, CHAR containing
#'   `people` rows of randomly generated dummy master data.
#'
#' @examples
#' # the values are drawn at random, so set a seed to get a fixed table
#' set.seed(1)
#' create_dummy_master_data(people = 5)
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
#' @section Spatio-temporal columns:
#'
#' With `spatiotemporal = TRUE` two further columns are appended, so that
#' [spatiotemporal_unicity()] (Issue #24) has something with the shape of a
#' mobility trace to measure:
#'
#' \describe{
#'   \item{`PLACE`}{a zero-padded location code, `"P001"` upwards. Codes are
#'     assigned so that **sort order tracks proximity** -- `"P004"` is next to
#'     `"P005"` -- because that is what [coarsen_place()] assumes when it
#'     merges neighbouring locations into a coarser grid.}
#'   \item{`TIME`}{hours since the start of the observation window, so a
#'     `time_resolution` of 1 means hourly and 24 means daily.}
#' }
#'
#' Each person is given a home location, a small repertoire of places around
#' it, and a preferred hour of the day; events are drawn from those habits.
#' Without per-person structure every trace would be a uniform draw from the
#' same pool and the unicity curve would say nothing about the method.
#'
#' The extra columns are **off by default** so that the returned schema is
#' unchanged for existing callers, and they are drawn *after* the original
#' columns, so a given seed produces the same `NUM_STATIC` / `NUM_DYNAMIC` /
#' `BIN` / `CHAR` either way.
#'
#' @param people number of people
#' @param size mean record number
#' @param spatiotemporal add the `PLACE` and `TIME` columns described above
#'   (default FALSE)
#' @param places number of distinct locations, used only when
#'   `spatiotemporal = TRUE` (default 50)
#' @param days length of the observation window in days, used only when
#'   `spatiotemporal = TRUE` (default 30)
#' @param seed integer seed, or NULL (the default) to use the ambient RNG
#'   stream as the function always has
#'
#' @return a tibble with columns ROW_NUMBER, ID, NUM_STATIC, NUM_DYNAMIC,
#'   BIN, CHAR -- plus PLACE and TIME when `spatiotemporal = TRUE` --
#'   containing `people * size` rows of randomly generated dummy
#'   transaction data.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr %>%
#' @importFrom stringi stri_rand_strings
#' @importFrom stats runif rnorm
#' @examples
#' data_tran = create_dummy_transaction_data(people = 10, size = 4)
#' create_dummy_transaction_data(people = 10, size = 4, spatiotemporal = TRUE, seed = 1)
#' @export
#' @encoding UTF-8
create_dummy_transaction_data <- function(people = 100, size = 2,
                                          spatiotemporal = FALSE,
                                          places = 50, days = 30,
                                          seed = NULL) {

  if (!is.numeric(people)) {
    stop("people is integer ( > 0 )")
  } else if (!is.numeric(size)) {
    stop("size is integer ( > 0 )")
  } else if (people < 1) {
    stop("people is integer ( > 0 )")
  } else if (size <= 0) {
    stop("size is integer ( > 0 )")
  }
  if (!is.numeric(places) || length(places) != 1 || is.na(places) || places < 1) {
    stop("places is integer ( > 0 )")
  }
  if (!is.numeric(days) || length(days) != 1 || is.na(days) || days < 1) {
    stop("days is integer ( > 0 )")
  }

  row_num <- people * size

  with_local_seed(seed, {
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

    ## Everything above is drawn exactly as it always was, and the branch below
    ## only ever adds draws at the end, so switching `spatiotemporal` on cannot
    ## change the columns that were there before.
    if (isTRUE(spatiotemporal)) {
      places <- as.integer(places)
      days <- as.integer(days)

      home <- sample.int(places, people, replace = TRUE)
      ## How far each person strays from home, in location codes. A mixture, so
      ## the population contains both people who never leave their block and
      ## people who move across the whole area.
      spread <- sample(c(1L, 3L, 10L), people, replace = TRUE, prob = c(3, 2, 1))
      favourite_hour <- sample.int(24L, people, replace = TRUE) - 1L

      pid <- RAW_ID
      offset <- round(stats::rnorm(row_num, sd = spread[pid]))
      ## Wrapped rather than clamped: clamping would pile everybody who lives
      ## near an edge onto the same few codes and invent collisions the model
      ## does not intend.
      place_code <- ((home[pid] + offset - 1L) %% places) + 1L

      day <- sample.int(days, row_num, replace = TRUE)
      hour <- (favourite_hour[pid] + round(stats::rnorm(row_num, sd = 2))) %% 24L

      dat_raw$PLACE <- sprintf("P%03d", place_code)
      dat_raw$TIME <- (day - 1L) * 24L + hour
    }

    dat_raw
  }) %>% return()
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
#' @examples
#' raw  <- data.frame(ROW_NUMBER = 1:3, V = c(10, 20, 30))
#' anon <- data.frame(ROW_NUMBER = 1:3, V = c(11, 19, 32))
#'
#' # every RAW row is paired with every ANON row: 3 x 3 = 9 candidate pairs.
#' # This is the shape the score_*() / reid_by_*() functions expect.
#' d <- join_raw_anon_data(raw, anon)
#' dim(d)
#' head(d, 3)
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
