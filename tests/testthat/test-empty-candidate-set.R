## Tests for Issues #101 and #109.
##
## Both issues are the same shape, and it is the shape docs/lessons-learned.md
## section 2 warns about: the measurement stops working and the output is a low
## number, which nobody questions. #101 is a candidate set that is *empty* --
## every RAW record excluded -- while the candidate table keeps its full shape,
## so `blocked`, `n_true_missing` and `truth_coverage` all report a healthy
## join. #109 is a hierarchy that was passed but could never apply, so the
## output of "hierarchy given" and "hierarchy not given" is identical.
##
## Every test here is written so that it FAILS on the code before the fix. The
## before-and-after numbers are in docs/investigation/empty-candidate-set-log.

SUP_SAI <- intToUtf8(0x6B73)                  # 歳    "years old"
SUP_IJOU <- intToUtf8(c(0x4EE5, 0x4E0A))      # 以上  "or more"
SUP_MIMAN <- intToUtf8(c(0x672A, 0x6E80))     # 未満  "under"
SUP_FUMEI <- intToUtf8(c(0x4E0D, 0x660E))     # 不明  "unknown"

## Each test file gets its own environment, so this is a local copy of the
## fixture test-generalize.R uses.
ascii_area <- function() {
  generalization_hierarchy(data.frame(
    attribute = "AREA",
    value = c("chiyoda", "shinjuku", "yokohama", "kawasaki",
              "osaka_city", "tokyo", "kanagawa", "osaka"),
    parent = c("tokyo", "tokyo", "kanagawa", "kanagawa",
               "osaka", "kanto", "kanto", "kinki"),
    stringsAsFactors = FALSE
  ))
}

## RAW ages, ANON the 10-year band each falls in, written the Japanese way.
banded_fixture <- function(raw_fmt = as.character, marker = NULL,
                           ages = c(21, 24, 33, 37, 38, 52)) {
  lo <- (ages %/% 10) * 10
  band <- paste0(lo, SUP_SAI, SUP_IJOU, lo + 10, SUP_SAI, SUP_MIMAN)
  if (!is.null(marker)) {
    band[1] <- marker
  }
  join_raw_anon_data(
    data.frame(ROW_NUMBER = seq_along(ages), AGE = raw_fmt(ages),
               stringsAsFactors = FALSE),
    data.frame(ROW_NUMBER = seq_along(ages), AGE = band,
               stringsAsFactors = FALSE)
  )
}

## ---------------------------------------------------------------------------
## #101 (1): the empty candidate set is announced
## ---------------------------------------------------------------------------

test_that("score_containment warns when an ANON record has no candidate left", {
  ## "tokyo" is not a region, is not in any hierarchy, and equals no RAW value,
  ## so it excludes everybody. Before the fix this returned silently.
  raw <- data.frame(ROW_NUMBER = 1:3, AREA = c("chiyoda", "shinjuku", "osaka"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:3, AREA = c("tokyo", "tokyo", "osaka"),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  expect_warning(score_containment(d, "AREA"), "EMPTY candidate set")
  expect_warning(score_containment(d, "AREA"), "2 of 3 ANON record")

  ## and it says which published value did it, and how that value was read
  expect_warning(score_containment(d, "AREA"),
                 "\"tokyo\" \\(not readable as a region")
})

test_that("the warning does not fire on a release that matches", {
  expect_silent(score_containment(banded_fixture(), "AGE"))
  expect_equal(sum(attr(score_containment(banded_fixture(), "AGE"),
                        "candidate_count") == 0), 0L)
})

test_that("the empty candidate set is invisible to every other guard", {
  ## This is the reason a separate check was needed at all: an excluded
  ## candidate keeps its row and scores 1, so the candidate table is still the
  ## complete cross join and still holds every true pair.
  d <- banded_fixture(function(a) paste0(a, "!"))
  s <- suppressWarnings(score_containment(d, "AGE"))
  ev <- reid_evaluate(s, seeds = 1:5, top_k = 1)

  expect_false(ev$blocked)
  expect_equal(ev$n_true_missing, 0L)
  expect_equal(ev$truth_coverage, 1)
  ## ... while every candidate set is in fact empty
  expect_equal(ev$n_zero_candidate, 6L)
})

## ---------------------------------------------------------------------------
## #101 (2): reid_evaluate() reads the count and prints it
## ---------------------------------------------------------------------------

test_that("reid_evaluate reports n_zero_candidate and prints it", {
  d <- banded_fixture(function(a) paste0(a, "!"))
  s <- suppressWarnings(score_containment(d, "AGE"))
  ev <- reid_evaluate(s, seeds = 1:5, top_k = 1)

  expect_equal(ev$n_zero_candidate, 6L)
  out <- paste(capture.output(print(ev)), collapse = "\n")
  expect_match(out, "EMPTY candidate set")
  expect_match(out, "6/6")
  expect_match(out, "cannot differ from the random baseline")

  ## and the reported rate really is the random baseline, which is what the
  ## printed line is warning about
  expect_equal(ev$success_analytic, ev$baseline$rate[ev$baseline$method == "random"])
})

test_that("n_zero_candidate is NA when the score table carries no count", {
  raw <- data.frame(ROW_NUMBER = 1:4, V = c(1, 2, 3, 4))
  ev <- reid_evaluate(score_num(join_raw_anon_data(raw, raw), "V"),
                      seeds = 1:5, top_k = 1)
  expect_true(is.na(ev$n_zero_candidate))
  expect_false(grepl("EMPTY candidate set",
                     paste(capture.output(print(ev)), collapse = "\n")))
})

## ---------------------------------------------------------------------------
## #101 (3): the diagnosis says how the value was read
## ---------------------------------------------------------------------------

test_that("the warning distinguishes an unreadable value from a readable one", {
  ## a value that parses as an interval but happens to contain nobody
  raw <- data.frame(ROW_NUMBER = 1:2, AGE = c(21, 24))
  anon <- data.frame(ROW_NUMBER = 1:2, AGE = c("[20,30)", "[80,90)"),
                     stringsAsFactors = FALSE)
  expect_warning(score_containment(join_raw_anon_data(raw, anon), "AGE"),
                 "\"\\[80,90\\)\" \\(read as an interval\\)")

  ## ... and one that never parsed, so it was compared as a literal string
  anon2 <- data.frame(ROW_NUMBER = 1:2, AGE = c("[20,30)", "over eighty"),
                      stringsAsFactors = FALSE)
  expect_warning(score_containment(join_raw_anon_data(raw, anon2), "AGE"),
                 "compared as a literal string")
})

## ---------------------------------------------------------------------------
## #101 (4): the suppression markers a release actually writes
## ---------------------------------------------------------------------------

test_that("the usual suppression markers are read as wildcards", {
  markers <- c("-", "--", "?", "N/A", "n/a", "N.A.", ".", "null", "NULL",
               "unknown", "Unknown", "missing", "no data", "not available",
               SUP_FUMEI, intToUtf8(c(0x975E, 0x516C, 0x958B)))
  expect_true(all(is_generalization_wildcard(markers)))

  ## a fullwidth dash is a dash
  expect_true(is_generalization_wildcard(intToUtf8(0xFF0D)))
})

test_that("a numeric sentinel and a bare NA string are NOT wildcards", {
  ## Both would match every RAW record, which enlarges candidate sets and
  ## lowers the reported risk -- the direction this package exists to catch.
  ## "NA" in any casing is 4 of the 3844 two-character strings over
  ## [A-Za-z0-9]; see docs/investigation/empty-candidate-set-benchmark.R.
  expect_false(any(is_generalization_wildcard(
    c("999", "-1", "0", "NA", "na", "Na", "none", "None", "M", "F", "30s")
  )))
})

test_that("a marked-up record keeps its candidates instead of losing them", {
  for (m in c("-", "N/A", "unknown", SUP_FUMEI)) {
    d <- banded_fixture(marker = m)
    cc <- containment_counts(d, "AGE")
    ## the suppressed record is now compatible with the whole file ...
    expect_equal(cc$N_CONTAINED[cc$ANON_ROW_NUMBER == 1], 6,
                 info = m)
    ## ... rather than with nobody, and its truth is inside its own region
    expect_true(all(cc$TRUTH_CONTAINED), info = m)
  }
})

## ---------------------------------------------------------------------------
## #101 (5): the RAW side goes through the same parser as the ANON side
## ---------------------------------------------------------------------------

test_that("a unit on the RAW side is read, not silently dropped", {
  expect_true(node_matches(paste0("37", SUP_SAI), "[30,40)"))
  expect_false(node_matches(paste0("47", SUP_SAI), "[30,40)"))
  expect_true(node_matches("30kg", "[25,35)"))
  expect_true(node_matches(intToUtf8(c(0xFF13, 0xFF17)), "[30,40)"))
})

test_that("a RAW range is not widened into a region", {
  ## A RAW value is a value. Reading "30-39" as a region would enlarge the
  ## candidate set, which is the direction that flatters the release.
  expect_false(node_matches("30-39", "[30,40)"))
  expect_false(node_matches("30s", "[30,40)"))
})

test_that("writing the unit on the RAW side does not change the answer", {
  ## The property #101 is really about: two spellings of the same file must
  ## report the same risk. Before the fix the second one reported the random
  ## baseline.
  plain <- suppressWarnings(score_containment(banded_fixture(), "AGE"))
  with_unit <- suppressWarnings(score_containment(
    banded_fixture(function(a) paste0(a, SUP_SAI)), "AGE"
  ))

  expect_equal(with_unit$SCORE, plain$SCORE)
  expect_equal(unname(attr(with_unit, "candidate_count")),
               unname(attr(plain, "candidate_count")))

  ev_plain <- reid_evaluate(plain, seeds = 1:5, top_k = 1)
  ev_unit <- reid_evaluate(with_unit, seeds = 1:5, top_k = 1)
  expect_equal(ev_unit$success_analytic, ev_plain$success_analytic)
  expect_gt(ev_unit$lift, 1)
})

## ---------------------------------------------------------------------------
## #109 (1): a hierarchy that could never apply is refused
## ---------------------------------------------------------------------------

test_that("a hierarchy whose attribute matches no target is an error", {
  h <- generalization_hierarchy(data.frame(
    attribute = "zip", value = c("1350041", "1350042"),
    parent = c("13500", "13500"), stringsAsFactors = FALSE
  ))
  raw <- data.frame(ROW_NUMBER = 1:2, ZIP = c("1350041", "1350042"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:2, ZIP = c("13500", "13500"),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  expect_error(score_containment(d, "ZIP", hierarchy = h), "\"zip\"")
  expect_error(score_containment(d, "ZIP", hierarchy = h),
               "differing only in case")
  expect_error(containment_counts(d, "ZIP", hierarchy = h),
               "none of which is a target column")

  ## the generator has always stopped on the same mismatch; the two sides
  ## disagreeing is what made the failure invisible
  expect_error(generalize_value(raw$ZIP, "ZIP", h), "no attribute \"ZIP\"")
})

test_that("a hierarchy covering only some of the targets is fine", {
  h <- ascii_area()
  raw <- data.frame(ROW_NUMBER = 1:2, AREA = c("chiyoda", "yokohama"),
                    SEX = c("M", "F"), stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:2, AREA = c("kanto", "kanto"),
                     SEX = c("M", "F"), stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  expect_silent(score_containment(d, c("AREA", "SEX"), hierarchy = h))
  expect_equal(containment_counts(d, c("AREA", "SEX"),
                                  hierarchy = h)$N_CONTAINED, c(1, 1))
})

test_that("the declared hierarchy changes the answer, so ignoring it mattered", {
  h <- ascii_area()
  raw <- data.frame(ROW_NUMBER = 1:3,
                    AREA = c("chiyoda", "yokohama", "osaka_city"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:3, AREA = c("kanto", "kanto", "kinki"),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  with_h <- reid_evaluate(score_containment(d, "AREA", hierarchy = h),
                          seeds = 1:5, top_k = 1)
  without <- reid_evaluate(suppressWarnings(score_containment(d, "AREA")),
                           seeds = 1:5, top_k = 1)
  expect_gt(with_h$success_analytic, without$success_analytic)
  expect_equal(with_h$n_zero_candidate, 0L)
  expect_equal(without$n_zero_candidate, 3L)
})

## ---------------------------------------------------------------------------
## #109 (2): rule = "auto" reads a mask as the mask it is
## ---------------------------------------------------------------------------

test_that("auto reads a suppression mask as a prefix", {
  expect_true(node_matches("1350041", "135****"))
  expect_false(node_matches("1600022", "135****"))
  ## a value with no trailing "*" is untouched: still a category
  expect_false(node_matches("1350041", "135"))
})

test_that("the default rules match a masked column as prefix does", {
  raw <- data.frame(ROW_NUMBER = 1:3,
                    ZIP = c("1350041", "1350055", "1600022"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:3,
                     ZIP = c("135****", "135****", "160****"),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  auto <- containment_counts(d, "ZIP")
  forced <- containment_counts(d, "ZIP", rules = c(ZIP = "prefix"))
  expect_equal(auto$N_CONTAINED, forced$N_CONTAINED)
  expect_equal(auto$N_CONTAINED, c(2, 2, 1))
  expect_true(all(auto$TRUTH_CONTAINED))

  ## and rule = "exact" still means exact, so the escape hatch is intact
  expect_equal(
    suppressWarnings(containment_counts(d, "ZIP",
                                        rules = c(ZIP = "exact")))$N_CONTAINED,
    c(0, 0, 0)
  )
})

test_that("reading the mask raises the reported risk, which is why it mattered", {
  set.seed(11)
  people <- 120
  zips <- paste0(sprintf("%05d", sample(10000:10023, people, replace = TRUE)),
                 sprintf("%02d", sample(0:99, people, replace = TRUE)))
  raw <- data.frame(ROW_NUMBER = seq_len(people), ZIP = zips,
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = raw$ROW_NUMBER,
                     ZIP = paste0(substr(zips, 1, 5), "**"),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  ev <- reid_evaluate(score_containment(d, "ZIP"), seeds = 1:5, top_k = 1)
  expect_gt(ev$lift, 2)
  expect_equal(ev$n_zero_candidate, 0L)
})
