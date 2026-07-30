## Tests for generalisation / containment matching (Issue #20).
##
## The fixtures here are deliberately ASCII: the shipped example hierarchy is
## in Japanese, and the tests that use it read the values out of the file
## rather than repeating them, so this file stays readable in any locale.

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

ascii_age <- function() {
  generalization_hierarchy(data.frame(
    attribute = "AGE",
    value = c("[30,35)", "[35,40)", "[40,45)", "[45,50)"),
    parent = c("[30,40)", "[30,40)", "[40,50)", "[40,50)"),
    stringsAsFactors = FALSE
  ))
}

iv_str <- function(x) {
  iv <- parse_generalized_interval(x)
  if (is.null(iv)) {
    return(NA_character_)
  }
  sprintf("%s%s,%s%s",
          if (iv$lower_closed) "[" else "(",
          format(iv$lower), format(iv$upper),
          if (iv$upper_closed) "]" else ")")
}

## ---------------------------------------------------------------------------
## interval parsing
## ---------------------------------------------------------------------------

test_that("bracket intervals parse, including the open/closed endpoints", {
  expect_equal(iv_str("[30,40)"), "[30,40)")
  expect_equal(iv_str("(30,40]"), "(30,40]")
  expect_equal(iv_str("[30,40]"), "[30,40]")
  expect_equal(iv_str("(30,40)"), "(30,40)")
  expect_equal(iv_str("[ 30 , 40 )"), "[30,40)")
  expect_equal(iv_str("[-10,5)"), "[-10,5)")
  expect_equal(iv_str("[2.5,7.5)"), "[2.5,7.5)")
})

test_that("an omitted bracket endpoint is infinite", {
  expect_equal(iv_str("[65,)"), "[65,Inf)")
  expect_equal(iv_str("(,20]"), "(-Inf,20]")
})

test_that("regression: the bracket class is not swallowed by the regex engine", {
  # "[\\]\\)]" would close the bracket expression at the first "]" under R's
  # default TRE engine, so every bracket interval silently fell through to the
  # categorical reading -- and an interval read as a category matches nothing,
  # which reports a safe data set.
  expect_false(is.null(parse_generalized_interval("[30,40)")))
  expect_true(node_matches("35", "[30,40)"))
  expect_false(node_matches("45", "[30,40)"))
})

test_that("decade and bare-number forms parse", {
  expect_equal(iv_str("30s"), "[30,40)")
  expect_equal(iv_str("35"), "[35,35]")
  expect_equal(iv_str("35.5"), "[35.5,35.5]")
})

test_that("dash and wave ranges parse inclusively", {
  expect_equal(iv_str("30-39"), "[30,39]")
  expect_equal(iv_str("30~39"), "[30,39]")
  expect_equal(iv_str("1000-1999"), "[1000,1999]")
})

test_that("open-ended dash ranges parse", {
  expect_equal(iv_str("30-"), "[30,Inf)")
  expect_equal(iv_str("~39"), "(-Inf,39]")
})

test_that("a leading minus is read as a negative number, not as 'up to'", {
  # documented, and deliberate: "-39" is ambiguous, and the number reading is
  # the one that cannot silently swallow a whole column
  expect_equal(iv_str("-39"), "[-39,-39]")
})

test_that("categorical values are not mistaken for intervals", {
  for (v in c("tokyo", "M", "F", "3S", "Z001", "abc", "a-b", "*", "")) {
    expect_true(is.na(iv_str(v)), info = v)
  }
})

test_that("only known units are stripped, so a code is not read as a number", {
  # "30kg" is 30; "30S" is a category, because S is not a unit
  expect_equal(iv_str("30kg"), "[30,30]")
  expect_true(is.na(iv_str("30S")))
  expect_equal(iv_str("30-39kg"), "[30,39]")
})

## ---------------------------------------------------------------------------
## node matching and wildcards
## ---------------------------------------------------------------------------

test_that("node_matches respects the interval endpoints exactly", {
  expect_equal(node_matches(c(29, 30, 39, 40), "[30,40)"),
               c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(node_matches(c(30, 40), "(30,40]"), c(FALSE, TRUE))
})

test_that("rule = exact, prefix and interval each do only their own thing", {
  expect_true(node_matches("30s", "30s", rule = "exact"))
  expect_false(node_matches("35", "30s", rule = "exact"))

  expect_true(node_matches("1350041", "135****", rule = "prefix"))
  expect_false(node_matches("1600022", "135****", rule = "prefix"))
  expect_true(node_matches("1350041", "135", rule = "prefix"))

  expect_true(node_matches("35", "[30,40)", rule = "interval"))
  expect_error(node_matches("x", "tokyo", rule = "interval"), "rule = \"interval\"")
})

test_that("auto also matches a value that was not generalised at all", {
  expect_true(node_matches("tokyo", "tokyo"))
  expect_true(node_matches("[30,40)", "[30,40)"))
})

test_that("wildcards match everything", {
  expect_true(all(is_generalization_wildcard(c("*", "**", "***", "", "  ", NA))))
  expect_false(any(is_generalization_wildcard(c("30s", "tokyo", "*a"))))

  raw <- data.frame(ROW_NUMBER = 1:3, Z = c("a", "b", "c"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:3, Z = c("*", "*", "*"),
                     stringsAsFactors = FALSE)
  cc <- containment_counts(join_raw_anon_data(raw, anon), "Z")
  expect_equal(cc$N_CONTAINED, c(3, 3, 3))
  expect_true(all(cc$TRUTH_CONTAINED))
})

## ---------------------------------------------------------------------------
## hierarchies
## ---------------------------------------------------------------------------

test_that("generalization_hierarchy computes descendants transitively", {
  h <- ascii_area()
  expect_s3_class(h, "reid_hierarchy")
  expect_setequal(descendants_of(h, "AREA", "tokyo"), c("chiyoda", "shinjuku"))
  expect_setequal(descendants_of(h, "AREA", "kanto"),
                  c("tokyo", "kanagawa", "chiyoda", "shinjuku",
                    "yokohama", "kawasaki"))
  expect_equal(descendants_of(h, "AREA", "chiyoda"), character(0))
  expect_equal(descendants_of(h, "AREA", "nowhere"), character(0))
  expect_equal(descendants_of(NULL, "AREA", "tokyo"), character(0))
})

test_that("levels count from the leaves upwards", {
  e <- ascii_area()$edges
  expect_equal(e$level[e$value == "chiyoda"], 1L)
  expect_equal(e$level[e$value == "tokyo"], 2L)
})

test_that("generalization_hierarchy rejects a malformed table", {
  expect_error(generalization_hierarchy(list(a = 1)), "data frame")
  expect_error(
    generalization_hierarchy(data.frame(value = "a", parent = "b")),
    "attribute"
  )
  expect_error(
    generalization_hierarchy(data.frame(attribute = "A", value = "a", parent = "")),
    "empty attribute"
  )
})

test_that("a value cannot roll up into two different coarser values", {
  expect_error(
    generalization_hierarchy(data.frame(
      attribute = "A", value = c("x", "x"), parent = c("p", "q"),
      stringsAsFactors = FALSE
    )),
    "more than one parent"
  )
})

test_that("a node cannot be its own parent", {
  expect_error(
    generalization_hierarchy(data.frame(attribute = "A", value = "x", parent = "x",
                                        stringsAsFactors = FALSE)),
    "its own"
  )
})

test_that("a cycle is reported rather than looping forever", {
  expect_error(
    generalization_hierarchy(data.frame(
      attribute = "A", value = c("x", "y", "z"), parent = c("y", "z", "x"),
      stringsAsFactors = FALSE
    )),
    "cycle"
  )
})

test_that("blank filler rows are tolerated", {
  h <- generalization_hierarchy(data.frame(
    attribute = c("A", "", "A"), value = c("x", "", "y"),
    parent = c("p", "", "p"), stringsAsFactors = FALSE
  ))
  expect_equal(nrow(h$edges), 2L)
})

test_that("print.reid_hierarchy summarises without erroring", {
  expect_output(print(ascii_area()), "generalization hierarchy")
  expect_output(print(ascii_area()), "AREA")
})

## ---------------------------------------------------------------------------
## reading hierarchies from files
## ---------------------------------------------------------------------------

test_that("read_generalization_hierarchy reads a CSV edge list", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("attribute,value,parent",
               "AREA,chiyoda,tokyo",
               "AREA,tokyo,kanto",
               "AGE,\"[30,35)\",\"[30,40)\""), path)

  h <- read_generalization_hierarchy(path)
  expect_equal(nrow(h$edges), 3L)
  expect_setequal(descendants_of(h, "AREA", "kanto"), c("tokyo", "chiyoda"))
  expect_equal(descendants_of(h, "AGE", "[30,40)"), "[30,35)")
})

test_that("read_generalization_hierarchy validates its path", {
  expect_error(read_generalization_hierarchy("no/such/file.csv"), "not found")
  expect_error(read_generalization_hierarchy(c("a", "b")), "single file path")
})

test_that("the shipped example CSV loads and covers AGE and AREA", {
  path <- system.file("extdata", "generalization-jp.csv", package = "reidentify")
  skip_if(!nzchar(path) || !file.exists(path), "example hierarchy not installed")

  h <- read_generalization_hierarchy(path)
  expect_setequal(unique(h$edges$attribute), c("AREA", "AGE"))

  # AGE: 5-year bins rolling up into 10-year bins, exactly as Issue #20 asks
  age <- h$edges[h$edges$attribute == "AGE", ]
  expect_equal(nrow(age), 20L)
  expect_setequal(descendants_of(h, "AGE", "[30,40)"), c("[30,35)", "[35,40)"))
  expect_true(node_matches("37", "[30,40)"))

  # AREA: three levels, municipality -> prefecture -> region
  area <- h$edges[h$edges$attribute == "AREA", ]
  expect_equal(max(area$level), 2L)
  region <- setdiff(unique(area$parent), unique(area$value))[1]
  # a leaf municipality is reachable from its region
  leaves <- area$value[area$level == 1L]
  expect_true(any(leaves %in% descendants_of(h, "AREA", region)))
})

test_that("the shipped YAML declares exactly the same hierarchy as the CSV", {
  skip_if_not_installed("yaml")
  csv <- system.file("extdata", "generalization-jp.csv", package = "reidentify")
  yml <- system.file("extdata", "generalization-jp.yaml", package = "reidentify")
  skip_if(!nzchar(csv) || !nzchar(yml), "example hierarchies not installed")

  a <- read_generalization_hierarchy(csv)$edges
  b <- read_generalization_hierarchy(yml)$edges
  k <- function(e) sort(paste(e$attribute, e$value, e$parent, sep = "\r"))
  expect_equal(k(a), k(b))
})

test_that("the YAML format is picked up by extension and by request", {
  skip_if_not_installed("yaml")
  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("AREA:",
               "  kanto:",
               "    tokyo: [chiyoda, shinjuku]",
               "    kanagawa: [yokohama]"), path)

  h <- read_generalization_hierarchy(path)
  expect_setequal(descendants_of(h, "AREA", "kanto"),
                  c("tokyo", "kanagawa", "chiyoda", "shinjuku", "yokohama"))

  h2 <- read_generalization_hierarchy(path, format = "yaml")
  expect_equal(h$edges, h2$edges)
})

test_that("a YAML file that is not a hierarchy is rejected", {
  skip_if_not_installed("yaml")
  path <- tempfile(fileext = ".yaml")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("- a", "- b"), path)
  expect_error(read_generalization_hierarchy(path), "mapping")
})

## ---------------------------------------------------------------------------
## generalize_value
## ---------------------------------------------------------------------------

test_that("generalize_value enters the hierarchy at the finest containing node", {
  h <- ascii_age()
  expect_equal(generalize_value(c(31, 37, 46), "AGE", h, levels = 0),
               c("[30,35)", "[35,40)", "[45,50)"))
  expect_equal(generalize_value(c(31, 37, 46), "AGE", h, levels = 1),
               c("[30,40)", "[30,40)", "[40,50)"))
})

test_that("generalize_value stops climbing at the root", {
  h <- ascii_age()
  expect_equal(generalize_value(37, "AGE", h, levels = 1),
               generalize_value(37, "AGE", h, levels = 9))
})

test_that("generalize_value walks a categorical hierarchy", {
  h <- ascii_area()
  expect_equal(generalize_value(c("chiyoda", "yokohama"), "AREA", h, levels = 1),
               c("tokyo", "kanagawa"))
  expect_equal(generalize_value(c("chiyoda", "yokohama"), "AREA", h, levels = 2),
               c("kanto", "kanto"))
})

test_that("generalize_value leaves an unknown value alone and returns no names", {
  h <- ascii_area()
  out <- generalize_value(c("chiyoda", "atlantis"), "AREA", h, levels = 1)
  expect_equal(out, c("tokyo", "atlantis"))
  expect_null(names(out))
})

test_that("generalize_value validates its arguments", {
  h <- ascii_age()
  expect_error(generalize_value(1, "AGE", "not a hierarchy"), "reid_hierarchy")
  expect_error(generalize_value(1, "NOPE", h), "no attribute")
  expect_error(generalize_value(1, "AGE", h, levels = -1), "non-negative")
})

## ---------------------------------------------------------------------------
## the score
## ---------------------------------------------------------------------------

test_that("score_containment gives every survivor 1 - 1/k and every other 1", {
  raw <- data.frame(ROW_NUMBER = 1:6, AGE = c(21, 24, 33, 37, 38, 52))
  anon <- data.frame(ROW_NUMBER = 1:6,
                     AGE = c("20s", "20s", "30s", "30s", "30s", "50s"),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)
  s <- score_containment(d, "AGE")

  expect_equal(attr(s, "score_type"), "distance")
  expect_true(all(s$SCORE >= 0 & s$SCORE <= 1))

  # ANON 1 is "20s": raw records 1 and 2 fall inside, so k = 2
  a1 <- s$SCORE[s$ANON_ROW_NUMBER == 1]
  expect_equal(sort(unique(a1)), c(0.5, 1))
  expect_equal(sum(a1 == 0.5), 2L)

  # ANON 6 is "50s": only raw record 6, so k = 1 and its score is 0
  expect_equal(s$SCORE[s$ANON_ROW_NUMBER == 6 & s$RAW_ROW_NUMBER == 6], 0)

  expect_equal(unname(attr(s, "candidate_count")[c("1", "3", "6")]), c(2, 3, 1))
})

test_that("an excluded candidate always scores worse than any survivor", {
  raw <- data.frame(ROW_NUMBER = 1:50, AGE = 20:69)
  anon <- data.frame(ROW_NUMBER = 1:50,
                     AGE = ifelse(20:69 < 45, "[20,45)", "[45,70)"),
                     stringsAsFactors = FALSE)
  s <- score_containment(join_raw_anon_data(raw, anon), "AGE")

  # k is 25 here, so survivors score 0.96 -- still strictly below 1
  expect_lt(max(s$SCORE[s$SCORE < 1]), 1)
  expect_true(any(s$SCORE == 1))
})

test_that("match_greedy reports CONFIDENCE = 1/k on containment scores", {
  raw <- data.frame(ROW_NUMBER = 1:6, AGE = c(21, 24, 33, 37, 38, 52))
  anon <- data.frame(ROW_NUMBER = 1:6,
                     AGE = c("20s", "20s", "30s", "30s", "30s", "50s"),
                     stringsAsFactors = FALSE)
  m <- match_greedy(score_containment(join_raw_anon_data(raw, anon), "AGE"))

  expect_equal(m$CONFIDENCE, c(1 / 2, 1 / 2, 1 / 3, 1 / 3, 1 / 3, 1))
  expect_true(m$RESULT[m$ANON_ROW_NUMBER == 6])
})

test_that("several targets are intersected, so narrowing is monotone", {
  set.seed(3)
  people <- 80
  raw <- data.frame(
    ROW_NUMBER = seq_len(people),
    AGE = sample(20:69, people, replace = TRUE),
    ZIP = sample(sprintf("Z%02d", 1:8), people, replace = TRUE),
    SEX = sample(c("M", "F"), people, replace = TRUE),
    stringsAsFactors = FALSE
  )
  anon <- data.frame(
    ROW_NUMBER = raw$ROW_NUMBER,
    AGE = sprintf("[%d,%d)", (raw$AGE %/% 10) * 10, (raw$AGE %/% 10) * 10 + 10),
    ZIP = raw$ZIP,
    SEX = raw$SEX,
    stringsAsFactors = FALSE
  )
  d <- join_raw_anon_data(raw, anon)

  k1 <- containment_counts(d, "AGE")$N_CONTAINED
  k2 <- containment_counts(d, c("AGE", "ZIP"))$N_CONTAINED
  k3 <- containment_counts(d, c("AGE", "ZIP", "SEX"))$N_CONTAINED

  # adding an attribute can never enlarge the candidate set
  expect_true(all(k2 <= k1))
  expect_true(all(k3 <= k2))
  # and here it strictly shrinks it on average
  expect_lt(mean(k3), mean(k1))

  # more attributes therefore never lower the measured risk
  ev <- function(t) reid_evaluate(score_containment(d, t), seeds = 1:5,
                                  top_k = 1)$success_analytic
  expect_gte(ev(c("AGE", "ZIP")), ev("AGE"))
  expect_gte(ev(c("AGE", "ZIP", "SEX")), ev(c("AGE", "ZIP")))
})

test_that("scores follow the rows of dat_raw_anon, whatever their order", {
  raw <- data.frame(ROW_NUMBER = c(9L, 4L, 7L), AGE = c(21, 33, 52))
  anon <- data.frame(ROW_NUMBER = c(4L, 7L, 9L),
                     AGE = c("30s", "50s", "20s"), stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)
  d <- d[sample.int(nrow(d)), , drop = FALSE]

  s <- score_containment(d, "AGE")
  expect_equal(s$RAW_ROW_NUMBER, d$RAW_ROW_NUMBER)
  expect_equal(s$ANON_ROW_NUMBER, d$ANON_ROW_NUMBER)
  expect_true(all(match_greedy(s)$RESULT))
})

test_that("a hierarchy widens containment to the whole subtree", {
  h <- ascii_area()
  raw <- data.frame(ROW_NUMBER = 1:3,
                    AREA = c("chiyoda", "yokohama", "osaka_city"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:3,
                     AREA = c("kanto", "kanto", "kinki"),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  # without the hierarchy, "kanto" matches nothing at all
  expect_equal(containment_counts(d, "AREA")$N_CONTAINED, c(0, 0, 0))
  expect_false(any(containment_counts(d, "AREA")$TRUTH_CONTAINED))

  cc <- containment_counts(d, "AREA", hierarchy = h)
  expect_equal(cc$N_CONTAINED, c(2, 2, 1))
  expect_true(all(cc$TRUTH_CONTAINED))
})

test_that("rules force a per-column reading", {
  raw <- data.frame(ROW_NUMBER = 1:2, ZIP = c("1350041", "1600022"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:2, ZIP = c("135****", "160****"),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  # auto reads "135****" as a category, which matches nobody
  expect_equal(containment_counts(d, "ZIP")$N_CONTAINED, c(0, 0))
  # prefix reads it as the mask it is
  cc <- containment_counts(d, "ZIP", rules = c(ZIP = "prefix"))
  expect_equal(cc$N_CONTAINED, c(1, 1))
  expect_true(all(cc$TRUTH_CONTAINED))
})

test_that("score_containment validates its arguments", {
  raw <- data.frame(ROW_NUMBER = 1:2, AGE = c(21, 33))
  d <- join_raw_anon_data(raw, raw)

  expect_error(score_containment(d, character(0)), "non-empty character")
  expect_error(score_containment(d, "NOPE"), "RAW_NOPE")
  expect_error(score_containment(d, "AGE", hierarchy = list(a = 1)),
               "reid_hierarchy")
  expect_error(score_containment(d, "AGE", rules = "prefix"), "named")
  expect_error(score_containment(d, "AGE", rules = c(AGE = "nonsense")),
               "unknown rule")
  expect_error(score_containment(d, "AGE", rules = c(ZIP = "prefix")),
               "not in `targets`")
})

## ---------------------------------------------------------------------------
## failure directions
## ---------------------------------------------------------------------------

test_that("a record whose truth is excluded is reported, not dropped", {
  # a release that claims everybody is in their twenties, which is false
  raw <- data.frame(ROW_NUMBER = 1:5, AGE = c(21, 33, 41, 55, 67))
  anon <- data.frame(ROW_NUMBER = 1:5, AGE = rep("20s", 5),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  cc <- containment_counts(d, "AGE")
  expect_equal(cc$TRUTH_CONTAINED, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(cc$N_CONTAINED, rep(1, 5))
  expect_equal(cc$INFORMATION, rep(1, 5))

  # every ANON record still gets exactly one trial
  m <- match_greedy(score_containment(d, "AGE"))
  expect_equal(nrow(m), 5L)
  expect_setequal(m$ANON_ROW_NUMBER, 1:5)
})

test_that("when nothing survives, the attack degenerates to guessing rather than vanishing", {
  raw <- data.frame(ROW_NUMBER = 1:4, AREA = c("a", "b", "c", "d"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:4, AREA = rep("elsewhere", 4),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  cc <- containment_counts(d, "AREA")
  expect_equal(cc$N_CONTAINED, rep(0, 4))
  expect_true(all(is.na(cc$INFORMATION)))
  expect_false(any(cc$TRUTH_CONTAINED))

  s <- score_containment(d, "AREA")
  expect_true(all(s$SCORE == 1))

  m <- match_greedy(s)
  expect_equal(nrow(m), 4L)
  expect_equal(m$CONFIDENCE, rep(1 / 4, 4))
})

## ---------------------------------------------------------------------------
## Issue #20's acceptance criterion
## ---------------------------------------------------------------------------

test_that("containment narrows a generalised release to the right candidates", {
  set.seed(99)
  h <- ascii_area()
  leaves <- c("chiyoda", "shinjuku", "yokohama", "kawasaki", "osaka_city")
  people <- 150

  raw <- data.frame(
    ROW_NUMBER = seq_len(people),
    AGE = sample(30:49, people, replace = TRUE),
    AREA = sample(leaves, people, replace = TRUE),
    stringsAsFactors = FALSE
  )
  age_h <- ascii_age()
  anon <- data.frame(
    ROW_NUMBER = raw$ROW_NUMBER,
    AGE = generalize_value(raw$AGE, "AGE", age_h, levels = 1),
    AREA = generalize_value(raw$AREA, "AREA", h, levels = 1),
    stringsAsFactors = FALSE
  )
  combined <- generalization_hierarchy(rbind(h$edges[, 1:3], age_h$edges[, 1:3]))
  d <- join_raw_anon_data(raw, anon)

  cc <- containment_counts(d, c("AGE", "AREA"), hierarchy = combined)

  # 1. the release and the raw file agree about every record
  expect_true(all(cc$TRUTH_CONTAINED))

  # 2. k equals an independently computed count of the raw records whose own
  #    generalised value matches the published one
  raw_gen_age <- generalize_value(raw$AGE, "AGE", age_h, levels = 1)
  raw_gen_area <- generalize_value(raw$AREA, "AREA", h, levels = 1)
  independent <- vapply(seq_len(people), function(i) {
    sum(raw_gen_age == anon$AGE[i] & raw_gen_area == anon$AREA[i])
  }, numeric(1))
  expect_equal(cc$N_CONTAINED[order(cc$ANON_ROW_NUMBER)], independent)

  # 3. it really is a narrowing. The release has 3 prefectures x 2 age bands
  #    = 6 distinct published cells over 150 people, so the average candidate
  #    set is around 25 and each record has discarded ~5/6 of the file.
  expect_true(all(cc$N_CANDIDATES == people))
  expect_lt(max(cc$N_CONTAINED), people)
  expect_lt(mean(cc$N_CONTAINED), people / 4)
  expect_gt(mean(1 - cc$N_CONTAINED / people), 0.75)

  # 4. and the resulting attack beats the random baseline by a wide margin
  ev <- reid_evaluate(score_containment(d, c("AGE", "AREA"), hierarchy = combined),
                      seeds = 1:5, top_k = 1)
  expect_gt(ev$lift, 5)
})

test_that("a weak attacker holding one attribute still narrows the file", {
  set.seed(100)
  people <- 120
  raw <- data.frame(ROW_NUMBER = seq_len(people),
                    AGE = sample(20:79, people, replace = TRUE))
  anon <- data.frame(
    ROW_NUMBER = raw$ROW_NUMBER,
    AGE = sprintf("%ds", (raw$AGE %/% 10) * 10),
    stringsAsFactors = FALSE
  )
  cc <- containment_counts(join_raw_anon_data(raw, anon), "AGE")

  expect_true(all(cc$TRUTH_CONTAINED))
  # six decades over 120 people: roughly a sixth of the file survives, so the
  # attacker who knows only the decade has still discarded most candidates
  expect_lt(mean(cc$N_CONTAINED), people / 3)
  expect_gt(mean(cc$N_CONTAINED), 1)
})

test_that("score_num cannot read a generalised column at all", {
  raw <- data.frame(ROW_NUMBER = 1:3, AGE = c(21, 33, 52))
  anon <- data.frame(ROW_NUMBER = 1:3, AGE = c("20s", "30s", "50s"),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  expect_error(score_num(d, "AGE"), "non-numeric")
  expect_true(all(match_greedy(score_containment(d, "AGE"))$RESULT))
})
