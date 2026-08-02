## Issue #40: score_char() did not stop on a generalised column. It computed
## adist("37", "[30,40)") = 6 -- the length of the bracket string -- and handed
## back a table of plausible-looking scores. On the generalisation benchmark
## that reported a success rate of 0.1017 where score_containment() reported
## 0.4450: a fourfold under-report with no error anywhere, which is the failure
## direction docs/lessons-learned.md section 2 is about.
##
## These tests pin down three things:
##   1. the generalised column is now refused, by every value-comparison score;
##   2. an ordinary column is untouched (a false positive here would break
##      working analyses, so the detector has to be quiet on normal data);
##   3. the escape hatch still produces the old number, so the under-report is
##      reachable deliberately and never by accident.

gen_fixture <- function(people = 60, seed = 42) {
  set.seed(seed)
  raw <- data.frame(
    ROW_NUMBER = seq_len(people),
    AGE = sample(20:69, people, replace = TRUE),
    SEX = sample(c("M", "F"), people, replace = TRUE),
    NAME = paste0("name", seq_len(people)),
    stringsAsFactors = FALSE
  )
  anon <- data.frame(
    ROW_NUMBER = raw$ROW_NUMBER,
    AGE = sprintf("[%d,%d)", floor(raw$AGE / 10) * 10, floor(raw$AGE / 10) * 10 + 10),
    SEX = raw$SEX,
    NAME = raw$NAME,
    stringsAsFactors = FALSE
  )
  join_raw_anon_data(raw, anon)
}

## ---------------------------------------------------------------------------
## the detector itself
## ---------------------------------------------------------------------------

test_that("is_generalized_value() marks regions and leaves values alone", {
  expect_true(all(is_generalized_value(
    c("[30,40)", "(30,40]", "30s", "30-39", "0-100", "[65,)", "*", "**", "135****")
  )))
  expect_false(any(is_generalized_value(
    c("37", "37.0", "-5", "M", "Z001", "chiyoda", "tokyo", "1:2:3", "a-b", "")
  )))
})

test_that("is_generalized_value() treats a bare number as a value, not a region", {
  ## "35" parses as the degenerate interval [35, 35]. Calling that a
  ## generalisation would flag every numeric-looking character column.
  expect_false(is_generalized_value("35"))
  expect_true(is_generalized_value("35-36"))
})

test_that("is_generalized_value() reports NA as FALSE", {
  ## A missing value can mean anything; treating it as a generalisation would
  ## flag every column that merely has a gap in it.
  expect_equal(is_generalized_value(c(NA, "30s", NA_character_)),
               c(FALSE, TRUE, FALSE))
})

test_that("is_generalized_value() only reads a multiple of ten as a decade", {
  ## parse_generalized_interval() reads "8s" as [8, 18) -- correct for
  ## containment, wrong as evidence of generalisation. Random two-character
  ## strings hit digit + "s" 0.26% of the time, and that is what first made
  ## this check stop test-statistical-properties.R, a test with nothing to do
  ## with generalised data.
  expect_false(is_generalized_value("8s"))
  expect_false(is_generalized_value("7s"))
  expect_true(is_generalized_value("30s"))
  expect_true(is_generalized_value("60s"))
  ## the containment parser is unchanged: "8s" is still a usable node
  expect_true(node_matches("11", "8s"))
})

test_that("random two-character strings are not called generalised", {
  set.seed(99)
  s <- stringi::stri_rand_strings(4000, length = 2)
  ## measured: 0.0245% -- allow generous headroom, but not 1%
  expect_lt(mean(is_generalized_value(s)), 0.01)
})

test_that("the guard needs a region-shaped column, not one stray region value", {
  ## exactly the shape of the false positive found in the existing suite:
  ## a column of ordinary two-character codes, one of which happens to parse
  ## as an interval.
  raw <- data.frame(ROW_NUMBER = 1:10,
                    CHAR = c("11", "ab", "cd", "ef", "gh",
                             "ij", "kl", "mn", "op", "qr"),
                    stringsAsFactors = FALSE)
  anon <- raw
  anon$CHAR[1] <- "10-20"
  d <- join_raw_anon_data(raw, anon)
  expect_true(is_generalized_value("10-20"))
  expect_no_error(score_char(d, "CHAR"))
  expect_no_warning(score_char(d, "CHAR"))

  ## ... and it does fire once the column really is regions
  anon2 <- raw
  anon2$CHAR <- "10-20"
  d2 <- join_raw_anon_data(raw, anon2)
  expect_error(score_char(d2, "CHAR"), regexp = "generalised")
})

test_that("the message reports the share of published values that are regions", {
  d <- gen_fixture()
  expect_error(score_char(d, "AGE"), regexp = "100% of its published values are regions",
               fixed = FALSE)
})

test_that("is_generalized_value() is vectorised and keeps the input length", {
  x <- rep(c("30s", "35", NA), times = 4)
  expect_length(is_generalized_value(x), length(x))
  expect_equal(is_generalized_value(x), rep(c(TRUE, FALSE, FALSE), times = 4))
})

## ---------------------------------------------------------------------------
## the guard: generalised columns are refused
## ---------------------------------------------------------------------------

test_that("score_char() stops on a generalised column and names score_containment()", {
  d <- gen_fixture()
  expect_error(score_char(d, "AGE"), regexp = "score_char\\(\\)")
  expect_error(score_char(d, "AGE"), regexp = "score_containment")
  ## the message must show the evidence, not just assert it
  expect_error(score_char(d, "AGE"), regexp = "falls inside ANON")
})

test_that("score_num() on a generalised column says so instead of 'non-numeric argument'", {
  d <- gen_fixture()
  ## it always stopped -- but with base R's "non-numeric argument to binary
  ## operator", which names neither the function, the column, nor the fix.
  expect_error(score_num(d, "AGE"), regexp = "score_num\\(\\)")
  expect_error(score_num(d, "AGE"), regexp = "AGE")
  expect_error(score_num(d, "AGE"), regexp = "score_containment")
  ## and it is no longer base R's message
  expect_false(grepl("non-numeric argument to binary operator",
                     tryCatch(score_num(d, "AGE"), error = conditionMessage),
                     fixed = TRUE))
})

test_that("score_num_rank() and score_dist() stop on a generalised column", {
  d <- gen_fixture()
  expect_error(score_num_rank(d, "AGE"), regexp = "score_num_rank\\(\\)")
  expect_error(score_num_rank(d, "AGE"), regexp = "score_containment")
  expect_error(score_dist(d, "AGE"), regexp = "score_dist\\(\\)")
  expect_error(score_dist(d, "AGE"), regexp = "score_containment")
})

test_that("a caller that wraps a score function gets the guard, and the message names the wrapper", {
  ## The reid_by_*() wrappers were removed in 3.0.0. What they exercised is
  ## `.fn_name`: the guard fires inside the score layer but must name the
  ## function the *user* called, or the message points at code they did not
  ## write. Any wrapper (this package's own score_multi(), or a user's) relies
  ## on it, so it is pinned here directly.
  d <- gen_fixture()
  my_attack <- function(dat, target) {
    score_char(dat, target, .fn_name = "my_attack")
  }
  expect_error(my_attack(d, "AGE"), regexp = "my_attack\\(\\)")
  expect_error(my_attack(d, "AGE"), regexp = "score_containment")

  ## and with no wrapper the score function names itself
  expect_error(score_char(d, "AGE"), regexp = "score_char\\(\\)")
  expect_error(score_num(d, "AGE"), regexp = "score_num\\(\\)")
  expect_error(score_num_rank(d, "AGE"), regexp = "score_num_rank\\(\\)")
  expect_error(score_dist(d, "AGE"), regexp = "score_dist\\(\\)")
})

test_that("a suppressed (masked) ANON column is refused too", {
  raw <- data.frame(ROW_NUMBER = 1:5, ZIP = c("1350001", "1350002", "1360003",
                                              "1360004", "1400005"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:5, ZIP = c("135****", "135****", "136****",
                                               "136****", "140****"),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)
  expect_error(score_char(d, "ZIP"), regexp = "generalised")

  anon2 <- anon
  anon2$ZIP <- "*"
  d2 <- join_raw_anon_data(raw, anon2)
  expect_error(score_char(d2, "ZIP"), regexp = "generalised")
})

## ---------------------------------------------------------------------------
## the escape hatches
## ---------------------------------------------------------------------------

test_that("generalized = 'warn' computes the score but says what it is doing", {
  d <- gen_fixture()
  expect_warning(s <- score_char(d, "AGE", generalized = "warn"),
                 regexp = "score_containment")
  expect_s3_class(s, "reid_scores")
  expect_equal(nrow(s), nrow(d))
})

test_that("generalized = 'ignore' is silent and reproduces the pre-fix numbers", {
  d <- gen_fixture()
  expect_silent(s <- score_char(d, "AGE", generalized = "ignore"))
  ## the same edit distances as before the fix: RAW "37" vs ANON "[30,40)"
  i <- which(d$RAW_ROW_NUMBER == d$ANON_ROW_NUMBER)[1]
  expect_equal(s$SCORE[i],
               as.numeric(utils::adist(as.character(d$ANON_AGE[i]),
                                       as.character(d$RAW_AGE[i]))[[1]]))
})

test_that("the escape hatches survive all the way through an assignment", {
  ## `generalized` is a score-layer argument, but the thing a caller actually
  ## produces is an assignment. "warn" must still yield a usable one, and
  ## "ignore" must stay silent end to end.
  d <- gen_fixture()
  expect_warning(m <- match_greedy(score_char(d, "AGE", generalized = "warn")))
  expect_equal(nrow(m), length(unique(d$ANON_ROW_NUMBER)))

  expect_silent(m2 <- match_greedy(score_char(d, "AGE", generalized = "ignore")))
  expect_equal(nrow(m2), length(unique(d$ANON_ROW_NUMBER)))
})

## ---------------------------------------------------------------------------
## no false positives: ordinary columns must be untouched
## ---------------------------------------------------------------------------

test_that("ordinary character columns still score exactly as before", {
  d <- gen_fixture()
  expect_no_error(score_char(d, "SEX"))
  expect_no_warning(score_char(d, "NAME"))

  ## identical to the pre-fix result: the guard must not touch the score
  s <- score_char(d, "NAME")
  raw_v <- as.character(d$RAW_NAME)
  anon_v <- as.character(d$ANON_NAME)
  expect_equal(s$SCORE,
               unname(mapply(function(x, y) utils::adist(x, y)[[1]],
                             anon_v, raw_v)))
})

test_that("the dummy fixtures the rest of the suite uses are not flagged", {
  q <- create_dummy_qi_data(people = 40, seed = 1)
  j <- join_raw_anon_data(q, q)
  expect_no_error(score_char(j, "ZIP"))
  expect_no_error(score_char(j, "SEX"))
  expect_no_error(score_num(j, "AGE"))
  expect_no_error(score_num_rank(j, "VISIT_COUNT"))
})

test_that("a distribution column is not mistaken for a generalisation", {
  raw <- data.frame(ROW_NUMBER = 1:4, D = c("1:2", "2:3", "3:4", "4:5"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:4, D = c("1:2.1", "2:3.1", "3:4.1", "5:6.1"),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)
  expect_no_error(score_dist(d, "D"))
  expect_no_error(score_char(d, "D"))
})

test_that("a hyphenated code column is not flagged when both sides carry it", {
  ## "100-8111" is structurally indistinguishable from the range [100, 8111],
  ## so is_generalized_value() does call it a region. The guard needs a second
  ## fact before it fires -- a RAW value that falls *inside* the region and is
  ## not equal to it -- and a postcode column has none, because the two sides
  ## carry the same strings.
  expect_true(is_generalized_value("100-8111"))
  zips <- c("100-8111", "150-0001", "100-0005", "231-0023")
  d <- join_raw_anon_data(
    data.frame(ROW_NUMBER = 1:4, ZIP = zips, stringsAsFactors = FALSE),
    data.frame(ROW_NUMBER = 1:4, ZIP = zips, stringsAsFactors = FALSE)
  )
  expect_no_error(score_char(d, "ZIP"))
  expect_no_warning(score_char(d, "ZIP"))
})

test_that("a column already binned on BOTH sides is compared literally, not refused", {
  ## RAW and ANON both hold "[30,40)": the attacker's knowledge is as coarse as
  ## the release, so literal comparison is the right thing and there is no
  ## region-containing-a-value mismatch to report.
  bins <- c("[20,30)", "[30,40)", "[40,50)", "[30,40)")
  d <- join_raw_anon_data(
    data.frame(ROW_NUMBER = 1:4, AGE = bins, stringsAsFactors = FALSE),
    data.frame(ROW_NUMBER = 1:4, AGE = bins, stringsAsFactors = FALSE)
  )
  expect_no_error(score_char(d, "AGE"))
  expect_no_warning(score_char(d, "AGE"))
})

## ---------------------------------------------------------------------------
## the cross-cutting finding: rank() on a character column
## ---------------------------------------------------------------------------

test_that("score_num_rank() refuses a plain character column instead of ranking it lexicographically", {
  ## Found while checking the other scores for the same defect: rank() accepts
  ## a character vector and orders it alphabetically, so a categorical column
  ## -- including one generalised into named categories, which no structural
  ## test can recognise -- came back as a full table of plausible rank gaps.
  d <- join_raw_anon_data(
    data.frame(ROW_NUMBER = 1:4, AREA = c("chiyoda", "shinjuku", "yokohama", "kobe"),
               stringsAsFactors = FALSE),
    data.frame(ROW_NUMBER = 1:4, AREA = c("tokyo", "tokyo", "kanagawa", "hyogo"),
               stringsAsFactors = FALSE)
  )
  expect_error(score_num_rank(d, "AREA"), regexp = "not\\s+numeric")
  expect_error(score_num_rank(d, "AREA"), regexp = "lexicographic")
  expect_error(score_num_rank(d, "AREA"), regexp = "score_num_rank\\(\\)")
})

test_that("score_num() refuses a plain character column with a message that names it", {
  d <- gen_fixture()
  expect_error(score_num(d, "SEX"), regexp = "score_num\\(\\)")
  expect_error(score_num(d, "SEX"), regexp = "SEX")
  ## not generalised, so it must NOT point at score_containment()
  expect_error(score_num(d, "SEX"), regexp = "score_char")
})

## ---------------------------------------------------------------------------
## the number the issue is about
## ---------------------------------------------------------------------------

test_that("the fourfold under-report is no longer reachable without asking for it", {
  d <- gen_fixture(people = 200, seed = 7)

  ## what the correct score reports
  cont <- match_greedy(score_containment(d, c("AGE", "SEX")), seed = 1L)
  rate_containment <- mean(cont$RAW_ROW_NUMBER == cont$ANON_ROW_NUMBER)

  ## what the misuse reported, now only reachable on purpose
  ch <- match_greedy(
    combine_scores(lapply(c("AGE", "SEX"), function(t) {
      score_char(d, t, generalized = "ignore")
    })),
    seed = 1L
  )
  rate_char <- mean(ch$RAW_ROW_NUMBER == ch$ANON_ROW_NUMBER)

  expect_lt(rate_char, rate_containment)
  ## and the misuse is not silently available
  expect_error(combine_scores(lapply(c("AGE", "SEX"), function(t) score_char(d, t))))
})

## ---------------------------------------------------------------------------
## Issue #100: the guard belongs to the computation, not to a list of functions
##
## The Issue #40 fix put check_generalized_target() into three score functions
## by hand. reid_score_types() lets a caller declare eight, attacker_knowledge()
## accepts all eight, and the package exports more score functions than that.
## Measured on a fully generalised AGE column (N = 200, decade intervals):
##
##   score_num / char / dist / rank / count / span   -> stopped
##   score_idf                                       -> ran, success 0.0100
##   score_profile                                   -> ran, success 0.0100
##   score_jaccard / minhash / scoreboard            -> ran, success 0.0100
##   score_idf_match(AGE, SEX)                       -> ran, success 0.0050
##   score_containment(AGE, SEX)  [the true answer]  ->      success 0.0500
##
## Five to ten times too low, no error, no warning. Every one of the missing
## functions was missing for the same reason: it was not on the list.
##
## The tests below are therefore written against the *package's own exports*
## rather than against a list written out here, so that a score function added
## later cannot be quietly left out of the guard the way these were.
## ---------------------------------------------------------------------------

gen_call <- function(fn_name, d, ...) {
  fn <- get(fn_name, envir = asNamespace("reidentify"))
  ## some score functions take one `target`, some a vector of `targets`; both
  ## accept a single column name in the same position
  do.call(fn, c(list(d, "AGE"), list(...)))
}

test_that("the guard policy table accounts for every exported score function", {
  ## THE RECURRENCE GUARD. Add an exported score_*() function and this fails
  ## until reid_generalized_guard_policy() says what it does with a generalised
  ## column -- which is the question nobody was made to answer for score_idf(),
  ## score_profile(), score_jaccard(), score_minhash() or score_scoreboard().
  policy <- reid_generalized_guard_policy()
  exported <- reid_exported_score_functions()

  expect_setequal(names(policy), exported)
  expect_true(all(policy %in% c("refuse", "containment", "delegates")))
  ## exactly one score reads a region as a region
  expect_equal(names(policy)[policy == "containment"], "score_containment")
})

test_that("every score function declared 'refuse' actually refuses a generalised column", {
  d <- gen_fixture()
  policy <- reid_generalized_guard_policy()
  refusing <- names(policy)[policy == "refuse"]
  ## the list is not empty by accident
  expect_gt(length(refusing), 10)

  for (fn_name in refusing) {
    msg <- tryCatch({
      gen_call(fn_name, d)
      NA_character_
    }, error = conditionMessage)

    expect_false(is.na(msg),
                 label = paste0(fn_name, "() returned a score for a fully ",
                                "generalised column instead of refusing it"))
    expect_match(msg, "score_containment", fixed = TRUE,
                 label = paste0(fn_name, "() error message"))
    expect_match(msg, fn_name, fixed = TRUE,
                 label = paste0(fn_name, "() error message"))
  }
})

test_that("'refuse' never means a silent warning: nothing merely warns", {
  d <- gen_fixture()
  policy <- reid_generalized_guard_policy()
  for (fn_name in names(policy)[policy == "refuse"]) {
    warned <- FALSE
    tryCatch(
      withCallingHandlers(gen_call(fn_name, d),
                          warning = function(w) {
                            warned <<- TRUE
                            invokeRestart("muffleWarning")
                          }),
      error = function(e) NULL
    )
    expect_false(warned,
                 label = paste0(fn_name, "() downgraded the guard to a warning"))
  }
})

test_that("generalized = 'ignore' is the only way past the guard, for every score", {
  ## The escape hatch has to stay reachable -- a tool people cannot switch off
  ## gets worked around -- but it must be the *only* way through. Some of these
  ## still stop afterwards for an unrelated reason (a region is not a number);
  ## what must not survive is the generalisation refusal itself.
  d <- gen_fixture()
  policy <- reid_generalized_guard_policy()
  for (fn_name in names(policy)[policy == "refuse"]) {
    msg <- tryCatch({
      suppressWarnings(gen_call(fn_name, d, generalized = "ignore"))
      NA_character_
    }, error = conditionMessage)
    if (!is.na(msg)) {
      expect_false(grepl("is generalised on the ANON side", msg, fixed = TRUE),
                   label = paste0(fn_name, "(generalized = \"ignore\")"))
    }
  }
})

test_that("every declarable score type has a policy, and only 'containment' survives", {
  ## The second recurrence guard, on reid_score_types() rather than on the
  ## exports: adding a type without pointing it at a guarded function fails
  ## here. Declaring a generalised column as anything but "containment" must
  ## stop, because that is the difference Issue #100 measured.
  d <- gen_fixture()
  policy <- reid_generalized_guard_policy()
  ns <- asNamespace("reidentify")

  for (ty in reid_score_types()) {
    fn <- score_fn_for_type(ty)
    nm <- names(policy)[vapply(names(policy),
                               function(n) identical(get(n, envir = ns), fn),
                               logical(1))]
    expect_length(nm, 1L)

    k <- attacker_knowledge("S", stats::setNames(c(ty, "char"), c("AGE", "SEX")))
    if (identical(ty, "containment")) {
      expect_s3_class(suppressWarnings(score_by_knowledge(d, k)), "reid_scores")
    } else {
      expect_error(suppressWarnings(score_by_knowledge(d, k)),
                   regexp = "score_containment",
                   label = paste0("score_by_knowledge with AGE declared \"", ty, "\""))
    }
  }
})

## ---------------------------------------------------------------------------
## containment is reachable from the recommended path
## ---------------------------------------------------------------------------

test_that("attacker_knowledge() accepts \"containment\"", {
  ## Before #100 this was an error: the correct score for generalised data was
  ## not among the declarable types at all, so a W/M/S user had only the six
  ## types that stop and the two that under-report.
  k <- attacker_knowledge("M", quasi_identifiers = c(AGE = "containment",
                                                     SEX = "char"))
  expect_equal(unname(k$visible[["AGE"]]), "containment")
  expect_true("containment" %in% reid_score_types())
})

test_that("declared containment columns are intersected, not summed", {
  ## score_containment() intersects its targets: a record must fall inside the
  ## published region of *every* attribute. Scoring the columns separately and
  ## adding the normalised results would give each exclusion a partial vote, so
  ## score_multi() has to hand them over as one block.
  d <- gen_fixture(people = 60, seed = 3)
  block <- score_multi(d, c(AGE = "containment", SEX = "containment"),
                       screen = "none")
  direct <- score_containment(d, c("AGE", "SEX"))
  ## combine_scores() normalises the single block to [0, 1]; the ordering --
  ## which is all an assignment reads -- must be the same
  expect_equal(rank(block$SCORE, ties.method = "min"),
               rank(direct$SCORE, ties.method = "min"))
})

test_that("the W/M/S curve on generalised data now runs and reports the real risk", {
  ## The number the issue is about, taken along the recommended path rather
  ## than by hand.
  d <- gen_fixture(people = 200, seed = 7)

  curve <- reid_knowledge_curve(
    d,
    quasi_identifiers = c(AGE = "containment", SEX = "containment"),
    weak_subset = "AGE",
    seeds = 1:3,
    screen = "none"
  )
  expect_equal(nrow(curve), 3L)
  expect_true(all(is.finite(curve$success_analytic)))

  ## and it beats what the two silently-completing types used to report
  truth <- mean(match_greedy(score_containment(d, c("AGE", "SEX")),
                             seed = 1L)$RESULT)
  quiet_idf <- mean(match_greedy(
    score_idf_match(d, c("AGE", "SEX"), generalized = "ignore"),
    seed = 1L
  )$RESULT)
  expect_gt(truth, quiet_idf)
})

test_that("a hierarchy reaches score_containment() through the declaration path", {
  ## The categorical case -- 千代田区 published as 東京都 -- is the one no
  ## structural test can recognise, so the guard cannot catch it and the
  ## hierarchy is the only thing that makes it scorable. If `hierarchy` did not
  ## reach score_containment() through score_multi(), declaring "containment"
  ## would work for intervals and silently exclude every record for categories.
  hier <- generalization_hierarchy(data.frame(
    attribute = "AREA",
    value = c("chiyoda", "shinjuku", "yokohama", "kobe"),
    parent = c("tokyo", "tokyo", "kanagawa", "hyogo"),
    stringsAsFactors = FALSE
  ))
  raw <- data.frame(ROW_NUMBER = 1:4,
                    AREA = c("chiyoda", "shinjuku", "yokohama", "kobe"),
                    stringsAsFactors = FALSE)
  anon <- data.frame(ROW_NUMBER = 1:4,
                     AREA = c("tokyo", "tokyo", "kanagawa", "hyogo"),
                     stringsAsFactors = FALSE)
  d <- join_raw_anon_data(raw, anon)

  with_h <- score_multi(d, c(AREA = "containment"), hierarchy = hier,
                        screen = "none")
  ## every record's own region contains it
  expect_true(all(containment_counts(d, "AREA", hierarchy = hier)$TRUTH_CONTAINED))
  expect_equal(with_h$SCORE,
               score_containment(d, "AREA", hierarchy = hier)$SCORE)

  ## without the hierarchy nothing is contained at all, which is exactly why
  ## the argument has to be reachable from here -- and since Issue #101 that
  ## is also announced rather than left to be noticed
  expect_warning(without_h <- score_containment(d, "AREA"),
                 "EMPTY candidate set")
  expect_true(all(without_h$SCORE == 1))
})

test_that("`rules` reaches score_containment() through the declaration path", {
  ## ISSUE #109 CHANGED WHICH RULE THIS HAS TO FORCE, not what it is testing.
  ##
  ## As written for #100 this used rules = c(ZIP = "prefix") on a masked code
  ## such as "135****": under "auto" the mask did not parse as an interval, fell
  ## back to exact string equality, and excluded "1350012" from its own
  ## published region -- k = 0 and the release read as perfectly safe. #109
  ## fixed that: "auto" now reads a trailing "*" as the prefix it always was, so
  ## "prefix" and "auto" agree on this fixture and forcing it no longer changes
  ## anything. See tests/testthat/test-empty-candidate-set.R.
  ##
  ## The thing being pinned here is that `rules` *arrives* at
  ## score_containment() through reid_knowledge_curve(), and that is now shown
  ## the other way round: rules = c(ZIP = "exact") forces the reading "auto"
  ## used to fall into by accident, and collapses the measured rate back onto
  ## the random baseline. A `rules` that went nowhere would leave the two runs
  ## identical, which is exactly what this test exists to catch.
  set.seed(4)
  n <- 120
  raw <- data.frame(
    ROW_NUMBER = seq_len(n),
    ZIP = sprintf("%07d", sample(1350000:1350040, n, replace = TRUE)),
    AGE = sample(20:69, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  anon <- data.frame(
    ROW_NUMBER = raw$ROW_NUMBER,
    ZIP = paste0(substr(raw$ZIP, 1, 3), "****"),
    AGE = sprintf("[%d,%d)", floor(raw$AGE / 10) * 10, floor(raw$AGE / 10) * 10 + 10),
    stringsAsFactors = FALSE
  )
  d <- join_raw_anon_data(raw, anon)
  rules <- c(ZIP = "exact")
  qi <- c(ZIP = "containment", AGE = "containment")

  ## the set-up check is what says the rule reaches the score: "auto" contains
  ## every record's truth, "exact" contains none of it
  expect_true(all(containment_counts(d, "ZIP")$TRUTH_CONTAINED))
  expect_equal(containment_counts(d, "ZIP")$N_CONTAINED,
               containment_counts(d, "ZIP",
                                  rules = c(ZIP = "prefix"))$N_CONTAINED)
  expect_false(any(suppressWarnings(
    containment_counts(d, c("ZIP", "AGE"), rules = rules)
  )$TRUTH_CONTAINED))

  baseline <- 1 / n
  without <- reid_knowledge_curve(d, quasi_identifiers = qi, weak_subset = "ZIP",
                                  seeds = 1:3, screen = "none")
  with_rules <- suppressWarnings(
    reid_knowledge_curve(d, quasi_identifiers = qi, weak_subset = "ZIP",
                         seeds = 1:3, screen = "none", rules = rules)
  )

  ## the forced rule empties every candidate set, so the declaration path
  ## reports the random baseline ...
  expect_equal(with_rules$success_analytic[with_rules$level == "M"], baseline)
  ## ... and the default reading gets several times that
  expect_gt(without$success_analytic[without$level == "M"], 3 * baseline)
})

## ---------------------------------------------------------------------------
## the two silent under-reports, pinned
## ---------------------------------------------------------------------------

test_that("score_idf() and score_profile() under-report, and only on request", {
  ## THE COMPARISON HAS TO BE MADE ON MORE THAN ONE COLUMN, and that was not
  ## the expectation going in. On the *single* generalised AGE column here,
  ## containment scores 0.0050 -- exactly the 1/200 random baseline -- because
  ## a decade bin leaves about forty candidates and 1/k is chance. The silent
  ## scores measure 0.0050-0.0100 on the same column, i.e. also chance, and
  ## comparing the two says nothing.
  ##
  ## The gap is in the *intersection*: two attributes cut the candidate set
  ## twice and containment reaches 0.0500, while the silent scores stay at the
  ## baseline because a raw value never equals the region printed over it, so
  ## adding a second such column adds no constraint at all. Tenfold, silently.
  d <- gen_fixture(people = 200, seed = 7)
  cols <- c("AGE", "SEX")

  truth <- mean(match_greedy(score_containment(d, cols), seed = 1L)$RESULT)
  baseline <- 1 / length(unique(d$ANON_ROW_NUMBER))
  expect_gt(truth, 5 * baseline)

  ## single-column containment really is at chance here -- pinned so that the
  ## comparison above is not quietly weakened later
  expect_equal(mean(match_greedy(score_containment(d, "AGE"), seed = 1L)$RESULT),
               baseline)

  for (fn in list(score_idf, score_profile)) {
    expect_error(fn(d, "AGE"), regexp = "score_containment")
    quiet <- mean(match_greedy(
      combine_scores(lapply(cols, function(t) fn(d, t, generalized = "ignore"))),
      seed = 1L
    )$RESULT)
    expect_lt(quiet, truth / 5)
  }
})

test_that("the set-similarity and scoreboard scores refuse a generalised column too", {
  ## Not declarable types, so not reachable from attacker_knowledge() -- but
  ## exported, documented and just as silent before #100.
  d <- gen_fixture()
  expect_error(score_jaccard(d, "AGE"), regexp = "score_containment")
  expect_error(score_minhash(d, "AGE"), regexp = "score_containment")
  expect_error(score_scoreboard(d, "AGE"), regexp = "score_containment")
  expect_error(score_mahalanobis(d, "AGE"), regexp = "score_containment")
})

test_that("the guard message names the declaration form of the remedy", {
  ## check_generalized_target() names score_containment(), which is the whole
  ## answer for hand-written score calls and none of it for the W/M/S workflow,
  ## where columns are declared rather than called. #103 is about that gap in
  ## the vignette; this is the same gap in the error message.
  d <- gen_fixture()
  expect_error(score_idf(d, "AGE"), regexp = "containment", fixed = FALSE)
  msg <- tryCatch(score_idf(d, "AGE"), error = conditionMessage)
  expect_match(msg, "attacker_knowledge() or score_multi()", fixed = TRUE)
  expect_match(msg, "AGE = \"containment\"", fixed = TRUE)
})

## ---------------------------------------------------------------------------
## no false positives, across every guarded score
## ---------------------------------------------------------------------------

test_that("no guarded score fires on ordinary (non-generalised) data", {
  ## The other half of the trade: a guard that misfires on normal data is one
  ## people switch off. Every "refuse" function is run on a fixture with no
  ## generalisation anywhere, and none of them may mention it.
  set.seed(11)
  n <- 40
  raw <- data.frame(
    ROW_NUMBER = seq_len(n),
    AGE = sample(20:69, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  anon <- raw
  anon$AGE <- raw$AGE + sample(c(-1, 0, 1), n, replace = TRUE)
  d <- join_raw_anon_data(raw, anon)

  policy <- reid_generalized_guard_policy()
  refusing <- names(policy)[policy == "refuse"]
  flagged <- vapply(refusing, function(fn_name) {
    msg <- tryCatch({
      suppressWarnings(gen_call(fn_name, d))
      NA_character_
    }, error = conditionMessage)
    !is.na(msg) && grepl("is generalised on the ANON side", msg, fixed = TRUE)
  }, logical(1))

  expect_equal(names(which(flagged)), character(0))
  ## and the run was not vacuous
  expect_gt(length(refusing), 10)
})
