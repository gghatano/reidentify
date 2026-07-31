## ---------------------------------------------------------------------------
## One definition of "these two rows are the same" (Issues #58, #70)
##
## Four places in the package have to decide whether two multi-column values
## agree: unicity, blocking, the generalisation hierarchy, and the score
## combiner. Each of them used to write its own paste(..., sep = "\r"), and a
## separator inside a value made that answer wrong -- in a different direction
## in each place, which is worse than being wrong in one:
##
##   unicity     -- two distinct records collapse onto one key, so the reported
##                  unicity goes DOWN. #58; the safe-looking direction.
##   blocking    -- two records that disagree land in one block, so the
##                  candidate set grows and the reported reduction is not the
##                  reduction that was asked for. #70.
##   hierarchy   -- two distinct (attribute, value) edges look like one value
##                  with two parents, so a legitimate tree is REJECTED. #70.
##
## The fixture below is the smallest thing that breaks all of them: two rows
## that share a naive concatenation and agree on nothing.
## ---------------------------------------------------------------------------

## ("a", "b\rc") and ("a\rb", "c") both paste to "a\rb\rc".
collide_fixture <- function(sep = "\r") {
  data.frame(
    ROW_NUMBER = 1:2,
    P = c("a", paste0("a", sep, "b")),
    Q = c(paste0("b", sep, "c"), "c"),
    stringsAsFactors = FALSE
  )
}

separators <- c(CR = "\r", LF = "\n", TAB = "\t", CRLF = "\r\n")

test_that("the fixture really would collide under a naive paste", {
  ## Guards the guard: if this stopped being true the tests below would still
  ## pass while checking nothing.
  d <- collide_fixture()
  naive <- paste(d$P, d$Q, sep = "\r")
  expect_equal(naive[1], naive[2])
  expect_false(d$P[1] == d$P[2])
  expect_false(d$Q[1] == d$Q[2])
})

test_that("unicity keeps two records distinct despite the separator", {
  d <- collide_fixture()
  expect_equal(unicity_fraction(d, c("P", "Q")), 1)
  expect_equal(anyDuplicated(reidentify:::unicity_key(d, c("P", "Q"))), 0L)
})

test_that("blocking keeps two records in separate blocks despite the separator", {
  for (nm in names(separators)) {
    d <- collide_fixture(separators[[nm]])
    cand <- block_candidates(d, d, keys = c("P", "Q"))

    ## only the two self-pairs: a collision would give all four
    expect_equal(nrow(cand), 2L, info = nm)
    expect_equal(cand$RAW_ROW_NUMBER, cand$ANON_ROW_NUMBER, info = nm)

    info <- attr(cand, "blocking")
    expect_equal(info$recall, 1, info = nm)
    expect_equal(info$n_pairs_kept, 2, info = nm)
    expect_equal(info$kept_fraction, 0.5, info = nm)
    expect_equal(info$reduction, 0.5, info = nm)
    expect_equal(info$n_anon_without_candidate, 0, info = nm)
  }
})

test_that("blocking agrees with unicity about which records are alike", {
  ## The two layers must not disagree about equality: a record unicity calls
  ## unique cannot be one blocking puts in a shared block, or the reduction
  ## and the uniqueness figure describe different data sets.
  d <- data.frame(
    ROW_NUMBER = 1:6,
    P = c("a", "a\rb", "a", "x", "a\rb", "x"),
    Q = c("b\rc", "c", "b\rc", "y", "c", "y"),
    stringsAsFactors = FALSE
  )
  key <- reidentify:::unicity_key(d, c("P", "Q"))
  cand <- block_candidates(d, d, keys = c("P", "Q"))

  ## blocked together exactly when unicity gives the same key
  blocked <- paste(cand$RAW_ROW_NUMBER, cand$ANON_ROW_NUMBER)
  expected <- expand.grid(r = 1:6, a = 1:6)
  expected <- expected[key[expected$r] == key[expected$a], ]
  expect_setequal(blocked, paste(expected$r, expected$a))
})

test_that("a transform that produces the separator does not merge blocks", {
  ## `transform` is user code, so its output is the least constrained value in
  ## the whole key path.
  d <- data.frame(
    ROW_NUMBER = 1:2,
    P = c("a", "a-b"),
    Q = c("b-c", "c"),
    stringsAsFactors = FALSE
  )
  swap <- function(x) gsub("-", "\r", x, fixed = TRUE)
  cand <- block_candidates(d, d, keys = c("P", "Q"),
                           transform = list(P = swap, Q = swap))
  expect_equal(nrow(cand), 2L)
})

test_that("blocking still puts genuinely equal rows together", {
  ## The other side of the fix: making keys collision-free must not make them
  ## over-discriminating. Codes are built over both sides at once precisely so
  ## that a RAW and an ANON record holding the same value still match.
  raw <- data.frame(ROW_NUMBER = 1:4, P = c("a\rb", "a\rb", "z", "q"),
                    stringsAsFactors = FALSE)
  ## the ANON side stores the same values as a factor, which is the other way
  ## the two sides routinely differ
  anon <- data.frame(ROW_NUMBER = 1:4, P = factor(raw$P))
  cand <- block_candidates(raw, anon, keys = "P")

  expect_true(all(cand$RAW_P == as.character(cand$ANON_P)))
  ## "a\rb": 2 RAW x 2 ANON = 4, "z": 1, "q": 1
  expect_equal(nrow(cand), 6L)
  expect_equal(attr(cand, "blocking")$recall, 1)

  ## a value present on one side only forms no block at all
  only_raw <- data.frame(ROW_NUMBER = 1:2, P = c("a", "b"),
                         stringsAsFactors = FALSE)
  only_anon <- data.frame(ROW_NUMBER = 1:2, P = c("a", "c"),
                          stringsAsFactors = FALSE)
  cand2 <- suppressWarnings(block_candidates(only_raw, only_anon, keys = "P"))
  expect_equal(nrow(cand2), 1L)
})

test_that("a generalisation hierarchy is not rejected because of a separator", {
  ## The dangerous reading of the old key: two different edges looked like one
  ## value with two parents, so generalization_hierarchy() refused a tree that
  ## is perfectly well formed -- and blamed the wrong value in the message.
  for (nm in names(separators)) {
    s <- separators[[nm]]
    h <- generalization_hierarchy(data.frame(
      attribute = c("A", paste0("A", s, "x")),
      value = c(paste0("x", s, "y"), "y"),
      parent = c("p1", "p2"),
      stringsAsFactors = FALSE
    ))
    expect_s3_class(h, "reid_hierarchy")
    expect_equal(nrow(h$edges), 2L, info = nm)
  }
})

test_that("a real two-parent edge is still rejected", {
  ## The fix must not have bought the above by weakening the check.
  expect_error(
    generalization_hierarchy(data.frame(
      attribute = c("A", "A"),
      value = c("x", "x"),
      parent = c("p1", "p2"),
      stringsAsFactors = FALSE
    )),
    "more than one parent"
  )
  ## including when the separator is inside the value both edges share
  expect_error(
    generalization_hierarchy(data.frame(
      attribute = c("A", "A"),
      value = c("x\ry", "x\ry"),
      parent = c("p1", "p2"),
      stringsAsFactors = FALSE
    )),
    "more than one parent"
  )
  ## and the same value under a different attribute is not a conflict
  expect_s3_class(
    generalization_hierarchy(data.frame(
      attribute = c("A", "B"),
      value = c("x", "x"),
      parent = c("p1", "p2"),
      stringsAsFactors = FALSE
    )),
    "reid_hierarchy"
  )
})

## ---------------------------------------------------------------------------
## Issue #73 -- the two places #70 left behind
##
## MEASURED BEFORE THE FIX, because the assumption in the issue was wrong. The
## report said spatiotemporal_unicity() pasted the *user's* place column with
## the separator. It does not: coarsen_place() returns an integer and
## coarsen_time() a double, so a carriage return in a place name never reaches
## the paste, and a fixture built out of separators finds nothing.
##
## What does reach it is the second failure mode of unicity_key(): paste()
## prints a double to 15 significant digits. coarsen_time() returns a double,
## so on a microsecond clock (~1.75e15) every distinct instant printed as the
## same "1.75e+15" and the points collapsed onto one -- and collapsed points
## enlarge every anonymity set, so the reported unicity FALLS. That is the
## under-reporting direction of docs/lessons-learned.md section 2.
##
## The score layer is the other way round. There the row numbers really are the
## user's values, and both directions were measured:
##   validate_unique_candidate_pairs() -- a collision REJECTED a valid table
##       and named a pair that does not repeat. Loud, so not the dangerous one.
##   combine_scores() -- the dangerous one. Its "different candidate sets"
##       guard exists to stop a combination that would "silently drop
##       candidates and under-report the reidentification rate", and a
##       collision walked straight past it.
## ---------------------------------------------------------------------------

## Number of distinct (place, time) points, computed without any concatenation:
## codes in 1..P are packed into one number by arithmetic, which cannot collide.
distinct_points <- function(place, time) {
  pc <- match(place, unique(place))
  tc <- match(time, unique(time))
  length(unique(pc + (tc - 1) * (max(pc) + 1)))
}

## A clock whose ticks are far enough apart to matter and large enough in
## magnitude that as.character() cannot tell them apart. Microseconds since the
## epoch -- what a Postgres timestamp holds -- are about 1.75e15, and 15
## significant digits stop resolving just below that. The value is still an
## exact double (2^53 is 9.0e15), so nothing here is floating-point noise: the
## five instants really are five distinct numbers, and only their printed form
## loses them.
microsecond_trace <- function(n_people = 5, magnitude = 1.75e15) {
  data.frame(
    ID = paste0("i", seq_len(n_people)),
    PLACE = rep("P001", n_people),
    TIME = magnitude + seq_len(n_people),
    stringsAsFactors = FALSE
  )
}

test_that("the microsecond fixture really would collide under a naive paste", {
  ## Guards the guard, as above: without this the tests below could pass while
  ## measuring nothing.
  d <- microsecond_trace()
  expect_equal(length(unique(d$TIME)), 5L)
  naive <- paste(coarsen_place(d$PLACE, 1), coarsen_time(d$TIME, 1), sep = "\r")
  expect_equal(length(unique(naive)), 1L)
})

test_that("spatio-temporal unicity survives a clock finer than 15 digits (#73)", {
  d <- microsecond_trace()
  out <- spatiotemporal_unicity(d, id = "ID", place = "PLACE", time = "TIME",
                                k = 1, seed = 1)

  ## Five people, five distinct instants, one place: every trace is unique.
  ## Before the fix this reported n_points 1, unicity 0, anonymity set 5.
  expect_equal(out$n_points, 5)
  expect_equal(out$unicity, 1)
  expect_equal(out$expected_id_rate, 1)
  expect_equal(out$mean_anonymity_set, 1)
})

test_that("the reported point count matches the truth over a magnitude sweep", {
  ## The generator is the point. Timestamps drawn from a plain runif() never
  ## reach the digits where as.character() gives up, so an ordinary property
  ## test finds nothing here -- the same way #58's first attempt found nothing.
  ## This one sweeps the magnitude across the 15-digit boundary and puts the
  ## separator at the start, the middle and the end of the place labels.
  set.seed(73)
  magnitudes <- c(1e2, 1e6, 1e12, 1e14, 1e15, 4e15, 8e15)
  labels <- c("P1", "\rP2", "P\r3", "P4\r", "P5")

  violations <- 0L
  for (m in magnitudes) {
    for (rep in 1:4) {
      n <- 12L
      d <- data.frame(
        ID = paste0("i", sample.int(4L, n, replace = TRUE)),
        PLACE = sample(labels, n, replace = TRUE),
        TIME = m + sample.int(6L, n, replace = TRUE),
        stringsAsFactors = FALSE
      )
      for (sr in c(1, 2)) {
        for (tr in c(1, 3)) {
          out <- spatiotemporal_unicity(d, id = "ID", place = "PLACE",
                                        time = "TIME", k = 1,
                                        space_resolution = sr,
                                        time_resolution = tr, seed = 1)
          truth <- distinct_points(coarsen_place(d$PLACE, sr),
                                   coarsen_time(d$TIME, tr))
          if (!isTRUE(all.equal(out$n_points, truth))) {
            violations <- violations + 1L
          }
        }
      }
    }
  }
  ## 112 checks. Before the fix 24 failed, all of them at 1e15 and above and
  ## every one reporting FEWER points than the data holds -- 24 under, 0 over.
  ## The separators in `labels` contributed nothing on their own, which is the
  ## measurement that corrected the issue's account of this defect; they are
  ## kept so that the case stays covered if coarsen_place() ever stops
  ## returning codes.
  expect_equal(violations, 0L)
})

test_that("combine_scores still refuses tables whose pairs merely collide (#73)", {
  ## The dangerous direction. These two tables share no candidate pair, but
  ## ("a", "b\rc") and ("a\rb", "c") paste to the same string, so the naive key
  ## made them look identical and the combination went through silently.
  s1 <- reidentify:::new_reid_scores(raw_row_number  = c("b\rc", "y"),
                                     anon_row_number = c("a", "x"),
                                     score = c(10, 20))
  s2 <- reidentify:::new_reid_scores(raw_row_number  = c("c", "y"),
                                     anon_row_number = c("a\rb", "x"),
                                     score = c(1, 2))
  expect_error(combine_scores(list(s1, s2)), "does not cover the same")
})

test_that("combine_scores accepts a valid table whose row numbers hold separators", {
  s <- reidentify:::new_reid_scores(raw_row_number  = c("b\rc", "c"),
                                    anon_row_number = c("a", "a\rb"),
                                    score = c(1, 2))
  out <- combine_scores(list(s, s))
  expect_equal(out$SCORE, c(2, 4))
  expect_equal(out$RAW_ROW_NUMBER, c("b\rc", "c"))

  ## and a genuine duplicate is still refused
  dup <- reidentify:::new_reid_scores(raw_row_number  = c("r", "r"),
                                      anon_row_number = c("a", "a"),
                                      score = c(1, 2))
  expect_error(combine_scores(list(dup, dup)), "duplicated")
})

test_that("the duplicate-pair guard neither invents nor misses a duplicate (#73)", {
  ## Before the fix this rejected the valid table and blamed (ANON "a\rb",
  ## RAW "c"), a pair appearing exactly once.
  ok <- reidentify:::new_reid_scores(raw_row_number  = c("b\rc", "c"),
                                     anon_row_number = c("a", "a\rb"),
                                     score = c(1, 2))
  expect_silent(reidentify:::validate_unique_candidate_pairs(ok, "demo"))

  bad <- reidentify:::new_reid_scores(raw_row_number  = c("x\ry", "x\ry"),
                                      anon_row_number = c("a", "a"),
                                      score = c(1, 2))
  expect_error(reidentify:::validate_unique_candidate_pairs(bad, "demo"),
               "duplicated")
})

test_that("only reid_value_key and band_keys embed the key separator (#73)", {
  ## THE RECURRENCE CHECK. The same defect has now been written four times in
  ## four files (#58, #70, #73), so the package is asked directly rather than
  ## the reviewer being asked to remember. Reading the namespace rather than
  ## the R/ sources means this holds for the installed package too, and cannot
  ## be fooled by a comment or a roxygen block that mentions the separator.
  ns <- asNamespace("reidentify")
  nms <- ls(ns, all.names = TRUE)
  offenders <- Filter(function(n) {
    obj <- get(n, envir = ns)
    is.function(obj) && grepl("\\r", paste(deparse(obj), collapse = "\n"),
                              fixed = TRUE)
  }, nms)

  ## reid_value_key() -- the one sanctioned key builder, joining integer codes
  ##   whose decimal form cannot contain the separator.
  ## band_keys()      -- justified in place in R/setsim.R: every part is this
  ##   package's own bounded output, and the two sides are coded separately by
  ##   necessity, so class codes are not available there.
  ## Anything else means a value the user supplied is being pasted into a key.
  ## Use reid_value_key() over reid_class_codes(), or record why not here.
  expect_setequal(offenders, c("reid_value_key", "band_keys"))

  ## The scan has to be looking at something: before Issue #73 this same scan
  ## returned five names.
  expect_gt(length(nms), 100L)
})

test_that("reid_value_key is injective where a paste of the values is not", {
  a <- c("a", "a\rb", "", "\r", "x")
  b <- c("b\rc", "c", "\r", "", "x")
  key <- reidentify:::reid_value_key(list(reidentify:::reid_class_codes(a),
                                          reidentify:::reid_class_codes(b)))
  expect_equal(anyDuplicated(key), 0L)
  expect_gt(anyDuplicated(paste(a, b, sep = "\r")), 0L)

  ## equal inputs still give equal keys
  key2 <- reidentify:::reid_value_key(
    list(reidentify:::reid_class_codes(c("p", "p")),
         reidentify:::reid_class_codes(c("q", "q")))
  )
  expect_equal(key2[1], key2[2])

  expect_error(reidentify:::reid_value_key(list()), "at least one column")
})
