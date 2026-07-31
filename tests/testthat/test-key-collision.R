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
