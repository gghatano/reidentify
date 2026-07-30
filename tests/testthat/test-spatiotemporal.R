## Issue #24 -- spatio-temporal unicity (k-point identification).
##
## de Montjoye et al., "Unique in the Crowd": four approximate (place, time)
## points identified 95% of the individuals in a mobility data set. What is
## pinned down here is that the measurement responds to coarsening the way a
## defence has to -- monotonically -- and that the number it reports is
## understood as the lower bound it is, not the upper bound it looks like.


## ---------------------------------------------------------------------------
## the generator extension
## ---------------------------------------------------------------------------

test_that("create_dummy_transaction_data() keeps its old schema by default (#24)", {
  set.seed(1)
  t <- create_dummy_transaction_data(people = 5, size = 3)
  expect_identical(
    names(t),
    c("ROW_NUMBER", "ID", "NUM_STATIC", "NUM_DYNAMIC", "BIN", "CHAR")
  )
})

test_that("switching on the spatio-temporal columns does not move the old ones", {
  ## The new draws happen after the old ones, so the same seed has to produce
  ## the same NUM_STATIC / NUM_DYNAMIC / BIN / CHAR either way. Anything else
  ## would silently change every existing fixture that seeds the generator.
  set.seed(11)
  plain <- create_dummy_transaction_data(people = 8, size = 4)
  set.seed(11)
  extended <- create_dummy_transaction_data(people = 8, size = 4,
                                            spatiotemporal = TRUE)

  expect_equal(names(extended), c(names(plain), "PLACE", "TIME"))
  expect_equal(as.data.frame(extended[names(plain)]), as.data.frame(plain))
})

test_that("the spatio-temporal columns have the shape the unicity measurement needs", {
  t <- create_dummy_transaction_data(people = 20, size = 10,
                                     spatiotemporal = TRUE, places = 30,
                                     days = 7, seed = 2)
  expect_equal(nrow(t), 200)
  expect_type(t$PLACE, "character")
  expect_type(t$TIME, "double")

  ## Zero-padded, so sorting the codes orders them the way coarsen_place()
  ## assumes when it merges neighbours.
  expect_true(all(grepl("^P[0-9]{3}$", t$PLACE)))
  expect_lte(length(unique(t$PLACE)), 30)
  expect_gte(min(t$TIME), 0)
  expect_lt(max(t$TIME), 7 * 24)

  ## People must differ from each other, or the unicity curve measures nothing.
  by_person <- tapply(t$PLACE, t$ID, function(v) length(unique(v)))
  expect_true(mean(by_person) < length(unique(t$PLACE)))
})

test_that("create_dummy_transaction_data() is reproducible via its seed argument", {
  a <- create_dummy_transaction_data(people = 6, size = 5,
                                     spatiotemporal = TRUE, seed = 4)
  b <- create_dummy_transaction_data(people = 6, size = 5,
                                     spatiotemporal = TRUE, seed = 4)
  expect_equal(as.data.frame(a), as.data.frame(b))

  c2 <- create_dummy_transaction_data(people = 6, size = 5,
                                      spatiotemporal = TRUE, seed = 5)
  expect_false(isTRUE(all.equal(as.data.frame(a), as.data.frame(c2))))
})

test_that("create_dummy_transaction_data() leaves the ambient RNG alone when seeded", {
  set.seed(77)
  before <- runif(1)
  set.seed(77)
  create_dummy_transaction_data(people = 5, size = 3, spatiotemporal = TRUE,
                                seed = 9)
  expect_equal(before, runif(1))
})

test_that("create_dummy_transaction_data() validates the new arguments", {
  expect_error(create_dummy_transaction_data(5, 2, spatiotemporal = TRUE, places = 0),
               "places")
  expect_error(create_dummy_transaction_data(5, 2, spatiotemporal = TRUE, places = "a"),
               "places")
  expect_error(create_dummy_transaction_data(5, 2, spatiotemporal = TRUE, days = 0),
               "days")
  ## the original checks are untouched
  expect_error(create_dummy_transaction_data(people = 0), "people")
  expect_error(create_dummy_transaction_data(people = 5, size = 0), "size")
})


## ---------------------------------------------------------------------------
## coarsening
## ---------------------------------------------------------------------------

test_that("coarsen_time() bins onto an absolute grid", {
  expect_equal(coarsen_time(c(0, 1, 5, 23, 24, 25), resolution = 12),
               c(0, 0, 0, 1, 2, 2))
  expect_equal(coarsen_time(c(0, 1, 23, 24), resolution = 24), c(0, 0, 0, 1))
  ## resolution 1 is the identity on whole numbers
  expect_equal(coarsen_time(c(0, 3, 17), resolution = 1), c(0, 3, 17))
})

test_that("coarsen_time() accepts a POSIXct and rejects a resolution that is not one", {
  tt <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + c(0, 3600, 7200)
  expect_equal(coarsen_time(tt, resolution = 3600),
               floor(as.numeric(tt) / 3600))

  expect_error(coarsen_time(1:3, resolution = 0), "single positive number")
  expect_error(coarsen_time(1:3, resolution = -1), "single positive number")
  expect_error(coarsen_time(1:3, resolution = c(1, 2)), "single positive number")
  expect_error(coarsen_time(c("a", "b")), "could not be read as numeric time")
})

test_that("coarsen_place() merges runs of neighbouring codes in sort order", {
  expect_equal(coarsen_place(c("P001", "P002", "P003", "P004"), resolution = 2),
               c(1L, 1L, 2L, 2L))
  ## resolution 1 gives every distinct location its own cell
  expect_equal(coarsen_place(c("B", "A", "C", "A"), resolution = 1),
               c(2L, 1L, 3L, 1L))
  ## grouping is over the values present, not an absolute grid: "P009" is the
  ## fifth distinct code here, not the ninth
  expect_equal(coarsen_place(c("P001", "P002", "P003", "P004", "P009"), resolution = 2),
               c(1L, 1L, 2L, 2L, 3L))
})

test_that("coarsen_place() collapses everything once the resolution covers the range", {
  x <- sprintf("P%03d", 1:7)
  expect_equal(length(unique(coarsen_place(x, resolution = 7))), 1)
  expect_error(coarsen_place(x, resolution = 0), "single number >= 1")
  expect_error(coarsen_place(x, resolution = c(1, 2)), "single number >= 1")
})


## ---------------------------------------------------------------------------
## the measurement, on a fixture small enough to work out by hand
## ---------------------------------------------------------------------------

## A: (P1, t0), (P1, t1)     -- t1 at P1 is hers alone
## B: (P1, t0), (P2, t1)     -- indistinguishable from C
## C: (P1, t0), (P2, t1)
hand_fixture <- data.frame(
  ID = c("A", "A", "B", "B", "C", "C"),
  PLACE = c("P1", "P1", "P1", "P2", "P1", "P2"),
  TIME = c(0, 1, 0, 1, 0, 1),
  stringsAsFactors = FALSE
)

test_that("spatiotemporal_unicity() reproduces a hand-computed answer at k = 1", {
  r <- spatiotemporal_unicity(hand_fixture, k = 1)

  ## A's two points give anonymity sets of 3 and 1 -> unique on half her draws;
  ## B and C are never alone.
  expect_equal(r$unicity, (0.5 + 0 + 0) / 3)
  expect_equal(r$expected_id_rate,
               (mean(c(1 / 3, 1)) + mean(c(1 / 3, 1 / 2)) + mean(c(1 / 3, 1 / 2))) / 3)
  expect_equal(r$mean_anonymity_set,
               (mean(c(3, 1)) + mean(c(3, 2)) + mean(c(3, 2))) / 3)
  expect_equal(r$n_individuals, 3)
  expect_equal(r$n_evaluated, 3)
  expect_equal(r$n_points, 3)
  expect_true(r$exhaustive)
})

test_that("spatiotemporal_unicity() reproduces a hand-computed answer at k = 2", {
  r <- spatiotemporal_unicity(hand_fixture, k = 2)

  ## Both of A's points together single her out; B and C hold identical traces
  ## and cannot be separated by any number of points.
  expect_equal(r$unicity, 1 / 3)
  expect_equal(r$expected_id_rate, (1 + 0.5 + 0.5) / 3)
  expect_equal(r$mean_anonymity_set, (1 + 2 + 2) / 3)
  expect_true(r$exhaustive)
})

test_that("coarsening the two locations together destroys the one unique trace", {
  ## P1 and P2 merge into a single cell, so A's trace becomes identical to
  ## B's and C's and nobody is unique any more.
  r <- spatiotemporal_unicity(hand_fixture, k = 2, space_resolution = 2)
  expect_equal(r$unicity, 0)
  expect_equal(r$mean_anonymity_set, 3)
  expect_equal(r$n_points, 2)
})

test_that("individuals with fewer than k distinct points are excluded, not counted as safe", {
  ## D has a single point. At k = 2 an attacker cannot hold two of her points,
  ## so she is not part of the measurement; counting her as "not unique" would
  ## credit the data with safety it has not got.
  dat <- rbind(hand_fixture,
               data.frame(ID = "D", PLACE = "P9", TIME = 5,
                          stringsAsFactors = FALSE))
  r <- spatiotemporal_unicity(dat, k = 1:2)

  expect_equal(r$n_individuals, c(4, 4))
  expect_equal(r$n_evaluated, c(4, 3))
  ## and the k = 2 row is the three-person answer from above, unchanged
  expect_equal(r$unicity[r$k == 2], 1 / 3)
})


## ---------------------------------------------------------------------------
## the properties the issue asks to be verified
## ---------------------------------------------------------------------------

sweep_fixture <- function() {
  create_dummy_transaction_data(people = 80, size = 25, spatiotemporal = TRUE,
                                places = 40, days = 14, seed = 1)
}

test_that("coarsening the time resolution lowers unicity monotonically (#24)", {
  r <- spatiotemporal_unicity(sweep_fixture(), k = c(1, 2, 4),
                              time_resolution = c(1, 6, 24, 24 * 7))
  for (kk in unique(r$k)) {
    v <- r$unicity[r$k == kk][order(r$time_resolution[r$k == kk])]
    expect_true(all(diff(v) <= 1e-12),
                info = paste("k =", kk, ":", paste(round(v, 4), collapse = " ")))
  }
  ## and it is a real effect, not a flat line
  expect_gt(max(r$unicity) - min(r$unicity), 0.5)
})

test_that("coarsening the space resolution lowers unicity monotonically (#24)", {
  r <- spatiotemporal_unicity(sweep_fixture(), k = c(1, 2, 4),
                              time_resolution = 24,
                              space_resolution = c(1, 2, 5, 10))
  for (kk in unique(r$k)) {
    v <- r$unicity[r$k == kk][order(r$space_resolution[r$k == kk])]
    expect_true(all(diff(v) <= 1e-12),
                info = paste("k =", kk, ":", paste(round(v, 4), collapse = " ")))
  }
  expect_gt(max(r$unicity) - min(r$unicity), 0.3)
})

test_that("knowing more points can only help the attacker", {
  r <- spatiotemporal_unicity(sweep_fixture(), k = 1:4, time_resolution = 24)
  v <- r$unicity[order(r$k)]
  expect_true(all(diff(v) >= -1e-12), info = paste(round(v, 4), collapse = " "))
})

test_that("unicity is a lower bound on the attack, never an upper one (#21, #24)", {
  ## The correction Issue #21 established by measurement: a record sharing its
  ## points with m - 1 others counts 0 towards unicity, but an attacker
  ## guessing inside the set still wins with probability 1/m.
  r <- spatiotemporal_unicity(sweep_fixture(), k = 1:3,
                              time_resolution = c(1, 24, 24 * 7),
                              space_resolution = c(1, 5))
  expect_true(all(r$unicity <= r$expected_id_rate + 1e-12))
  expect_true(all(r$expected_id_rate <= 1 + 1e-12))
  expect_true(all(r$mean_anonymity_set >= 1 - 1e-12))

  ## The gap is the part a unicity-only report would hide, and it is not small.
  expect_gt(max(r$expected_id_rate - r$unicity), 0.1)
})

test_that("a handful of points identifies nearly everybody at full resolution", {
  ## The de Montjoye result the issue is named after: the curve has to rise
  ## steeply in k when nothing has been coarsened.
  r <- spatiotemporal_unicity(sweep_fixture(), k = c(1, 4), time_resolution = 1)
  expect_lt(r$unicity[r$k == 1], 0.99)
  expect_gt(r$unicity[r$k == 4], 0.95)
})


## ---------------------------------------------------------------------------
## shape, reproducibility, arguments
## ---------------------------------------------------------------------------

test_that("spatiotemporal_unicity() returns one row per grid point, in a stable order", {
  r <- spatiotemporal_unicity(sweep_fixture(), k = 1:2,
                              time_resolution = c(1, 24),
                              space_resolution = c(1, 4))
  expect_equal(nrow(r), 8)
  expect_equal(
    names(r),
    c("k", "time_resolution", "space_resolution", "n_individuals",
      "n_evaluated", "n_points", "exhaustive", "unicity", "expected_id_rate",
      "mean_anonymity_set")
  )
  expect_equal(r$space_resolution, rep(c(1, 4), each = 4))
  expect_equal(r$time_resolution, rep(rep(c(1, 24), each = 2), 2))
  expect_equal(r$k, rep(c(1, 2), 4))
  ## a plain data frame, like unicity(), so it goes straight into a report
  expect_identical(class(r), "data.frame")
  expect_identical(rownames(r), as.character(seq_len(nrow(r))))
})

test_that("spatiotemporal_unicity() is reproducible and honest about being sampled", {
  dat <- sweep_fixture()
  a <- spatiotemporal_unicity(dat, k = 3, time_resolution = 24, seed = 0L)
  b <- spatiotemporal_unicity(dat, k = 3, time_resolution = 24, seed = 0L)
  expect_equal(a, b)

  ## Long traces have far more k-subsets than n_samples, so the row is an
  ## estimate and says so.
  expect_false(a$exhaustive)
  c2 <- spatiotemporal_unicity(dat, k = 3, time_resolution = 24, seed = 1L)
  expect_true(abs(a$unicity - c2$unicity) < 0.1)
})

test_that("raising n_samples does not move the answer much", {
  dat <- sweep_fixture()
  low <- spatiotemporal_unicity(dat, k = 2, time_resolution = 24, n_samples = 5)
  high <- spatiotemporal_unicity(dat, k = 2, time_resolution = 24, n_samples = 60)
  expect_lt(abs(low$unicity - high$unicity), 0.1)
})

test_that("column names can be pointed anywhere", {
  dat <- hand_fixture
  names(dat) <- c("person", "cell", "stamp")
  r <- spatiotemporal_unicity(dat, id = "person", place = "cell", time = "stamp",
                              k = 2)
  expect_equal(r$unicity, 1 / 3)
})

test_that("spatiotemporal_unicity() validates its arguments", {
  expect_error(spatiotemporal_unicity(1:5), "must be a data frame")
  expect_error(spatiotemporal_unicity(hand_fixture, id = "nope"),
               "column\\(s\\) not found")
  expect_error(
    spatiotemporal_unicity(hand_fixture[0, ]),
    "no rows"
  )
  expect_error(spatiotemporal_unicity(hand_fixture, k = 0),
               "at least one point")
  expect_error(spatiotemporal_unicity(hand_fixture, k = -1),
               "at least one point")
  expect_error(spatiotemporal_unicity(hand_fixture, time_resolution = 0),
               "positive numbers")
  expect_error(spatiotemporal_unicity(hand_fixture, space_resolution = -2),
               "positive numbers")
  expect_error(spatiotemporal_unicity(hand_fixture, n_samples = 0),
               "n_samples")
})

test_that("a missing place or time is refused rather than matched against other NAs", {
  ## Two rows with a missing location would share the literal point "NA", which
  ## inflates the anonymity set and reports a *lower* unicity than the data
  ## supports -- the under-reporting direction.
  for (cn in c("ID", "PLACE", "TIME")) {
    dat <- hand_fixture
    dat[[cn]][2] <- NA
    expect_error(spatiotemporal_unicity(dat, k = 1),
                 paste0("column \"", cn, "\" contains NA"))
  }
})

test_that("k larger than every trace leaves nothing to measure and reports NA", {
  r <- spatiotemporal_unicity(hand_fixture, k = 5)
  expect_equal(r$n_evaluated, 0L)
  expect_true(is.na(r$unicity))
  expect_true(is.na(r$expected_id_rate))
  ## no answer to be exact about either
  expect_true(is.na(r$exhaustive))
})

test_that("a population of identical traces is never unique at any k", {
  dat <- data.frame(
    ID = rep(c("A", "B", "C"), each = 3),
    PLACE = rep(c("P1", "P2", "P3"), times = 3),
    TIME = rep(c(0, 1, 2), times = 3),
    stringsAsFactors = FALSE
  )
  r <- spatiotemporal_unicity(dat, k = 1:3)
  expect_true(all(r$unicity == 0))
  expect_true(all(r$mean_anonymity_set == 3))
  expect_true(all(abs(r$expected_id_rate - 1 / 3) < 1e-12))
})

test_that("a population of disjoint traces is always unique", {
  dat <- data.frame(
    ID = rep(c("A", "B", "C"), each = 2),
    PLACE = sprintf("P%d", 1:6),
    TIME = 1:6,
    stringsAsFactors = FALSE
  )
  r <- spatiotemporal_unicity(dat, k = 1:2)
  expect_true(all(r$unicity == 1))
  expect_true(all(r$expected_id_rate == 1))
  expect_true(all(r$mean_anonymity_set == 1))
})

test_that("spatiotemporal_unicity() runs on the dummy generator's own output", {
  ## The end-to-end path the issue asked for: the generator now emits the
  ## columns, and the measurement reads them with its default column names.
  tran <- create_dummy_transaction_data(people = 30, size = 12,
                                        spatiotemporal = TRUE, seed = 3)
  r <- spatiotemporal_unicity(tran, k = c(1, 2))
  expect_equal(nrow(r), 2)
  expect_equal(r$n_individuals, c(30, 30))
  expect_true(all(r$unicity >= 0 & r$unicity <= 1))
})
