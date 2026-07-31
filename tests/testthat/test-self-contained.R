## These tests verify that reidentify's 8 exported functions work when
## dplyr/magrittr are NOT attached (only loaded as Imports). This guards
## against unqualified dplyr/magrittr calls (pull(), n(), %<>%, ...)
## silently depending on the caller having attached those packages.
##
## WHICH COPY IS BEING TESTED (Issue #39). The subprocess test below runs
## `library(reidentify)`, which loads whatever copy the library path happens to
## offer. Under `R CMD check` that is the freshly built copy and everything is
## fine. Under `pkgload::load_all()` + `test_dir()` it was a *different* copy:
## on 2026-07-30 a stale install (built the previous evening) sat in the user
## library, and for as long as it did, this test was passing on code nobody was
## editing. A green test that says nothing about the working tree is worse than
## no test, because it is indistinguishable from a real pass.
##
## Option (a) of the issue is implemented here: the copy under test is
## installed into a temporary library of our own, and that library is put at
## the front of the subprocess's R_LIBS. Two details matter.
##
##   * The temp library is *prepended* to the existing paths, never used alone.
##     reidentify's own Imports (dplyr, magrittr, stringi, ...) live in the
##     user/system libraries, and a subprocess that could not see them would
##     fail for a reason that has nothing to do with self-containedness.
##   * Under `R CMD check` there is no source tree to install from -- the tests
##     run inside `<pkg>.Rcheck` -- and none is needed, because the installed
##     copy *is* the copy under test. In that case the check library is used
##     directly, so the behaviour under `R CMD check` is unchanged.
##
## Either way the subprocess reports the library it actually loaded from, and
## the test asserts it is the intended one. That assertion is the part that
## cannot silently rot.

## the working tree, when this run is testing one (NULL under R CMD check)
package_source_dir <- function() {
  candidates <- list()
  if (requireNamespace("pkgload", quietly = TRUE)) {
    dev <- tryCatch(isTRUE(pkgload::is_dev_package("reidentify")),
                    error = function(e) FALSE)
    if (dev) {
      candidates <- c(candidates,
                      list(tryCatch(pkgload::pkg_path(), error = function(e) NULL)))
    }
  }
  candidates <- c(candidates,
                  list(tryCatch(normalizePath(test_path("..", ".."), winslash = "/"),
                                error = function(e) NULL)))

  for (p in candidates) {
    if (is.null(p) || !nzchar(p)) {
      next
    }
    desc <- file.path(p, "DESCRIPTION")
    if (!file.exists(desc) || !dir.exists(file.path(p, "R"))) {
      next
    }
    nm <- tryCatch(unname(read.dcf(desc, fields = "Package")[1, 1]),
                   error = function(e) NA_character_)
    if (identical(nm, "reidentify")) {
      return(normalizePath(p, winslash = "/"))
    }
  }
  NULL
}

## A library directory holding exactly the copy of reidentify this run is
## verifying. Built once per session: R CMD INSTALL is not free.
verified_library <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      return(cache)
    }

    src <- package_source_dir()
    if (is.null(src)) {
      ## installed run (R CMD check): the loaded copy is the copy under test
      cache <<- normalizePath(dirname(find.package("reidentify")), winslash = "/")
      return(cache)
    }

    lib <- file.path(tempdir(), "reidentify-verified-lib")
    dir.create(lib, showWarnings = FALSE, recursive = TRUE)
    log_file <- tempfile()
    status <- system2(
      file.path(R.home("bin"), "R"),
      args = c("--vanilla", "CMD", "INSTALL", "--no-docs", "--no-byte-compile",
               paste0("--library=", shQuote(normalizePath(lib, winslash = "/"))),
               shQuote(src)),
      stdout = log_file, stderr = log_file
    )
    if (status != 0L || !dir.exists(file.path(lib, "reidentify"))) {
      ## Do not skip(): "could not install the working tree" is a real failure,
      ## and skipping it would restore exactly the false comfort Issue #39 is
      ## about.
      stop("could not install the working tree at ", src, " into ", lib,
           ":\n", paste(readLines(log_file, warn = FALSE), collapse = "\n"))
    }
    cache <<- normalizePath(lib, winslash = "/")
    cache
  }
})

## run `code` in a fresh --vanilla Rscript with `libs` at the front of R_LIBS
run_fresh_rscript <- function(code, libs) {
  out_file <- tempfile()
  err_file <- tempfile()
  on.exit(unlink(c(out_file, err_file)), add = TRUE)

  ## system2()'s `env=` is not honoured on Windows, so set and restore it here.
  old <- Sys.getenv("R_LIBS", unset = NA)
  Sys.setenv(R_LIBS = paste(unique(c(libs, .libPaths())),
                            collapse = .Platform$path.sep))
  on.exit({
    if (is.na(old)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old)
  }, add = TRUE)

  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    args = c("--vanilla", "-e", shQuote(code)),
    stdout = out_file,
    stderr = err_file
  )

  list(
    status = status,
    stdout = paste(readLines(out_file, warn = FALSE), collapse = "\n"),
    stderr = paste(readLines(err_file, warn = FALSE), collapse = "\n")
  )
}

make_fixture <- function() {
  raw <- data.frame(
    ROW_NUMBER = 1:5, VAL = c(1, 2, 3, 4, 5),
    TXT = c("aa", "bb", "cc", "dd", "ee"),
    D = c("1:2", "2:3", "3:4", "4:5", "5:6"),
    stringsAsFactors = FALSE
  )
  anon <- data.frame(
    ROW_NUMBER = 1:5, VAL = c(1.1, 2.1, 3.1, 4.1, 5.1),
    TXT = c("aa", "bb", "cc", "dd", "ex"),
    D = c("1:2.1", "2:3.1", "3:4.1", "4:5.1", "5:6.1"),
    stringsAsFactors = FALSE
  )
  list(raw = raw, anon = anon)
}

test_that("dplyr/magrittr are not attached during testthat run", {
  # sanity check for the in-process test below: if this ever fails, the
  # in-process assertions no longer prove anything about self-containedness.
  expect_false("package:dplyr" %in% search())
  expect_false("package:magrittr" %in% search())
})

test_that("all 8 exported functions work in-process without dplyr/magrittr attached", {
  fx <- make_fixture()
  dra <- join_raw_anon_data(fx$raw, fx$anon)
  expect_true(is.data.frame(dra))

  r_num <- reid_by_num(dra, "VAL")
  expect_true(is.data.frame(r_num))

  r_rank <- reid_by_num_rank(dra, "VAL")
  expect_true(is.data.frame(r_rank))

  r_char <- reid_by_char(dra, "TXT")
  expect_true(is.data.frame(r_char))

  r_dist <- reid_by_dist(dra, "D")
  expect_true(is.data.frame(r_dist))

  txt <- reid_result(r_num, method = "x")
  expect_type(txt, "character")

  master_dummy <- create_dummy_master_data(5)
  expect_equal(nrow(master_dummy), 5)

  tran <- create_dummy_transaction_data(5, 2)
  expect_true(is.data.frame(tran))

  master <- transform_transaction_to_master(
    tran,
    STATIC_NUM = "NUM_STATIC", DYNAMIC_NUM = "NUM_DYNAMIC", DYNAMIC_CHAR = "CHAR"
  )
  expect_true(is.data.frame(master))
})

## the subprocess script: exercises the 8 functions, and reports which copy of
## the package it actually loaded so the caller can check it was the right one
self_contained_script <- paste(
  "library(reidentify)",
  "cat('LIB:', normalizePath(dirname(find.package('reidentify')), winslash = '/'), '\\n')",
  "cat('VER:', as.character(utils::packageVersion('reidentify')), '\\n')",
  "raw <- data.frame(ROW_NUMBER = 1:5, VAL = c(1,2,3,4,5),",
  "                   TXT = c('aa','bb','cc','dd','ee'),",
  "                   D = c('1:2','2:3','3:4','4:5','5:6'), stringsAsFactors = FALSE)",
  "anon <- data.frame(ROW_NUMBER = 1:5, VAL = c(1.1,2.1,3.1,4.1,5.1),",
  "                   TXT = c('aa','bb','cc','dd','ex'),",
  "                   D = c('1:2.1','2:3.1','3:4.1','4:5.1','5:6.1'), stringsAsFactors = FALSE)",
  "dra <- join_raw_anon_data(raw, anon)",
  "r <- reid_by_num(dra, 'VAL')",
  "reid_by_num_rank(dra, 'VAL')",
  "reid_by_char(dra, 'TXT')",
  "reid_by_dist(dra, 'D')",
  "reid_result(r, method = 'x')",
  "create_dummy_master_data(5)",
  "t <- create_dummy_transaction_data(5, 2)",
  "transform_transaction_to_master(t, STATIC_NUM = 'NUM_STATIC', DYNAMIC_NUM = 'NUM_DYNAMIC', DYNAMIC_CHAR = 'CHAR')",
  "stopifnot(!('package:dplyr' %in% search()))",
  "stopifnot(!('package:magrittr' %in% search()))",
  "cat('ALL OK\\n')",
  sep = "\n"
)

test_that("all 8 exported functions work in a fresh Rscript subprocess without attaching dplyr/magrittr", {
  # This is the decisive check: a brand-new R session that only does
  # library(reidentify) (no dplyr/magrittr attach at all), run out-of-process
  # so nothing testthat itself loads can mask a missing import.
  lib <- verified_library()
  res <- run_fresh_rscript(self_contained_script, lib)

  expect_equal(res$status, 0L, info = paste("stderr:\n", res$stderr))
  expect_match(res$stdout, "ALL OK", fixed = TRUE)
  expect_false(grepl("could not find function", res$stderr, fixed = TRUE))
})

test_that("the subprocess loaded the copy under test, not some other install (Issue #39)", {
  # Without this assertion the test above proves nothing about the working
  # tree: it passed for a whole day on a stale copy in the user library.
  lib <- verified_library()
  res <- run_fresh_rscript(self_contained_script, lib)

  ## NB: cat() puts a space between its arguments, so the reported line ends
  ## with a trailing space before the newline. Windows' normalizePath() strips
  ## it via the Win32 API but Linux does not, so trim here rather than relying
  ## on platform behaviour (see #55).
  loaded <- trimws(sub("^LIB:\\s*", "",
                       grep("^LIB:", strsplit(res$stdout, "\n")[[1]], value = TRUE)[1]))
  expect_false(is.na(loaded))
  expect_equal(normalizePath(loaded, winslash = "/", mustWork = FALSE), lib)
})

test_that("verified_library() holds the working tree when there is one, and the check library otherwise", {
  src <- package_source_dir()
  lib <- verified_library()

  if (is.null(src)) {
    ## R CMD check: no source tree; the installed copy is the copy under test
    expect_equal(lib, normalizePath(dirname(find.package("reidentify")),
                                    winslash = "/"))
  } else {
    ## dev run: the library was built from the working tree, in this session,
    ## and is not one of the ambient libraries
    expect_true(dir.exists(file.path(lib, "reidentify")))
    expect_true(startsWith(lib, normalizePath(tempdir(), winslash = "/")))
    expect_false(lib %in% normalizePath(.libPaths(), winslash = "/"))
  }
})

test_that("an older copy earlier in the user's libraries does not win (Issue #39)", {
  # The failure mode this issue is about, staged deliberately: a second library
  # containing a *different* build of reidentify. The verified library is
  # prepended, so the subprocess must report the version under test and the
  # path under test, not the decoy's.
  lib <- verified_library()
  decoy <- file.path(tempdir(), "reidentify-decoy-lib")
  unlink(decoy, recursive = TRUE)
  dir.create(decoy, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(decoy, recursive = TRUE), add = TRUE)

  ## a copy of the real install, relabelled -- cheap, and a genuinely loadable
  ## package, which a hand-written stub would not be
  file.copy(file.path(lib, "reidentify"), decoy, recursive = TRUE)
  desc <- file.path(decoy, "reidentify", "DESCRIPTION")
  d <- readLines(desc, warn = FALSE)
  d[startsWith(d, "Version:")] <- "Version: 0.0.0.1"
  writeLines(d, desc)
  ## packageVersion() reads Meta/package.rds in preference to DESCRIPTION, so
  ## relabelling only the text file leaves the decoy still calling itself
  ## 1.0.0.0 -- which is how this test first failed.
  meta_file <- file.path(decoy, "reidentify", "Meta", "package.rds")
  meta <- readRDS(meta_file)
  meta$DESCRIPTION[["Version"]] <- "0.0.0.1"
  saveRDS(meta, meta_file)

  real_version <- as.character(read.dcf(file.path(lib, "reidentify", "DESCRIPTION"),
                                        fields = "Version")[1, 1])
  expect_false(identical(real_version, "0.0.0.1"))

  ## Put the decoy on the *ambient* library path, which is how the real
  ## accident happened: a stale copy sitting in the user library, picked up by
  ## a plain library(reidentify). run_fresh_rscript() must still win.
  old_paths <- .libPaths()
  .libPaths(c(old_paths, normalizePath(decoy, winslash = "/")))
  on.exit(.libPaths(old_paths), add = TRUE)
  expect_true(normalizePath(decoy, winslash = "/") %in%
                normalizePath(.libPaths(), winslash = "/"))

  res <- run_fresh_rscript(self_contained_script, lib)
  expect_equal(res$status, 0L, info = paste("stderr:\n", res$stderr))

  lines <- strsplit(res$stdout, "\n")[[1]]
  ## trimws() for the same reason as above (see #55): the VER: comparisons
  ## below already trim, the LIB: one used to rely on normalizePath().
  loaded <- trimws(sub("^LIB:\\s*", "", grep("^LIB:", lines, value = TRUE)[1]))
  version <- sub("^VER:\\s*", "", grep("^VER:", lines, value = TRUE)[1])
  expect_equal(normalizePath(loaded, winslash = "/", mustWork = FALSE), lib)
  expect_equal(trimws(version), real_version)

  ## and the decoy is a real, loadable package, so the assertion above is not
  ## passing for want of anything to compete with: point the subprocess at the
  ## decoy instead and it reports the decoy's version.
  res_decoy <- run_fresh_rscript(self_contained_script,
                                 normalizePath(decoy, winslash = "/"))
  expect_equal(res_decoy$status, 0L, info = paste("stderr:\n", res_decoy$stderr))
  decoy_version <- sub("^VER:\\s*", "",
                       grep("^VER:", strsplit(res_decoy$stdout, "\n")[[1]],
                            value = TRUE)[1])
  expect_equal(trimws(decoy_version), "0.0.0.1")
})
