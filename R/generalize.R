## ---------------------------------------------------------------------------
## Generalisation / containment matching (Issue #20)
##
## Every score in this package so far assumes the ANON side holds a *value* --
## a number to subtract, a string to edit-distance, a set to intersect. Real
## Japanese anonymised data (匿名加工情報) does not. It holds "30代", "東京都",
## "135****": the value has been replaced by a region of the value space that
## contains it. There is no number to subtract, and score_num() on such a
## column either errors or, worse, coerces it to NA and reports that nobody
## could be reidentified -- which reads as "safe".
##
## The right question for generalised data is not "how far apart are these two
## values" but "could this RAW record have produced this ANON record at all".
## That is a containment test, and it yields a candidate *set*: the RAW records
## whose values fall inside every published region. If k records survive, the
## attacker's chance is 1/k, and 1/k is the information the release gave away.
## This file implements exactly that.
##
## WHY THIS MATTERS FOR WEAK ATTACKERS. Containment needs no distance and no
## scale, so it works on any subset of the quasi-identifiers the attacker
## happens to hold. An attacker who knows only "born in the 1980s, lives in
## Tokyo" still narrows the file; each further attribute intersects the
## candidate set again. The narrowing is multiplicative, which is why
## score_containment() takes several targets at once and intersects them,
## rather than scoring each separately and adding -- adding would count the
## same person as "half excluded" instead of excluded.
##
## SCORE ORIENTATION AND FAILURE DIRECTION. The score is a distance in [0, 1]:
##
##   contained     ->  1 - 1/k   (k = size of the surviving candidate set)
##   not contained ->  1
##
## Every contained candidate gets the *same* score, so match_greedy() draws
## uniformly among the k survivors and reports CONFIDENCE = 1/k. That is the
## honest answer: containment says which records are possible, not which is
## likeliest. A non-contained candidate always scores strictly worse than any
## contained one (1 > 1 - 1/k for every k >= 1), so exclusion is never
## overridden. When nothing is contained, every candidate ties at 1 and the
## attack degenerates to guessing -- rather than silently dropping the record,
## which would shrink the trial count and flatter the data
## (docs/lessons-learned.md section 2).
##
## HIERARCHIES ARE DECLARED, NEVER INFERRED. Issue #20 puts automatic hierarchy
## discovery out of scope, and rightly: guessing that "東京都" generalises
## "千代田区" from the data is guessing what the anonymiser meant, and a wrong
## guess moves the reported risk. Hierarchies are read from a CSV or YAML file
## the user writes; see read_generalization_hierarchy() and
## inst/extdata/generalization-*.
## ---------------------------------------------------------------------------

## Numbers accepted at interval endpoints.
GEN_NUM <- "[-+]?[0-9]+(?:\\.[0-9]+)?"

## Unsigned endpoints, used wherever a leading "-" could not be told apart from
## the range separator.
GEN_UNSIGNED <- "[0-9]+(?:\\.[0-9]+)?"

## Characters used as a range separator: ASCII hyphen and tilde, en/em dash,
## and the two Japanese wave dashes U+301C / U+FF5E (which look identical and
## are both in circulation).
##
## The functional strings in this file are built from explicit Unicode code
## points rather than written as literal characters. The regular expressions
## and suffix comparisons below must behave identically whatever locale and
## source encoding R was started with, and a code point says exactly which
## character is meant -- U+301C and U+FF5E in particular are indistinguishable
## on screen but are different characters, and both are in circulation.
GEN_DASH <- paste0("[-~", intToUtf8(c(0x2013, 0x2014, 0x301C, 0xFF5E)), "]")

## Suffixes recognised on the ANON side.
GEN_DECADE <- intToUtf8(0x4EE3)              # dai   "the thirties"
GEN_BAND <- intToUtf8(0x53F0)                # dai   "in the 3000s" (magnitude)
GEN_OR_MORE <- intToUtf8(c(0x4EE5, 0x4E0A))  # ijou  "or more"
GEN_OR_LESS <- intToUtf8(c(0x4EE5, 0x4E0B))  # ika   "or less"
GEN_UNDER <- intToUtf8(c(0x672A, 0x6E80))    # miman "under"
GEN_OVER <- intToUtf8(0x8D85)                # chou  "over", endpoint excluded
GEN_WITHIN <- intToUtf8(c(0x4EE5, 0x5185))   # inai  "within"
GEN_ONWARD <- intToUtf8(c(0x4EE5, 0x964D))   # ikou  "from ... onwards"
GEN_EARLIER <- intToUtf8(c(0x4EE5, 0x524D))  # izen  "up to and including"
GEN_UPTO <- intToUtf8(c(0x307E, 0x3067))     # made  "up to"
GEN_FROM <- intToUtf8(c(0x304B, 0x3089))     # kara  "from"
GEN_FULL <- intToUtf8(0x6E80)                # man   prefix in "満20歳以上"
GEN_EARLY <- intToUtf8(c(0x524D, 0x534A))    # zenhan "early" half of a decade
GEN_LATE <- intToUtf8(c(0x5F8C, 0x534A))     # kouhan "late"  half of a decade

## Bound markers, grouped by whether the endpoint itself is inside the region.
## The two halves are kept apart because "以下" and "未満" differ by exactly one
## record's worth of candidates at every band edge.
GEN_LOWER_CLOSED <- paste0("\\+|", GEN_OR_MORE, "|", GEN_ONWARD)
GEN_LOWER_OPEN <- GEN_OVER
GEN_UPPER_CLOSED <- paste0(GEN_OR_LESS, "|", GEN_WITHIN, "|", GEN_EARLIER,
                           "|", GEN_UPTO)
GEN_UPPER_OPEN <- GEN_UNDER

## Myriad grouping. Japanese writes large numbers in powers of 10^4, so a
## release says "5万円未満" and not "50000円未満"; read literally the first is
## not a number at all and the column falls through every interval form.
GEN_SCALE_CHAR <- vapply(c(0x767E, 0x5343, 0x4E07, 0x5104, 0x5146),
                         intToUtf8, character(1))  # 百 千 万 億 兆
GEN_SCALE_VALUE <- c(1e2, 1e3, 1e4, 1e8, 1e12)
GEN_SCALE_ALT <- paste(GEN_SCALE_CHAR, collapse = "|")

## Fullwidth forms, mapped to their ASCII equivalents before anything is
## parsed. A Japanese release mixes the two freely -- "６５歳以上" and
## "65歳以上" are the same band -- and a form that is only recognised in one
## width is a form the guard misses half the time.
GEN_NORM_FROM <- vapply(
  c(0xFF10:0xFF19,                       # ０ .. ９
    0xFF0B, 0xFF0D, 0x2212, 0x2010,      # ＋ － − ‐
    0xFF0E, 0xFF0C, 0x3000,              # ． ， ideographic space
    0xFF08, 0xFF09, 0xFF3B, 0xFF3D),     # （ ） ［ ］
  intToUtf8, character(1)
)
GEN_NORM_TO <- c(as.character(0:9),
                 "+", "-", "-", "-",
                 ".", ",", " ",
                 "(", ")", "[", "]")

## The patterns are assembled once, at namespace load. They depend only on the
## constants above, and rebuilding them inside the parser made paste0() one of
## its three most expensive calls -- the parser runs once per distinct value of
## a column, which on a wide candidate table is tens of thousands of times.
GEN_RE_BRACKET <- paste0("^([[(])\\s*(", GEN_NUM, ")?\\s*,\\s*(", GEN_NUM,
                         ")?\\s*([])])$")
GEN_RE_MAN_PREFIX <- paste0("^", GEN_FULL, "([0-9])")
GEN_RE_KARA <- paste0("([0-9])\\s*", GEN_FROM)
GEN_RE_SCALE_TOKEN <- paste0("(?:", GEN_UNSIGNED, "\\s*(?:", GEN_SCALE_ALT,
                             "))+(?:", GEN_UNSIGNED, ")?")
GEN_RE_SCALE_HEAD <- paste0("^(", GEN_UNSIGNED, ")\\s*(", GEN_SCALE_ALT, ")")
GEN_RE_DECADE <- paste0("^(", GEN_NUM, ")\\s*", GEN_DECADE, "\\s*(", GEN_EARLY,
                        "|", GEN_LATE, ")?$")
GEN_RE_DECADE_ASCII <- paste0("^(", GEN_NUM, ")\\s*s$")
GEN_RE_BAND <- paste0("^(", GEN_UNSIGNED, ")\\s*", GEN_BAND, "$")
GEN_RE_VALUE <- paste0("^", GEN_NUM, "$")
GEN_RE_TWO_SIDED <- paste0("^(", GEN_UNSIGNED, ")\\s*(", GEN_LOWER_CLOSED, "|",
                           GEN_LOWER_OPEN, ")\\s*(", GEN_UNSIGNED, ")\\s*(",
                           GEN_UPPER_CLOSED, "|", GEN_UPPER_OPEN, ")$")
GEN_RE_LOWER_CLOSED <- paste0("^(", GEN_NUM, ")\\s*(", GEN_LOWER_CLOSED, ")$")
GEN_RE_LOWER_OPEN <- paste0("^(", GEN_NUM, ")\\s*(", GEN_LOWER_OPEN, ")$")
GEN_RE_UPPER_CLOSED <- paste0("^(", GEN_NUM, ")\\s*(", GEN_UPPER_CLOSED, ")$")
GEN_RE_UPPER_OPEN <- paste0("^(", GEN_NUM, ")\\s*(", GEN_UPPER_OPEN, ")$")
GEN_RE_RANGE <- paste0("^(", GEN_UNSIGNED, ")\\s*", GEN_DASH, "\\s*(",
                       GEN_UNSIGNED, ")\\s*(", GEN_UPTO, ")?$")
GEN_RE_RANGE_FROM <- paste0("^(", GEN_UNSIGNED, ")\\s*", GEN_DASH, "$")
GEN_RE_RANGE_TO <- paste0("^", GEN_DASH, "\\s*(", GEN_UNSIGNED, ")$")

#' units that may follow a number in a generalised value
#'
#' A generalised value is often written with its unit attached -- "30-39歳",
#' "1000-1999円", "65歳以上". The unit has to be removed before the endpoints
#' can be read, and it is removed wherever it sits directly after a number, not
#' only at the end of the value: "65歳以上" is the ordinary Japanese form and
#' reading only "65以上" left the commonest top-coded band invisible
#' (Issue #92).
#'
#' Stripping *any* text after a number would misread a categorical code such as
#' "3S" as the number 3, so only these known units are removed. Pass your own
#' vector to `score_containment(units = )` to extend the list.
#'
#' @return a character vector of unit strings
#'
#' @examples
#' generalization_units()
#'
#' @export
generalization_units <- function() {
  c(
    ## sai (years old), sai (informal), nen (year), en (yen), nin (people),
    ## ken (cases), kai (times), nichi (days), getsu (months), fullwidth %,
    ## jikan (hours), fun (minutes), byou (seconds), do (degrees),
    ## ten (points), ko (items), mei (people, formal)
    intToUtf8(c(0x6642, 0x9593)),  # jikan "hours", before the single 時
    vapply(c(0x6B73, 0x624D, 0x5E74, 0x5186, 0x4EBA,
             0x4EF6, 0x56DE, 0x65E5, 0x6708, 0xFF05,
             0x6642, 0x5206, 0x79D2, 0x5EA6,
             0x70B9, 0x500B, 0x540D),
           intToUtf8, character(1)),
    "%", "yr", "yrs", "y", "years", "kg", "km", "cm", "m", "g"
  )
}

#' does this value mean "suppressed / any value"?
#'
#' @param x character vector
#'
#' @return logical vector
#'
#' @keywords internal
is_generalization_wildcard <- function(x) {
  is.na(x) | !nzchar(trimws(x)) | grepl("^\\*+$", trimws(x))
}

## ---------------------------------------------------------------------------
## reading a Japanese generalised value (Issue #92)
##
## The unit used to be stripped only off the *end* of the value, so "65以上"
## was read and "65歳以上" was not -- and "65歳以上" is how a Japanese release
## actually writes a top-coded age band. The consequence was not a parse error
## but silence: is_generalized_value() returned FALSE, the Issue #40 guard did
## not fire, and score_char() went ahead and edit-distanced "37" against
## "65歳以上" -- measuring the printed shape of the band and reporting a
## success rate 1.8x below the one score_containment() gets on the same data
## (docs/investigation/japanese-generalization-benchmark-log.txt). The unit
## therefore has to be removed wherever it sits between a number and what
## follows it, not only at the end.
##
## FALSE POSITIVES ARE THE BINDING CONSTRAINT. Issue #40 already found that a
## detector which is too eager stops working analyses: reading "8s" as [8, 18)
## made the check fire on a column of random two-character codes. Everything
## added here is therefore either non-ASCII (Japanese suffixes, fullwidth
## digits, myriad grouping) or requires a character that
## `stringi::stri_rand_strings()`'s default alphabet cannot produce ("+", "-",
## "~"). The one ASCII form, the "30s" decade, is deliberately still read on
## the *unstripped* string: allowing units inside it would turn "0ms" into a
## decade. See docs/investigation/japanese-generalization-benchmark.R for the
## measured rates.
## ---------------------------------------------------------------------------

#' is this string made only of ASCII characters?
#'
#' Used to skip every Japanese-specific reading for a value that cannot
#' possibly contain one. That is not only a speed question: it is the
#' guarantee that an ASCII column is treated exactly as it was before Issue
#' #92, and therefore that the false-positive rate Issue #40 calibrated on
#' random alphanumeric strings cannot have moved.
#'
#' Written with [utf8ToInt()] rather than a regular expression because a
#' bracket range such as `[ -~]` is collation-dependent, and this file has to
#' behave identically under every locale the CI matrix runs.
#'
#' @param v a single string
#'
#' @return TRUE or FALSE; FALSE if the string cannot be read as UTF-8
#'
#' @keywords internal
gen_is_ascii <- function(v) {
  cp <- suppressWarnings(utf8ToInt(v))
  !anyNA(cp) && all(cp < 128L)
}

#' map fullwidth characters onto their ASCII equivalents
#'
#' @param v a single string
#'
#' @return the string with fullwidth digits and punctuation replaced
#'
#' @keywords internal
gen_normalize <- function(v) {
  if (gen_is_ascii(v)) {
    ## Nothing to map, and every existing ASCII form must come out of this
    ## function byte for byte unchanged.
    return(v)
  }
  for (i in seq_along(GEN_NORM_FROM)) {
    v <- gsub(GEN_NORM_FROM[i], GEN_NORM_TO[i], v, fixed = TRUE)
  }
  v
}

#' remove the thousands separators inside a number
#'
#' Only a comma with exactly three digits after it and a digit before it, so
#' the comma of an explicit interval (`"[1,000)"` is parsed before this runs)
#' and a list such as `"1,2"` are left alone.
#'
#' @param v a single string
#'
#' @return the string with `"1,000"` rewritten as `"1000"`
#'
#' @keywords internal
gen_strip_thousands <- function(v) {
  repeat {
    w <- sub("([0-9]),([0-9]{3})($|[^0-9])", "\\1\\2\\3", v)
    if (identical(w, v)) {
      return(v)
    }
    v <- w
  }
}

#' escape a literal string for use inside a regular expression
#'
#' @param v character vector
#'
#' @return `v` with the regex metacharacters backslash-escaped
#'
#' @keywords internal
gen_escape_regex <- function(v) {
  gsub("([][{}()*+?.\\\\^$|-])", "\\\\\\1", v)
}

#' add up a run of myriad-grouped numbers
#'
#' `"5万"` is 50000 and `"1億2000万"` is 120000000: each number multiplies the
#' scale character that follows it, and a trailing number with no scale is
#' added as it stands.
#'
#' @param tok the matched run, e.g. `"1億2000万"`
#'
#' @return a numeric value
#'
#' @keywords internal
gen_scale_sum <- function(tok) {
  total <- 0
  repeat {
    m <- regmatches(tok, regexec(GEN_RE_SCALE_HEAD, tok))[[1]]
    if (length(m) != 3L) {
      break
    }
    total <- total +
      as.numeric(m[2]) * GEN_SCALE_VALUE[match(m[3], GEN_SCALE_CHAR)]
    tok <- substring(tok, nchar(m[1]) + 1L)
  }
  if (nzchar(tok)) {
    total <- total + as.numeric(tok)
  }
  total
}

#' render a number without scientific notation
#'
#' `format()` would print 1e8 as "1e+08" and round 1234567.8 to seven
#' significant digits; either would be re-read as the wrong endpoint.
#'
#' @param x a single numeric value
#'
#' @return a string
#'
#' @keywords internal
gen_format_number <- function(x) {
  if (!is.finite(x)) {
    return("")
  }
  if (isTRUE(x == round(x)) && abs(x) < 1e15) {
    return(sprintf("%.0f", x))
  }
  sub("\\.?0+$", "", sprintf("%.6f", x))
}

#' rewrite myriad-grouped numbers as plain digits
#'
#' @param v a single string
#'
#' @return the string with `"5万円"` rewritten as `"50000円"`
#'
#' @keywords internal
gen_expand_scale <- function(v) {
  m <- gregexpr(GEN_RE_SCALE_TOKEN, v)[[1]]
  if (m[1] < 0) {
    return(v)
  }
  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  out <- character(0)
  pos <- 1L
  for (i in seq_along(starts)) {
    out <- c(out,
             substring(v, pos, starts[i] - 1L),
             gen_format_number(gen_scale_sum(
               substring(v, starts[i], starts[i] + lens[i] - 1L)
             )))
    pos <- starts[i] + lens[i]
  }
  paste0(paste(out, collapse = ""), substring(v, pos, nchar(v)))
}

#' the two unit patterns, built once per distinct `units` vector
#'
#' The units are alternated longest-first so that `"yrs"` is not read as
#' `"y"` + `"rs"` under either leftmost-longest or leftmost-first matching, and
#' the result is cached: [parse_generalized_interval()] is called once per
#' distinct value of a column, and rebuilding a 30-branch alternation each time
#' dominated the cost.
#'
#' @param units unit strings, see [generalization_units()]
#'
#' @return a list with `trailing` and `after_digit` regular expressions, or
#'   NULL when `units` is empty
#'
#' @keywords internal
gen_unit_patterns <- local({
  last_units <- NULL
  last_value <- NULL
  function(units) {
    if (!is.null(last_units) && identical(units, last_units)) {
      return(last_value)
    }
    u <- units[!is.na(units) & nzchar(units)]
    out <- NULL
    if (length(u) > 0) {
      u <- gen_escape_regex(u[order(nchar(u), decreasing = TRUE)])
      alt <- paste(u, collapse = "|")
      out <- list(
        trailing = paste0("\\s*(?:", alt, ")$"),
        after_digit = paste0("([0-9])\\s*(?:", alt, ")")
      )
    }
    last_units <<- units
    last_value <<- out
    out
  }
})

#' remove the known units from a generalised value
#'
#' Two passes: the unit is taken off the end of the value (which is all the old
#' implementation did, and is what reads `"30+yrs"`), and it is also removed
#' wherever it sits directly after a digit -- the `"歳"` of `"65歳以上"` and
#' both of the `"歳"`s of `"20歳～29歳"`.
#'
#' The digit anchor is what keeps a categorical code intact: `"3S"` has no
#' unit, and `"千代田区"` has no digit before its `"千"`.
#'
#' @param v a single string
#' @param units unit strings, see [generalization_units()]
#'
#' @return the string with its units removed
#'
#' @keywords internal
gen_strip_units <- function(v, units) {
  pat <- gen_unit_patterns(units)
  if (is.null(pat)) {
    return(v)
  }
  v <- trimws(sub(pat$trailing, "", v))
  gsub(pat$after_digit, "\\1", v)
}

#' parse a generalised value into a numeric interval
#'
#' Recognises, in this order:
#' \describe{
#'   \item{`[a,b)` `(a,b]` `[a,b]` `(a,b)`}{explicit intervals; an omitted
#'     endpoint is infinite, so `[65,)` is "65 and over"}
#'   \item{`30代`, `30歳代`, `30s`}{a decade: `[30, 40)`. `30代前半` is
#'     `[30, 35)` and `30代後半` is `[35, 40)`, and `1990年代` is
#'     `[1990, 2000)`}
#'   \item{`3000円台`}{a magnitude band: `[3000, 4000)`, the step being the
#'     place value of the leading digit}
#'   \item{`35`, `35歳`, `5万円`}{a single value: the degenerate interval
#'     `[35, 35]`}
#'   \item{`20歳以上30歳未満`}{a two-sided band, `[20, 30)`}
#'   \item{`30-39`, `30〜39`, `30歳～39歳`, `30歳から39歳`}{an inclusive range
#'     `[30, 39]`}
#'   \item{`30-`, `30以上`, `30歳以上`, `30+`, `2020年以降`}{`[30, Inf)`;
#'     `30歳超` is `(30, Inf)`}
#'   \item{`〜39`, `39以下`, `39歳まで`, `39日以内`}{`(-Inf, 39]`; `39未満` is
#'     `(-Inf, 39)`}
#' }
#'
#' Fullwidth digits and punctuation are read as their ASCII equivalents,
#' thousands separators are ignored (`"1,000円以上"`), and myriad grouping is
#' expanded (`"5万円未満"` is `(-Inf, 50000)`).
#'
#' The bare number is tried **before** the range forms, so `"-39"` is the
#' number -39 and not "up to 39". Write "up to 39" as `"〜39"`, `"39以下"` or
#' `"(,39]"`. A dash range with a negative endpoint is likewise unreadable and
#' must use the bracket form.
#'
#' Every pattern is anchored at both ends. A band wrapped in prose
#' (`"65歳以上の方"`) and a number written in kanji digits (`"三十歳以上"`) are
#' therefore not read, and a column written that way is a gap the Issue #40
#' guard cannot close; matching a band anywhere inside a string would instead
#' read one out of every sentence that mentions an age.
#'
#' @param x a single character value
#' @param units unit strings that may follow the numbers
#'
#' @return a list with `lower`, `upper`, `lower_closed`, `upper_closed` and
#'   `form` (which of the readings above matched), or NULL if `x` is not an
#'   interval
#'
#' @keywords internal
parse_generalized_interval <- function(x, units = generalization_units()) {
  if (is.na(x)) {
    return(NULL)
  }
  s <- trimws(gen_normalize(as.character(x)))
  if (!nzchar(s)) {
    return(NULL)
  }

  num <- function(v) if (!nzchar(v)) NA_real_ else as.numeric(v)
  iv <- function(lower, upper, lower_closed, upper_closed, form) {
    list(lower = lower, upper = upper, lower_closed = lower_closed,
         upper_closed = upper_closed, form = form)
  }

  ## --- explicit bracket interval -------------------------------------------
  ## NB the bracket classes are written "[[(]" and "[])]", not "[\\[\\(]" and
  ## "[\\]\\)]". R's default (TRE) engine treats a backslash inside a bracket
  ## expression as a literal backslash, so "[\\]" is the class containing "\"
  ## and the following "]" *closes* it -- the pattern then silently matches
  ## nothing at all and every interval is misread as a category.
  m <- regmatches(s, regexec(GEN_RE_BRACKET, s))[[1]]
  if (length(m) == 5) {
    lo <- num(m[3])
    hi <- num(m[4])
    return(iv(
      lower = if (is.na(lo)) -Inf else lo,
      upper = if (is.na(hi)) Inf else hi,
      lower_closed = m[2] == "[",
      upper_closed = m[5] == "]",
      form = "bracket"
    ))
  }

  ## Everything below is read on a copy with the notation removed: thousands
  ## separators, the "満" of "満20歳以上", myriad grouping, the units, and
  ## "から" written out as a wave dash.
  ##
  ## The Japanese-only steps and readings are skipped outright for a value that
  ## holds no non-ASCII character. That is what makes it verifiable, and not
  ## merely likely, that an ASCII column behaves exactly as it did before this
  ## change -- including the false-positive rate Issue #40 calibrated.
  ascii <- gen_is_ascii(s)
  su <- s
  if (grepl(",", su, fixed = TRUE)) {
    su <- gen_strip_thousands(su)
  }
  if (!ascii) {
    su <- sub(GEN_RE_MAN_PREFIX, "\\1", su)
    su <- gen_expand_scale(su)
  }
  su <- gen_strip_units(su, units)
  if (!ascii) {
    su <- gsub(GEN_RE_KARA, "\\1~", su)
  }
  su <- trimws(su)

  if (!ascii) {
    ## --- decade: "30代", "20歳代", "20代前半" ------------------------------
    m <- regmatches(su, regexec(GEN_RE_DECADE, su))[[1]]
    if (length(m) == 3) {
      lo <- as.numeric(m[2])
      if (identical(m[3], GEN_EARLY)) {
        return(iv(lo, lo + 5, TRUE, FALSE, "decade"))
      }
      if (identical(m[3], GEN_LATE)) {
        return(iv(lo + 5, lo + 10, TRUE, FALSE, "decade"))
      }
      return(iv(lo, lo + 10, TRUE, FALSE, "decade"))
    }
  }

  ## --- decade, ASCII: "30s" -------------------------------------------------
  ## Read on `s`, not `su`: with the units removed "0ms" would become "0s" and
  ## a column of random codes would start reporting decades. This is the only
  ## region form an alphanumeric string can reach, so it is the only one whose
  ## input is deliberately not normalised any further.
  m <- regmatches(s, regexec(GEN_RE_DECADE_ASCII, s))[[1]]
  if (length(m) == 2) {
    lo <- as.numeric(m[2])
    return(iv(lo, lo + 10, TRUE, FALSE, "decade_ascii"))
  }

  if (!ascii) {
    ## --- magnitude band: "3000円台" -> [3000, 4000) ------------------------
    ## The step is the place value of the leading digit, which is what the
    ## notation means at every magnitude: "20万円台" is [200000, 300000) and
    ## "20台" is [20, 30), the same band "20代" names.
    m <- regmatches(su, regexec(GEN_RE_BAND, su))[[1]]
    if (length(m) == 2) {
      lo <- as.numeric(m[2])
      if (isTRUE(lo > 0)) {
        return(iv(lo, lo + 10^floor(log10(lo)), TRUE, FALSE, "band"))
      }
    }
  }

  ## --- bare number: "35", "35歳", "5万円" ----------------------------------
  if (grepl(GEN_RE_VALUE, su)) {
    v <- as.numeric(su)
    return(iv(v, v, TRUE, TRUE, "value"))
  }

  if (!ascii) {
    ## --- two-sided band: "20歳以上30歳未満" --------------------------------
    ## This is the commonest way a Japanese release writes an age bin, and no
    ## one-sided pattern would match it: both are anchored at both ends.
    m <- regmatches(su, regexec(GEN_RE_TWO_SIDED, su))[[1]]
    if (length(m) == 5) {
      return(iv(as.numeric(m[2]), as.numeric(m[4]),
                !identical(m[3], GEN_LOWER_OPEN),
                !identical(m[5], GEN_UPPER_OPEN),
                "band"))
    }
  }

  ## --- "a 以上" / "a+" / "a 以降" ------------------------------------------
  m <- regmatches(su, regexec(GEN_RE_LOWER_CLOSED, su))[[1]]
  if (length(m) == 3) {
    return(iv(as.numeric(m[2]), Inf, TRUE, FALSE, "lower"))
  }

  if (!ascii) {
    ## --- "a 超": the endpoint itself is outside ----------------------------
    m <- regmatches(su, regexec(GEN_RE_LOWER_OPEN, su))[[1]]
    if (length(m) == 3) {
      return(iv(as.numeric(m[2]), Inf, FALSE, FALSE, "lower"))
    }

    ## --- "a 以下" / "a 以内" / "a 以前" / "a まで" -------------------------
    m <- regmatches(su, regexec(GEN_RE_UPPER_CLOSED, su))[[1]]
    if (length(m) == 3) {
      return(iv(-Inf, as.numeric(m[2]), FALSE, TRUE, "upper"))
    }

    ## --- "a 未満": the endpoint itself is outside --------------------------
    m <- regmatches(su, regexec(GEN_RE_UPPER_OPEN, su))[[1]]
    if (length(m) == 3) {
      return(iv(-Inf, as.numeric(m[2]), FALSE, FALSE, "upper"))
    }
  }

  ## --- inclusive range "a-b" ------------------------------------------------
  ## Endpoints are unsigned here: a leading "-" cannot be told apart from the
  ## separator. Signed endpoints must use the bracket form.
  m <- regmatches(su, regexec(GEN_RE_RANGE, su))[[1]]
  if (length(m) == 4) {
    return(iv(as.numeric(m[2]), as.numeric(m[3]), TRUE, TRUE, "range"))
  }

  ## --- open-ended range "a-" / "-b" ----------------------------------------
  m <- regmatches(su, regexec(GEN_RE_RANGE_FROM, su))[[1]]
  if (length(m) == 2) {
    return(iv(as.numeric(m[2]), Inf, TRUE, FALSE, "range"))
  }
  m <- regmatches(su, regexec(GEN_RE_RANGE_TO, su))[[1]]
  if (length(m) == 2) {
    return(iv(-Inf, as.numeric(m[2]), FALSE, TRUE, "range"))
  }

  NULL
}

#' test whether values fall inside one generalisation node
#'
#' A node is either an interval (tested numerically) or a literal category
#' (tested by string equality). `rule` forces one reading; `"auto"` tries the
#' interval reading first and falls back to equality, which is what makes a
#' mixed file of "30代" and "東京都" work with no configuration.
#'
#' @param values character vector of RAW values
#' @param node the ANON-side node
#' @param rule one of "auto", "interval", "exact", "prefix"
#' @param units unit strings, see [generalization_units()]
#'
#' @return logical vector, one per element of `values`
#'
#' @keywords internal
node_matches <- function(values, node, rule = "auto", units = generalization_units()) {
  if (is.na(node)) {
    return(rep(FALSE, length(values)))
  }
  vals <- as.character(values)

  if (identical(rule, "exact")) {
    return(!is.na(vals) & vals == node)
  }
  if (identical(rule, "prefix")) {
    prefix <- sub("\\*+$", "", node)
    return(!is.na(vals) & startsWith(vals, prefix))
  }

  iv <- parse_generalized_interval(node, units)
  if (is.null(iv)) {
    if (identical(rule, "interval")) {
      stop("score_containment(): the value \"", node, "\" could not be read as ",
           "an interval, but rule = \"interval\" was requested. Use ",
           "rule = \"auto\" to fall back to exact matching, or fix the value.",
           call. = FALSE)
    }
    return(!is.na(vals) & vals == node)
  }

  numv <- suppressWarnings(as.numeric(vals))
  inside <- !is.na(numv) &
    (if (iv$lower_closed) numv >= iv$lower else numv > iv$lower) &
    (if (iv$upper_closed) numv <= iv$upper else numv < iv$upper)

  if (identical(rule, "interval")) {
    return(inside)
  }
  ## "auto": a value that is literally the same string is also contained, so a
  ## column that has not been generalised at all still matches itself.
  inside | (!is.na(vals) & vals == node)
}

## ---------------------------------------------------------------------------
## detecting a generalised column (Issue #40)
##
## score_num() and score_dist() stop on a generalised column: neither "30代"
## nor "[30,40)" can be coerced to a number. score_char() does not stop.
## adist("37", "[30,40)") is 6, and 6 looks exactly like a score -- it is
## really the length of the bracket string. Measured on
## docs/investigation/generalization-benchmark.R that misuse reports a success
## rate of 0.1017 where score_containment() reports 0.4450: a fourfold
## under-report that raises no error, which is precisely the failure direction
## docs/lessons-learned.md section 2 is about.
##
## WHAT COUNTS AS EVIDENCE. Only a *demonstrated* mismatch: an ANON value that
## is structurally a region (an interval spanning more than one point, or a
## suppression mask) together with a RAW value that falls inside it and is not
## literally equal to it. Both halves are required. Flagging every
## interval-looking string on its own would stop a perfectly valid call in
## which both sides carry the same already-binned column ("30代" vs "30代"),
## where literal comparison is the right thing to do; requiring only the
## containment half would flag "37" against "37.0", which is a formatting
## difference and not a generalisation.
##
## WHAT THIS CANNOT SEE. A categorical generalisation -- 千代田区 published as
## 東京都 -- is invisible to any structural test: nothing about the string
## "東京都" says it contains 千代田区. That relationship exists only in a
## declared hierarchy (generalization_hierarchy()), which the value-comparison
## scores do not take. Structural detection is a floor, not a guarantee. For
## anything generalised categorically the only safe route is
## score_containment() with the hierarchy, and containment_counts() to see
## that the narrowing is what you expect.
## ---------------------------------------------------------------------------

#' is this value a suppression mask?
#'
#' @param x character vector
#'
#' @return logical vector
#'
#' @keywords internal
is_generalization_mask <- function(x) {
  s <- trimws(as.character(x))
  !is.na(s) & grepl("\\*$", s)
}

#' does a value name a region rather than a single value?
#'
#' `TRUE` for a value that is structurally a *generalisation*: an interval
#' covering more than one point (`"[30,40)"`, `"30代"`, `"30-39"`, `"65以上"`),
#' or a suppression mask (`"*"`, `"135****"`). `FALSE` for an ordinary value,
#' including a bare number -- `"35"` parses as the degenerate interval
#' `[35, 35]`, which is a value and not a region.
#'
#' `NA` is `FALSE`. A missing value can mean anything, and treating it as a
#' generalisation would flag every column that merely has a gap in it.
#'
#' The ASCII `"30s"` decade form is only accepted for a multiple of ten.
#' [parse_generalized_interval()] reads `"8s"` as `[8, 18)`, which is right for
#' containment (a hierarchy may legitimately declare such a node) but wrong
#' here: over 200,000 random two-character strings, 0.256% were of the form
#' digit + `"s"` and every one of them was reported as a generalisation --
#' which is how this check first stopped a passing test that had nothing to do
#' with generalisation. Requiring a multiple of ten drops the rate to 0.0245%,
#' and nobody writes "the 8s" for a decade. The restriction is on the ASCII
#' form only: `"8代"` needs a character no alphanumeric string can produce, so
#' it costs nothing to read it as written (Issue #92).
#'
#' A *categorical* generalisation cannot be recognised this way: nothing about
#' the string `"東京都"` says it contains `"千代田区"`. That relationship lives
#' in a declared hierarchy, so this returns `FALSE` for it. See the note at the
#' head of `R/generalize.R`.
#'
#' @param x vector of values
#' @param units unit strings that may follow a number, see
#'   [generalization_units()]
#'
#' @return logical vector, one per element of `x`
#'
#' @seealso [score_containment()], the score to use once a column turns out to
#'   hold generalised values.
#'
#' @examples
#' is_generalized_value(c("37", "30s", "[30,40)", "135****", "M", NA))
#'
#' @export
is_generalized_value <- function(x, units = generalization_units()) {
  v <- as.character(x)
  out <- rep(FALSE, length(v))
  known <- !is.na(v)
  if (!any(known)) {
    return(out)
  }

  ## Parse once per distinct value: a candidate table repeats the same handful
  ## of generalised strings across every pair.
  s <- trimws(v[known])
  u <- unique(s)
  hit <- is_generalization_mask(u)

  for (i in which(!hit)) {
    iv <- parse_generalized_interval(u[i], units)
    ## "8s" parses as [8, 18); see the note above for why that does not count
    ## as a generalisation here.
    odd_decade <- identical(iv$form, "decade_ascii") &&
      !isTRUE(iv$lower %% 10 == 0)
    hit[i] <- !is.null(iv) && isTRUE(iv$lower < iv$upper) && !odd_decade
  }

  out[known] <- hit[match(s, u)]
  out
}

## How much of the ANON side has to look like a region before the column is
## called generalised.
##
## A single region-looking value is not enough, and this is measured rather
## than assumed. `stringi::stri_rand_strings(n, 2)` -- the generator behind
## create_dummy_master_data()'s CHAR column, and as ordinary a character column
## as exists -- produces a value this test calls a region 0.0245% of the time
## (see is_generalized_value()); over 2000 draws of 40 such strings the largest
## share of any one column was 0.025 and the mean was 0.00026. A real
## generalised column sits at 1.00: every published value is a region, and a
## fully masked one likewise. The threshold below leaves roughly an order of
## magnitude of clearance on each side.
##
## The cost of the threshold is that a column where only a small minority of
## values are suppressed is not reported. That is the deliberate trade: a stop
## on ordinary data would be a tool that cries wolf, and a tool people switch
## off is worth less than one that catches the case Issue #40 measured, where
## every value is a region.
GENERALIZATION_SHARE_THRESHOLD <- 0.2

#' pairs showing that a target column is generalised on the ANON side
#'
#' Requires two independent facts, because either alone misfires: enough of the
#' ANON side has to be region-shaped (`min_share`), *and* some RAW value has to
#' fall inside one of those regions without being equal to it. Without the
#' second, `"37"` against `"37.0"` would be reported as a generalisation;
#' without the first, one accidental `"0s"` in a column of random codes would.
#'
#' @param raw_vals,anon_vals the two sides of one column of a candidate table
#' @param units unit strings, see [generalization_units()]
#' @param max_examples how many distinct example pairs to keep for the message
#' @param min_share the share of non-missing ANON entries that must be regions
#'
#' @return a data frame with columns `RAW` and `ANON`, with zero rows when
#'   there is no evidence. The share actually observed is attached as the
#'   `share` attribute.
#'
#' @keywords internal
generalization_evidence <- function(raw_vals, anon_vals,
                                    units = generalization_units(),
                                    max_examples = 3L,
                                    min_share = GENERALIZATION_SHARE_THRESHOLD) {
  none <- data.frame(RAW = character(0), ANON = character(0),
                     stringsAsFactors = FALSE)

  rv <- as.character(raw_vals)
  av <- as.character(anon_vals)
  known <- av[!is.na(av)]
  uv <- unique(rv[!is.na(rv)])
  if (length(uv) == 0 || length(known) == 0) {
    return(none)
  }

  ug <- unique(known)
  is_region <- is_generalized_value(ug, units)
  share <- mean(is_region[match(known, ug)])
  if (share < min_share) {
    return(structure(none, share = share))
  }

  ug <- ug[is_region]
  if (length(ug) == 0) {
    return(structure(none, share = share))
  }

  hit_raw <- character(0)
  hit_anon <- character(0)
  for (g in ug) {
    rule <- if (is_generalization_mask(g)) "prefix" else "auto"
    inside <- node_matches(uv, g, rule, units) & uv != g
    if (any(inside)) {
      hit_raw <- c(hit_raw, uv[which(inside)[1]])
      hit_anon <- c(hit_anon, g)
      if (length(hit_anon) >= max_examples) {
        break
      }
    }
  }
  if (length(hit_anon) == 0) {
    return(structure(none, share = share))
  }
  structure(
    data.frame(RAW = hit_raw, ANON = hit_anon, stringsAsFactors = FALSE),
    share = share
  )
}

#' render the evidence pairs for an error message
#'
#' @param ev the data frame from [generalization_evidence()]
#'
#' @return a single string
#'
#' @keywords internal
generalization_evidence_text <- function(ev) {
  paste(sprintf("RAW \"%s\" falls inside ANON \"%s\"", ev$RAW, ev$ANON),
        collapse = "; ")
}

#' stop, or warn, when a value-comparison score is given a generalised column
#'
#' @param dat_raw_anon dataframe of raw_anon form
#' @param cols resolved column names from [reid_prefixed_columns()]
#' @param target the target name as the caller wrote it
#' @param action `"stop"`, `"warn"` or `"ignore"`
#' @param fn_name the user-facing function name, for the message
#' @param units unit strings, see [generalization_units()]
#'
#' @return `invisible(TRUE)` when evidence was found and `action` was not
#'   `"stop"`, `invisible(FALSE)` when there was none
#'
#' @keywords internal
check_generalized_target <- function(dat_raw_anon, cols, target, action,
                                     fn_name, units = generalization_units()) {
  action <- match.arg(action, c("stop", "warn", "ignore"))
  if (identical(action, "ignore")) {
    return(invisible(FALSE))
  }

  ev <- generalization_evidence(dat_raw_anon[[cols$raw_target]],
                                dat_raw_anon[[cols$anon_target]], units)
  if (nrow(ev) == 0) {
    return(invisible(FALSE))
  }

  msg <- paste0(
    fn_name, "(): column \"", target, "\" is generalised on the ANON side (",
    format(round(100 * (attr(ev, "share") %||% 1), 1)), "% of its published ",
    "values are regions) -- the published value is a region containing the raw ",
    "one, not a value to compare with it (",
    generalization_evidence_text(ev), "). Comparing them ",
    "directly measures the printed shape of the region, not the risk: on ",
    "generalised data it reports a success rate several times lower than the ",
    "real one and raises no error, so the release looks safer than it is ",
    "(docs/lessons-learned.md section 2). Use score_containment(dat, \"",
    target, "\"), which asks which RAW records could have produced each ",
    "published region. If this column really is meant to be compared ",
    "literally, pass generalized = \"warn\" or generalized = \"ignore\"."
  )

  if (identical(action, "stop")) {
    stop(msg, call. = FALSE)
  }
  warning(msg, call. = FALSE)
  invisible(TRUE)
}

#' the error message for a target column that has to be numeric and is not
#'
#' Named separately from [check_generalized_target()] because these callers
#' cannot continue at all: there is no arithmetic to do on a character column,
#' so there is no `"warn"` to offer. The message still runs the generalisation
#' check, because "this column is character" and "this column is a set of
#' published regions" call for completely different fixes.
#'
#' @param dat_raw_anon dataframe of raw_anon form
#' @param cols resolved column names from [reid_prefixed_columns()]
#' @param target the target name as the caller wrote it
#' @param fn_name the user-facing function name
#' @param alternative what to suggest when the column is *not* generalised
#' @param units unit strings, see [generalization_units()]
#'
#' @return a single string
#'
#' @keywords internal
non_numeric_target_message <- function(dat_raw_anon, cols, target, fn_name,
                                       alternative,
                                       units = generalization_units()) {
  ev <- generalization_evidence(dat_raw_anon[[cols$raw_target]],
                                dat_raw_anon[[cols$anon_target]], units)
  base <- paste0(
    fn_name, "(): column \"", target, "\" is character or factor, not ",
    "numeric, so no arithmetic distance is defined on it."
  )
  if (nrow(ev) == 0) {
    return(paste0(base, " ", alternative))
  }
  paste0(
    base, " Its ANON values are generalised regions (",
    generalization_evidence_text(ev), "). Use score_containment(dat, \"",
    target, "\"): a generalised column answers \"which RAW records could have ",
    "produced this published region\", not \"how far apart are these two ",
    "values\" (docs/lessons-learned.md section 2)."
  )
}

## ---------------------------------------------------------------------------
## generalisation hierarchies
## ---------------------------------------------------------------------------

#' build a generalisation hierarchy from a table of edges
#'
#' A hierarchy says which coarse value each fine value rolls up into: 千代田区
#' into 東京都 into 関東, `[35,40)` into `[30,40)`. It is a *declaration*, not
#' something inferred from the data (Issue #20 puts inference out of scope):
#' guessing the anonymiser's intent would move the reported risk in an
#' uncontrolled direction.
#'
#' Only the edges have to be listed. A raw value that is not itself a node --
#' an exact age of 37, say -- is matched into the finest node that contains it
#' by [node_matches()], so a numeric hierarchy only needs its bin definitions
#' and not one row per possible value.
#'
#' @param x a data frame with columns `attribute`, `value` and `parent` (case
#'   insensitive; any further columns are ignored). Each row is one edge:
#'   `value` generalises to `parent` within `attribute`.
#'
#' @return an object of class "reid_hierarchy": a list with `edges` (the
#'   validated data frame, plus a derived `level` column) and `descendants`
#'   (for each attribute, the transitive closure below every node).
#'
#' @importFrom stats setNames
#'
#' @examples
#' h <- generalization_hierarchy(data.frame(
#'   attribute = "AREA",
#'   value = c("chiyoda", "shinjuku", "yokohama", "tokyo", "kanagawa"),
#'   parent = c("tokyo", "tokyo", "kanagawa", "kanto", "kanto"),
#'   stringsAsFactors = FALSE
#' ))
#' h
#'
#' @export
generalization_hierarchy <- function(x) {
  if (!is.data.frame(x)) {
    stop("generalization_hierarchy(): `x` must be a data frame with columns ",
         "attribute, value and parent.", call. = FALSE)
  }

  nm <- tolower(names(x))
  required <- c("attribute", "value", "parent")
  missing_cols <- setdiff(required, nm)
  if (length(missing_cols) > 0) {
    stop("generalization_hierarchy(): missing column(s): ",
         paste(missing_cols, collapse = ", "),
         ". Expected attribute, value, parent; got: ",
         paste(names(x), collapse = ", "), ".", call. = FALSE)
  }

  edges <- data.frame(
    attribute = trimws(as.character(x[[which(nm == "attribute")[1]]])),
    value = trimws(as.character(x[[which(nm == "value")[1]]])),
    parent = trimws(as.character(x[[which(nm == "parent")[1]]])),
    stringsAsFactors = FALSE
  )
  ## Tolerate blank filler rows, but never a half-specified edge: an edge with
  ## a missing parent would silently stop generalising at that node.
  blank <- !nzchar(edges$attribute) & !nzchar(edges$value) & !nzchar(edges$parent)
  edges <- edges[!blank, , drop = FALSE]

  bad <- is.na(edges$attribute) | is.na(edges$value) | is.na(edges$parent) |
    !nzchar(edges$attribute) | !nzchar(edges$value) | !nzchar(edges$parent)
  if (any(bad)) {
    stop("generalization_hierarchy(): row(s) ", paste(which(bad), collapse = ", "),
         " have an empty attribute, value or parent. Every edge must say which ",
         "attribute it belongs to and what the value generalises into.",
         call. = FALSE)
  }

  if (any(edges$value == edges$parent)) {
    i <- which(edges$value == edges$parent)[1]
    stop("generalization_hierarchy(): \"", edges$value[i], "\" is its own ",
         "parent (attribute \"", edges$attribute[i], "\").", call. = FALSE)
  }

  edges <- edges[!duplicated(edges), , drop = FALSE]
  ## Issue #70: pasting the two fields with "\r" made ("A", "x\ry") and
  ## ("A\rx", "y") the same key, so a hierarchy holding both was rejected as
  ## having two parents for one value -- a legitimate tree refused, with a
  ## message pointing at the wrong thing. reid_value_key() cannot collide.
  key <- reid_value_key(list(reid_class_codes(edges$attribute),
                             reid_class_codes(edges$value)))
  if (anyDuplicated(key) > 0) {
    i <- which(duplicated(key))[1]
    stop("generalization_hierarchy(): \"", edges$value[i], "\" (attribute \"",
         edges$attribute[i], "\") has more than one parent. A generalisation ",
         "hierarchy must be a tree: a value cannot roll up into two different ",
         "coarser values.", call. = FALSE)
  }
  rownames(edges) <- NULL

  descendants <- lapply(split(edges, edges$attribute), hierarchy_descendants)
  edges$level <- unsplit(
    lapply(split(edges, edges$attribute), function(e) hierarchy_levels(e)),
    edges$attribute
  )

  structure(
    list(edges = edges, descendants = descendants),
    class = "reid_hierarchy"
  )
}

#' transitive closure below every node of one attribute
#'
#' @param e edge data frame for a single attribute
#'
#' @return a named list mapping every node to the vector of nodes below it
#'
#' @keywords internal
hierarchy_descendants <- function(e) {
  children <- split(e$value, e$parent)
  nodes <- unique(c(e$value, e$parent))

  out <- vector("list", length(nodes))
  names(out) <- nodes

  for (n in nodes) {
    seen <- character(0)
    frontier <- children[[n]]
    if (is.null(frontier)) {
      frontier <- character(0)
    }
    ## Bounded by the number of nodes: a cycle would otherwise spin forever.
    steps <- 0L
    while (length(frontier) > 0) {
      steps <- steps + 1L
      if (steps > length(nodes) + 1L) {
        stop("generalization_hierarchy(): the hierarchy for attribute \"",
             e$attribute[1], "\" contains a cycle involving \"", n, "\".",
             call. = FALSE)
      }
      seen <- unique(c(seen, frontier))
      nxt <- unlist(children[frontier], use.names = FALSE)
      frontier <- setdiff(nxt, seen)
    }

    ## The `seen` set also terminates a cycle, so the loop above finishes
    ## quietly on one and would hand back a node that is its own descendant.
    ## That is worse than hanging: containment would then widen a node to
    ## everything below it *and back up through itself*, quietly enlarging
    ## every candidate set.
    if (n %in% seen) {
      stop("generalization_hierarchy(): the hierarchy for attribute \"",
           e$attribute[1], "\" contains a cycle: \"", n, "\" generalises, ",
           "directly or indirectly, into itself.", call. = FALSE)
    }
    out[[n]] <- seen
  }
  out
}

#' derive a level for every edge's `value`
#'
#' Level 1 is a leaf (a value that is nobody's parent); a node's level is one
#' more than the deepest node below it. Only used for display and for
#' [generalize_value()]'s choice of the finest matching node.
#'
#' @param e edge data frame for a single attribute
#'
#' @return integer vector, one per row of `e`
#'
#' @keywords internal
hierarchy_levels <- function(e) {
  nodes <- unique(c(e$value, e$parent))
  level <- setNames(rep(1L, length(nodes)), nodes)

  ## Longest-path relaxation; the tree constraint bounds it by the node count.
  for (i in seq_len(length(nodes))) {
    changed <- FALSE
    cand <- level[e$value] + 1L
    for (p in unique(e$parent)) {
      v <- max(cand[e$parent == p])
      if (v > level[[p]]) {
        level[[p]] <- v
        changed <- TRUE
      }
    }
    if (!changed) {
      break
    }
  }
  unname(level[e$value])
}

#' read a generalisation hierarchy from a CSV or YAML file
#'
#' **CSV** is the long edge list [generalization_hierarchy()] takes, one edge
#' per row:
#'
#' ```
#' attribute,value,parent
#' AREA,chiyoda,tokyo
#' AREA,tokyo,kanto
#' AGE,"[30,35)","[30,40)"
#' ```
#'
#' **YAML** is the same tree written as nesting, which is easier to keep
#' correct by hand:
#'
#' ```
#' AREA:
#'   kanto:
#'     tokyo: [chiyoda, shinjuku]
#'     kanagawa: [yokohama]
#' AGE:
#'   "[30,40)": ["[30,35)", "[35,40)"]
#' ```
#'
#' YAML needs the `yaml` package, which is a **Suggests**, not a hard
#' dependency: CSV covers the same ground with nothing extra installed. If
#' `yaml` is missing this stops with a message saying so rather than guessing.
#'
#' @param path path to the file
#' @param format `"auto"` (default, by file extension), `"csv"` or `"yaml"`
#'
#' @return a "reid_hierarchy" object
#'
#' @seealso [generalization_hierarchy()] to build one in memory, and the
#'   example files in `system.file("extdata", package = "reidentify")`.
#'
#' @examples
#' p <- system.file("extdata", "generalization-jp.csv", package = "reidentify")
#' if (nzchar(p)) read_generalization_hierarchy(p)
#'
#' @importFrom utils read.csv
#' @export
read_generalization_hierarchy <- function(path, format = c("auto", "csv", "yaml")) {
  format <- match.arg(format)
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("read_generalization_hierarchy(): `path` must be a single file path.",
         call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("read_generalization_hierarchy(): file not found: ", path, call. = FALSE)
  }

  if (format == "auto") {
    ext <- tolower(sub(".*\\.", "", basename(path)))
    format <- if (ext %in% c("yml", "yaml")) "yaml" else "csv"
  }

  if (format == "csv") {
    x <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                         encoding = "UTF-8", check.names = FALSE)
    return(generalization_hierarchy(x))
  }

  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("read_generalization_hierarchy(): reading YAML needs the `yaml` ",
         "package, which is only a Suggests of reidentify. Install it, or use ",
         "the CSV form -- it expresses exactly the same hierarchy and needs no ",
         "extra package.", call. = FALSE)
  }
  generalization_hierarchy(
    yaml_hierarchy_edges(yaml::yaml.load_file(path))
  )
}

#' flatten a nested YAML hierarchy into an edge table
#'
#' @param y the parsed YAML: a mapping from attribute to a tree, where a tree
#'   node is either a mapping (parent -> children) or a character vector of
#'   leaves
#'
#' @return a data frame with columns attribute, value, parent
#'
#' @keywords internal
yaml_hierarchy_edges <- function(y) {
  if (!is.list(y) || is.null(names(y)) || any(!nzchar(names(y)))) {
    stop("read_generalization_hierarchy(): the YAML file must be a mapping ",
         "from attribute name to a hierarchy, e.g.\n",
         "  AREA:\n    kanto:\n      tokyo: [chiyoda, shinjuku]",
         call. = FALSE)
  }

  walk <- function(node, parent, attribute) {
    if (is.null(node)) {
      return(NULL)
    }
    if (is.character(node) || is.numeric(node)) {
      return(data.frame(attribute = attribute, value = as.character(node),
                        parent = parent, stringsAsFactors = FALSE))
    }
    if (!is.list(node)) {
      stop("read_generalization_hierarchy(): unexpected YAML node under \"",
           parent, "\" (", class(node)[1], ").", call. = FALSE)
    }
    ## An unnamed list is a list of leaves; a named one is parent -> children.
    if (is.null(names(node))) {
      return(do.call(rbind, lapply(node, walk, parent = parent,
                                   attribute = attribute)))
    }
    do.call(rbind, lapply(names(node), function(child) {
      rbind(
        data.frame(attribute = attribute, value = child, parent = parent,
                   stringsAsFactors = FALSE),
        walk(node[[child]], parent = child, attribute = attribute)
      )
    }))
  }

  out <- do.call(rbind, lapply(names(y), function(attribute) {
    tree <- y[[attribute]]
    if (!is.list(tree) || is.null(names(tree))) {
      stop("read_generalization_hierarchy(): attribute \"", attribute,
           "\" must map to a mapping of coarse value -> finer values.",
           call. = FALSE)
    }
    do.call(rbind, lapply(names(tree), function(top) {
      walk(tree[[top]], parent = top, attribute = attribute)
    }))
  }))

  if (is.null(out) || nrow(out) == 0) {
    stop("read_generalization_hierarchy(): the YAML file declares no edges.",
         call. = FALSE)
  }
  out
}

#' print a generalisation hierarchy
#'
#' @param x a "reid_hierarchy" object
#' @param ... unused
#'
#' @return `x`, invisibly
#'
#' @importFrom utils head
#' @export
print.reid_hierarchy <- function(x, ...) {
  e <- x$edges
  cat(sprintf("generalization hierarchy: %d edge(s) over %d attribute(s)\n",
              nrow(e), length(unique(e$attribute))))
  for (a in unique(e$attribute)) {
    sub <- e[e$attribute == a, , drop = FALSE]
    roots <- setdiff(unique(sub$parent), unique(sub$value))
    cat(sprintf("  %s: %d edge(s), %d level(s), root(s): %s\n",
                a, nrow(sub), max(sub$level) + 1L,
                paste(utils::head(roots, 5), collapse = ", ")))
  }
  invisible(x)
}

#' look up the descendants of a node
#'
#' @param hierarchy a "reid_hierarchy" object, or NULL
#' @param attribute attribute name
#' @param node node name
#'
#' @return a character vector of the nodes strictly below `node`
#'
#' @keywords internal
descendants_of <- function(hierarchy, attribute, node) {
  if (is.null(hierarchy)) {
    return(character(0))
  }
  d <- hierarchy$descendants[[attribute]]
  if (is.null(d)) {
    return(character(0))
  }
  out <- d[[node]]
  if (is.null(out)) character(0) else out
}

#' generalise raw values with a declared hierarchy
#'
#' Walks each value `levels` steps up the hierarchy. Values that are not
#' themselves nodes are first matched into the finest node that contains them,
#' so an exact age of 37 enters the hierarchy at `[35,40)` and one more step
#' takes it to `[30,40)`.
#'
#' This is the generator's side of Issue #20: it builds the generalised column
#' a release would contain, which is what makes it possible to *test* the
#' matching side on data whose ground truth is known.
#'
#' @param values vector of raw values
#' @param attribute attribute name, used to select the hierarchy
#' @param hierarchy a "reid_hierarchy" object
#' @param levels how many steps to climb (default 1)
#' @param units unit strings, see [generalization_units()]
#'
#' @return a character vector the same length as `values`. A value with no
#'   matching node and no parent is returned unchanged.
#'
#' @examples
#' h <- generalization_hierarchy(data.frame(
#'   attribute = "AGE",
#'   value = c("[30,35)", "[35,40)", "[40,45)", "[45,50)"),
#'   parent = c("[30,40)", "[30,40)", "[40,50)", "[40,50)"),
#'   stringsAsFactors = FALSE
#' ))
#' generalize_value(c(31, 37, 46), "AGE", h, levels = 0)
#' generalize_value(c(31, 37, 46), "AGE", h, levels = 1)
#'
#' @export
generalize_value <- function(values, attribute, hierarchy, levels = 1,
                             units = generalization_units()) {
  if (!inherits(hierarchy, "reid_hierarchy")) {
    stop("generalize_value(): `hierarchy` must be a \"reid_hierarchy\" object ",
         "from generalization_hierarchy() or read_generalization_hierarchy().",
         call. = FALSE)
  }
  if (!is.numeric(levels) || length(levels) != 1L || is.na(levels) || levels < 0) {
    stop("generalize_value(): `levels` must be a single non-negative number.",
         call. = FALSE)
  }

  e <- hierarchy$edges[hierarchy$edges$attribute == attribute, , drop = FALSE]
  if (nrow(e) == 0) {
    stop("generalize_value(): the hierarchy has no attribute \"", attribute,
         "\"; it has: ", paste(unique(hierarchy$edges$attribute), collapse = ", "),
         ".", call. = FALSE)
  }

  vals <- as.character(values)
  parent_of <- setNames(e$parent, e$value)

  ## Enter the hierarchy at the finest node that contains the value.
  nodes <- e$value[order(e$level)]
  cur <- vals
  entered <- rep(FALSE, length(vals))
  for (n in nodes) {
    todo <- !entered
    if (!any(todo)) {
      break
    }
    hit <- todo & node_matches(vals, n, "auto", units)
    cur[hit] <- n
    entered <- entered | hit
  }

  for (i in seq_len(as.integer(levels))) {
    up <- unname(parent_of[cur])
    cur <- ifelse(is.na(up), cur, up)
  }
  ## ifelse() carries the names of its `test` argument; the caller wants a
  ## plain character vector the same shape as `values`.
  unname(cur)
}

## ---------------------------------------------------------------------------
## the score
## ---------------------------------------------------------------------------

#' resolve the per-target rules
#'
#' @param targets character vector of target columns
#' @param rules NULL, or a named character vector of column -> rule
#' @param fn_name used in error messages
#'
#' @return a named character vector with one rule per target
#'
#' @keywords internal
resolve_containment_rules <- function(targets, rules, fn_name) {
  valid <- c("auto", "interval", "exact", "prefix")
  out <- setNames(rep("auto", length(targets)), targets)
  if (is.null(rules)) {
    return(out)
  }
  if (!is.character(rules) || is.null(names(rules)) || any(!nzchar(names(rules)))) {
    stop(fn_name, "(): `rules` must be a *named* character vector, e.g. ",
         "c(ZIP = \"prefix\", AGE = \"interval\").", call. = FALSE)
  }
  unknown_rule <- setdiff(rules, valid)
  if (length(unknown_rule) > 0) {
    stop(fn_name, "(): unknown rule(s): ", paste(unknown_rule, collapse = ", "),
         ". Expected one of: ", paste(valid, collapse = ", "), ".", call. = FALSE)
  }
  unknown_col <- setdiff(names(rules), targets)
  if (length(unknown_col) > 0) {
    stop(fn_name, "(): `rules` names column(s) that are not in `targets`: ",
         paste(unknown_col, collapse = ", "), ".", call. = FALSE)
  }
  out[names(rules)] <- rules
  out
}

#' containment of every RAW value in every ANON value, for one target
#'
#' Computed over the *distinct* values on each side and then broadcast back
#' onto the candidate rows: a cross join repeats the same few generalised
#' values thousands of times, and parsing "30代" once per pair would dominate
#' the cost.
#'
#' @param raw_vals,anon_vals per-row character vectors
#' @param attribute target name, used to select the hierarchy
#' @param hierarchy a "reid_hierarchy" object, or NULL
#' @param rule containment rule
#' @param units unit strings
#'
#' @return logical vector, one per row
#'
#' @keywords internal
containment_vector <- function(raw_vals, anon_vals, attribute, hierarchy,
                               rule, units) {
  uv <- unique(raw_vals)
  ug <- unique(anon_vals)

  m <- matrix(FALSE, nrow = length(uv), ncol = length(ug))
  for (j in seq_along(ug)) {
    g <- ug[j]
    if (is_generalization_wildcard(g)) {
      m[, j] <- TRUE
      next
    }
    ## A raw value is inside a published node if it is inside the node itself
    ## or inside anything the hierarchy places below it.
    hit <- node_matches(uv, g, rule, units)
    for (d in descendants_of(hierarchy, attribute, g)) {
      if (all(hit)) {
        break
      }
      hit <- hit | node_matches(uv, d, rule, units)
    }
    m[, j] <- hit
  }

  m[cbind(match(raw_vals, uv), match(anon_vals, ug))]
}

#' compute joint containment over several targets
#'
#' @param dat_raw_anon dataframe of raw_anon form
#' @param targets character vector of target columns
#' @param row_number row-number column name
#' @param hierarchy a "reid_hierarchy" object, or NULL
#' @param rules named character vector of per-column rules, or NULL
#' @param units unit strings
#' @param fn_name used in error messages
#'
#' @return a list with `contained` (logical, one per row), `raw_row_number`,
#'   `anon_row_number` and `per_target` (a logical matrix)
#'
#' @keywords internal
joint_containment <- function(dat_raw_anon, targets, row_number, hierarchy,
                              rules, units, fn_name) {
  if (!is.character(targets) || length(targets) == 0 || anyNA(targets)) {
    stop(fn_name, "(): `targets` must be a non-empty character vector of ",
         "column names.", call. = FALSE)
  }
  if (!is.null(hierarchy) && !inherits(hierarchy, "reid_hierarchy")) {
    stop(fn_name, "(): `hierarchy` must be NULL or a \"reid_hierarchy\" object ",
         "from generalization_hierarchy() / read_generalization_hierarchy().",
         call. = FALSE)
  }
  rule_of <- resolve_containment_rules(targets, rules, fn_name)

  cols <- lapply(targets, function(t) {
    reid_prefixed_columns(dat_raw_anon, t, row_number, fn_name)
  })

  per_target <- vapply(seq_along(targets), function(i) {
    containment_vector(
      as.character(dat_raw_anon[[cols[[i]]$raw_target]]),
      as.character(dat_raw_anon[[cols[[i]]$anon_target]]),
      attribute = targets[i],
      hierarchy = hierarchy,
      rule = rule_of[[targets[i]]],
      units = units
    )
  }, logical(nrow(dat_raw_anon)))
  per_target <- matrix(per_target, nrow = nrow(dat_raw_anon),
                       dimnames = list(NULL, targets))

  list(
    contained = as.logical(rowSums(per_target) == length(targets)),
    raw_row_number = dat_raw_anon[[cols[[1]]$raw_row_number]],
    anon_row_number = dat_raw_anon[[cols[[1]]$anon_row_number]],
    per_target = per_target
  )
}

#' score generalised (interval / categorical) attributes by containment
#'
#' The score for anonymised data that publishes *regions* instead of values:
#' "30代" rather than 37, "東京都" rather than 千代田区, "135****" rather than a
#' full postcode. No distance can be computed on such a column -- and
#' [score_num()] on it either errors or coerces to NA and reports that nobody
#' was reidentified, which reads as "safe". The question containment asks
#' instead is which RAW records *could* have produced this ANON record.
#'
#' Several `targets` are intersected, not added: a record must fall inside the
#' published region of **every** attribute to stay a candidate. That is what
#' makes weak attackers measurable -- each attribute the attacker holds cuts
#' the candidate set again, and the cuts multiply.
#'
#' The score is `1 - 1/k` for a contained candidate, where `k` is the number of
#' RAW records that survive for that ANON record, and `1` for an excluded one.
#' All survivors therefore tie, [match_greedy()] draws among them uniformly and
#' reports `CONFIDENCE = 1/k`, and `1/k` is exactly the information the release
#' gave away. Use [containment_counts()] to see the `k` values directly.
#'
#' A value of `"*"`, `"**"`, `""` or `NA` on the ANON side means "suppressed"
#' and matches every RAW value, so a fully suppressed column contributes
#' nothing rather than excluding everybody.
#'
#' @inheritParams score_num
#' @param targets character vector of column names (before RAW_/ANON_
#'   prefixing) to intersect
#' @param hierarchy a "reid_hierarchy" object from
#'   [read_generalization_hierarchy()] or [generalization_hierarchy()], or NULL
#'   (the default) when every generalisation is an interval that speaks for
#'   itself
#' @param rules optional named character vector forcing a rule per column, one
#'   of `"auto"` (default: interval if the value parses as one, else exact
#'   string equality, in both cases widened by the hierarchy), `"interval"`,
#'   `"exact"` or `"prefix"` (for masked codes such as `"135****"`)
#' @param units unit strings that may follow a number, see
#'   [generalization_units()]
#'
#' @return a "reid_scores" table (a distance in \[0, 1\]: smaller is a better
#'   match), carrying a `candidate_count` attribute -- the `k` per ANON record.
#'
#' @seealso [containment_counts()] for the per-record narrowing, and
#'   [generalize_value()] for building generalised columns.
#'
#' @examples
#' raw <- data.frame(ROW_NUMBER = 1:4, AGE = c(31, 37, 46, 52),
#'                   SEX = c("M", "F", "F", "M"), stringsAsFactors = FALSE)
#' anon <- data.frame(ROW_NUMBER = 1:4, AGE = c("30s", "30s", "40s", "50s"),
#'                    SEX = c("M", "F", "F", "M"), stringsAsFactors = FALSE)
#' d <- join_raw_anon_data(raw, anon)
#' match_greedy(score_containment(d, c("AGE", "SEX")))
#'
#' @export
score_containment <- function(dat_raw_anon, targets, row_number = "ROW_NUMBER",
                              hierarchy = NULL, rules = NULL,
                              units = generalization_units(),
                              .fn_name = "score_containment") {
  jc <- joint_containment(dat_raw_anon, targets, row_number, hierarchy,
                          rules, units, .fn_name)

  k <- containment_k(jc$contained, jc$anon_row_number, jc$raw_row_number)

  ## 1 - 1/k for a survivor, 1 for an excluded candidate. 1 - 1/k < 1 for every
  ## finite k, so exclusion is never beaten by a merely-large candidate set.
  score <- ifelse(jc$contained, 1 - 1 / k[as.character(jc$anon_row_number)], 1)

  out <- new_reid_scores(
    raw_row_number = jc$raw_row_number,
    anon_row_number = jc$anon_row_number,
    score = unname(score)
  )
  attr(out, "candidate_count") <- k
  out
}

#' number of distinct RAW records surviving containment, per ANON record
#'
#' @param contained logical vector, one per candidate row
#' @param anon_row_number,raw_row_number the row identifiers of those rows
#'
#' @return a named numeric vector indexed by `as.character(ANON_ROW_NUMBER)`
#'
#' @keywords internal
containment_k <- function(contained, anon_row_number, raw_row_number) {
  by_anon <- split(raw_row_number[contained], anon_row_number[contained])
  k <- vapply(by_anon, function(v) length(unique(v)), numeric(1))

  ## ANON records with no surviving candidate must still appear, with k = 0;
  ## split() drops them entirely.
  all_anon <- as.character(unique(anon_row_number))
  out <- setNames(rep(0, length(all_anon)), all_anon)
  out[names(k)] <- k
  out
}

#' how far containment narrows the candidate set, per ANON record
#'
#' The direct read-out of Issue #20's verification question: given the
#' generalised release, how many RAW records could each published record have
#' come from?
#'
#' `TRUTH_CONTAINED` is a correctness check on the *set-up*, not a risk
#' measure. If the record's real counterpart is not inside its own published
#' region, the generalisation and the raw data disagree -- a wrongly declared
#' hierarchy, a unit mismatch, a column that was rounded rather than binned --
#' and every risk number computed from it is meaningless. It should be `TRUE`
#' for every record of a correctly generalised release, and it is reported
#' rather than assumed because a silent `FALSE` looks exactly like safety.
#'
#' @inheritParams score_containment
#'
#' @return a data frame with one row per ANON record and columns
#'   \describe{
#'     \item{ANON_ROW_NUMBER}{the record}
#'     \item{N_CANDIDATES}{RAW records offered to it}
#'     \item{N_CONTAINED}{how many survive containment (the `k` above)}
#'     \item{NARROWED_TO}{`N_CONTAINED / N_CANDIDATES`}
#'     \item{INFORMATION}{`1 / N_CONTAINED`, the attacker's chance, `NA` when
#'       nothing survives}
#'     \item{TRUTH_CONTAINED}{whether the record's own RAW counterpart survived}
#'   }
#'
#' @examples
#' raw <- data.frame(ROW_NUMBER = 1:6, AGE = c(21, 24, 33, 37, 38, 52))
#' anon <- data.frame(ROW_NUMBER = 1:6, AGE = c("20s", "20s", "30s", "30s",
#'                                              "30s", "50s"))
#' containment_counts(join_raw_anon_data(raw, anon), "AGE")
#'
#' @export
containment_counts <- function(dat_raw_anon, targets, row_number = "ROW_NUMBER",
                               hierarchy = NULL, rules = NULL,
                               units = generalization_units()) {
  jc <- joint_containment(dat_raw_anon, targets, row_number, hierarchy,
                          rules, units, "containment_counts")

  anon <- jc$anon_row_number
  k <- containment_k(jc$contained, anon, jc$raw_row_number)

  n_cand <- vapply(split(jc$raw_row_number, anon),
                   function(v) length(unique(v)), numeric(1))
  truth <- jc$contained & (jc$raw_row_number == anon)
  truth_by_anon <- vapply(split(truth, anon), any, logical(1))

  ids <- names(n_cand)
  out <- data.frame(
    ANON_ROW_NUMBER = unique(anon)[match(ids, as.character(unique(anon)))],
    N_CANDIDATES = unname(n_cand[ids]),
    N_CONTAINED = unname(k[ids]),
    stringsAsFactors = FALSE
  )
  out$NARROWED_TO <- out$N_CONTAINED / out$N_CANDIDATES
  out$INFORMATION <- ifelse(out$N_CONTAINED > 0, 1 / out$N_CONTAINED, NA_real_)
  out$TRUTH_CONTAINED <- unname(truth_by_anon[ids])

  out[order(out$ANON_ROW_NUMBER), , drop = FALSE]
}
