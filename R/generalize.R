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
GEN_OR_MORE <- intToUtf8(c(0x4EE5, 0x4E0A))  # ijou  "or more"
GEN_OR_LESS <- intToUtf8(c(0x4EE5, 0x4E0B))  # ika   "or less"
GEN_UNDER <- intToUtf8(c(0x672A, 0x6E80))    # miman "under"

#' units that may follow a number in a generalised value
#'
#' A generalised value is often written with its unit attached -- "30-39歳",
#' "1000-1999円". The unit has to be stripped before the endpoints can be read,
#' but stripping *any* trailing text would misread a categorical code such as
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
    ## ken (cases), kai (times), nichi (days), getsu (months), fullwidth %
    vapply(c(0x6B73, 0x624D, 0x5E74, 0x5186, 0x4EBA,
             0x4EF6, 0x56DE, 0x65E5, 0x6708, 0xFF05),
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

#' parse a generalised value into a numeric interval
#'
#' Recognises, in this order:
#' \describe{
#'   \item{`[a,b)` `(a,b]` `[a,b]` `(a,b)`}{explicit intervals; an omitted
#'     endpoint is infinite, so `[65,)` is "65 and over"}
#'   \item{`30代`, `30s`}{a decade: `[30, 40)`}
#'   \item{`35`, `35歳`}{a single value: the degenerate interval `[35, 35]`}
#'   \item{`30-39`, `30〜39`, `30-39歳`}{an inclusive range `[30, 39]`}
#'   \item{`30-`, `30以上`, `30+`}{`[30, Inf)`}
#'   \item{`〜39`, `39以下`}{`(-Inf, 39]`; `39未満` is `(-Inf, 39)`}
#' }
#'
#' The bare number is tried **before** the range forms, so `"-39"` is the
#' number -39 and not "up to 39". Write "up to 39" as `"〜39"`, `"39以下"` or
#' `"(,39]"`. A dash range with a negative endpoint is likewise unreadable and
#' must use the bracket form.
#'
#' @param x a single character value
#' @param units unit strings that may follow the numbers
#'
#' @return a list with `lower`, `upper`, `lower_closed`, `upper_closed`, or
#'   NULL if `x` is not an interval
#'
#' @keywords internal
parse_generalized_interval <- function(x, units = generalization_units()) {
  if (is.na(x)) {
    return(NULL)
  }
  s <- trimws(as.character(x))
  if (!nzchar(s)) {
    return(NULL)
  }

  num <- function(v) if (!nzchar(v)) NA_real_ else as.numeric(v)

  ## --- explicit bracket interval -------------------------------------------
  ## NB the bracket classes are written "[[(]" and "[])]", not "[\\[\\(]" and
  ## "[\\]\\)]". R's default (TRE) engine treats a backslash inside a bracket
  ## expression as a literal backslash, so "[\\]" is the class containing "\"
  ## and the following "]" *closes* it -- the pattern then silently matches
  ## nothing at all and every interval is misread as a category.
  m <- regmatches(s, regexec(
    paste0("^([[(])\\s*(", GEN_NUM, ")?\\s*,\\s*(", GEN_NUM, ")?\\s*([])])$"),
    s
  ))[[1]]
  if (length(m) == 5) {
    lo <- num(m[3])
    hi <- num(m[4])
    return(list(
      lower = if (is.na(lo)) -Inf else lo,
      upper = if (is.na(hi)) Inf else hi,
      lower_closed = m[2] == "[",
      upper_closed = m[5] == "]"
    ))
  }

  ## Strip a known unit, longest first so "yrs" is not read as "y" + "rs".
  strip_unit <- function(v) {
    u <- units[order(nchar(units), decreasing = TRUE)]
    for (one in u) {
      if (nzchar(one) && endsWith(v, one)) {
        return(trimws(substr(v, 1L, nchar(v) - nchar(one))))
      }
    }
    v
  }
  su <- strip_unit(s)

  ## --- decade: "30代" / "30s" ----------------------------------------------
  m <- regmatches(s, regexec(
    paste0("^(", GEN_NUM, ")\\s*(", GEN_DECADE, "|s)$"), s
  ))[[1]]
  if (length(m) == 3) {
    lo <- as.numeric(m[2])
    return(list(lower = lo, upper = lo + 10, lower_closed = TRUE, upper_closed = FALSE))
  }

  ## --- bare number: "35", "35歳" -------------------------------------------
  if (grepl(paste0("^", GEN_NUM, "$"), su)) {
    v <- as.numeric(su)
    return(list(lower = v, upper = v, lower_closed = TRUE, upper_closed = TRUE))
  }

  ## --- "a 以上" / "a+" -----------------------------------------------------
  m <- regmatches(su, regexec(
    paste0("^(", GEN_NUM, ")\\s*(\\+|", GEN_OR_MORE, ")$"), su
  ))[[1]]
  if (length(m) == 3) {
    return(list(lower = as.numeric(m[2]), upper = Inf,
                lower_closed = TRUE, upper_closed = FALSE))
  }

  ## --- "a 以下" / "a 未満" -------------------------------------------------
  m <- regmatches(su, regexec(
    paste0("^(", GEN_NUM, ")\\s*(", GEN_OR_LESS, ")$"), su
  ))[[1]]
  if (length(m) == 3) {
    return(list(lower = -Inf, upper = as.numeric(m[2]),
                lower_closed = FALSE, upper_closed = TRUE))
  }
  m <- regmatches(su, regexec(
    paste0("^(", GEN_NUM, ")\\s*(", GEN_UNDER, ")$"), su
  ))[[1]]
  if (length(m) == 3) {
    return(list(lower = -Inf, upper = as.numeric(m[2]),
                lower_closed = FALSE, upper_closed = FALSE))
  }

  ## --- inclusive range "a-b" ------------------------------------------------
  ## Endpoints are unsigned here: a leading "-" cannot be told apart from the
  ## separator. Signed endpoints must use the bracket form.
  unsigned <- "[0-9]+(?:\\.[0-9]+)?"
  m <- regmatches(su, regexec(
    paste0("^(", unsigned, ")\\s*", GEN_DASH, "\\s*(", unsigned, ")$"), su
  ))[[1]]
  if (length(m) == 3) {
    return(list(lower = as.numeric(m[2]), upper = as.numeric(m[3]),
                lower_closed = TRUE, upper_closed = TRUE))
  }

  ## --- open-ended range "a-" / "-b" ----------------------------------------
  m <- regmatches(su, regexec(paste0("^(", unsigned, ")\\s*", GEN_DASH, "$"), su))[[1]]
  if (length(m) == 2) {
    return(list(lower = as.numeric(m[2]), upper = Inf,
                lower_closed = TRUE, upper_closed = FALSE))
  }
  m <- regmatches(su, regexec(paste0("^", GEN_DASH, "\\s*(", unsigned, ")$"), su))[[1]]
  if (length(m) == 2) {
    return(list(lower = -Inf, upper = as.numeric(m[2]),
                lower_closed = FALSE, upper_closed = TRUE))
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
  key <- paste(edges$attribute, edges$value, sep = "\r")
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
