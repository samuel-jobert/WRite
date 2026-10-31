#' Clean a Word document
#'
#' * `bookmark_ids`: checks that different bookmarks do not share the same
#'   identifier (`w:id`); duplicates receive a new unique identifier (the
#'   first bookmark keeps its own, the associated end marker is updated).
#' * `merge_runs`: merges adjacent runs whose formatting properties
#'   (`w:rPr`) are identical. Spell-check marks (`w:proofErr`), which prevent
#'   these merges, are removed. Only runs containing nothing but text, tabs
#'   or line breaks are merged (fields, images, note references... are left
#'   untouched).
#' * `spelling`: removes the spelling and grammar error marks
#'   (`w:proofErr`).
#' * `comments`: removes the comments (anchors in the text and content of the
#'   comments part).
#' * `revisions`: accepts all the tracked changes (insertions are kept,
#'   deletions are removed, formatting-change records are dropped, paragraphs
#'   whose mark was deleted are joined to the next one: the result is what
#'   Word shows once all the changes are accepted).
#' * `all`: enables every option. Options explicitly given as `FALSE`
#'   stay disabled (`doc_clean(doc, all = TRUE, comments = FALSE)` cleans
#'   everything except the comments).
#'
#' By default only `bookmark_ids` and `merge_runs` are enabled.
#'
#' Cleaning applies to the body, headers, footers and notes.
#'
#' @param doc WRite document object.
#' @param bookmark_ids Fix duplicate bookmark identifiers?
#' @param merge_runs Merge runs with identical formatting?
#' @param spelling Remove the spelling and grammar error marks?
#' @param comments Remove the comments?
#' @param revisions Accept the tracked changes (remove the revision marks)?
#' @param all Enable all the options not explicitly given?
#' @return `doc`, invisibly. The number of corrections is available in the
#'   `"cleaning"` attribute of the returned value.
#' @section Illustration:
#' ```
#' Word often splits a text into several "runs" with the same formatting:
#'   <bold run>I will be inser</bold run><bold run>t</bold run><bold run>ed into another document</bold run>
#' After merge_runs:
#'   <bold run>I will be inserted into another document</bold run>
#'
#' Two different bookmarks with the same identifier w:id = 5:
#'   [a (id 5): A]  [b (id 5): B]   ->   [a (id 5): A]  [b (id 6): B]
#' ```
#' @examples
#' doc <- doc_read(write_example("to_insert"))
#' doc_clean(doc)
#' attr(doc, "cleaning")   # number of identifiers fixed and runs merged
#'
#' # Only the correction of bookmark identifiers
#' doc2 <- doc_read(write_example("example")) |> doc_clean(merge_runs = FALSE)
#' attr(doc2, "cleaning")
#'
#' # Everything except the comments
#' doc_clean(doc2, all = TRUE, comments = FALSE)
#' @export
doc_clean <- function(doc, bookmark_ids = TRUE, merge_runs = TRUE, spelling = FALSE,
                      comments = FALSE, revisions = FALSE, all = FALSE) {
  .check_doc(doc)
  .check_flag(all, "all")
  if (all) {
    if (missing(bookmark_ids)) bookmark_ids <- TRUE
    if (missing(merge_runs)) merge_runs <- TRUE
    if (missing(spelling)) spelling <- TRUE
    if (missing(comments)) comments <- TRUE
    if (missing(revisions)) revisions <- TRUE
  }
  summary <- c(bookmark_ids = 0L, runs_merged = 0L, spelling = 0L, comments = 0L, revisions = 0L)
  if (isTRUE(revisions)) summary[["revisions"]] <- .accept_revisions(doc)
  if (isTRUE(comments)) summary[["comments"]] <- .remove_comments(doc)
  if (isTRUE(spelling)) {
    for (p in .content_parts(doc)) {
      marks <- .find_all(.xml(doc, p), "//w:proofErr")
      summary[["spelling"]] <- summary[["spelling"]] + length(marks)
      xml2::xml_remove(marks)
    }
  }
  if (isTRUE(bookmark_ids)) summary[["bookmark_ids"]] <- .fix_bookmark_ids(doc)
  if (isTRUE(merge_runs)) {
    for (p in .content_parts(doc)) summary[["runs_merged"]] <- summary[["runs_merged"]] + .merge_runs(.xml(doc, p))
    doc$runs_merged <- TRUE
  }
  attr(doc, "cleaning") <- summary
  invisible(doc)
}

# Accepts all the tracked changes; returns the number of marks processed.
.accept_revisions <- function(doc) {
  n <- 0L
  for (p in .content_parts(doc)) {
    x <- .xml(doc, p)
    merged <- .find_all(x, "//w:p[w:pPr/w:rPr/w:del]")
    rows <- .find_all(x, "//w:tr[w:trPr/w:del]")
    n <- n + length(rows)
    xml2::xml_remove(rows)
    drop <- .find_all(x, paste("//w:del[not(parent::w:rPr)]", "//w:moveFrom", "//w:rPrChange", "//w:pPrChange",
                               "//w:sectPrChange", "//w:tblPrChange", "//w:tcPrChange", "//w:trPrChange",
                               "//w:tblGridChange", "//w:numberingChange", "//w:tblPrExChange",
                               "//w:cellIns", "//w:cellDel", "//w:cellMerge",
                               "//w:moveFromRangeStart", "//w:moveFromRangeEnd",
                               "//w:moveToRangeStart", "//w:moveToRangeEnd",
                               "//w:rPr/w:del", "//w:rPr/w:ins", "//w:trPr/w:ins", sep = " | "))
    n <- n + length(drop)
    xml2::xml_remove(drop)
    repeat {
      wrappers <- .find_all(x, "//w:ins | //w:moveTo")
      if (length(wrappers) == 0) break
      n <- n + length(wrappers)
      for (w in wrappers) {
        for (ch in xml2::xml_children(w)) .move(ch, w, "before")
        xml2::xml_remove(w)
      }
    }
    # A deleted paragraph mark joins the paragraph to the next one.
    for (par in rev(merged)) {
      nxt <- .find1(par, "following-sibling::*[1][self::w:p]")
      if (.is_missing(nxt)) next
      if (.count(par, "count(*[not(self::w:pPr)])") == 0) {
        xml2::xml_remove(par)
      } else {
        for (ch in xml2::xml_children(nxt)) if (.name(ch) != "w:pPr") .move(ch, par, "child")
        xml2::xml_remove(nxt)
      }
      n <- n + 1L
    }
  }
  n
}

# Removes the comment anchors and the comments; returns the number of comments removed.
.remove_comments <- function(doc) {
  n <- 0L
  for (p in .content_parts(doc)) {
    x <- .xml(doc, p)
    xml2::xml_remove(.find_all(x, "//w:commentRangeStart | //w:commentRangeEnd"))
    refs <- .find_all(x, "//w:r[w:commentReference]")
    for (r in refs) {
      if (.count(r, "count(*[not(self::w:rPr or self::w:commentReference)])") == 0) xml2::xml_remove(r)
      else xml2::xml_remove(.find_all(r, "w:commentReference"))
    }
  }
  cp <- .part_by_type(doc, doc$main, "comments")
  if (!is.na(cp)) {
    cm <- .find_all(.xml(doc, cp), "/w:comments/w:comment")
    n <- length(cm)
    xml2::xml_remove(cm)
  }
  n
}

# Starts and ends are paired in document order (first end encountered =
# first open start carrying this identifier).
.fix_bookmark_ids <- function(doc) {
  next_id <- .bookmark_max_id(doc) + 1L
  used <- character()
  n_fixed <- 0L
  for (p in .content_parts(doc)) {
    open <- list()
    for (m in .find_all(.xml(doc, p), "//w:bookmarkStart | //w:bookmarkEnd")) {
      id <- .attr(m, "w:id")
      if (.name(m) == "w:bookmarkStart") {
        new_id <- id
        if (is.na(id) || id %in% used || is.na(suppressWarnings(as.integer(id)))) {
          new_id <- as.character(next_id)
          next_id <- next_id + 1L
          .set_attr(m, "w:id", new_id)
          n_fixed <- n_fixed + 1L
        }
        used <- c(used, new_id)
        key <- .or(id, "NA")
        open[[key]] <- c(open[[key]], new_id)
      } else {
        key <- .or(id, "NA")
        if (length(open[[key]]) == 0) next
        new_id <- open[[key]][1]
        open[[key]] <- open[[key]][-1]
        if (!identical(new_id, id)) .set_attr(m, "w:id", new_id)
      }
    }
  }
  n_fixed
}

.mergeable_children <- c("w:rPr", "w:t", "w:tab", "w:br", "w:cr", "w:noBreakHyphen",
                         "w:softHyphen", "w:lastRenderedPageBreak")

.rpr_key <- function(r) {
  rpr <- .find1(r, "w:rPr")
  if (.is_missing(rpr)) return("")
  gsub(' xmlns(:[A-Za-z0-9_.-]+)?="[^"]*"', "", as.character(rpr, options = character()))
}

.merge_runs <- function(x) {
  xml2::xml_remove(.find_all(x, "//w:proofErr"))
  n_merged <- 0L
  for (parent in .find_all(x, "//*[w:r]")) {
    prev <- NULL
    prev_key <- NULL
    for (e in xml2::xml_children(parent)) {
      mergeable <- .name(e) == "w:r" && all(vapply(xml2::xml_children(e), .name, "") %in% .mergeable_children)
      if (!mergeable) {
        prev <- NULL
        next
      }
      key <- .rpr_key(e)
      if (!is.null(prev) && identical(key, prev_key)) {
        for (ch in xml2::xml_children(e)) if (.name(ch) != "w:rPr") .move(ch, prev, "child")
        xml2::xml_remove(e)
        n_merged <- n_merged + 1L
      } else {
        prev <- e
        prev_key <- key
      }
    }
  }
  for (r in .find_all(x, "//w:r[count(w:t) > 1]")) .merge_texts(r)
  n_merged
}

# Merges the adjacent w:t of a run.
.merge_texts <- function(r) {
  prev <- NULL
  for (e in xml2::xml_children(r)) {
    if (.name(e) != "w:t") {
      prev <- NULL
      next
    }
    if (is.null(prev)) {
      prev <- e
      next
    }
    xml2::xml_text(prev) <- paste0(xml2::xml_text(prev), xml2::xml_text(e))
    xml2::xml_remove(e)
  }
  for (t in .find_all(r, "w:t")) {
    if (grepl("^\\s|\\s$", xml2::xml_text(t))) .set_attr(t, "xml:space", "preserve")
  }
  invisible(r)
}
