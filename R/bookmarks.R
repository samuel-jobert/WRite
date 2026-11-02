# Bookmark manipulation

# Locates a bookmark: list(part, xml, start, end, id).
.bkm_find <- function(doc, name) {
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) stop("`name` must be a bookmark name.", call. = FALSE)
  for (p in .content_parts(doc)) {
    x <- .xml(doc, p)
    s <- .find_all(x, sprintf("//w:bookmarkStart[@w:name=%s]", .xpath_string(name)))
    if (length(s) == 0) next
    if (length(s) > 1) warning(sprintf("Several bookmarks named '%s': only the first one is used.", name), call. = FALSE)
    s <- s[[1]]
    id <- .attr(s, "w:id")
    e <- .find1(s, sprintf("following::w:bookmarkEnd[@w:id=%s]", .xpath_string(id)))
    if (.is_missing(e)) stop(sprintf("The end of bookmark '%s' was not found (corrupted document?).", name), call. = FALSE)
    return(list(part = p, xml = x, start = s, end = e, id = id))
  }
  stop(sprintf("Bookmark '%s' not found.", name), call. = FALSE)
}

# Locates several bookmarks with one search per part (same rules as .bkm_find()).
# Returns a named list: a bookmark as in .bkm_find(), or the error message.
.bkm_find_all <- function(doc, names) {
  if (length(names) == 1) {
    res <- list(tryCatch(.bkm_find(doc, names), error = function(e) conditionMessage(e)))
    names(res) <- names
    return(res)
  }
  res <- as.list(sprintf("Bookmark '%s' not found.", names))
  names(res) <- names
  todo <- names
  for (p in .content_parts(doc)) {
    if (length(todo) == 0) break
    x <- .xml(doc, p)
    # Starts and ends together, in document order.
    m <- .find_all(x, "//w:bookmarkStart | //w:bookmarkEnd")
    if (length(m) == 0) next
    is_start <- .name(m) == "w:bookmarkStart"
    ids <- .attr(m, "w:id")
    nms <- .attr(m, "w:name")
    for (nm in intersect(todo, nms[is_start])) {
      k <- which(is_start & nms == nm)
      if (length(k) > 1) warning(sprintf("Several bookmarks named '%s': only the first one is used.", nm), call. = FALSE)
      i <- k[1]
      j <- which(!is_start & ids == ids[i] & seq_along(m) > i)
      res[[nm]] <- if (length(j) == 0) {
        sprintf("The end of bookmark '%s' was not found (corrupted document?).", nm)
      } else {
        list(part = p, xml = x, start = m[[i]], end = m[[j[1]]], id = ids[i])
      }
    }
    todo <- setdiff(todo, nms[is_start])
  }
  res
}

.bookmark_max_id <- function(doc) {
  ids <- unlist(lapply(.content_parts(doc), function(p) {
    .attr(.find_all(.xml(doc, p), "//w:bookmarkStart | //w:bookmarkEnd"), "w:id")
  }))
  ids <- suppressWarnings(as.integer(ids))
  max(c(-1L, ids), na.rm = TRUE)
}

# Bookmark names of the document (same as bkm_list(doc)$name, without computing the texts).
.bkm_names <- function(doc) {
  as.character(unlist(lapply(.content_parts(doc), function(p) {
    .attr(.find_all(.xml(doc, p), "//w:bookmarkStart"), "w:name")
  })))
}

# "Maximal" elements strictly between the start and the end of the bookmark,
# in document order (ancestors of the end, partially covered, are excluded).
# Walks from the start to the end: the cost depends on the bookmark, not on the document.
.range <- function(bkm) {
  end_ancestors <- as.list(.find_all(bkm$end, "ancestor::*"))
  is_end_ancestor <- function(n) any(vapply(end_ancestors, identical, TRUE, n))
  # Next element after the subtree of `n`.
  next_node <- function(n) {
    repeat {
      s <- .find1(n, "following-sibling::*[1]")
      if (!.is_missing(s)) return(s)
      n <- xml2::xml_parent(n)
      if (.is_missing(n)) return(n)
    }
  }
  res <- list()
  n <- next_node(bkm$start)
  while (!identical(n, bkm$end)) {
    if (.is_missing(n)) stop("The end of the bookmark was not found (corrupted document?).", call. = FALSE)
    if (is_end_ancestor(n)) {
      n <- .find1(n, "*[1]")
    } else {
      res[[length(res) + 1]] <- n
      n <- next_node(n)
    }
  }
  res
}

# Text contained in a list of elements (paragraphs separated by "\r").
.nodes_text <- function(nodes) {
  pieces <- character()
  prev_p <- NULL
  for (n in nodes) {
    els <- .find_all(n, paste(
      "descendant-or-self::w:t[parent::w:r]", "descendant-or-self::w:tab[parent::w:r]",
      "descendant-or-self::w:br[parent::w:r]", "descendant-or-self::w:cr[parent::w:r]", sep = " | "))
    for (e in els) {
      p <- .paragraph_of(e)
      if (!is.null(prev_p) && !is.null(p) && !identical(p, prev_p)) pieces <- c(pieces, "\r")
      prev_p <- p
      nm <- .name(e)
      pieces <- c(pieces, switch(nm, "w:t" = xml2::xml_text(e), "w:tab" = "\t",
                                 if (identical(.attr(e, "w:type"), "page")) "\f" else "\n"))
    }
  }
  paste(pieces, collapse = "")
}

# Deletes the content of a bookmark while keeping the markers of other bookmarks:
# ends of other bookmarks are moved before the start, starts after the end.
.empty_range <- function(bkm, nodes = .range(bkm)) {
  p_start <- .paragraph_of(bkm$start)
  p_end <- .paragraph_of(bkm$end)
  if (!is.null(p_start) && !is.null(p_end) && !identical(p_start, p_end) &&
      !identical(xml2::xml_parent(p_start), xml2::xml_parent(p_end))) {
    stop("The bookmark spans paragraphs of different containers (e.g. table cells): unsupported operation.", call. = FALSE)
  }
  if (any(vapply(nodes, function(n) .name(n) == "w:tc", TRUE))) {
    stop("The bookmark partially spans a table row: unsupported operation.", call. = FALSE)
  }
  ends <- list()
  starts <- list()
  to_remove <- list()
  for (n in nodes) {
    nm <- .name(n)
    is_marker <- nm %in% c("w:bookmarkStart", "w:bookmarkEnd")
    if (!is_marker) to_remove[[length(to_remove) + 1]] <- n
    for (m in .find_all(n, "descendant-or-self::w:bookmarkStart | descendant-or-self::w:bookmarkEnd")) {
      if (identical(.attr(m, "w:id"), bkm$id)) next
      if (.name(m) == "w:bookmarkEnd") ends[[length(ends) + 1]] <- m else starts[[length(starts) + 1]] <- m
    }
  }
  # A bookmark entirely inside the range is kept (empty) just before the end.
  end_ids <- vapply(ends, .attr, "", name = "w:id")
  start_ids <- vapply(starts, .attr, "", name = "w:id")
  inner <- c(starts, ends)[c(start_ids %in% end_ids, end_ids %in% start_ids)]
  inner <- inner[order(vapply(inner, .index, 0))]
  ends <- ends[!end_ids %in% start_ids]
  starts <- starts[!start_ids %in% end_ids]
  for (m in ends) .move(m, bkm$start, "before")
  for (m in rev(starts)) .move(m, bkm$end, "after")
  for (m in inner) .move(m, bkm$end, "before")
  for (n in to_remove) xml2::xml_remove(n)
  if (!is.null(p_start) && !is.null(p_end) && !identical(p_start, p_end)) {
    for (ch in xml2::xml_children(p_end)) {
      if (.name(ch) != "w:pPr") .move(ch, p_start, "child")
    }
    xml2::xml_remove(p_end)
  }
  invisible(bkm)
}

.remove_markers <- function(bkm) {
  xml2::xml_remove(bkm$start)
  xml2::xml_remove(bkm$end)
  invisible()
}

# Block elements (paragraphs, tables, rows...) containing the bookmark, from the
# one holding its start to the one holding its end: children of their closest
# common container. Cells are never returned (their row is taken instead).
# As Word does when whole paragraphs are selected, a marker placed at the very
# start (end) of a paragraph other than the one of the other marker does not
# include this paragraph.
.containing_blocks <- function(bkm) {
  not_markers <- "[not(self::w:bookmarkStart or self::w:bookmarkEnd or self::w:proofErr)]"
  at_edge <- function(m, p, axis) {
    identical(xml2::xml_parent(m), p) &&
      .count(m, sprintf("count(%s-sibling::*%s[not(self::w:pPr)])", axis, not_markers)) == 0
  }
  p_s <- .paragraph_of(bkm$start)
  p_e <- .paragraph_of(bkm$end)
  d <- p_s
  f <- p_e
  if (is.null(p_s)) {
    d <- .find1(bkm$start, sprintf("following-sibling::*%s[1]", not_markers))
  } else if (!identical(p_s, p_e) && at_edge(bkm$start, p_s, "following")) {
    d <- .find1(p_s, sprintf("following-sibling::*%s[1]", not_markers))
  }
  if (is.null(p_e)) {
    f <- .find1(bkm$end, sprintf("preceding-sibling::*%s[1]", not_markers))
  } else if (!identical(p_s, p_e) && at_edge(bkm$end, p_e, "preceding")) {
    f <- .find1(p_e, sprintf("preceding-sibling::*%s[1]", not_markers))
  }
  if (.is_missing(d) || .is_missing(f)) return(NULL)
  # Closest common container: first divergence of the ancestor chains.
  anc_d <- as.list(.find_all(d, "ancestor-or-self::*"))
  anc_f <- as.list(.find_all(f, "ancestor-or-self::*"))
  k <- 1
  while (k <= min(length(anc_d), length(anc_f)) && identical(anc_d[[k]], anc_f[[k]])) k <- k + 1
  if (k == 1) return(NULL)
  d <- if (k <= length(anc_d)) anc_d[[k]] else anc_d[[k - 1]]
  f <- if (k <= length(anc_f)) anc_f[[k]] else anc_f[[k - 1]]
  if (.name(d) == "w:tc") {
    d <- xml2::xml_parent(d)
    f <- xml2::xml_parent(f)
  }
  if (.name(d) %in% c("w:body", "w:document", "w:hdr", "w:ftr", "w:footnotes", "w:endnotes") ||
      !identical(xml2::xml_parent(d), xml2::xml_parent(f)) || .index(d) > .index(f)) {
    return(NULL)
  }
  .siblings_between(d, f, xml2::xml_parent(d))
}

# Deletes whole block elements with the markers of the bookmark, preserving
# the markers of the other bookmarks.
.delete_blocks <- function(elements, bkm) {
  .remove_elements(elements, bkm$id)
  for (m in list(bkm$start, bkm$end)) if (.attached(m)) xml2::xml_remove(m)
  invisible(bkm)
}

# Deletes the paragraphs (or other block elements) containing the bookmark.
# Without any whole block (e.g. empty bookmark across a paragraph boundary),
# only the content and the markers are deleted.
.delete_paragraphs <- function(bkm) {
  elements <- .containing_blocks(bkm)
  if (is.null(elements)) {
    .empty_range(bkm)
    .remove_markers(bkm)
    return(invisible(bkm))
  }
  .delete_blocks(elements, bkm)
}

#' List the bookmarks of a document
#'
#' @param doc WRite document object.
#' @return A `data.frame` with columns `name`, `id`, `part` (XML part
#'   containing the bookmark: body, header, footer, notes) and `text`
#'   (same conventions as [bkm_text()]).
#' @examples
#' doc <- doc_read(write_example("example"))
#' bkm_list(doc)
#'
#' # Names of empty bookmarks (places to fill in)
#' s <- bkm_list(doc)
#' s$name[s$text == ""]
#'
#' # Bookmarks of a richer Word document
#' bkm_list(doc_read(write_example("complex")))
#' @export
bkm_list <- function(doc) {
  .check_doc(doc)
  res <- list()
  for (p in .content_parts(doc)) {
    x <- .xml(doc, p)
    for (s in .find_all(x, "//w:bookmarkStart")) {
      id <- .attr(s, "w:id")
      e <- .find1(s, sprintf("following::w:bookmarkEnd[@w:id=%s]", .xpath_string(id)))
      text <- if (.is_missing(e)) NA_character_ else .nodes_text(.range(list(xml = x, start = s, end = e)))
      res[[length(res) + 1]] <- data.frame(name = .attr(s, "w:name"), id = id, part = p,
                                           text = text, stringsAsFactors = FALSE)
    }
  }
  if (length(res) == 0) {
    return(data.frame(name = character(), id = character(), part = character(),
                      text = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, res)
}

#' Text contained in a bookmark
#'
#' @param doc WRite document object.
#' @param name Bookmark name.
#' @return A string: paragraphs separated by `"\r"`, line break = `"\n"`,
#'   tab = `"\t"`, page break = `"\f"` (the conventions of the insertion
#'   functions, see [doc_add_paragraph()]: the text can be given back to
#'   [bkm_replace_text()]).
#' @examples
#' doc <- doc_read(write_example("example"))
#' bkm_text(doc, "my_bookmark")            # "Middle"
#' bkm_text(doc, "empty_bookmark")         # "": empty bookmark
#'
#' # A bookmark spanning several paragraphs
#' bkm_insert_docx(doc, "empty_bookmark", write_example("to_insert"))
#' strsplit(bkm_text(doc, "empty_bookmark"), "\r")[[1]]   # one element per paragraph
#' @export
bkm_text <- function(doc, name) {
  .check_doc(doc)
  .bkm_guard(.nodes_text(.range(.bkm_find(doc, name))), NA_character_)
}

#' Delete a bookmark
#'
#' By default, deletes the whole paragraph(s) containing the bookmark, from the
#' paragraph of its start to the paragraph of its end, bookmark included (no
#' empty paragraph is left). With `delete_paragraph = FALSE`, only the content
#' surrounded by the bookmark is deleted and the (now empty) bookmark is kept.
#' In both cases, the start/end markers of **other** bookmarks found in the
#' deleted content are kept (the end of a preceding bookmark is moved before
#' the deleted content, the start of a following bookmark after it, and a
#' bookmark entirely inside the deleted content is kept, empty), so that these
#' bookmarks are not destroyed.
#'
#' If the bookmark starts and ends in different structures (e.g. in a paragraph
#' and in a table), the whole elements between them are deleted (the table
#' included); if it starts and ends in different cells of the same table, the
#' corresponding rows are deleted.
#'
#' In a table, the paragraphs (or the content) of the cell are deleted by
#' default. With `delete_row = TRUE`, the whole row containing the bookmark is
#' deleted:
#'
#' * if the bookmark is in a cell merged vertically, all the rows of this
#'   merged cell are deleted;
#' * if the bookmark is in another cell of a row where some other cells are
#'   merged vertically, only this row is deleted and the value of the merged
#'   cells is kept (the content is moved to the next row of the merged cell,
#'   which becomes its start).
#'
#' The other bookmarks having a marker in a deleted row are deleted as well.
#'
#' @param doc WRite document object.
#' @param name Bookmark name, or vector of bookmark names. All the bookmarks
#'   are located in a single search, then deleted in the order given;
#'   overlapping or nested bookmarks are handled as with successive calls
#'   (each deletion preserves the markers of the others). When the operation
#'   cannot be done for a bookmark (name not found, unsupported structure,
#'   bookmark already deleted with a table row), a warning is given and the
#'   other bookmarks are still processed.
#'   Not used if `whole_document = TRUE`.
#' @param delete_content Delete the content of the bookmark?
#' @param delete_bookmark Delete the markers of the bookmark? Ignored (always
#'   deleted) when the paragraph is deleted.
#' @param delete_paragraph When deleting the content, delete the whole
#'   paragraph(s) containing the bookmark, with its markers? If `FALSE`, only
#'   the content between the markers is deleted. Ignored if
#'   `delete_content = FALSE`.
#' @param whole_document Delete all the bookmark markers of the document
#'   (no content is deleted; the other arguments are ignored)?
#' @param delete_row If the bookmark is in a table, delete its row (see
#'   Details)?
#' @return `doc`, invisibly.
#' @section Illustration:
#' ```
#' Before                      : Price [a: 10 [b: euros]] incl. tax
#' defaults                    : (the paragraph is deleted)
#' delete_paragraph = FALSE    : Price [a: ] incl. tax
#' delete_content = FALSE,
#'   delete_bookmark = TRUE    : Price 10 [b: euros] incl. tax
#' delete_paragraph = FALSE,
#'   delete_bookmark = TRUE    : Price  incl. tax
#'
#' Overlapping bookmarks       : text "Price 10 euros incl. tax", b = "Price 10", a = "10 euros"
#' delete a, delete_paragraph = FALSE,
#'   delete_bookmark = TRUE    : text "Price  incl. tax", b = "Price " (the end of b, located in a, is preserved)
#' ```
#' @examples
#' doc <- doc_read(write_example("example"))
#'
#' # Remove only the bookmark: the text stays
#' bkm_delete(doc, "my_bookmark", delete_content = FALSE, delete_bookmark = TRUE)
#' "Middle" %in% doc_text(doc)                 # TRUE
#' "my_bookmark" %in% bkm_list(doc)$name       # FALSE
#'
#' # Remove only the content (the empty bookmark stays)
#' bkm_delete(doc, "amount_2025", delete_paragraph = FALSE)
#' doc_text(doc)[4]   # "The order amount is  euros in 2025."
#'
#' # Remove the whole paragraph containing the bookmark
#' n <- length(doc_text(doc))
#' bkm_delete(doc, "amount_2025")
#' length(doc_text(doc)) == n - 1   # TRUE
#'
#' # Remove all the bookmarks of the document (the text stays)
#' bkm_delete(doc, whole_document = TRUE)
#' nrow(bkm_list(doc))   # 0
#' @export
bkm_delete <- function(doc, name = NULL, delete_content = TRUE, delete_bookmark = FALSE,
                       delete_paragraph = TRUE, whole_document = FALSE, delete_row = FALSE) {
  .check_doc(doc)
  .check_flag(delete_content, "delete_content")
  .check_flag(delete_bookmark, "delete_bookmark")
  .check_flag(delete_paragraph, "delete_paragraph")
  .check_flag(whole_document, "whole_document")
  .check_flag(delete_row, "delete_row")
  if (whole_document) {
    for (p in .content_parts(doc)) {
      xml2::xml_remove(.find_all(.xml(doc, p), "//w:bookmarkStart | //w:bookmarkEnd"))
    }
    return(invisible(doc))
  }
  if (!is.character(name) || length(name) < 1 || anyNA(name) || !all(nzchar(name))) {
    stop("`name` must be a bookmark name (or a vector of names).", call. = FALSE)
  }
  names_u <- unique(name)
  found <- .bkm_find_all(doc, names_u)
  for (nm in names_u) {
    tryCatch({
      bkm <- found[[nm]]
      if (is.character(bkm)) stop(bkm, call. = FALSE)
      # Possible with delete_row: a bookmark with a marker in a deleted row is deleted too.
      if (!.attached(bkm$start) || !.attached(bkm$end)) {
        stop(sprintf("Bookmark '%s' was deleted with a previous bookmark.", nm), call. = FALSE)
      }
      .delete_one(bkm, delete_content, delete_bookmark, delete_paragraph, delete_row)
    }, error = function(e) warning(sprintf("%s Bookmark ignored.", conditionMessage(e)), call. = FALSE))
  }
  invisible(doc)
}

# Core of bkm_delete() for one bookmark (errors are left to the caller).
.delete_one <- function(bkm, delete_content = TRUE, delete_bookmark = FALSE,
                        delete_paragraph = TRUE, delete_row = FALSE) {
  if (delete_row && .delete_table_rows(bkm)) return(invisible(bkm))
  if (delete_content && delete_paragraph) {
    .delete_paragraphs(bkm)
  } else {
    if (delete_content) .empty_range(bkm)
    if (delete_bookmark) .remove_markers(bkm)
  }
  invisible(bkm)
}

# "none", "restart" or "continue" (vertical merge) for a w:tc (NULL = none).
.vmerge <- function(tc) {
  if (is.null(tc)) return("none")
  n <- .find1(tc, "w:tcPr/w:vMerge")
  if (.is_missing(n)) return("none")
  v <- .attr(n, "w:val")
  if (is.na(v) || v == "continue") "continue" else "restart"
}

.cell_spans <- function(tr) {
  vapply(.find_all(tr, "w:tc"), function(c) {
    g <- .find1(c, "w:tcPr/w:gridSpan")
    v <- if (.is_missing(g)) NA_integer_ else suppressWarnings(as.integer(.attr(g, "w:val")))
    if (is.na(v)) 1L else v
  }, 1L)
}

# Cell of the row starting at the grid column `col` (NULL if none).
.cell_at <- function(tr, col) {
  cells <- .find_all(tr, "w:tc")
  start <- cumsum(c(1L, .cell_spans(tr)))[seq_along(cells)]
  i <- which(start == col)
  if (length(i) == 0) NULL else cells[[i[1]]]
}

# Deletes the row(s) of the table containing the start of the bookmark.
# Returns FALSE (nothing done) if the bookmark is not in a table cell.
.delete_table_rows <- function(bkm) {
  tc <- .find1(bkm$start, "ancestor::w:tc[1]")
  if (.is_missing(tc)) {
    warning("The bookmark is not in a table: `delete_row` is ignored.", call. = FALSE)
    return(FALSE)
  }
  tr <- .find1(tc, "parent::w:tr")
  tbl <- .find1(tr, "parent::w:tbl")
  rows <- .find_all(tbl, "w:tr")
  idx <- .count(tr, "count(preceding-sibling::w:tr)") + 1
  col <- sum(.cell_spans(tr)[seq_len(.count(tc, "count(preceding-sibling::w:tc)"))]) + 1L
  del <- idx
  if (.vmerge(tc) != "none") {
    i <- idx
    while (i > 1 && .vmerge(.cell_at(rows[[i]], col)) == "continue") i <- i - 1
    j <- idx
    while (j < length(rows) && .vmerge(.cell_at(rows[[j + 1]], col)) == "continue") j <- j + 1
    del <- i:j
  }
  # The merged cells starting in a deleted row but continuing below keep their value.
  for (r in del) {
    cells <- .find_all(rows[[r]], "w:tc")
    starts <- cumsum(c(1L, .cell_spans(rows[[r]])))
    for (ci in seq_along(cells)) {
      if (.vmerge(cells[[ci]]) != "restart") next
      k <- r + 1
      while (k <= length(rows) && .vmerge(.cell_at(rows[[k]], starts[ci])) == "continue") {
        if (!k %in% del) {
          target <- .cell_at(rows[[k]], starts[ci])
          for (ch in xml2::xml_children(target)) if (.name(ch) != "w:tcPr") xml2::xml_remove(ch)
          for (ch in xml2::xml_children(cells[[ci]])) if (.name(ch) != "w:tcPr") .move(ch, target, "child")
          vm <- .find1(target, "w:tcPr/w:vMerge")
          if (k < length(rows) && .vmerge(.cell_at(rows[[k + 1]], starts[ci])) == "continue") {
            .set_attr(vm, "w:val", "restart")
          } else {
            xml2::xml_remove(vm)
          }
          break
        }
        k <- k + 1
      }
    }
  }
  # A bookmark with a marker in a deleted row is deleted entirely.
  ids <- unique(unlist(lapply(rows[del], function(r) {
    .attr(.find_all(r, ".//w:bookmarkStart | .//w:bookmarkEnd"), "w:id")
  })))
  for (id in ids[!is.na(ids)]) {
    xml2::xml_remove(.find_all(bkm$xml, sprintf("//w:bookmarkStart[@w:id=%s] | //w:bookmarkEnd[@w:id=%s]",
                                                .xpath_string(id), .xpath_string(id))))
  }
  if (length(del) == length(rows)) {
    holder <- .find1(tbl, "parent::w:tc")
    xml2::xml_remove(tbl)
    if (!.is_missing(holder) && .count(holder, "count(w:p)") == 0) .add(holder, "w:p")
  } else {
    for (r in del) xml2::xml_remove(rows[[r]])
  }
  TRUE
}
#' Replace the text of a bookmark
#'
#' Replaces the content of the bookmark with `text`, keeping the formatting of
#' the first text run of the bookmark (or, if the bookmark is empty, that of
#' the run preceding it). The markers of other bookmarks are preserved (see
#' [bkm_delete()]).
#'
#' With a vector of names, all the bookmarks are located in a single search,
#' then filled in the order given (faster than successive calls). A bookmark
#' that is not found gives a warning and the others are still processed.
#'
#' @param doc WRite document object.
#' @param name Bookmark name, or vector of bookmark names.
#' @param text New text (`"\n"` = line break, `"\r"` = new paragraph, `"\t"` = tab,
#'   `"\f"` = page break; see the "Special characters" section). With several names:
#'   one text for all the bookmarks, or a vector of the same length as `name`.
#'   With a single name, a vector of texts is joined with line breaks.
#' @param keep_bookmark Keep the bookmark around the new text?
#' @param html Interpret the HTML tags of `text` (`<b>`, `<i>`, `<u>`, `<s>`,
#'   `<br>`, `<p>`...)? See [doc_add_paragraph()]. The formatting of the tags is
#'   added to the kept formatting.
#' @param style Style of the document (identifier or name, see
#'   [doc_add_style()]). A character style is applied to the new text; a
#'   paragraph style to the paragraph(s) containing it.
#' @return `doc`, invisibly.
#' @inheritSection doc_add_paragraph Special characters
#' @section Illustration:
#' ```
#' Before : Amount: [amount: **0**] euros
#' After  : Amount: [amount: **1,500**] euros      (bold kept)
#' keep_bookmark = FALSE : Amount: **1,500** euros
#' ```
#' @seealso [bkm_insert_text()] to impose a formatting,
#'   [doc_fill()] to fill several bookmarks at once.
#' @examples
#' doc <- doc_read(write_example("example"))
#' doc_text(doc)[4]
#' bkm_replace_text(doc, "amount_2025", "2,000,000")
#' doc_text(doc)[4]   # "The order amount is 2,000,000 euros in 2025."
#'
#' # Multi-line text (line break) and tab
#' bkm_replace_text(doc, "my_bookmark", "Line 1\nLine 2\tcolumn")
#' bkm_text(doc, "my_bookmark")
#'
#' # Existing formatting is kept
#' doc2 <- doc_new() |> doc_add_paragraph("0", bold = TRUE, bookmark = "amount")
#' bkm_replace_text(doc2, "amount", "1,500")   # "1,500" stays bold
#'
#' # Replace then delete the bookmark
#' bkm_replace_text(doc2, "amount", "1,600", keep_bookmark = FALSE)
#' bkm_list(doc2)
#'
#' # HTML tags
#' doc3 <- doc_new() |> doc_add_paragraph("x", bookmark = "bk")
#' bkm_replace_text(doc3, "bk", "Total: <b>12</b> <i>euros</i>", html = TRUE)
#' doc_text(doc3)
#'
#' # Several bookmarks at once
#' doc4 <- doc_read(write_example("example"))
#' bkm_replace_text(doc4, c("amount_2025", "my_bookmark"), c("2,000,000", "New middle"))
#' doc_text(doc4)
#' @export
bkm_replace_text <- function(doc, name, text, keep_bookmark = TRUE, html = FALSE,
                             style = NULL) {
  .check_doc(doc)
  .check_flag(html, "html")
  st <- .resolve_style(doc, style)
  if (length(name) == 1) {
    .bkm_guard(.replace_bkm_text(.bkm_find(doc, name), text, keep_bookmark, html, st), NULL)
    return(invisible(doc))
  }
  if (!is.character(name) || length(name) < 1 || anyNA(name) || !all(nzchar(name))) {
    stop("`name` must be a bookmark name (or a vector of names).", call. = FALSE)
  }
  if (!length(text) %in% c(1, length(name))) {
    stop("`text` must be a string or a vector of the same length as `name`.", call. = FALSE)
  }
  text <- rep_len(text, length(name))
  found <- .bkm_find_all(doc, unique(name))
  for (i in seq_along(name)) {
    .bkm_guard({
      bkm <- found[[name[i]]]
      if (is.character(bkm)) stop(bkm, call. = FALSE)
      # Markers already removed by a previous element (same name with keep_bookmark = FALSE).
      if (!.attached(bkm$start) || !.attached(bkm$end)) {
        stop(sprintf("Bookmark '%s' no longer exists.", name[i]), call. = FALSE)
      }
      .replace_bkm_text(bkm, text[[i]], keep_bookmark, html, st)
    }, NULL)
  }
  invisible(doc)
}

# Core of bkm_replace_text() for one bookmark (`st`: resolved style or NULL).
.replace_bkm_text <- function(bkm, text, keep_bookmark, html, st) {
  nodes <- .range(bkm)
  rpr <- NULL
  in_range <- FALSE
  for (n in nodes) {
    r <- .find1(n, "descendant-or-self::w:r[w:t][1]")
    if (!.is_missing(r)) {
      rpr <- .find1(r, "w:rPr")
      in_range <- TRUE
      break
    }
  }
  if (is.null(rpr)) rpr <- .find1(bkm$start, "preceding-sibling::w:r[1]/w:rPr")
  if (.is_missing(rpr)) rpr <- NULL
  # The rPr of the deleted content is detached to be reused; otherwise it is copied.
  if (!is.null(rpr) && in_range) xml2::xml_remove(rpr)
  .empty_range(bkm, nodes)
  f <- .format(char_style = if (!is.null(st) && st$type == "character") st$id)
  ins <- .insert_runs(bkm$start, "after", text, f, html)
  if (!is.null(rpr)) for (r in ins$runs) .merge_rpr(r, rpr)
  if (!is.null(st) && st$type == "paragraph") for (p in ins$ps) .set_paragraph_style(p, st$id)
  if (!isTRUE(keep_bookmark)) .remove_markers(bkm)
  invisible(bkm)
}

# Inserts the runs of `text` next to a marker (see .paragraph_segments()); creates
# a paragraph if the marker is outside a paragraph. Each "\r" splits the paragraph:
# the new paragraphs take its properties, and what followed the insertion point
# goes at the end of the last one.
# Returns list(p = first paragraph, ps = all the paragraphs, r = first run, runs).
.insert_runs <- function(anchor, where, text, f, html = FALSE) {
  pars <- .paragraph_segments(text, f, html)
  p <- .paragraph_of(anchor)
  if (is.null(p)) {
    p <- .add(anchor, "w:p", sibling = where)
    runs <- .segment_runs(pars[[1]], parent = p)
  } else {
    if (length(pars) > 1 && !identical(xml2::xml_parent(anchor), p)) {
      stop("The bookmark is placed in a structure (hyperlink, field...) where a new paragraph cannot be created.", call. = FALSE)
    }
    runs <- .segment_runs(pars[[1]], anchor = anchor, where = where)
  }
  ps <- list(p)
  if (length(pars) > 1) {
    tail <- as.list(xml2::xml_find_all(runs[[length(runs)]], "following-sibling::*"))
    ppr <- .find1(p, "w:pPr")
    for (k in 2:length(pars)) {
      q <- .add(ps[[k - 1]], "w:p", sibling = "after")
      if (!.is_missing(ppr)) xml2::xml_add_child(q, ppr)
      runs <- c(runs, .segment_runs(pars[[k]], parent = q))
      ps[[k]] <- q
    }
    for (n in tail) .move(n, ps[[length(ps)]], "child")
    # A section break stays with the last paragraph.
    for (q in ps[-length(ps)]) xml2::xml_remove(.find_all(q, "w:pPr/w:sectPr"))
  }
  list(p = p, ps = ps, r = runs[[1]], runs = runs)
}

#' Insert formatted text on a bookmark
#'
#' @param doc WRite document object.
#' @param name Bookmark name.
#' @param text Text to insert (`"\n"` = line break, `"\r"` = new paragraph,
#'   `"\t"` = tab, `"\f"` = page break; see the "Special characters" section).
#' @param position `"replace"` (replaces the content of the bookmark),
#'   `"before"` (at the start of the bookmark, before its content) or
#'   `"after"` (at the end of the bookmark, after its content). The inserted
#'   text is always inside the bookmark.
#' @param bold,italic `TRUE`/`FALSE` (`NULL` = inherited).
#' @param underline `TRUE`, `FALSE` or Word underline type (`"double"`, `"dotted"`...).
#' @param size Font size in points.
#' @param font Font name.
#' @param style Predefined style (identifier or name). A character style is
#'   applied to the inserted text; a paragraph style is applied to the
#'   paragraph containing the text.
#' @param keep_bookmark Keep the bookmark after the insertion?
#' @param keep_with_previous Keep the previous paragraph on the same page as
#'   the paragraph containing the inserted text ("Keep with next" set on the
#'   previous paragraph).
#' @param keep_lines_together Keep all the lines of the paragraph containing
#'   the inserted text on the same page ("Keep lines together").
#' @param html Interpret the HTML tags of `text` (`<b>`/`<strong>`,
#'   `<i>`/`<em>`, `<u>`, `<s>`/`<strike>`/`<del>`, `<br>`, `<p>`)? See
#'   [doc_add_paragraph()].
#' @return `doc`, invisibly.
#' @inheritSection doc_add_paragraph Special characters
#' @section Illustration:
#' ```
#' Before                  : Total: [total: 100] euros
#' position = "replace"    : Total: [total: **250**] euros
#' position = "before"     : Total: [total: **250**100] euros
#' position = "after"      : Total: [total: 100**250**] euros
#' ```
#' @examples
#' doc <- doc_read(write_example("example"))
#'
#' # Replace the content with formatted text
#' bkm_insert_text(doc, "my_bookmark", "Important text", bold = TRUE,
#'                 size = 14, font = "Arial")
#' bkm_text(doc, "my_bookmark")
#'
#' # Add before / after the existing content (the text stays in the bookmark)
#' bkm_insert_text(doc, "amount_2025", "about ", position = "before", italic = TRUE)
#' bkm_insert_text(doc, "amount_2025", " (provisional)", position = "after",
#'                 underline = "double")
#' bkm_text(doc, "amount_2025")
#'
#' # Apply a style of the document (by identifier or by name)
#' bkm_insert_text(doc, "empty_bookmark", "Special clauses", style = "Heading1")
#'
#' # Keep the previous paragraph on the same page, lines together
#' bkm_insert_text(doc, "empty_bookmark", " (continued)", position = "after",
#'                 keep_with_previous = TRUE, keep_lines_together = TRUE)
#'
#' # HTML tags
#' bkm_insert_text(doc, "my_bookmark", "A <b>bold</b> and <i>italic</i> text", html = TRUE)
#' bkm_text(doc, "my_bookmark")
#' @export
bkm_insert_text <- function(doc, name, text, position = c("replace", "before", "after"),
                            bold = NULL, italic = NULL, underline = NULL, size = NULL,
                            font = NULL, style = NULL, keep_bookmark = TRUE,
                            keep_with_previous = FALSE, keep_lines_together = FALSE,
                            html = FALSE) {
  .check_doc(doc)
  position <- match.arg(position)
  .check_flag(keep_with_previous, "keep_with_previous")
  .check_flag(keep_lines_together, "keep_lines_together")
  .check_flag(html, "html")
  st <- .resolve_style(doc, style)
  f <- .format(bold, italic, underline, size, font,
               char_style = if (!is.null(st) && st$type == "character") st$id)
  .bkm_guard({
  bkm <- .bkm_find(doc, name)
  if (position == "replace") .empty_range(bkm)
  ins <- if (position == "after") .insert_runs(bkm$end, "before", text, f, html) else .insert_runs(bkm$start, "after", text, f, html)
  if (!is.null(st) && st$type == "paragraph") for (p in ins$ps) .set_paragraph_style(p, st$id)
  .set_keep_options(ins$p, keep_with_previous, FALSE)
  if (keep_lines_together) for (p in ins$ps) .enable_property(p, "w:keepLines")
  if (!isTRUE(keep_bookmark)) .remove_markers(bkm)
  }, NULL)
  invisible(doc)
}
