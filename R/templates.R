#' Fill a template
#'
#' Fills in one go the bookmarks and/or the tags (`{{name}}` by default) of a
#' document from a named list of values.
#'
#' For each `name = value` element:
#' * if a bookmark `name` exists, its content is replaced by the value
#'   (formatting kept, see [bkm_replace_text()]);
#' * all the `{{name}}` tags of the document are replaced by the value
#'   (see [doc_replace_text()]).
#'
#' A warning lists the names for which neither a bookmark nor a tag was found.
#'
#' @param doc WRite document object.
#' @param values Named list (or one-row `data.frame`) of values. Values are
#'   converted to text with `as.character()`; `NA` becomes `""`. `"\n"`, `"\r"`,
#'   `"\t"` and `"\f"` give a line break, a new paragraph, a tab and a page
#'   break, in the bookmarks as in the tags (see the "Special characters" section).
#' @param tags Opening and closing tag delimiters, or `NULL` to not process
#'   tags.
#' @param bookmarks Also fill the bookmarks with these names?
#' @param warn Emit a warning for the names that are not found?
#' @return `doc`, invisibly. The `"fill_report"` attribute contains a
#'   `data.frame` (`name`, `bookmark`: bookmark filled or not, `tags`: number of
#'   tags replaced).
#' @inheritSection doc_add_paragraph Special characters
#' @section Illustration:
#' ```
#' Template: Dear {{name}},
#'           Amount due: [amount: ...] euros.
#' doc_fill(doc, list(name = "Smith", amount = "1,500"))
#' Result:   Dear Smith,
#'           Amount due: [amount: 1,500] euros.
#' ```
#' @examples
#' doc <- doc_new() |>
#'   doc_add_paragraph("Dear {{name}},") |>
#'   doc_add_paragraph("Amount due: ") |>
#'   doc_add_paragraph("", bookmark = "amount")
#' doc_fill(doc, list(name = "Smith", amount = 1500))
#' doc_text(doc)
#' attr(doc, "fill_report")
#'
#' # On the example document: a bookmark and a tag
#' doc2 <- doc_read(write_example("example"))
#' doc_fill(doc2, list(amount_2025 = "2,000,000", amount_2024 = "1,800,000"))
#' doc_text(doc2)[3:4]
#'
#' # Name not found: warning (can be disabled with warn = FALSE)
#' doc_fill(doc2, list(unknown = "x"), warn = FALSE)
#' attr(doc2, "fill_report")
#'
#' # Custom delimiters and one-row data.frame
#' doc3 <- doc_new() |> doc_add_paragraph("City: [city] ([zip])")
#' doc_fill(doc3, data.frame(city = "Lyon", zip = "69000"), tags = c("[", "]"))
#' doc_text(doc3)
#' @export
doc_fill <- function(doc, values, tags = c("{{", "}}"), bookmarks = TRUE, warn = TRUE) {
  .check_doc(doc)
  report <- .fill(doc, values, tags, bookmarks)
  missing <- report$name[!report$bookmark & report$tags == 0]
  if (isTRUE(warn) && length(missing) > 0) {
    warning(sprintf("Neither bookmark nor tag found for: %s", paste(missing, collapse = ", ")), call. = FALSE)
  }
  attr(doc, "fill_report") <- report
  invisible(doc)
}

.check_tags <- function(tags) {
  if (!is.null(tags) && !(is.character(tags) && length(tags) == 2 && all(nzchar(tags)))) {
    stop("`tags` must be NULL or a vector of two strings (opening and closing delimiters).", call. = FALSE)
  }
  invisible(tags)
}

.value_text <- function(v) {
  if (length(v) != 1) stop("Each value must have length 1.", call. = FALSE)
  if (is.na(v)) "" else as.character(v)
}

.fill <- function(doc, values, tags, bookmarks) {
  .check_tags(tags)
  if (is.data.frame(values)) {
    if (nrow(values) != 1) stop("`values` must be a data.frame with a single row.", call. = FALSE)
    values <- lapply(values, function(x) if (is.factor(x)) as.character(x) else x)
  }
  if (!is.list(values) || is.null(names(values)) || any(!nzchar(names(values)))) {
    stop("`values` must be a named list.", call. = FALSE)
  }
  bkm_names <- if (isTRUE(bookmarks)) .bkm_names(doc) else character()
  report <- data.frame(name = names(values), bookmark = FALSE, tags = 0L, stringsAsFactors = FALSE)
  texts <- vapply(values, .value_text, "")
  report$bookmark <- names(values) %in% bkm_names
  if (any(report$bookmark)) {
    bkm_replace_text(doc, names(values)[report$bookmark], unname(texts[report$bookmark]))
  }
  # All the tags in a single pass over the document.
  if (!is.null(tags) && length(values) > 0) {
    report$tags <- .replace_rich(doc, paste0(tags[1], names(values), tags[2]), unname(texts))
  }
  report
}

#' Conditional block
#'
#' Keeps or removes the content of a bookmark depending on a condition. If
#' `condition` is `FALSE`, the bookmark and its content are removed: when the
#' bookmark covers whole paragraphs (or table rows), they are removed
#' completely, without leaving an empty paragraph; when it covers only part
#' of a paragraph, just its content is removed (as [bkm_delete()] with
#' `delete_paragraph = FALSE`). The markers of the other bookmarks are
#' preserved (see [bkm_delete()]).
#'
#' @param doc WRite document object.
#' @param name Name of the bookmark delimiting the block.
#' @param condition `TRUE` to keep the block, `FALSE` to remove it.
#' @param keep_bookmark If the block is kept, keep the bookmark?
#' @return `doc`, invisibly.
#' @section Illustration:
#' ```
#' Template                    condition = FALSE        condition = TRUE
#' Article 1                   Article 1                Article 1
#' [business_clause: Art. 2    Article 3                [business_clause: Art. 2
#'  (businesses)]                                        (businesses)]
#' Article 3                                            Article 3
#' ```
#' @examples
#' template <- doc_new() |>
#'   doc_add_paragraph("Article 1") |>
#'   doc_add_paragraph("Article 2 (businesses)", bookmark = "business_clause") |>
#'   doc_add_paragraph("Article 3")
#'
#' individual <- doc_copy(template) |> bkm_conditional("business_clause", FALSE)
#' doc_text(individual)   # "Article 1" "Article 3": no empty paragraph
#'
#' business <- doc_copy(template) |> bkm_conditional("business_clause", TRUE, keep_bookmark = FALSE)
#' doc_text(business)
#'
#' # Computed condition and inline content (in the middle of a paragraph)
#' doc <- doc_read(write_example("example"))
#' amount <- 0
#' bkm_conditional(doc, "amount_2025", amount > 0)
#' doc_text(doc)[4]
#' @export
bkm_conditional <- function(doc, name, condition, keep_bookmark = TRUE) {
  .check_doc(doc)
  .check_flag(condition, "condition")
  .bkm_guard({
  bkm <- .bkm_find(doc, name)
  if (condition) {
    if (!isTRUE(keep_bookmark)) .remove_markers(bkm)
    return(invisible(doc))
  }
  block <- .bookmark_block(bkm)
  aligned <- !is.null(block) && block$aligned
  if (aligned && .name(block$elements[[1]]) == "w:tr") {
    .delete_blocks(block$elements, bkm)
  } else {
    .delete_one(bkm, delete_bookmark = TRUE, delete_paragraph = aligned)
  }
  }, NULL)
  invisible(doc)
}

#' Repeat a block for each row of a data table
#'
#' The content of the bookmark is used as a template: it is duplicated for each
#' row of `data`, and in each copy the `{{column}}` tags are replaced by the
#' values of the row.
#'
#' * If the bookmark is in a Word table, the template is made of the **table
#'   rows** covered by the bookmark (typical case: a fixed header row, then a
#'   repeated template row).
#' * Otherwise, the template is made of the **whole paragraphs** covered by the
#'   bookmark.
#'
#' With `keep_bookmark = TRUE`, the bookmark surrounds all the copies.
#' Any other bookmarks present in the template are only kept in the first copy.
#' With no data row, the template is removed (and so is the table if it only
#' contained the template rows).
#'
#' @param doc WRite document object.
#' @param name Name of the bookmark delimiting the template.
#' @param data `data.frame`: one copy of the template per row. The special
#'   characters of the values are interpreted (see [doc_add_paragraph()]).
#' @param tags Opening and closing tag delimiters.
#' @param keep_bookmark Keep the bookmark around the copies?
#' @return `doc`, invisibly.
#' @section Illustration:
#' ```
#' Template (table)                      data = data.frame(product =
#' | Product       | Price     |                           c("Apples", "Pears"),
#' [rows: | {{product}} | {{price}} |]                price = c("2.50", "3.10"))
#' | Total         | 5.60      |
#'
#' Result
#' | Product       | Price     |
#' [rows: | Apples | 2.50 |
#'        | Pears  | 3.10 |]
#' | Total         | 5.60      |
#'
#' Template (paragraphs): [members: - {{name}} ({{role}})]
#' Result                : [members: - Ana (president)
#'                                   - Bob (treasurer)]
#' ```
#' @examples
#' # Repeating paragraphs
#' doc <- doc_new() |>
#'   doc_add_paragraph("Board members:") |>
#'   doc_add_paragraph("- {{name}} ({{role}})", bookmark = "members") |>
#'   doc_add_paragraph("End of the list")
#' board <- data.frame(name = c("Ana", "Bob", "Chloe"),
#'                     role = c("president", "treasurer", "secretary"))
#' bkm_repeat(doc, "members", board)
#' doc_text(doc)
#'
#' # No rows: the template disappears
#' doc2 <- doc_new() |>
#'   doc_add_paragraph("- {{name}}", bookmark = "list") |>
#'   doc_add_paragraph("Next")
#' bkm_repeat(doc2, "list", data.frame(name = character()))
#' doc_text(doc2)   # "Next"
#'
#' # Table rows: see the illustration above. In Word, put a bookmark "rows" on
#' # the template row of the table, then:
#' # bkm_repeat(doc, "rows", data.frame(product = ..., price = ...))
#' @export
bkm_repeat <- function(doc, name, data, tags = c("{{", "}}"), keep_bookmark = TRUE) {
  .check_doc(doc)
  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  .check_tags(tags)
  if (is.null(tags)) stop("`tags` cannot be NULL here.", call. = FALSE)
  .merge_runs_once(doc)
  .bkm_guard({
  bkm <- .bkm_find(doc, name)
  block <- .bookmark_block(bkm)
  if (is.null(block)) stop(sprintf("Cannot determine the template block of bookmark '%s'.", name), call. = FALSE)
  template <- block$elements
  n <- nrow(data)
  if (n == 0) {
    .remove_elements(template, bkm$id)
    for (m in list(bkm$start, bkm$end)) {
      if (.attached(m) && !isTRUE(keep_bookmark)) xml2::xml_remove(m)
    }
    return(invisible(doc))
  }
  columns <- names(data)
  store <- new.env()
  store$.html <- FALSE
  ref <- template[[length(template)]]
  for (k in seq_len(n)) {
    copies <- list()
    for (el in template) {
      ref <- xml2::xml_add_sibling(ref, el, .where = "after")
      copies[[length(copies) + 1]] <- ref
    }
    for (cp in copies) {
      for (mk in .find_all(cp, "descendant-or-self::w:bookmarkStart | descendant-or-self::w:bookmarkEnd")) {
        own <- identical(.attr(mk, "w:id"), bkm$id)
        keep <- if (own) {
          isTRUE(keep_bookmark) && ((.name(mk) == "w:bookmarkStart" && k == 1) || (.name(mk) == "w:bookmarkEnd" && k == n))
        } else {
          k == 1
        }
        if (!keep) xml2::xml_remove(mk)
      }
      for (p in .find_all(cp, "descendant-or-self::w:p")) {
        for (col in columns) {
          tag <- paste0(tags[1], col, tags[2])
          .replace_in_paragraph(p, tag, NULL, .value_text(data[[col]][k]), FALSE, store = store)
        }
      }
    }
  }
  for (el in template) xml2::xml_remove(el)
  .apply_tokens(doc, store)
  for (m in list(bkm$start, bkm$end)) {
    if (!isTRUE(keep_bookmark) && .attached(m)) xml2::xml_remove(m)
  }
  }, NULL)
  invisible(doc)
}

# Is the node still attached to a part of the document (and not in a removed subtree)?
.attached <- function(n) {
  .count(n, paste0("count(ancestor::w:document | ancestor::w:hdr | ancestor::w:ftr | ",
                   "ancestor::w:footnotes | ancestor::w:endnotes)")) > 0
}

#' Mail merge
#'
#' Produces one document per row of `data` from a template, filling the
#' bookmarks and tags with [doc_fill()] (one column per name).
#'
#' @param template Path of a `.docx` file or WRite document object (unchanged).
#' @param data `data.frame`: one document per row.
#' @param dir Output folder (created if needed).
#' @param file_name File name template: `{column}` is replaced by the value of
#'   the row and `{.row}` by the row number. Characters not allowed in file
#'   names are replaced by `_`.
#' @param process Optional function `function(doc, row)` applied to each
#'   document after filling (`row`: list of the values of the row), for
#'   additional processing (conditional blocks, images...).
#' @param tags,bookmarks See [doc_fill()].
#' @return The vector of the paths of the files created, invisibly.
#' @examples
#' dir <- file.path(tempdir(), "letters")
#' template <- doc_new() |>
#'   doc_add_paragraph("Hello {{name}},") |>
#'   doc_add_paragraph("Your balance is {{amount}} euros.") |>
#'   doc_add_paragraph("Offer reserved for loyal customers.", bookmark = "offer")
#' customers <- data.frame(name = c("Smith", "Jones"), amount = c("1,500", "2,300"),
#'                         loyal = c(TRUE, FALSE))
#'
#' # Expected warning: the "loyal" column matches no tag
#' # (it is only used by `process`).
#' files <- doc_mail_merge(
#'   template, customers, dir,
#'   file_name = "letter_{.row}_{name}.docx",
#'   process = function(doc, row) bkm_conditional(doc, "offer", row$loyal),
#'   bookmarks = FALSE)   # "offer" is not to be filled
#' basename(files)
#' doc_text(doc_read(files[1]))
#' doc_text(doc_read(files[2]))   # without the offer
#' @export
doc_mail_merge <- function(template, data, dir, file_name = "document_{.row}.docx",
                           process = NULL, tags = c("{{", "}}"), bookmarks = TRUE) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  if (!is.null(process) && !is.function(process)) stop("`process` must be a function.", call. = FALSE)
  if (is.character(template)) template <- doc_read(template)
  .check_doc(template)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  names_out <- vapply(seq_len(nrow(data)), function(i) .file_name(file_name, data[i, , drop = FALSE], i), "")
  if (anyDuplicated(names_out)) stop("The `file_name` template produces duplicate file names.", call. = FALSE)
  paths <- file.path(dir, names_out)
  missing <- NULL
  for (i in seq_len(nrow(data))) {
    row <- lapply(data[i, , drop = FALSE], function(x) if (is.factor(x)) as.character(x) else x)
    doc <- doc_copy(template)
    report <- .fill(doc, row, tags, bookmarks)
    a <- report$name[!report$bookmark & report$tags == 0]
    missing <- if (is.null(missing)) a else intersect(missing, a)
    if (!is.null(process)) process(doc, row)
    doc_save(doc, paths[i])
  }
  if (length(missing) > 0) {
    warning(sprintf("Neither bookmark nor tag found in the template for: %s", paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(paths)
}

.file_name <- function(template, row, i) {
  name <- gsub("{.row}", i, template, fixed = TRUE)
  for (col in names(row)) name <- gsub(paste0("{", col, "}"), .value_text(row[[col]][1]), name, fixed = TRUE)
  gsub('[<>:"/\\\\|?*[:cntrl:]]', "_", name)
}

# Block of whole elements (paragraphs, tables or table rows) covered by a bookmark.
# `aligned` tells whether the bookmark starts and ends exactly at the limits of these elements.
.bookmark_block <- function(bkm) {
  s <- bkm$start
  e <- bkm$end
  not_markers <- "[not(self::w:bookmarkStart or self::w:bookmarkEnd or self::w:proofErr)]"
  nothing_before <- function(m) .count(m, sprintf("count(preceding-sibling::*%s[not(self::w:pPr)])", not_markers)) == 0
  nothing_after <- function(m) .count(m, sprintf("count(following-sibling::*%s)", not_markers)) == 0

  tr_s <- .find1(s, "ancestor::w:tr[1]")
  if (!.is_missing(tr_s)) {
    tbl <- xml2::xml_parent(tr_s)
    tr_e <- .find1(e, "ancestor::w:tr[1]")
    if (!.is_missing(tr_e) && identical(xml2::xml_parent(tr_e), tbl)) {
      f <- tr_e
    } else if (.count(tbl, "count(following::*)") > .count(e, "count(following::*)")) {
      f <- .find1(tbl, "w:tr[last()]")
    } else {
      return(NULL)
    }
    return(list(elements = .siblings_between(tr_s, f, tbl), aligned = TRUE))
  }

  p_s <- .paragraph_of(s)
  p_e <- .paragraph_of(e)
  aligned <- TRUE
  if (is.null(p_s)) {
    d <- .find1(s, sprintf("following-sibling::*%s[1]", not_markers))
  } else if (!identical(p_s, p_e) && nothing_after(s)) {
    d <- .find1(p_s, sprintf("following-sibling::*%s[1]", not_markers))
  } else {
    d <- p_s
    aligned <- nothing_before(s)
  }
  if (is.null(p_e)) {
    f <- .find1(e, sprintf("preceding-sibling::*%s[1]", not_markers))
  } else if (!identical(p_s, p_e) && nothing_before(e)) {
    f <- .find1(p_e, sprintf("preceding-sibling::*%s[1]", not_markers))
  } else {
    f <- p_e
    aligned <- aligned && nothing_after(e)
  }
  if (.is_missing(d) || .is_missing(f) || !identical(xml2::xml_parent(d), xml2::xml_parent(f)) || .index(d) > .index(f)) {
    return(NULL)
  }
  list(elements = .siblings_between(d, f, xml2::xml_parent(d)), aligned = aligned)
}

.siblings_between <- function(d, f, parent) {
  children <- xml2::xml_children(parent)
  i <- which(vapply(children, identical, TRUE, d))
  j <- which(vapply(children, identical, TRUE, f))
  as.list(children[i:j])
}

# Removes whole elements while preserving the markers of the other bookmarks
# (ends before the block, starts and inner bookmarks after); removes the table
# if no row is left and guarantees a paragraph in an emptied cell.
.remove_elements <- function(elements, id) {
  parent <- xml2::xml_parent(elements[[1]])
  if (.name(parent) == "w:tbl" && length(elements) == .count(parent, "count(w:tr)")) {
    elements <- list(parent)
    parent <- xml2::xml_parent(parent)
  }
  ends <- list()
  starts <- list()
  for (el in elements) {
    for (m in .find_all(el, "descendant-or-self::w:bookmarkStart | descendant-or-self::w:bookmarkEnd")) {
      if (identical(.attr(m, "w:id"), id)) next
      if (.name(m) == "w:bookmarkEnd") ends[[length(ends) + 1]] <- m else starts[[length(starts) + 1]] <- m
    }
  }
  start_ids <- vapply(starts, .attr, "", name = "w:id")
  inner <- vapply(ends, .attr, "", name = "w:id") %in% start_ids
  outer_ends <- ends[!inner]
  after <- c(starts, ends[inner])
  after <- after[order(vapply(after, .index, 0))]
  first <- elements[[1]]
  last <- elements[[length(elements)]]
  for (m in outer_ends) .move(m, first, "before")
  ref <- last
  for (m in after) {
    .move(m, ref, "after")
    ref <- m
  }
  for (el in elements) xml2::xml_remove(el)
  if (.name(parent) == "w:tc" && .count(parent, "count(w:p)") == 0) .add(parent, "w:p")
  invisible()
}
