#' Insert a flextable table
#'
#' `bkm_insert_flextable()` inserts a [flextable][flextable::flextable] table
#' on a bookmark; `doc_replace_text_with_flextable()` puts it in place of each
#' occurrence of a text (the paragraph is split around the table, and removed
#' if it contained only that text).
#'
#' The table is first produced in a temporary document by
#' `flextable::body_add_flextable()`, then inserted like a Word document (see
#' [bkm_insert_docx()]): formatting, images and links of the table are kept.
#' The **flextable** and **officer** packages must be installed.
#'
#' @param doc WRite document object.
#' @param name Bookmark name.
#' @param ft A `flextable` object.
#' @param position `"replace"` (replaces the content of the bookmark),
#'   `"before"` (at the start of the bookmark) or `"after"` (at the end of the
#'   bookmark).
#' @param keep_bookmark Keep the bookmark around the table?
#' @return `doc`, invisibly.
#' @section Illustration:
#' ```
#' Before (bookmark): [table: ]
#' After            : [table: | mpg | cyl | disp |
#'                            | 21  | 6   | 160  | ...]
#'
#' Before (text)    : Here are the results [TABLE] comment.
#' After            : Here are the results
#'                    | mpg | cyl | disp |
#'                    |  comment.
#' ```
#' @examples
#' if (requireNamespace("flextable", quietly = TRUE)) {
#'   ft <- flextable::flextable(head(mtcars[, 1:4], 3))
#'   ft <- flextable::theme_vanilla(ft)
#'
#'   # On a bookmark (empty paragraph replaced by the table)
#'   doc <- doc_read(write_example("example"))
#'   bkm_insert_flextable(doc, "empty_bookmark", ft)
#'   bkm_text(doc, "empty_bookmark")
#'
#'   # In place of a text: the paragraph is split around the table
#'   doc_replace_text_with_flextable(doc, "{{amount_2024}}", ft)
#'   doc_text(doc)[3:4]
#'
#'   # Template containing a tag alone in its paragraph
#'   doc2 <- doc_new() |>
#'     doc_add_paragraph("Results:") |>
#'     doc_add_paragraph("[TABLE]") |>
#'     doc_add_paragraph("End")
#'   doc_replace_text_with_flextable(doc2, "[TABLE]", ft)
#'   doc_save(doc2, tempfile(fileext = ".docx"))
#' }
#' @export
bkm_insert_flextable <- function(doc, name, ft, position = c("replace", "before", "after"),
                                 keep_bookmark = TRUE) {
  .check_doc(doc)
  position <- match.arg(position)
  source <- .flextable_source(ft)
  .bkm_guard(.insert_source(doc, .bkm_find(doc, name), source, position, keep_bookmark,
                            blocks = .flextable_blocks(source)), NULL)
  invisible(doc)
}

#' @rdname bkm_insert_flextable
#' @param text Text to replace with the table (literal search, case-sensitive).
#' @export
doc_replace_text_with_flextable <- function(doc, text, ft) {
  .check_doc(doc)
  if (!is.character(text) || length(text) != 1 || !nzchar(text)) stop("`text` must be a non-empty string.", call. = FALSE)
  source <- .flextable_source(ft)
  token <- "\uE000WRite-table\uE000"
  n <- .replace_everywhere(doc, text, token)
  if (n == 0) {
    warning(sprintf("Text '%s' not found in the document. Operation ignored.", text), call. = FALSE)
    return(invisible(doc))
  }
  name <- "_WRite_table_tmp"
  while (.token_to_bookmark(doc, token, name)) {
    .insert_source(doc, .bkm_find(doc, name), source, "replace", keep_bookmark = FALSE,
                   blocks = .flextable_blocks(source))
  }
  attr(doc, "replacements") <- n
  invisible(doc)
}

.flextable_source <- function(ft) {
  for (p in c("flextable", "officer")) {
    if (!requireNamespace(p, quietly = TRUE)) stop(sprintf("Package '%s' is required.", p), call. = FALSE)
  }
  if (!inherits(ft, "flextable")) stop("`ft` must be a flextable object.", call. = FALSE)
  f <- tempfile(fileext = ".docx")
  on.exit(unlink(f))
  d <- flextable::body_add_flextable(officer::read_docx(), ft)
  print(d, target = f)
  doc_read(f)
}

# Blocks to insert: table and optional caption, without the empty paragraphs of the officer template.
.flextable_blocks <- function(source) {
  blocks <- .find_all(.xml(source, source$main), "/w:document/w:body/*[not(self::w:sectPr)]")
  Filter(function(b) .name(b) != "w:p" || !.is_empty_paragraph(b), blocks)
}
