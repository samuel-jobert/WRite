#' WRite: manipulating Word documents
#'
#' WRite opens, creates, transforms and saves Word documents (`.docx`) by
#' directly editing their XML, without Word.
#'
#' @section Function families:
#' * **Documents**: [doc_read()], [doc_new()], [doc_save()],
#'   [doc_copy()], [doc_text()], [doc_add_paragraph()],
#'   [doc_add_page_break()], [doc_clean()].
#' * **Bookmarks**: [bkm_list()], [bkm_text()], [bkm_delete()],
#'   [bkm_replace_text()], [bkm_insert_text()],
#'   [bkm_insert_image()], [bkm_insert_page_break()],
#'   [bkm_insert_docx()], [bkm_insert_doc()],
#'   [bkm_insert_flextable()].
#' * **Text**: [doc_replace_text()], [doc_replace_text_with_flextable()].
#' * **Templates**: [doc_fill()], [bkm_conditional()], [bkm_repeat()],
#'   [doc_mail_merge()].
#'
#' @section Reference semantics:
#' A WRite document is **modified in place**: each modification function
#' changes the object and returns it invisibly, which allows chaining calls
#' with `|>`. Use [doc_copy()] to work on an independent copy. The original
#' file is never modified unless [doc_save()] overwrites it.
#'
#' @section Illustration notation:
#' The "Illustration" sections of the help pages represent the document as text:
#' * one line = one paragraph;
#' * `[name: text]` = the bookmark `name` surrounding `text` (`[name: ]` = empty bookmark);
#' * `**text**` = bold text, `<page break>` = page break,
#'   `<image>` = image, `| a | b |` = table row.
#'
#' @section Example documents:
#' Three documents are shipped with the package (see [write_example()]):
#' * `"example"`: target document. Illustration:
#'   ```
#'   Start of doc
#'
#'   [my_bookmark: Middle]
#'
#'   The order amount is {{amount_2024}} euros in 2024.
#'   The order amount is [amount_2025: {{amount_2025}}] euros in 2025.
#'
#'
#'   [empty_bookmark: ]
#'
#'   End of doc
#'   ```
#' * `"to_insert"`: small document to insert (text formatted over several runs);
#' * `"complex"`: document to insert containing a title, bulleted list, image,
#'   footnote, link, table, custom style and the bookmark
#'   `source_bookmark`.
#'
#' @section AI generation:
#' The code, tests and documentation of this package were generated with the
#' assistance of an artificial intelligence: GitHub Copilot, model
#' Claude Opus 5.5, from specifications and under the supervision of the
#' maintainer, who verified that it works and remains responsible for it.
#'
#' @keywords internal
"_PACKAGE"
