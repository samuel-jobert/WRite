# WRite 0.2.0

* First released version.
* Documents: `doc_read()`, `doc_new()`, `doc_save()`, `doc_copy()`,
  `doc_text()`, `doc_add_paragraph()`, `doc_add_page_break()`.
* Bookmarks: `bkm_list()`, `bkm_text()`, `bkm_delete()`,
  `bkm_replace_text()`, `bkm_insert_text()`, `bkm_insert_image()`,
  `bkm_insert_page_break()`, `bkm_insert_docx()`, `bkm_insert_doc()`,
  `bkm_insert_flextable()`.
* Text: `doc_replace_text()`, `doc_replace_text_with_flextable()`.
* Templates: `doc_fill()`, `bkm_conditional()`, `bkm_repeat()`,
  `doc_mail_merge()`.
* Cleaning: `doc_clean()` (duplicate bookmark identifiers, merging of runs).
* `doc_add_paragraph()`: `"\n\n"` creates several paragraphs, `html = TRUE`
  interprets basic HTML tags, and `at_text` / `where` / `occurrence` insert the
  paragraph before, after or in place of a text of the document.
* `html = TRUE` is also available in `bkm_insert_text()`, `bkm_replace_text()` (the
  formatting of the replaced text is kept) and `doc_replace_text()`.
* `doc_replace_text()` accepts vectors of texts and replacements (applied in order,
  in a single pass over the document).
* `doc_clean()`: new options `spelling`, `comments`, `revisions` and `all`.
* `bkm_delete()`: `delete_content` is now `TRUE` by default; new `delete_bookmark`,
  `whole_document` and `delete_row` (table rows, merged cells) options; `name` can be a vector.
  New `delete_paragraph` option (`TRUE` by default): the whole paragraph(s) containing the
  bookmark are deleted, markers included; `delete_paragraph = FALSE` only deletes the content.
* `doc_add_style()`: creates a Word style (paragraph or character) usable by name
  in the `style` argument of the insertion functions; `bkm_replace_text()`
  gets a `style` argument.
* The `bkm_*` functions now warn ("Operation ignored") instead of failing when
  the bookmark is missing or the operation is impossible; so does
  `doc_replace_text_with_flextable()` when the text is not found.
* Package logo (`man/figures/logo.png`).
* The `magrittr` `%>%` operator is re-exported (the native pipe `|>` works too).
* Performance: the content of a bookmark is located by walking from its start
  to its end (cost proportional to the bookmark, no longer to the document), and
  the insertion functions, `doc_fill()` and `doc_add_paragraph(bookmark = )` no
  longer compute the text of every bookmark of the document. `bkm_replace_text()`,
  `bkm_text()`, `bkm_list()`, `doc_fill()` and the `bkm_insert_*()` functions are
  much faster on large documents.
* Performance: `doc_fill()` (and `doc_mail_merge()`) replace all the tags in a
  single pass over the document, and the list of the parts that can contain
  bookmarks (body, headers, footers, notes) is cached instead of being
  recomputed at each bookmark or text search.
* `doc_save()` writes only file entries in the archive (no directory entries),
  like Word.
* `bkm_delete()` with several names locates all the bookmarks in a single
  search (faster than successive calls, same result, including for overlapping
  or nested bookmarks).
* `bkm_replace_text()` accepts a vector of names (and one text or a vector of
  texts): the bookmarks are located in a single search and filled in order.
  `doc_fill()` uses it to fill all its bookmarks at once.
