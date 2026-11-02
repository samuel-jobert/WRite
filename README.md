# WRite <img src="man/figures/logo.png" align="right" height="139" alt="WRite logo" />

R package to manipulate Word documents (`.docx`): open or create a document,
manage bookmarks (delete, replace text, insert formatted text, images,
flextable tables or another Word document), replace text throughout the
document, clean it up and save it.

The package works directly on the document XML (Office Open XML format) with
`xml2` and `zip`. It does not depend on Word; `flextable` and `officer` are
only needed to insert flextable tables.

WRite is an independent project, not affiliated with or endorsed by Microsoft or
 the R Foundation. Word is a trademark of Microsoft Corporation.

## Installation

From the project folder:

```r
# In R
install.packages(c("xml2", "zip"))      # dependencies
install.packages("path/to/WRite", repos = NULL, type = "source")
```

or from the command line:

```
R CMD INSTALL path/to/WRite
```

## Help and examples

Every function has a help page with several runnable examples and, for
transformation functions, an "Illustration" section showing the document
before / after:

```r
?WRite                      # overview, notation used in the illustrations
?bkm_insert_text            # help of one function
example(bkm_repeat)         # run the examples of one function
```

The examples use the documents shipped with the package
(`write_example("example")`, `"to_insert"`, `"complex"`) and `doc_text()` to
display the result.

## Principle

```r
library(WRite)

doc <- doc_read("template.docx")          # the original file is never modified
bkm_list(doc)                              # name, id, part, text of each bookmark

doc |>
  bkm_replace_text("amount", "1,234,567") |>
  bkm_insert_text("title", "Annual report", bold = TRUE, size = 16) |>
  bkm_insert_docx("appendix", "appendix.docx") |>
  doc_clean() |>
  doc_save("result.docx")
```

### Reference semantics

A WRite document (class `write_doc`) is an object **modified in place**: each
modifying function changes the object and returns it invisibly, so calls can be
chained with `|>` or `%>%` (the latter is re-exported by WRite: no need to load
magrittr). After `bkm_delete(doc, "x")`, `doc` is already modified even without
reassignment. To work on an independent copy, use `doc_copy()`.

## Functions

### Documents

| Function | Purpose |
|---|---|
| `doc_read(path)` | Opens a `.docx` file. |
| `doc_new()` | Creates a blank document (A4, styles `Normal`, `Heading1` to `Heading3`). |
| `doc_add_paragraph(doc, text, ..., style, bookmark, keep_with_previous, keep_lines_together, html, at_text, where, occurrence)` | Adds a paragraph at the end of the document (or next to a text), optionally wrapped in a bookmark. |
| `doc_add_page_break(doc)` | Appends a page break at the end of the document. |
| `doc_copy(doc)` | Independent copy of a document. |
| `doc_text(doc, keep_empty = FALSE)` | Text of each paragraph (quick check of a result). |
| `write_example(name)` | Path of an example document shipped with the package. |
| `doc_save(doc, path)` | Writes the document (an existing file is overwritten). |
| `doc_add_style(doc, name, font, size, bold, italic, underline, strike, color, align, spacing_before, spacing_after, line_spacing, indent_left, indent_right, indent_first_line, keep_with_next, keep_lines_together, bullet, bullet_font, bullet_indent, bullet_spacing, type, based_on, overwrite)` | Creates a real Word style (units: points, cm, line spacing as a multiple), usable with `style = "name"` in `doc_add_paragraph()`, `bkm_insert_text()` and `bkm_replace_text()`. `bullet` makes a bulleted list style (`"disc"`, `"circle"`, `"square"`, `"dash"`, `"arrow"`, `"check"` or any character), with `bullet_indent` (position of the bullet) and `bullet_spacing` (distance to the text), in cm. Word has no "keep with previous" in a style: use `keep_with_previous` of the insertion functions. |
| `doc_clean(doc, bookmark_ids = TRUE, merge_runs = TRUE, spelling = FALSE, comments = FALSE, revisions = FALSE, all = FALSE)` | Cleaning (see below). |
| `doc_replace_text(doc, text, replacement, regex, ignore_case, html)` | Replaces one or several texts throughout the document (see below). |
| `doc_replace_text_with_flextable(doc, text, ft)` | Replaces each occurrence of a text with a flextable. |
| `doc_fill(doc, values, tags, bookmarks, warn)` | Fills bookmarks and `{{name}}` tags from a named list. |
| `doc_mail_merge(template, data, dir, file_name, process, ...)` | Produces one document per row of a `data.frame`. |

### Bookmarks

| Function | Purpose |
|---|---|
| `bkm_list(doc)` | `data.frame` of bookmarks: `name`, `id`, `part`, `text`. |
| `bkm_text(doc, name)` | Text contained in a bookmark. |
| `bkm_delete(doc, name, delete_content = TRUE, delete_bookmark = FALSE, delete_paragraph = TRUE, whole_document = FALSE, delete_row = FALSE)` | Deletes the paragraph(s) containing one or several bookmarks (or only their content with `delete_paragraph = FALSE`), optionally the bookmark itself (or all the bookmarks of the document), or the table row containing it. |
| `bkm_replace_text(doc, name, text, keep_bookmark = TRUE, html = FALSE, style)` | Replaces the content while keeping the existing formatting. |
| `bkm_insert_text(doc, name, text, position, bold, italic, underline, size, font, style, keep_bookmark, keep_with_previous, keep_lines_together, html)` | Inserts formatted text. |
| `bkm_insert_page_break(doc, name, position, keep_bookmark)` | Inserts a page break. |
| `bkm_insert_docx(doc, name, file, position, keep_bookmark)` | Inserts the content of a `.docx` file. |
| `bkm_insert_doc(doc, name, source, position, keep_bookmark)` | Inserts the content of a WRite document object. |
| `bkm_insert_image(doc, name, image, width, height, position, alt_text, keep_bookmark)` | Inserts an image (replaces the bookmark content by default). |
| `bkm_insert_flextable(doc, name, ft, position, keep_bookmark)` | Inserts a flextable. |
| `bkm_conditional(doc, name, condition, keep_bookmark)` | Keeps or removes a block depending on a condition. |
| `bkm_repeat(doc, name, data, tags, keep_bookmark)` | Repeats a block (paragraphs or table rows) for each row of a `data.frame`. |

Bookmarks are searched in the body, headers, footers and footnotes / endnotes.

#### Missing bookmark or impossible operation

The `bkm_*` functions do not stop when a bookmark does not exist or when the operation is
 impossible (e.g. a bookmark spanning several table rows): they issue a warning ending in
 `Operation ignored.` (`Bookmark ignored.` for `bkm_delete()`) and leave the document unchanged;
 `bkm_text()` returns `NA`. Invalid arguments still raise an error.

#### Insertion position

`position` is one of:

* `"replace"` (default): the bookmark content is replaced;
* `"before"`: insertion at the start of the bookmark, before its content;
* `"after"`: insertion at the end of the bookmark, after its content.

The inserted content is always **inside** the bookmark (like `InsertBefore` /
`InsertAfter` in VBA). With `keep_bookmark = FALSE`, the bookmark is removed
after the insertion.

#### Formatting of inserted text

* `bold`, `italic`: `TRUE` / `FALSE` (`NULL` = inherited from the style);
* `underline`: `TRUE`, `FALSE` or a Word type (`"double"`, `"dotted"`, `"wave"`...);
* `size`: size in points (e.g. `11.5`);
* `font`: font name (e.g. `"Arial"`);
* `style`: predefined style of the document, by identifier (`"Heading1"`) or by
  name (`"heading 1"`). A **character** style applies to the inserted text, a
  **paragraph** style to the paragraph that contains the text.

#### Special characters in texts

All the functions that insert or replace text (`doc_add_paragraph()`,
`bkm_insert_text()`, `bkm_replace_text()`, `doc_replace_text()`, `doc_fill()`,
`bkm_repeat()`, `doc_mail_merge()`) interpret the texts in the same way:

| Character | Result |
|---|---|
| `"\n"` (or `"\v"`) | line break within the paragraph (Shift+Enter in Word); `"\n\n"` = two line breaks |
| `"\r"` | new paragraph, with the properties (style, indentation, alignment...) of the paragraph where the text is inserted |
| `"\t"` | tab |
| `"\f"` | page break |
| `"\r\n"` | line break (Windows line end, e.g. text typed in a form) |

With `html = TRUE`, `<br>` is a line break and `<p>...</p>` delimit
paragraphs. Only the control characters are interpreted: the two characters
backslash and `n` (written `"\\n"` in R) stay as they are. `bkm_text()` and
`bkm_list()` return texts with the same conventions (paragraphs separated by
`"\r"`), so they can be given back to `bkm_replace_text()`.

#### Chaining paragraphs

`bkm_insert_text()` and `doc_add_paragraph()` accept:

* `keep_with_previous = TRUE`: the previous paragraph stays on the same page as
  the inserted paragraph (Word option "Keep with next" set on the previous
  paragraph; if it is a table, on its last row). On a page break, the previous
  paragraph therefore moves to the next page too;
* `keep_lines_together = TRUE`: all the lines of the paragraph stay on the same
  page (Word option "Keep lines together").

With `bkm_insert_text()`, these options apply to the paragraph that contains
the bookmark.

#### Rich text and position of `doc_add_paragraph()`

* In `text`, `"\n"` is a line break and `"\r"` starts a new paragraph (see
  "Special characters in texts" above).
* `html = TRUE` interprets `<b>`/`<strong>`, `<i>`/`<em>`, `<u>`, `<s>`/`<strike>`/`<del>`
  (nestable), `<br>` and `<p>`; other tags are kept as text.
* `at_text = "..."` inserts next to a text of the document: `where = "after"`
  (default), `"before"` or `"replace"` (the paragraph replaces the text; the
  paragraph containing it is split if it has other text). If the text is absent,
  a warning is issued; if present several times, the first occurrence is used
  (with a warning) unless `occurrence = "all"`.

#### Page break

`bkm_insert_page_break()` inserts a page break on a bookmark (default
`position = "replace"`, like the other insertions); `doc_add_page_break()`
appends one at the end of the document. In `bkm_list()` / `bkm_text()`, a page
break appears as `"\f"`.

`bkm_replace_text()` has no formatting parameters: it reuses the formatting of
the first text of the bookmark (or of the text preceding an empty bookmark).
With `html = TRUE`, the HTML tags of the text (`<b>`, `<i>`, `<u>`, `<s>`, `<br>`...)
add their formatting to it. `html = TRUE` is also available in `bkm_insert_text()` and `doc_replace_text()`.

#### Deleting content and neighbouring bookmarks

By default, `bkm_delete()` removes the whole paragraph(s) containing the
bookmark (from the paragraph of its start to the paragraph of its end, markers
included), without leaving an empty paragraph. With `delete_paragraph = FALSE`,
only the content between the markers is removed.

When the content of a bookmark is removed (`bkm_delete()`, or
`position = "replace"`), the markers of the
**other** bookmarks present in that content are kept:

* the end of a bookmark that starts earlier is moved just before the start of
  the processed bookmark;
* the start of a bookmark that ends later is moved just after its end;
* a bookmark entirely contained is kept, empty.

In a table, `delete_row = TRUE` deletes the row containing the bookmark. If the
bookmark is in a vertically merged cell, all the rows of the merged cell are
deleted; if only other cells of the row are merged, just this row is deleted
and the merged value is kept (moved to the next row of the merged cell).

If the bookmark spans several paragraphs, they are all deleted; with
`delete_paragraph = FALSE` (or `position = "replace"`), they are merged (as
when deleting in Word).

#### Inserting a Word document

The body content of the source document is actually merged into the target
document:

* if the bookmark's paragraph only contains the bookmark, it is replaced by the
  inserted content; otherwise it is split in two around that content;
* **styles**: used styles missing from the target are imported; a style
  existing in the target (same identifier or same name) is kept as is;
* **numbered / bulleted lists**: definitions copied with new identifiers;
* **images, hyperlinks** and other related parts: copied;
* **footnotes and endnotes**: copied and renumbered;
* **bookmarks** of the inserted document: renumbered. A bookmark whose name
  already exists in the target is renamed with a suffix (`name_1`, then
  `name_2` if `name_1` also exists...) and a **warning** lists the renamings
  (`name -> name_1`). References to that bookmark in the inserted content
  (internal links, `REF` / `PAGEREF` fields) are updated. The `_GoBack`
  bookmark (Word cursor position) of the inserted document is removed if it
  already exists in the target;
* section breaks and comments of the inserted document are ignored; headers
  and footers of the source document are not imported.

The source document is not modified. A document can be inserted into itself.

#### Inserting an image

`bkm_insert_image()` accepts PNG, JPEG, GIF and BMP. The image is inserted
inline. Dimensions are in **centimetres**:

* `width` and `height` provided: imposed size;
* only one provided: the other is computed to keep the proportions;
* none: native size of the image (96 dots per inch).

#### Flextable tables

The table is produced by `flextable::body_add_flextable()` in a temporary
document, then inserted like a Word document: its formatting is preserved. With
`doc_replace_text_with_flextable()`, the paragraph containing the text is split
around the table (and removed if it only contained that text); all occurrences
are replaced and an error is raised if the text is absent.

### Text replacement

`doc_replace_text(doc, text, replacement, regex = FALSE, ignore_case = FALSE, html = FALSE)`
replaces all occurrences in the body, headers, footers and notes. 	ext can be a
vector (with one replacement or one per text): the replacements are applied in
order, in a single pass over the document.

* on the first call on an object, runs with identical formatting are merged
  (only once per object, even if the function is called again; also
  unnecessary if `doc_clean()` has already been called);
* the search is then done on the text of each paragraph: an occurrence spread
  over runs with different formatting is found, and the replacement takes the
  formatting of the start of the occurrence;
* bookmarks located within an occurrence are kept;
* `regex = TRUE`: PCRE regular expression, `replacement` may contain `\\1`,
  `\\2`...;
* the number of replacements is in `attr(doc, "replacements")`.

An occurrence cannot span two paragraphs.

### Document templates

#### `doc_fill()`

Fills a template in one go from a named list (or a one-row `data.frame`). For
each `name = value`: the bookmark `name` is filled if it exists (formatting
kept) and all the `{{name}}` tags are replaced. A warning lists the names with
neither bookmark nor tag; the details are in `attr(doc, "fill_report")`.
Delimiters can be changed with `tags = c("[", "]")`; `NA` gives an empty text.

#### `bkm_conditional()`

`bkm_conditional(doc, "special_clause", client$type == "pro")`: if the
condition is false, the bookmark and its content are removed. When the bookmark
covers whole paragraphs or table rows, they disappear completely (no leftover
empty paragraph; a table without rows is removed). If the condition is true,
the content is kept.

#### `bkm_repeat()`

The bookmark content is used as a template, duplicated for each row of `data`;
in each copy, the `{{column}}` tags are replaced by the values of the row.

* Bookmark in a table: the template is the **table row(s)** covered by the
  bookmark. Typical case: fixed header, template row `{{product}} | {{price}}`,
  fixed total row.
* Otherwise: the template is made of the **whole paragraphs** covered.
* The bookmark then wraps all the copies (`keep_bookmark = TRUE`).
* With no data row, the template is removed.

```r
rows <- data.frame(product = c("Apples", "Pears"), price = c("2.50", "3.10"))
doc_read("quote_template.docx") |>
  bkm_repeat("product_row", rows) |>
  doc_fill(list(client = "Martin", total = "5.60")) |>
  doc_save("quote_martin.docx")
```

#### `doc_mail_merge()`

Produces one document per row of a `data.frame` (one column per bookmark or
tag). `file_name` accepts `{column}` and `{.row}`; `process` is a
`function(doc, row)` applied to each document for extra steps. The template is
not modified.

```r
clients <- data.frame(name = c("Smith", "Martin"), amount = c("1,500", "2,300"),
                      pro = c(TRUE, FALSE))
doc_mail_merge("letter_template.docx", clients, "output",
               file_name = "letter_{name}.docx",
               process = function(doc, row) bkm_conditional(doc, "pro_clause", row$pro))
```

### Cleaning

`doc_clean()`:

* **`bookmark_ids`**: if different bookmarks share the same identifier
  (`w:id`), duplicates get a new unique identifier. The first bookmark keeps
  its own; starts and ends are paired in document order;
* **`merge_runs`**: merges adjacent "runs" with identical formatting (a text
  split into several pieces by Word is put back in one piece). Spell-check
  marks (`w:proofErr`) are removed. Runs containing fields, images, note
  references... are not merged;
* **`spelling`**: removes the spelling and grammar error marks;
* **`comments`**: removes the comments;
* **`revisions`**: accepts all the tracked changes;
* **`all`**: enables every option not explicitly given, e.g.
  `doc_clean(doc, all = TRUE, comments = FALSE)` cleans everything except the
  comments.

The number of fixes is available in `attr(doc, "cleaning")`.

## Examples

### Create a document from scratch

```r
doc <- doc_new() |>
  doc_add_paragraph("Meeting minutes", style = "heading 1") |>
  doc_add_paragraph("Date: ", bookmark = "date") |>
  doc_add_paragraph("", bookmark = "body")

doc |>
  bkm_insert_text("date", format(Sys.Date(), "%Y-%m-%d"), position = "after", bold = TRUE) |>
  bkm_insert_docx("body", "content.docx") |>
  doc_save("minutes.docx")
```

### Fill a template with several values

```r
values <- list(name = "Smith", city = "Lyon", amount = "12,500")
doc <- doc_read("template.docx")
for (n in names(values)) bkm_replace_text(doc, n, values[[n]])
doc_save(doc, "letter_smith.docx")
```

### Insert an already opened document

```r
appendix <- doc_read("appendix.docx") |> bkm_replace_text("ref", "A-12")
doc_read("report.docx") |>
  bkm_insert_doc("appendices", appendix) |>
  doc_save("full_report.docx")
```

### Fill a template by text replacement, image and table

```r
library(flextable)
ft <- flextable(head(mtcars[, 1:4])) |> theme_vanilla() |> autofit()

doc_read("template.docx") |>
  doc_replace_text("{{client}}", "Martin Ltd") |>
  doc_replace_text("\\{\\{date\\}\\}", format(Sys.Date(), "%Y-%m-%d"), regex = TRUE) |>
  doc_replace_text_with_flextable("[SALES_TABLE]", ft) |>
  bkm_insert_image("logo", "logo.png", width = 4) |>
  doc_save("report_martin.docx")
```

## Known limitations

* A bookmark that starts in a table cell and ends elsewhere cannot be emptied
  (explicit error).
* Inserting a document requires the bookmark to be directly in a paragraph
  (not in a hyperlink, a field or a content control).
* Comments of the inserted document are dropped.
* Headers / footers and section settings of the inserted document are not
  imported.

## Tests

```r
testthat::test_local("path/to/WRite")
```

Test documents are in `tests/word/`:

* `example.docx`: target document with three bookmarks;
* `to_insert.docx`: document to insert;
* `complex.docx`: document to insert containing a title, a custom style, a
  bulleted list, an image, a footnote, a hyperlink, a table and a bookmark
  (generated by `dev-tools/create_complex_source.ps1`).

The script `dev-tools/check_word.ps1` opens `.docx` files in Word (invisible,
read-only) to check that they are not corrupted:

```
powershell -ExecutionPolicy Bypass -File dev-tools/check_word.ps1 -Files "a.docx,b.docx"
```

## Acknowledgements

Many thanks to [David Gohel](https://github.com/davidgohel) for his fantastic
[officer](https://davidgohel.github.io/officer/) package, which was a great
source of inspiration for WRite, and for his excellent
[flextable](https://davidgohel.github.io/flextable/) package, which WRite uses
to insert tables.
