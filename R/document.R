#' Open a Word document
#'
#' Reads a `.docx` file and returns a WRite document object. The original
#' file is never modified: the content is extracted into a temporary folder
#' and is only written back by [doc_save()].
#'
#' The object has **reference semantics**: modification functions
#' (`bkm_*()`, `doc_clean()`...) modify the object in place and return it
#' invisibly, which allows chaining them with `|>`. Use [doc_copy()] to get an
#' independent copy.
#'
#' @param path Path of the `.docx` file.
#' @return An object of class `write_doc`.
#' @seealso [doc_new()], [doc_save()], [write_example()]
#' @examples
#' # Open a document and look at its content
#' doc <- doc_read(write_example("example"))
#' doc                       # summary: source, paragraphs, bookmarks
#' doc_text(doc)             # text of the non-empty paragraphs
#' bkm_list(doc)             # bookmarks and their content
#'
#' # The original file is not modified: save under another name
#' doc |>
#'   bkm_replace_text("my_bookmark", "New middle") |>
#'   doc_save(tempfile(fileext = ".docx"))
#' @export
doc_read <- function(path) {
  if (!is.character(path) || length(path) != 1 || !file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }
  dir <- tempfile("write_")
  dir.create(dir)
  tryCatch(zip::unzip(path, exdir = dir), error = function(e) {
    unlink(dir, recursive = TRUE)
    stop("Unable to read the file (is it a valid .docx?): ", conditionMessage(e), call. = FALSE)
  })
  .create_doc(dir, normalizePath(path))
}

#' Create a new blank Word document
#'
#' Creates an empty document (A4 format, styles `Normal`, `Heading1` to
#' `Heading3`). Content can then be added with [doc_add_paragraph()].
#'
#' @return An object of class `write_doc`.
#' @examples
#' # Document built from scratch
#' doc <- doc_new() |>
#'   doc_add_paragraph("Meeting minutes", style = "heading 1") |>
#'   doc_add_paragraph("Date: ", bookmark = "date") |>
#'   doc_add_paragraph("Attendees: ", bookmark = "attendees")
#' doc_text(doc)
#'
#' # The blank document can then be used as a template
#' doc |>
#'   bkm_insert_text("date", "12/03/2026", position = "after", bold = TRUE) |>
#'   bkm_insert_text("attendees", "A. Martin, B. Durand", position = "after")
#' doc_text(doc)
#' f <- tempfile(fileext = ".docx")
#' doc_save(doc, f)
#' @export
doc_new <- function() {
  dir <- tempfile("write_")
  dir.create(dir)
  for (f in names(.blank_template)) {
    path <- file.path(dir, f)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(enc2utf8(.blank_template[[f]])), path)
  }
  .create_doc(dir, NA_character_)
}

.create_doc <- function(dir, path) {
  doc <- new.env(parent = emptyenv())
  doc$dir <- dir
  doc$path <- path
  doc$parts <- new.env(parent = emptyenv())
  class(doc) <- "write_doc"
  reg.finalizer(doc, function(e) unlink(e$dir, recursive = TRUE), onexit = TRUE)
  r <- .rels_df(doc, "")
  r <- r[r$type == .rel_type("officeDocument"), , drop = FALSE]
  if (nrow(r) == 0) stop("Main document part not found (invalid .docx file).", call. = FALSE)
  doc$main <- .absolute_target("", r$target[1])
  if (is.null(.xml(doc, doc$main))) stop("Main document part is unreadable.", call. = FALSE)
  doc
}

#' Save a Word document
#'
#' Writes the document to a `.docx` file. The document remains usable after
#' saving (you can keep modifying it and save it under another name).
#'
#' @param doc WRite document object.
#' @param path Path of the `.docx` file to write (overwritten if it exists;
#'   missing folders are created).
#' @return `doc`, invisibly.
#' @examples
#' doc <- doc_read(write_example("example"))
#' f1 <- tempfile(fileext = ".docx")
#' f2 <- tempfile(fileext = ".docx")
#'
#' # At the end of a processing chain
#' doc |> doc_fill(list(amount_2025 = "2,000,000")) |> doc_save(f1)
#'
#' # Several successive versions of the same object
#' bkm_delete(doc, "my_bookmark", delete_paragraph = FALSE)
#' doc_save(doc, f2)
#' file.exists(c(f1, f2))
#' @export
doc_save <- function(doc, path) {
  .check_doc(doc)
  .ensure_final_paragraph(doc)
  for (p in ls(doc$parts, all.names = TRUE)) {
    .write_xml(get(p, envir = doc$parts), file.path(doc$dir, p))
  }
  files <- list.files(doc$dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  files <- c("[Content_Types].xml", setdiff(files, "[Content_Types].xml"))
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path)) file.remove(path)
  zip::zip(path, files = files, root = doc$dir, mode = "mirror")
  invisible(doc)
}

#' Copy a document
#'
#' Returns an independent copy of a WRite document (changes to one do not
#' affect the other). Essential to produce several variants from a single
#' template, since modification functions act in place.
#'
#' @param doc WRite document object.
#' @return A new `write_doc` object.
#' @examples
#' template <- doc_read(write_example("example"))
#'
#' # Without a copy, `template` would be modified too
#' v1 <- doc_copy(template) |> bkm_replace_text("my_bookmark", "Version 1")
#' v2 <- doc_copy(template) |> bkm_replace_text("my_bookmark", "Version 2")
#' bkm_text(template, "my_bookmark")  # "Middle": untouched
#' bkm_text(v1, "my_bookmark")        # "Version 1"
#' bkm_text(v2, "my_bookmark")        # "Version 2"
#' @export
doc_copy <- function(doc) {
  .check_doc(doc)
  dir <- tempfile("write_")
  dir.create(dir)
  file.copy(list.files(doc$dir, full.names = TRUE, all.files = TRUE, no.. = TRUE), dir, recursive = TRUE)
  for (p in ls(doc$parts, all.names = TRUE)) {
    .write_xml(get(p, envir = doc$parts), file.path(dir, p))
  }
  .create_doc(dir, doc$path)
}

#' @export
print.write_doc <- function(x, ...) {
  cat("<WRite Word document>\n")
  cat("  Source     :", if (is.na(x$path)) "(new document)" else x$path, "\n")
  n <- .count(.xml(x, x$main), "count(/w:document/w:body//w:p)")
  cat("  Paragraphs :", n, "\n")
  s <- bkm_list(x)
  cat("  Bookmarks  :", nrow(s), if (nrow(s) > 0) paste0("(", paste(s$name, collapse = ", "), ")"), "\n")
  invisible(x)
}

#' Text of the paragraphs of a document
#'
#' Returns the text of each paragraph of the document body (including
#' paragraphs in table cells), to quickly check the result of a
#' transformation.
#'
#' @param doc WRite document object.
#' @param keep_empty Keep empty paragraphs?
#' @return A character vector, one element per paragraph. A tab gives `"\t"`,
#'   a line break `"\n"` and a page break `"\f"`.
#' @examples
#' doc <- doc_read(write_example("example"))
#' doc_text(doc)
#' doc_text(doc, keep_empty = TRUE)
#'
#' # Check before / after a transformation
#' doc_replace_text(doc, "{{amount_2024}}", "1,000,000")
#' doc_text(doc)[3]
#' @export
doc_text <- function(doc, keep_empty = FALSE) {
  .check_doc(doc)
  ps <- .find_all(.xml(doc, doc$main), "/w:document/w:body//w:p[not(ancestor::w:txbxContent)]")
  txt <- vapply(as.list(ps), function(p) .nodes_text(list(p)), "")
  if (isTRUE(keep_empty)) txt else txt[nzchar(txt)]
}

#' Path of an example document
#'
#' Returns the path of one of the Word documents shipped with the package,
#' used in the help examples (see [WRite-package] for their content).
#'
#' @param name `"example"` (target document with three bookmarks),
#'   `"to_insert"` (small document to insert) or `"complex"` (document with
#'   list, image, note, link, table and bookmark).
#' @return The path of the `.docx` file.
#' @examples
#' write_example()
#' bkm_list(doc_read(write_example("complex")))
#' @export
write_example <- function(name = c("example", "to_insert", "complex")) {
  name <- match.arg(name)
  system.file("extdata", paste0(name, ".docx"), package = "WRite", mustWork = TRUE)
}

# The body of a Word document must contain at least one paragraph.
.ensure_final_paragraph <- function(doc) {
  body <- .find1(.xml(doc, doc$main), "/w:document/w:body")
  if (.is_missing(body) || .count(body, "count(w:p)") > 0) return(invisible())
  sect <- .find1(body, "w:sectPr")
  if (.is_missing(sect)) .add(body, "w:p") else .add(sect, "w:p", sibling = "before")
  invisible()
}

.root_ns <- paste(
  sprintf('xmlns:%s="%s"', c("w", "r", "wp", "a", "m", "mc", "w14"),
          .ns[c("w", "r", "wp", "a", "m", "mc", "w14")]),
  collapse = " ")

.blank_template <- list(
  "[Content_Types].xml" = paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Types xmlns="', .ns_ct, '">',
    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
    '<Default Extension="xml" ContentType="application/xml"/>',
    '<Override PartName="/word/document.xml" ContentType="', .ct[["document"]], '"/>',
    '<Override PartName="/word/styles.xml" ContentType="', .ct[["styles"]], '"/>',
    '</Types>'),
  "_rels/.rels" = paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="', .ns_rels, '">',
    '<Relationship Id="rId1" Type="', .rel_type("officeDocument"), '" Target="word/document.xml"/>',
    '</Relationships>'),
  "word/_rels/document.xml.rels" = paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="', .ns_rels, '">',
    '<Relationship Id="rId1" Type="', .rel_type("styles"), '" Target="styles.xml"/>',
    '</Relationships>'),
  "word/document.xml" = paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<w:document ', .root_ns, ' mc:Ignorable="w14"><w:body>',
    '<w:sectPr><w:pgSz w:w="11906" w:h="16838"/>',
    '<w:pgMar w:top="1417" w:right="1417" w:bottom="1417" w:left="1417" w:header="708" w:footer="708" w:gutter="0"/>',
    '<w:cols w:space="708"/><w:docGrid w:linePitch="360"/></w:sectPr>',
    '</w:body></w:document>'),
  "word/styles.xml" = paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<w:styles xmlns:w="', .ns[["w"]], '">',
    '<w:docDefaults><w:rPrDefault><w:rPr>',
    '<w:rFonts w:ascii="Calibri" w:hAnsi="Calibri" w:eastAsia="Calibri" w:cs="Times New Roman"/>',
    '<w:sz w:val="22"/><w:szCs w:val="22"/><w:lang w:val="en-US" w:eastAsia="en-US" w:bidi="ar-SA"/>',
    '</w:rPr></w:rPrDefault><w:pPrDefault><w:pPr>',
    '<w:spacing w:after="160" w:line="259" w:lineRule="auto"/></w:pPr></w:pPrDefault></w:docDefaults>',
    '<w:style w:type="paragraph" w:default="1" w:styleId="Normal"><w:name w:val="Normal"/><w:qFormat/></w:style>',
    '<w:style w:type="character" w:default="1" w:styleId="DefaultParagraphFont"><w:name w:val="Default Paragraph Font"/>',
    '<w:uiPriority w:val="1"/><w:semiHidden/><w:unhideWhenUsed/></w:style>',
    '<w:style w:type="table" w:default="1" w:styleId="TableNormal"><w:name w:val="Normal Table"/>',
    '<w:uiPriority w:val="99"/><w:semiHidden/><w:unhideWhenUsed/><w:tblPr><w:tblInd w:w="0" w:type="dxa"/>',
    '<w:tblCellMar><w:top w:w="0" w:type="dxa"/><w:left w:w="108" w:type="dxa"/><w:bottom w:w="0" w:type="dxa"/>',
    '<w:right w:w="108" w:type="dxa"/></w:tblCellMar></w:tblPr></w:style>',
    paste0(sprintf(paste0(
      '<w:style w:type="paragraph" w:styleId="Heading%d"><w:name w:val="heading %d"/>',
      '<w:basedOn w:val="Normal"/><w:next w:val="Normal"/><w:uiPriority w:val="9"/><w:qFormat/>',
      '<w:pPr><w:keepNext/><w:keepLines/><w:spacing w:before="%d" w:after="80"/><w:outlineLvl w:val="%d"/></w:pPr>',
      '<w:rPr><w:b/><w:bCs/><w:sz w:val="%d"/><w:szCs w:val="%d"/></w:rPr></w:style>'),
      1:3, 1:3, c(360, 160, 160), 0:2, c(36, 30, 26), c(36, 30, 26)), collapse = ""),
    '</w:styles>')
)
