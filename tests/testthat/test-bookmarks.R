p_xml <- function(...) paste0("<w:p>", paste0(...), "</w:p>")
r_xml <- function(t, rpr = "") sprintf('<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r>', rpr, t)
s_xml <- function(id, name) sprintf('<w:bookmarkStart w:id="%s" w:name="%s"/>', id, name)
e_xml <- function(id) sprintf('<w:bookmarkEnd w:id="%s"/>', id)

test_that("bkm_delete removes the markers without touching the text", {
  doc <- doc_read(word_path("example.docx")) |> bkm_delete("my_bookmark", delete_content = FALSE, delete_bookmark = TRUE)
  expect_false("my_bookmark" %in% bkm_list(doc)$name)
  expect_match(body_xml(doc), "Middle", fixed = TRUE)
})

test_that("bkm_delete with content keeps the end of a preceding bookmark", {
  doc <- doc_from_xml(p_xml(s_xml(1, "b"), r_xml("XX"), s_xml(2, "a"), e_xml(1), r_xml("YY"), e_xml(2), r_xml("ZZ")))
  bkm_delete(doc, "a", delete_content = TRUE, delete_bookmark = TRUE, delete_paragraph = FALSE)
  expect_equal(bkm_list(doc)$name, "b")
  expect_equal(bkm_text(doc, "b"), "XX")
  expect_false(grepl("YY", body_xml(doc)))
  expect_match(body_xml(doc), "ZZ", fixed = TRUE)
})

test_that("bkm_delete with content keeps the start of a following bookmark", {
  doc <- doc_from_xml(p_xml(s_xml(1, "a"), r_xml("AA"), s_xml(2, "c"), r_xml("BB"), e_xml(1), r_xml("CC"), e_xml(2)))
  bkm_delete(doc, "a", delete_content = TRUE, delete_bookmark = TRUE, delete_paragraph = FALSE)
  expect_equal(bkm_list(doc)$name, "c")
  expect_equal(bkm_text(doc, "c"), "CC")
})

test_that("bookmarks nested in the deleted content are kept (empty)", {
  doc <- doc_from_xml(p_xml(s_xml(1, "a"), r_xml("A"), s_xml(2, "n"), r_xml("N"), e_xml(2), e_xml(1)))
  bkm_delete(doc, "a", delete_content = TRUE, delete_bookmark = TRUE, delete_paragraph = FALSE)
  expect_equal(bkm_list(doc)$name, "n")
  expect_equal(bkm_text(doc, "n"), "")
})

test_that("bkm_replace_text keeps the formatting of the replaced text", {
  doc <- doc_from_xml(p_xml(s_xml(1, "a"), r_xml("old", "<w:rPr><w:b/></w:rPr>"), e_xml(1)))
  bkm_replace_text(doc, "a", "new")
  expect_equal(bkm_text(doc, "a"), "new")
  expect_match(body_xml(doc), "<w:r><w:rPr[^>]*><w:b/></w:rPr><w:t xml:space=\"preserve\">new</w:t>")
  expect_false(grepl("old", body_xml(doc)))
})

test_that("bkm_replace_text on an empty bookmark reuses the previous formatting", {
  doc <- doc_from_xml(p_xml(r_xml("Before ", "<w:rPr><w:i/></w:rPr>"), s_xml(1, "a"), e_xml(1)))
  bkm_replace_text(doc, "a", "text")
  expect_equal(bkm_text(doc, "a"), "text")
  expect_equal(lengths(regmatches(body_xml(doc), gregexpr("<w:i/>", body_xml(doc)))), 2)
})

test_that("bkm_replace_text merges a bookmark spanning several paragraphs", {
  doc <- doc_from_xml(paste0(
    p_xml(r_xml("A1"), s_xml(1, "a"), r_xml("A2")), p_xml(r_xml("B")), p_xml(r_xml("C1"), e_xml(1), r_xml("C2"))))
  bkm_replace_text(doc, "a", "N")
  expect_equal(bkm_text(doc, "a"), "N")
  x <- .xml(doc, doc$main)
  expect_equal(.count(x, "count(//w:body/w:p)"), 1)
  expect_equal(xml2::xml_text(.find1(x, "//w:body/w:p")), "A1NC2")
})

test_that("bkm_replace_text handles line breaks and tabs", {
  doc <- doc_read(word_path("example.docx")) |>
    bkm_replace_text("my_bookmark", "line 1\nline 2\tcol")
  expect_equal(bkm_text(doc, "my_bookmark"), "line 1\nline 2\tcol")
  expect_match(body_xml(doc), "<w:br/>", fixed = TRUE)
  expect_match(body_xml(doc), "<w:tab/>", fixed = TRUE)
})

test_that("bkm_insert_text respects the position", {
  base <- p_xml(s_xml(1, "a"), r_xml("Middle"), e_xml(1))
  expect_equal(bkm_text(bkm_insert_text(doc_from_xml(base), "a", "<", position = "before"), "a"), "<Middle")
  expect_equal(bkm_text(bkm_insert_text(doc_from_xml(base), "a", ">", position = "after"), "a"), "Middle>")
  expect_equal(bkm_text(bkm_insert_text(doc_from_xml(base), "a", "R"), "a"), "R")
})

test_that("bkm_insert_text applies the formatting", {
  doc <- doc_read(word_path("example.docx")) |>
    bkm_insert_text("my_bookmark", "Formatted", bold = TRUE, italic = TRUE, underline = "double",
                    size = 14, font = "Arial")
  xml <- body_xml(doc)
  expect_match(xml, paste0('<w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:eastAsia="Arial" w:cs="Arial"/>',
                           '<w:b w:val="1"/><w:bCs w:val="1"/><w:i w:val="1"/><w:iCs w:val="1"/>',
                           '<w:sz w:val="28"/><w:szCs w:val="28"/><w:u w:val="double"/></w:rPr>'), fixed = TRUE)
  expect_equal(bkm_text(doc, "my_bookmark"), "Formatted")
})

test_that("bkm_insert_text applies a character or paragraph style", {
  doc <- doc_read(word_path("example.docx")) |>
    bkm_insert_text("my_bookmark", "Char", style = "Heading1Char") |>
    bkm_insert_text("amount_2025", "Para", style = "Heading2")
  xml <- body_xml(doc)
  expect_match(xml, '<w:rStyle w:val="Heading1Char"/>', fixed = TRUE)
  expect_match(xml, '<w:pPr><w:pStyle w:val="Heading2"/></w:pPr>', fixed = TRUE)
  expect_error(bkm_insert_text(doc, "my_bookmark", "x", style = "UnknownStyle"), "not found")
  expect_error(bkm_insert_text(doc, "my_bookmark", "x", size = -1), "size")
})

test_that("keep_bookmark = FALSE removes the bookmark", {
  doc <- doc_read(word_path("example.docx")) |>
    bkm_insert_text("my_bookmark", "No bookmark", keep_bookmark = FALSE) |>
    bkm_replace_text("amount_2025", "100", keep_bookmark = FALSE)
  expect_equal(bkm_list(doc)$name, "empty_bookmark")
  expect_match(body_xml(doc), "No bookmark", fixed = TRUE)
  expect_match(body_xml(doc), "amount is </w:t></w:r><w:r><w:t xml:space=\"preserve\">100</w:t>", fixed = TRUE)
})

test_that("a footer bookmark can be manipulated", {
  doc <- doc_read(word_path("example.docx"))
  footer <- setdiff(.content_parts(doc, notes = FALSE), doc$main)[1]
  p <- .find1(.xml(doc, footer), "//w:p")
  .add(p, "w:bookmarkStart", "w:id" = 50, "w:name" = "footer")
  .add(p, "w:bookmarkEnd", "w:id" = 50)
  bkm_insert_text(doc, "footer", "Footer text", italic = TRUE)
  d2 <- round_trip(doc)
  s <- bkm_list(d2)
  expect_equal(s$text[s$name == "footer"], "Footer text")
  expect_equal(s$part[s$name == "footer"], footer)
})

test_that("an unknown bookmark raises an error", {
  doc <- doc_read(word_path("example.docx"))
  expect_warning(bkm_delete(doc, "missing"), "not found")
})

cell_xml <- function(text, bk = NULL, vmerge = NULL, extra = "") {
  vm <- if (is.null(vmerge)) "" else if (vmerge == "restart") '<w:vMerge w:val="restart"/>' else "<w:vMerge/>"
  inner <- if (is.null(bk)) r_xml(text) else paste0(s_xml(bk$id, bk$name), r_xml(text), e_xml(bk$id))
  sprintf("<w:tc><w:tcPr><w:tcW w:w=\"1000\" w:type=\"dxa\"/>%s</w:tcPr><w:p>%s</w:p></w:tc>", vm, inner)
}
tbl_xml <- function(...) paste0("<w:tbl><w:tblPr/><w:tblGrid><w:gridCol w:w=\"1000\"/><w:gridCol w:w=\"1000\"/></w:tblGrid>",
                                paste0("<w:tr>", c(...), "</w:tr>", collapse = ""), "</w:tbl><w:p/>")
bk <- function(id, name) list(id = id, name = name)

test_that("bkm_delete: defaults, delete_bookmark, lists and whole_document", {
  doc <- doc_from_xml(paste0(p_xml(s_xml(1, "a"), r_xml("A"), e_xml(1)), p_xml(s_xml(2, "b"), r_xml("B"), e_xml(2))))
  bkm_delete(doc, "a", delete_paragraph = FALSE)
  expect_equal(bkm_text(doc, "a"), "")
  expect_true("a" %in% bkm_list(doc)$name)
  expect_warning(bkm_delete(doc, c("a", "zzz", "b"), delete_bookmark = TRUE, delete_paragraph = FALSE), "zzz")
  expect_equal(nrow(bkm_list(doc)), 0)
  expect_false(any(nzchar(doc_text(doc))))
  doc2 <- doc_from_xml(paste0(p_xml(s_xml(1, "a"), r_xml("A"), e_xml(1))))
  bkm_delete(doc2, whole_document = TRUE)
  expect_equal(nrow(bkm_list(doc2)), 0)
  expect_equal(doc_text(doc2), "A")
})

test_that("bkm_delete deletes the whole paragraph by default", {
  mk <- function() doc_from_xml(paste0(
    p_xml(r_xml("A")), p_xml(r_xml("Before "), s_xml(1, "x"), r_xml("X"), e_xml(1), r_xml(" after")), p_xml(r_xml("B"))))
  d <- mk(); bkm_delete(d, "x")
  expect_equal(doc_text(d), c("A", "B"))
  expect_equal(nrow(bkm_list(d)), 0)
  # delete_bookmark has no effect: the markers go with the paragraph
  d <- mk(); bkm_delete(d, "x", delete_bookmark = FALSE)
  expect_equal(nrow(bkm_list(d)), 0)
  # delete_content = FALSE: nothing deleted
  d <- mk(); bkm_delete(d, "x", delete_content = FALSE)
  expect_equal(doc_text(d), c("A", "Before X after", "B"))
  expect_equal(bkm_list(d)$name, "x")
  # Only the content
  d <- mk(); bkm_delete(d, "x", delete_paragraph = FALSE)
  expect_equal(doc_text(d), c("A", "Before  after", "B"))
  expect_equal(bkm_text(d, "x"), "")
  expect_error(bkm_delete(d, "x", delete_paragraph = NA), "TRUE or FALSE")
})

test_that("bkm_delete: paragraph deletion over several paragraphs preserves other bookmarks", {
  doc <- doc_from_xml(paste0(
    p_xml(s_xml(1, "before"), r_xml("A")),
    p_xml(r_xml("P1 "), s_xml(2, "x"), r_xml("X1"), e_xml(1)),
    p_xml(s_xml(3, "inner"), r_xml("X2"), e_xml(3)),
    p_xml(r_xml("X3"), e_xml(2), s_xml(4, "after"), r_xml(" P3")),
    p_xml(r_xml("B"), e_xml(4))))
  bkm_delete(doc, "x")
  expect_equal(doc_text(doc), c("A", "B"))
  expect_setequal(bkm_list(doc)$name, c("before", "inner", "after"))
  expect_equal(bkm_text(doc, "before"), "A")
  expect_equal(bkm_text(doc, "inner"), "")
  expect_equal(bkm_text(doc, "after"), "B")
  # Bookmark end directly in the body (after the paragraph)
  doc2 <- doc_from_xml(paste0(p_xml(r_xml("A")), p_xml(s_xml(1, "c"), r_xml("C1")), p_xml(r_xml("C2")), e_xml(1), p_xml(r_xml("B"))))
  bkm_delete(doc2, "c")
  expect_equal(doc_text(doc2), c("A", "B"))
  expect_equal(nrow(bkm_list(doc2)), 0)
  # Last paragraph of the body: the document stays valid once saved
  doc3 <- doc_from_xml(p_xml(s_xml(1, "x"), r_xml("X"), e_xml(1)))
  bkm_delete(doc3, "x")
  expect_equal(doc_text(round_trip(doc3), keep_empty = TRUE), "")
  # Case produced by Word: bookmark end at the start of the next paragraph, which is kept
  doc4 <- doc_from_xml(paste0(p_xml(s_xml(1, "c"), r_xml("C")), p_xml(e_xml(1), r_xml("B"))))
  bkm_delete(doc4, "c")
  expect_equal(doc_text(doc4), "B")
  # ... but an end in the middle of the next paragraph deletes it
  doc5 <- doc_from_xml(paste0(p_xml(s_xml(1, "c"), r_xml("C")), p_xml(r_xml("B"), e_xml(1), r_xml("2"))))
  bkm_delete(doc5, "c")
  expect_equal(doc_text(doc5, keep_empty = TRUE), character())
  # Empty bookmark across a paragraph boundary: the paragraphs are merged
  doc6 <- doc_from_xml(paste0(p_xml(r_xml("A"), s_xml(1, "c")), p_xml(e_xml(1), r_xml("B"))))
  bkm_delete(doc6, "c")
  expect_equal(doc_text(doc6), "AB")
  expect_equal(nrow(bkm_list(doc6)), 0)
})

test_that("bkm_delete: paragraph deletion in a table cell and across structures", {
  mk <- function() doc_from_xml(tbl_xml(
    paste0(cell_xml("a1", bk(1, "a")), cell_xml("a2", bk(2, "other"))), paste0(cell_xml("b1"), cell_xml("b2"))))
  d <- mk(); bkm_delete(d, "a")
  expect_equal(.count(.xml(d, d$main), "count(//w:tr)"), 2)
  expect_equal(.count(.xml(d, d$main), "count(//w:tc[1]/w:p)"), 2)   # an empty paragraph is kept in the cell
  expect_equal(bkm_list(d)$name, "other")
  expect_false(grepl("a1", body_xml(d)))
  # Start and end in different cells of a table: the rows are deleted
  d <- doc_from_xml(paste0("<w:tbl><w:tblPr/>",
    "<w:tr><w:tc>", p_xml(r_xml("h")), "</w:tc></w:tr>",
    "<w:tr><w:tc>", p_xml(s_xml(1, "x"), r_xml("1")), "</w:tc></w:tr>",
    "<w:tr><w:tc>", p_xml(r_xml("2"), e_xml(1)), "</w:tc></w:tr>",
    "</w:tbl>", p_xml(r_xml("after"))))
  bkm_delete(d, "x")
  expect_equal(.count(.xml(d, d$main), "count(//w:tr)"), 1)
  expect_equal(doc_text(d), c("h", "after"))
  # Start in a paragraph, end in a table: the paragraph and the table are deleted
  d <- doc_from_xml(paste0(p_xml(r_xml("A")), p_xml(s_xml(1, "x"), r_xml("P")),
    "<w:tbl><w:tblPr/><w:tr><w:tc>", p_xml(r_xml("c"), e_xml(1)), "</w:tc></w:tr></w:tbl>", p_xml(r_xml("B"))))
  bkm_delete(d, "x")
  expect_equal(.count(.xml(d, d$main), "count(//w:tbl)"), 0)
  expect_equal(doc_text(d), c("A", "B"))
})

test_that("bkm_delete: table cell content and row", {
  mk <- function() doc_from_xml(tbl_xml(
    paste0(cell_xml("a1", bk(1, "a")), cell_xml("a2", bk(2, "other"))), paste0(cell_xml("b1"), cell_xml("b2"))))
  d <- mk(); bkm_delete(d, "a", delete_paragraph = FALSE)
  expect_equal(.count(.xml(d, d$main), "count(//w:tr)"), 2)
  expect_equal(bkm_text(d, "a"), "")
  d <- mk(); bkm_delete(d, "a", delete_row = TRUE)
  expect_equal(.count(.xml(d, d$main), "count(//w:tr)"), 1)
  expect_equal(bkm_list(d)$name, character())
  expect_warning(bkm_delete(doc_from_xml(p_xml(s_xml(1, "x"), e_xml(1))), "x", delete_row = TRUE), "not in a table")
})

test_that("bkm_delete: row in a group of vertically merged cells keeps the value", {
  mk <- function(name_row) {
    doc_from_xml(tbl_xml(
      paste0(cell_xml("M", vmerge = "restart"), cell_xml("r1", if (name_row == 1) bk(1, "x"))),
      paste0(cell_xml("", vmerge = "continue"), cell_xml("r2", if (name_row == 2) bk(1, "x"))),
      paste0(cell_xml("", vmerge = "continue"), cell_xml("r3", if (name_row == 3) bk(1, "x")))))
  }
  d <- mk(2); bkm_delete(d, "x", delete_row = TRUE)
  rows <- .find_all(.xml(d, d$main), "//w:tr")
  expect_length(rows, 2)
  expect_equal(doc_text(d)[1:3], c("M", "r1", "r3"))
  d <- mk(1); bkm_delete(d, "x", delete_row = TRUE)
  x <- .xml(d, d$main)
  rows <- .find_all(x, "//w:tr")
  expect_length(rows, 2)
  expect_equal(xml2::xml_text(.find_all(rows[[1]], "w:tc")[[1]]), "M")
  expect_equal(.count(rows[[1]], "count(w:tc[1]/w:tcPr/w:vMerge[@w:val='restart'])"), 1)
  expect_equal(.count(rows[[2]], "count(w:tc[1]/w:tcPr/w:vMerge[not(@w:val)])"), 1)
  d <- mk(3); bkm_delete(d, "x", delete_row = TRUE)
  expect_equal(.count(.xml(d, d$main), "count(//w:tr)"), 2)
})

test_that("bkm_delete: bookmark in a merged cell deletes all the merged rows", {
  d <- doc_from_xml(tbl_xml(
    paste0(cell_xml("M", vmerge = "restart"), cell_xml("r1")),
    paste0(cell_xml("", vmerge = "continue", bk = NULL), cell_xml("r2")),
    paste0(cell_xml("N"), cell_xml("r3"))))
  x <- .xml(d, d$main)
  xml2::xml_add_child(.find_all(x, "//w:tr[2]/w:tc[1]/w:p")[[1]],
                      xml2::read_xml(sprintf('<w:bookmarkStart xmlns:w="%s" w:id="9" w:name="m"/>', .ns[["w"]])), .where = 0)
  xml2::xml_add_child(.find_all(x, "//w:tr[2]/w:tc[1]/w:p")[[1]],
                      xml2::read_xml(sprintf('<w:bookmarkEnd xmlns:w="%s" w:id="9"/>', .ns[["w"]])))
  bkm_delete(d, "m", delete_row = TRUE)
  expect_equal(doc_text(d)[1:2], c("N", "r3"))
  expect_equal(.count(.xml(d, d$main), "count(//w:tr)"), 1)
})

test_that("operations on a missing bookmark warn instead of failing", {
  doc <- doc_new() |> doc_add_paragraph("x", bookmark = "bk")
  before <- doc_text(doc)
  expect_warning(bkm_replace_text(doc, "nope", "z"), "Operation ignored")
  expect_warning(bkm_insert_text(doc, "nope", "z"), "Operation ignored")
  expect_warning(bkm_insert_page_break(doc, "nope"), "Operation ignored")
  expect_warning(bkm_conditional(doc, "nope", FALSE), "Operation ignored")
  expect_warning(bkm_repeat(doc, "nope", data.frame(a = 1)), "Operation ignored")
  expect_warning(res <- bkm_text(doc, "nope"), "Operation ignored")
  expect_true(is.na(res))
  expect_equal(doc_text(doc), before)
})