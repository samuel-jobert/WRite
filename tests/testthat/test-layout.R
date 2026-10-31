ppr_of <- function(p) .serialize(.find1(p, "w:pPr"))

test_that("bkm_insert_page_break inserts a page break on the bookmark", {
  doc <- doc_read(word_path("example.docx")) |>
    bkm_insert_page_break("empty_bookmark") |>
    bkm_insert_page_break("my_bookmark", position = "after")
  d2 <- round_trip(doc)
  x <- .xml(d2, d2$main)
  expect_equal(.count(x, "count(//w:br[@w:type='page'])"), 2)
  expect_equal(.count(x, "count(//w:bookmarkStart[@w:name='empty_bookmark']/following-sibling::*[1][self::w:r/w:br[@w:type='page']])"), 1)
  expect_equal(bkm_text(d2, "my_bookmark"), "Middle\f")
  expect_match(.serialize(.find1(x, "//w:bookmarkStart[@w:name='my_bookmark']/..")),
               'Middle</w:t></w:r><w:r><w:br w:type="page"/></w:r><w:bookmarkEnd', fixed = TRUE)
})

test_that("doc_add_page_break adds a page break at the end of the document", {
  doc <- doc_new() |> doc_add_paragraph("Page 1") |> doc_add_page_break() |>
    doc_add_paragraph("Page 2")
  nodes <- .find_all(.xml(doc, doc$main), "//w:body/w:p")
  expect_length(nodes, 3)
  expect_equal(.count(nodes[[2]], "count(w:r/w:br[@w:type='page'])"), 1)
})

test_that("keep_with_previous sets keepNext on the previous paragraph", {
  doc <- doc_new() |>
    doc_add_paragraph("Title", style = "Heading1") |>
    doc_add_paragraph("Text", keep_with_previous = TRUE, keep_lines_together = TRUE)
  p <- .find_all(.xml(doc, doc$main), "//w:body/w:p")
  expect_equal(ppr_of(p[[1]]), '<w:pPr><w:pStyle w:val="Heading1"/><w:keepNext/></w:pPr>')
  expect_equal(ppr_of(p[[2]]), "<w:pPr><w:keepLines/></w:pPr>")
})

test_that("the keep options work with bkm_insert_text", {
  doc <- doc_read(word_path("example.docx")) |>
    bkm_insert_text("empty_bookmark", "Clause", keep_with_previous = TRUE, keep_lines_together = TRUE)
  x <- .xml(doc, doc$main)
  p <- .find1(x, "//w:p[w:bookmarkStart[@w:name='empty_bookmark']]")
  prev <- .find1(p, "preceding-sibling::w:p[1]")
  # schema order respected: keepNext after pStyle and before spacing
  expect_equal(ppr_of(prev), '<w:pPr><w:pStyle w:val="SmallParagraph"/><w:keepNext/><w:spacing w:after="160"/></w:pPr>')
  expect_equal(ppr_of(p), "<w:pPr><w:keepLines/></w:pPr>")
  # an existing disabled keepNext (w:val="0") is re-enabled
  q <- .find1(x, "//w:p[w:bookmarkStart[@w:name='amount_2025']]/preceding-sibling::w:p[1]")
  .set_attr(.paragraph_property(q, "w:keepNext"), "w:val", "0")
  bkm_insert_text(doc, "amount_2025", "1", keep_with_previous = TRUE)
  expect_equal(.count(x, "count(//w:keepNext[@w:val])"), 0)
  expect_equal(ppr_of(q), "<w:pPr><w:keepNext/></w:pPr>")
})

test_that("keep_with_previous after a table and without a previous paragraph", {
  doc <- doc_from_xml(paste0(
    "<w:tbl><w:tr><w:tc><w:p><w:r><w:t>A</w:t></w:r></w:p></w:tc></w:tr>",
    "<w:tr><w:tc><w:p><w:r><w:t>B</w:t></w:r></w:p></w:tc><w:tc><w:p/></w:tc></w:tr></w:tbl>"))
  doc_add_paragraph(doc, "After", keep_with_previous = TRUE)
  x <- .xml(doc, doc$main)
  expect_equal(.count(x, "count(//w:tr[2]//w:p/w:pPr/w:keepNext)"), 2)
  expect_equal(.count(x, "count(//w:tr[1]//w:keepNext)"), 0)
  empty <- doc_new()
  expect_warning(doc_add_paragraph(empty, "Alone", keep_with_previous = TRUE), "No previous paragraph")
  expect_error(doc_add_paragraph(empty, "x", keep_lines_together = "yes"), "TRUE or FALSE")
})
