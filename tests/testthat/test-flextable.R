make_table <- function() flextable::flextable(head(mtcars[, 1:3], 3))

test_that("bkm_insert_flextable inserts a table on a bookmark", {
  skip_if_not_installed("flextable")
  doc <- doc_read(word_path("example.docx"))
  n_p <- .count(.xml(doc, doc$main), "count(//w:body/w:p)")
  bkm_insert_flextable(doc, "empty_bookmark", make_table())
  d2 <- round_trip(doc)
  x <- .xml(d2, d2$main)
  expect_equal(.count(x, "count(//w:body/w:tbl)"), 1)
  expect_match(bkm_text(d2, "empty_bookmark"), "mpg")
  # the paragraph that contained only the bookmark is replaced by the table
  expect_equal(.count(x, "count(//w:body/w:p)"), n_p - 1)
})

test_that("doc_replace_text_with_flextable replaces a text with a table", {
  skip_if_not_installed("flextable")
  doc <- doc_read(word_path("example.docx")) |>
    doc_replace_text_with_flextable("{{amount_2024}}", make_table())
  x <- .xml(doc, doc$main)
  expect_equal(attr(doc, "replacements"), 1)
  expect_equal(.count(x, "count(//w:body/w:tbl)"), 1)
  tbl <- .find1(x, "//w:body/w:tbl")
  expect_equal(xml2::xml_text(.find1(tbl, "preceding-sibling::w:p[1]")), "The order amount is ")
  expect_equal(xml2::xml_text(.find1(tbl, "following-sibling::w:p[1]")), " euros in 2024.")
  expect_false(any(grepl("WRite", bkm_list(doc)$name)))
  expect_false(grepl("\uE000", body_xml(doc), fixed = TRUE))
})

test_that("a paragraph containing only the text is replaced by the table", {
  skip_if_not_installed("flextable")
  doc <- doc_new() |>
    doc_add_paragraph("Before") |>
    doc_add_paragraph("[TABLE]") |>
    doc_add_paragraph("After [TABLE]")
  doc_replace_text_with_flextable(doc, "[TABLE]", make_table())
  x <- .xml(doc, doc$main)
  names <- vapply(xml2::xml_children(.find1(x, "//w:body")), .name, "")
  expect_equal(names, c("w:p", "w:tbl", "w:p", "w:tbl", "w:sectPr"))
  expect_warning(doc_replace_text_with_flextable(doc, "missing", make_table()), "not found")
})
