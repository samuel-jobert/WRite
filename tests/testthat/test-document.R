test_that("doc_read reads a docx and lists its bookmarks", {
  doc <- doc_read(word_path("example.docx"))
  expect_s3_class(doc, "write_doc")
  s <- bkm_list(doc)
  expect_equal(s$name, c("my_bookmark", "amount_2025", "empty_bookmark"))
  expect_equal(s$text, c("Middle", "{{amount_2025}}", ""))
  expect_output(print(doc), "Bookmarks")
})

test_that("doc_read reports a missing file", {
  expect_error(doc_read("missing.docx"), "not found")
})

test_that("the source file is unchanged and the round trip is faithful", {
  f <- word_path("example.docx")
  before <- unname(tools::md5sum(f))
  doc <- doc_read(f) |> bkm_replace_text("my_bookmark", "X")
  expect_equal(unname(tools::md5sum(f)), before)
  d2 <- round_trip(doc)
  expect_equal(bkm_text(d2, "my_bookmark"), "X")
  expect_equal(nrow(bkm_list(d2)), 3)
})

test_that("doc_new creates a usable document", {
  doc <- doc_new() |>
    doc_add_paragraph("Title", style = "heading 1") |>
    doc_add_paragraph("Hello", bold = TRUE, bookmark = "s1")
  d2 <- round_trip(doc)
  expect_equal(bkm_text(d2, "s1"), "Hello")
  expect_match(body_xml(d2), '<w:pStyle w:val="Heading1"/>', fixed = TRUE)
  expect_error(doc_add_paragraph(doc, "x", bookmark = "s1"), "already exists")
})

test_that("a saved empty document contains a paragraph", {
  d2 <- round_trip(doc_new())
  expect_match(body_xml(d2), "<w:p/>", fixed = TRUE)
})

test_that("doc_copy produces an independent copy", {
  doc <- doc_read(word_path("example.docx"))
  copy <- doc_copy(doc)
  bkm_replace_text(copy, "my_bookmark", "Copy")
  expect_equal(bkm_text(doc, "my_bookmark"), "Middle")
  expect_equal(bkm_text(copy, "my_bookmark"), "Copy")
})

test_that("functions reject an object that is not a document", {
  expect_error(bkm_list(list()), "not a WRite document")
})
