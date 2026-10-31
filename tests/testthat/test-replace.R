p_xml <- function(...) paste0("<w:p>", paste0(...), "</w:p>")
r_xml <- function(t, rpr = "") sprintf('<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r>', rpr, t)
body_text <- function(doc) xml2::xml_text(.find_all(.xml(doc, doc$main), "//w:body/w:p"))

test_that("doc_replace_text replaces a text in the document", {
  doc <- doc_read(word_path("example.docx")) |>
    doc_replace_text("{{amount_2024}}", "999 999")
  expect_equal(attr(doc, "replacements"), 1)
  expect_true("The order amount is 999 999 euros in 2024." %in% body_text(doc))
  expect_equal(bkm_text(doc, "amount_2025"), "{{amount_2025}}")
})

test_that("an occurrence spread over runs with different formatting is replaced", {
  doc <- doc_from_xml(p_xml(r_xml("Hel"), r_xml("lo", "<w:rPr><w:b/></w:rPr>"), r_xml(" world")))
  doc_replace_text(doc, "Hello", "Hi")
  expect_equal(body_text(doc), "Hi world")
  expect_false(grepl("<w:b/>", body_xml(doc), fixed = TRUE))
})

test_that("bookmarks inside an occurrence are kept", {
  doc <- doc_from_xml(p_xml(r_xml("AB"), '<w:bookmarkStart w:id="1" w:name="s"/>', r_xml("CD"),
                            '<w:bookmarkEnd w:id="1"/>', r_xml("E")))
  doc_replace_text(doc, "BC", "x")
  expect_equal(body_text(doc), "AxDE")
  expect_equal(bkm_text(doc, "s"), "D")
})

test_that("several occurrences, regular expressions and case", {
  doc <- doc_from_xml(paste0(p_xml(r_xml("a{{x}}b"), r_xml("{{x}}c")), p_xml(r_xml("Year 2024, 2025"))))
  doc_replace_text(doc, "{{x}}", "-")
  expect_equal(attr(doc, "replacements"), 2)
  doc_replace_text(doc, "(\\d{4})", "[\\1]", regex = TRUE)
  doc_replace_text(doc, "year", "Yr", ignore_case = TRUE)
  expect_equal(body_text(doc), c("a-b-c", "Yr [2024], [2025]"))
  doc_replace_text(doc, "missing", "x")
  expect_equal(attr(doc, "replacements"), 0)
})

test_that("runs are merged only once per object", {
  doc <- doc_from_xml(p_xml(r_xml("A"), r_xml("B")))
  doc_replace_text(doc, "zzz", "y")
  expect_true(doc$runs_merged)
  p <- .find1(.xml(doc, doc$main), "//w:body/w:p")
  expect_equal(.count(p, "count(w:r)"), 1)
  .new_run("C", .format(), parent = p)
  doc_replace_text(doc, "zzz", "y")
  expect_equal(.count(p, "count(w:r)"), 2)
})
