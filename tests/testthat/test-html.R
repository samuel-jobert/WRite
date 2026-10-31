runs_of <- function(doc, xpath = "//w:body/w:p//w:r") {
  .find_all(.xml(doc, doc$main), xpath)
}
run_text <- function(r) paste(xml2::xml_text(.find_all(r, "w:t")), collapse = "")
has_prop <- function(r, tag) .count(r, sprintf("count(w:rPr/w:%s)", tag)) == 1

test_that("bkm_insert_text: html tags", {
  doc <- doc_new() |> doc_add_paragraph("x", bookmark = "bk")
  bkm_insert_text(doc, "bk", "a <b>b</b> <i>c</i>", html = TRUE, size = 14)
  doc <- round_trip(doc)
  expect_equal(bkm_text(doc, "bk"), "a b c")
  r <- runs_of(doc)
  expect_true(has_prop(r[[2]], "b"))
  expect_true(has_prop(r[[3]], "sz") || has_prop(r[[2]], "sz"))
  expect_false(has_prop(r[[4]], "b"))
  bkm_insert_text(doc, "bk", "<b>z</b>", position = "after")
  expect_equal(bkm_text(doc, "bk"), "a b c<b>z</b>")
})

test_that("bkm_replace_text: html keeps the base formatting", {
  doc <- doc_from_xml(paste0(
    '<w:p><w:r><w:rPr><w:i/></w:rPr><w:t>A </w:t></w:r><w:bookmarkStart w:id="1" w:name="bk"/>',
    '<w:r><w:rPr><w:i/><w:sz w:val="30"/></w:rPr><w:t>old</w:t></w:r><w:bookmarkEnd w:id="1"/></w:p>'))
  bkm_replace_text(doc, "bk", "n<b>ew</b>", html = TRUE)
  doc <- round_trip(doc)
  expect_equal(bkm_text(doc, "bk"), "new")
  r <- runs_of(doc)
  r <- r[vapply(r, run_text, "") %in% c("n", "ew")]
  expect_length(r, 2)
  for (x in r) expect_true(has_prop(x, "i") && has_prop(x, "sz"))
  expect_true(has_prop(r[[2]], "b"))
  expect_false(has_prop(r[[1]], "b"))
  # without html, tags stay literal
  bkm_replace_text(doc, "bk", "<b>x</b>")
  expect_equal(bkm_text(doc, "bk"), "<b>x</b>")
})

test_that("doc_replace_text: html with the formatting of the replaced text", {
  doc <- doc_from_xml(paste0(
    '<w:p><w:r><w:rPr><w:i/></w:rPr><w:t>Hello {{a}}, bye.</w:t></w:r></w:p>'))
  doc_replace_text(doc, "{{a}}", "<b>Ann</b> and Bob", html = TRUE)
  doc <- round_trip(doc)
  expect_equal(doc_text(doc), "Hello Ann and Bob, bye.")
  expect_equal(attr(doc_replace_text(doc, "zzz", "y", html = TRUE), "replacements"), 0)
  r <- runs_of(doc)
  ann <- r[vapply(r, run_text, "") == "Ann"][[1]]
  expect_true(has_prop(ann, "b") && has_prop(ann, "i"))
  expect_equal(.count(.xml(doc, doc$main), "count(//w:bookmarkStart[@w:name='_WRite_html_tmp'])"), 0)
})

test_that("doc_replace_text: html with regex backreferences and several occurrences", {
  doc <- doc_new() |> doc_add_paragraph("n=1 and n=2")
  doc_replace_text(doc, "n=(\\d)", "<b>\\1</b>", regex = TRUE, html = TRUE)
  expect_equal(doc_text(doc), "1 and 2")
  expect_equal(.count(.xml(doc, doc$main), "count(//w:r[w:rPr/w:b])"), 2)
})

test_that("doc_replace_text: vectors of texts, applied in order", {
  doc <- doc_new() |> doc_add_paragraph("{{a}} {{b}} {{a}}") |> doc_add_paragraph("none")
  doc_replace_text(doc, c("{{a}}", "{{b}}"), c("1", "2"))
  expect_equal(doc_text(doc), c("1 2 1", "none"))
  expect_equal(attr(doc, "replacements"), 3)
  doc_replace_text(doc, c("1", "2"), "X")
  expect_equal(doc_text(doc)[1], "X X X")
  doc_replace_text(doc, c("X", "Y"), c("Y", "Z"))
  expect_equal(doc_text(doc)[1], "Z Z Z")
  expect_error(doc_replace_text(doc, c("a", "b"), c("1", "2", "3")), "replacement")
  expect_error(doc_replace_text(doc, character(), "1"), "text")
})
