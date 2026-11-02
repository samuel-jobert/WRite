test_that("doc_add_paragraph: \\r creates paragraphs, \\n a line break", {
  doc <- doc_new() |> doc_add_paragraph("A\nB\n\nC\rD\r\nE")
  p <- .find_all(.xml(doc, doc$main), "//w:body/w:p")
  expect_length(p, 2)
  expect_equal(.count(p[[1]], "count(.//w:br)"), 3)
  expect_equal(doc_text(doc), c("A\nB\n\nC", "D\nE"))
})

test_that("doc_add_paragraph: a bookmark spans all the paragraphs", {
  doc <- doc_new() |> doc_add_paragraph("A\rB", bookmark = "bk", style = "Heading1")
  expect_equal(bkm_text(round_trip(doc), "bk"), "A\rB")
  p <- .find_all(.xml(doc, doc$main), "//w:body/w:p")
  expect_equal(.count(p[[2]], "count(w:pPr/w:pStyle[@w:val='Heading1'])"), 1)
})

test_that("doc_add_paragraph: html tags are formatted when html = TRUE", {
  doc <- doc_new() |>
    doc_add_paragraph("a <b>b<i>bi</i></b> <u>u</u><s>s</s><br>x &lt;y&gt; <em>e</em><span>", html = TRUE)
  x <- .xml(doc, doc$main)
  runs <- .find_all(x, "//w:body/w:p/w:r")
  txt <- vapply(runs, function(r) paste(xml2::xml_text(.find_all(r, "w:t")), collapse = ""), "")
  expect_equal(paste(txt, collapse = "|"), "a |b|bi| |u|s||x <y> |e|<span>")
  has <- function(i, tag) .count(runs[[i]], sprintf("count(w:rPr/w:%s)", tag)) == 1
  expect_true(has(2, "b") && !has(2, "i"))
  expect_true(has(3, "b") && has(3, "i"))
  expect_true(has(5, "u"))
  expect_true(has(6, "strike"))
  expect_equal(.count(x, "count(//w:br)"), 1)
  expect_true(has(9, "i"))
  expect_equal(.count(x, "count(//w:r[w:rPr/w:strike]/w:rPr/w:strike/following-sibling::*)"), 0)
  expect_length(.find_all(round_trip(doc) |> (\(d) .xml(d, d$main))(), "//w:strike"), 1)
})

test_that("doc_add_paragraph: html = FALSE keeps the tags as text", {
  doc <- doc_new() |> doc_add_paragraph("a <b>b</b>")
  expect_equal(doc_text(doc), "a <b>b</b>")
})

test_that("doc_add_paragraph: HTML formatting combines with the base format", {
  doc <- doc_new() |> doc_add_paragraph("x <u>y</u>", underline = "double", html = TRUE)
  expect_match(body_xml(doc), '<w:u w:val="double"/>', fixed = TRUE)
  expect_false(grepl('<w:u w:val="single"/>', body_xml(doc), fixed = TRUE))
})

test_that("doc_add_paragraph with at_text: after, before", {
  doc <- doc_new() |> doc_add_paragraph("One") |> doc_add_paragraph("Two") |> doc_add_paragraph("Three")
  doc_add_paragraph(doc, "After one", at_text = "One")
  doc_add_paragraph(doc, "Before three\rBefore three bis", at_text = "Three", where = "before")
  expect_equal(doc_text(doc), c("One", "After one", "Two", "Before three", "Before three bis", "Three"))
})

test_that("doc_add_paragraph with at_text: absent text gives a warning and no change", {
  doc <- doc_new() |> doc_add_paragraph("One")
  expect_warning(doc_add_paragraph(doc, "X", at_text = "absent"), "not found")
  expect_equal(doc_text(doc), "One")
})

test_that("doc_add_paragraph with at_text: first occurrence (with warning) or all", {
  doc <- doc_new() |> doc_add_paragraph("cat 1") |> doc_add_paragraph("cat 2")
  expect_warning(doc_add_paragraph(doc, "N", at_text = "cat"), "2 times")
  expect_equal(doc_text(doc), c("cat 1", "N", "cat 2"))
  doc_add_paragraph(doc, "A", at_text = "cat", occurrence = "all")
  expect_equal(doc_text(doc), c("cat 1", "A", "N", "cat 2", "A"))
  expect_error(doc_add_paragraph(doc, "B", at_text = "cat", occurrence = "all", bookmark = "b"), "occurrence")
})

test_that("doc_add_paragraph with at_text: text spread over several runs", {
  doc <- doc_read(word_path("to_insert.docx"))
  doc_add_paragraph(doc, "New", at_text = "will be inserted into")
  txt <- doc_text(doc)
  i <- which(txt == "New")
  expect_equal(txt[i - 1], "This title will be inserted into another document")
})

test_that("doc_add_paragraph with at_text and replace: whole paragraph", {
  doc <- doc_new() |> doc_add_paragraph("One") |> doc_add_paragraph("Two") |> doc_add_paragraph("Three")
  doc_add_paragraph(doc, "Deux", at_text = "Two", where = "replace", bold = TRUE)
  expect_equal(doc_text(doc), c("One", "Deux", "Three"))
  expect_equal(.count(.xml(doc, doc$main), "count(//w:p/w:r[w:rPr/w:b]/w:t[.='Deux'])"), 1)
})

test_that("doc_add_paragraph with at_text and replace: paragraph split around the text", {
  doc <- doc_new() |> doc_add_paragraph("Before [X] after")
  doc_add_paragraph(doc, "New 1\rNew 2", at_text = "[X]", where = "replace", bookmark = "nb")
  expect_equal(doc_text(doc), c("Before ", "New 1", "New 2", " after"))
  expect_equal(bkm_text(round_trip(doc), "nb"), "New 1\rNew 2")
  expect_false(any(grepl("_WRite", bkm_list(doc)$name)))
})

test_that("doc_add_paragraph with at_text and replace: all occurrences", {
  doc <- doc_new() |> doc_add_paragraph("a [X] b [X] c") |> doc_add_paragraph("[X]")
  doc_add_paragraph(doc, "N", at_text = "[X]", where = "replace", occurrence = "all")
  expect_equal(doc_text(doc), c("a ", "N", " b ", "N", " c", "N"))
})

test_that("doc_add_paragraph with at_text: keep_with_previous applies after placement", {
  doc <- doc_new() |> doc_add_paragraph("One") |> doc_add_paragraph("Two")
  doc_add_paragraph(doc, "Mid", at_text = "One", keep_with_previous = TRUE, keep_lines_together = TRUE)
  p <- .find_all(.xml(doc, doc$main), "//w:body/w:p")
  expect_equal(.count(p[[1]], "count(w:pPr/w:keepNext)"), 1)
  expect_equal(.count(p[[2]], "count(w:pPr/w:keepLines)"), 1)
})

test_that("doc_add_paragraph with at_text: the result opens again", {
  doc <- doc_read(word_path("example.docx"))
  doc_add_paragraph(doc, "Y", at_text = "End of doc", where = "replace")
  d2 <- round_trip(doc)
  expect_true("Y" %in% doc_text(d2))
})
