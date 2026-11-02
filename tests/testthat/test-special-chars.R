p_xml <- function(..., ppr = "") paste0("<w:p>", ppr, paste0(...), "</w:p>")
r_xml <- function(t, rpr = "") sprintf('<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r>', rpr, t)
s_xml <- function(id, name) sprintf('<w:bookmarkStart w:id="%s" w:name="%s"/>', id, name)
e_xml <- function(id) sprintf('<w:bookmarkEnd w:id="%s"/>', id)
ppr_center <- '<w:pPr><w:jc w:val="center"/></w:pPr>'
bold <- "<w:rPr><w:b/></w:rPr>"
body_ps <- function(doc) .find_all(.xml(doc, doc$main), "/w:document/w:body/w:p")

test_that("bkm_replace_text: \\r splits the paragraph, which keeps its properties", {
  doc <- doc_from_xml(p_xml(r_xml("Before "), s_xml(1, "x"), r_xml("X", bold), e_xml(1), r_xml(" after"), ppr = ppr_center))
  bkm_replace_text(doc, "x", "L1\rL2\nL2b\rL3")
  expect_equal(doc_text(doc), c("Before L1", "L2\nL2b", "L3 after"))
  ps <- body_ps(doc)
  expect_true(all(vapply(ps, function(p) .count(p, "count(w:pPr/w:jc[@w:val='center'])") == 1, TRUE)))
  # The replaced text keeps its formatting, in every paragraph
  expect_equal(.count(.xml(doc, doc$main), "count(//w:r[w:rPr/w:b])"), 3)
  # The bookmark spans the whole new text
  expect_equal(bkm_text(doc, "x"), "L1\rL2\nL2b\rL3")
  expect_equal(bkm_text(round_trip(doc), "x"), "L1\rL2\nL2b\rL3")
})

test_that("bkm_insert_text: \\r before / after the content of the bookmark", {
  mk <- function() doc_from_xml(p_xml(s_xml(1, "x"), r_xml("X"), e_xml(1), r_xml(" end")))
  d <- mk(); bkm_insert_text(d, "x", "A\rB ", position = "before")
  expect_equal(doc_text(d), c("A", "B X end"))
  expect_equal(bkm_text(d, "x"), "A\rB X")
  d <- mk(); bkm_insert_text(d, "x", " A\rB", position = "after", keep_lines_together = TRUE)
  expect_equal(doc_text(d), c("X A", "B end"))
  expect_equal(bkm_text(d, "x"), "X A\rB")
  expect_equal(.count(.xml(d, d$main), "count(//w:p[w:pPr/w:keepLines])"), 2)
})

test_that("the section break stays with the last paragraph", {
  doc <- doc_from_xml(p_xml(s_xml(1, "x"), e_xml(1), ppr = '<w:pPr><w:sectPr/></w:pPr>'))
  bkm_replace_text(doc, "x", "A\rB")
  ps <- body_ps(doc)
  expect_equal(vapply(ps, function(p) .count(p, "count(w:pPr/w:sectPr)"), 0), c(0, 1))
})

test_that("line breaks, tabs and page breaks", {
  doc <- doc_from_xml(p_xml(s_xml(1, "x"), e_xml(1)))
  bkm_replace_text(doc, "x", "a\nb\vc\td\fe\r\nf")
  r <- .find1(.xml(doc, doc$main), "//w:r")
  expect_equal(.count(r, "count(w:br[not(@w:type)])"), 3)
  expect_equal(.count(r, "count(w:tab)"), 1)
  expect_equal(.count(r, "count(w:br[@w:type='page'])"), 1)
  expect_equal(bkm_text(doc, "x"), "a\nb\nc\td\fe\nf")
  expect_length(body_ps(doc), 1)
  # The characters backslash + n are not interpreted
  bkm_replace_text(doc, "x", "C:\\new\\path")
  expect_equal(bkm_text(doc, "x"), "C:\\new\\path")
})

test_that("doc_replace_text and doc_fill interpret the special characters like bkm_*", {
  mk <- function() doc_from_xml(p_xml(r_xml("Start {{t}} end", bold), ppr = ppr_center))
  d <- mk(); doc_replace_text(d, "{{t}}", "a\nb\tc")
  expect_equal(doc_text(d), "Start a\nb\tc end")
  d <- mk(); doc_replace_text(d, "{{t}}", "P1\rP2")
  expect_equal(doc_text(d), c("Start P1", "P2 end"))
  expect_equal(.count(.xml(d, d$main), "count(//w:p[w:pPr/w:jc])"), 2)
  expect_equal(.count(.xml(d, d$main), "count(//w:r[not(w:rPr/w:b)])"), 0)
  d <- mk(); doc_fill(d, list(t = "x\ry"))
  expect_equal(doc_text(d), c("Start x", "y end"))
  # Plain replacements are unchanged
  d <- mk(); doc_replace_text(d, c("{{t}}", "Start"), c("plain", "Begin"))
  expect_equal(doc_text(d), "Begin plain end")
  expect_equal(attr(d, "replacements"), 2)
})

test_that("html: <p> creates paragraphs, <br> a line break", {
  doc <- doc_from_xml(p_xml(s_xml(1, "x"), e_xml(1)))
  bkm_replace_text(doc, "x", "<p>One <b>bold</b></p><p>Two<br>lines</p>", html = TRUE)
  expect_equal(doc_text(doc), c("One bold", "Two\nlines"))
  d <- doc_new() |> doc_add_paragraph("<p>A</p>\n<p><i>B</i></p>", html = TRUE)
  expect_equal(doc_text(d), c("A", "B"))
})

test_that("bkm_repeat interprets the special characters of the values", {
  doc <- doc_from_xml(paste0(p_xml(s_xml(1, "row"), r_xml("- {{v}}"), e_xml(1)), p_xml(r_xml("End"))))
  bkm_repeat(doc, "row", data.frame(v = c("a\nb", "c\rd")))
  expect_equal(doc_text(doc), c("- a\nb", "- c", "d", "End"))
})
