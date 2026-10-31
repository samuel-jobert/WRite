test_that("duplicate bookmark identifiers are fixed", {
  doc <- doc_from_xml(paste0(
    '<w:p><w:bookmarkStart w:id="5" w:name="a"/><w:r><w:t>A</w:t></w:r><w:bookmarkEnd w:id="5"/></w:p>',
    '<w:p><w:bookmarkStart w:id="5" w:name="b"/><w:r><w:t>B</w:t></w:r><w:bookmarkEnd w:id="5"/></w:p>'))
  doc_clean(doc)
  s <- bkm_list(doc)
  expect_false(any(duplicated(s$id)))
  expect_equal(s$id[s$name == "a"], "5")
  expect_equal(bkm_text(doc, "a"), "A")
  expect_equal(bkm_text(doc, "b"), "B")
  expect_equal(attr(doc, "cleaning")[["bookmark_ids"]], 1)
})

test_that("overlapping duplicate identifiers are fixed", {
  doc <- doc_from_xml(paste0(
    '<w:p><w:bookmarkStart w:id="1" w:name="a"/><w:r><w:t>A</w:t></w:r>',
    '<w:bookmarkStart w:id="1" w:name="b"/><w:r><w:t>B</w:t></w:r>',
    '<w:bookmarkEnd w:id="1"/><w:r><w:t>C</w:t></w:r><w:bookmarkEnd w:id="1"/></w:p>'))
  doc_clean(doc)
  s <- bkm_list(doc)
  expect_false(any(duplicated(s$id)))
  expect_equal(bkm_text(doc, "a"), "AB")
  expect_equal(bkm_text(doc, "b"), "BC")
})

test_that("runs with identical formatting are merged", {
  doc <- doc_read(word_path("to_insert.docx")) |> doc_clean()
  x <- .xml(doc, doc$main)
  p <- .find_all(x, "//w:body/w:p")[[3]]
  expect_equal(.count(p, "count(w:r)"), 1)
  expect_equal(.count(p, "count(w:r/w:t)"), 1)
  expect_equal(xml2::xml_text(p), "This title will be inserted into another document")
  # runs with different formatting: not merged
  p6 <- .find_all(x, "//w:body/w:p")[[6]]
  expect_equal(.count(p6, "count(w:r)"), 3)
  expect_gt(attr(doc, "cleaning")[["runs_merged"]], 0)
})

test_that("runs separated by a bookmark or containing a field are not merged", {
  doc <- doc_from_xml(paste0(
    '<w:p><w:r><w:t>A</w:t></w:r><w:bookmarkStart w:id="1" w:name="s"/><w:r><w:t>B</w:t></w:r>',
    '<w:bookmarkEnd w:id="1"/><w:r><w:fldChar w:fldCharType="begin"/></w:r><w:r><w:t xml:space="preserve"> C</w:t></w:r>',
    '<w:r><w:t>D</w:t></w:r></w:p>'))
  doc_clean(doc)
  p <- .find1(.xml(doc, doc$main), "//w:body/w:p")
  expect_equal(.count(p, "count(w:r)"), 4)
  expect_equal(bkm_text(doc, "s"), "B")
  expect_match(body_xml(doc), '<w:t xml:space="preserve"> CD</w:t>', fixed = TRUE)
})

test_that("the cleaned document stays readable", {
  d2 <- doc_read(word_path("example.docx")) |> doc_clean() |> round_trip()
  expect_equal(nrow(bkm_list(d2)), 3)
  expect_match(body_xml(d2), "The order amount is {{amount_2024}} euros in 2024.", fixed = TRUE)
})

test_that("doc_clean: spelling marks", {
  doc <- doc_from_xml('<w:p><w:r><w:t>a</w:t></w:r><w:proofErr w:type="spellStart"/><w:r><w:rPr><w:b/></w:rPr><w:t>b</w:t></w:r><w:proofErr w:type="spellEnd"/></w:p>')
  doc_clean(doc, merge_runs = FALSE)
  expect_equal(.count(.xml(doc, doc$main), "count(//w:proofErr)"), 2)
  doc_clean(doc, merge_runs = FALSE, spelling = TRUE)
  expect_equal(.count(.xml(doc, doc$main), "count(//w:proofErr)"), 0)
  expect_equal(attr(doc, "cleaning")[["spelling"]], 2)
})

test_that("doc_clean: comments", {
  doc <- doc_from_xml('<w:p><w:commentRangeStart w:id="0"/><w:r><w:t>a</w:t></w:r><w:commentRangeEnd w:id="0"/><w:r><w:commentReference w:id="0"/></w:r></w:p>')
  doc_clean(doc, comments = TRUE)
  x <- .xml(doc, doc$main)
  expect_equal(.count(x, "count(//w:commentRangeStart | //w:commentRangeEnd | //w:commentReference)"), 0)
  expect_equal(doc_text(doc), "a")
})

test_that("doc_clean: revisions are accepted", {
  doc <- doc_from_xml(paste0('<w:p><w:r><w:t>keep </w:t></w:r>',
    '<w:ins w:id="1" w:author="x"><w:r><w:t>added </w:t></w:r></w:ins>',
    '<w:del w:id="2" w:author="x"><w:r><w:delText>gone</w:delText></w:r></w:del>',
    '<w:r><w:rPr><w:b/><w:rPrChange w:id="3" w:author="x"><w:rPr/></w:rPrChange></w:rPr><w:t>end</w:t></w:r></w:p>'))
  doc_clean(doc, revisions = TRUE, merge_runs = FALSE)
  x <- .xml(doc, doc$main)
  expect_equal(.count(x, "count(//w:ins | //w:del | //w:delText | //w:rPrChange)"), 0)
  expect_equal(doc_text(doc), "keep added end")
  expect_equal(.count(x, "count(//w:rPr/w:b)"), 1)
})

test_that("doc_clean: all and explicit FALSE", {
  xml <- '<w:p><w:r><w:t>a</w:t></w:r><w:proofErr w:type="spellStart"/><w:ins w:id="1" w:author="x"><w:r><w:t>b</w:t></w:r></w:ins></w:p>'
  d1 <- doc_from_xml(xml)
  doc_clean(d1, all = TRUE)
  expect_equal(.count(.xml(d1, d1$main), "count(//w:ins | //w:proofErr)"), 0)
  d2 <- doc_from_xml(xml)
  doc_clean(d2, all = TRUE, revisions = FALSE, merge_runs = FALSE)
  expect_equal(.count(.xml(d2, d2$main), "count(//w:ins)"), 1)
  expect_equal(.count(.xml(d2, d2$main), "count(//w:proofErr)"), 0)
})

test_that("doc_clean: a deleted paragraph mark joins the paragraphs", {
  doc <- doc_from_xml(paste0(
    '<w:p><w:pPr><w:rPr><w:del w:id="1" w:author="x"/></w:rPr></w:pPr><w:r><w:t>A </w:t></w:r></w:p>',
    '<w:p><w:pPr><w:rPr><w:del w:id="2" w:author="x"/></w:rPr></w:pPr><w:r><w:t>B </w:t></w:r></w:p>',
    '<w:p><w:r><w:t>C</w:t></w:r></w:p><w:p><w:r><w:t>D</w:t></w:r></w:p>'))
  doc_clean(doc, revisions = TRUE)
  expect_equal(doc_text(doc), c("A B C", "D"))
})
