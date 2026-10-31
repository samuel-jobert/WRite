test_that("bkm_insert_docx replaces a paragraph containing only the bookmark", {
  doc <- doc_read(word_path("example.docx"))
  n_before <- .count(.xml(doc, doc$main), "count(//w:body/w:p)")
  bkm_insert_docx(doc, "empty_bookmark", word_path("to_insert.docx"))
  x <- .xml(doc, doc$main)
  # 7 paragraphs inserted, the bookmark paragraph disappears
  expect_equal(.count(x, "count(//w:body/w:p)"), n_before + 7 - 1)
  txt <- bkm_text(doc, "empty_bookmark")
  expect_match(txt, "^Document to insert\\.")
  expect_match(txt, "Normal text, bold text, normal text.$")
  expect_equal(.count(x, "count(//w:p[not(.//w:t) and w:bookmarkStart[@w:name='empty_bookmark']])"), 0)
  expect_equal(.count(x, "count(//w:body/w:p[@w14:paraId])"), n_before - 1)
})

test_that("an inserted bookmark with the same name is renamed with a suffix and its references followed", {
  target <- doc_read(word_path("example.docx")) |>
    doc_add_paragraph("Existing", bookmark = "dup") |>
    doc_add_paragraph("Existing 1", bookmark = "dup_1")
  source <- doc_from_xml(paste0(
    '<w:p><w:bookmarkStart w:id="0" w:name="dup"/><w:r><w:t>Source</w:t></w:r><w:bookmarkEnd w:id="0"/>',
    '<w:bookmarkStart w:id="1" w:name="_GoBack"/><w:bookmarkEnd w:id="1"/></w:p>',
    '<w:p><w:hyperlink w:anchor="dup"><w:r><w:t>link</w:t></w:r></w:hyperlink>',
    '<w:fldSimple w:instr=" REF dup \\h "><w:r><w:t>reference</w:t></w:r></w:fldSimple>',
    '<w:r><w:instrText xml:space="preserve"> PAGEREF dup \\h </w:instrText></w:r>',
    '<w:r><w:instrText xml:space="preserve"> REF dup_x </w:instrText></w:r></w:p>'))
  .add(.find1(.xml(target, target$main), "//w:body/w:p"), "w:bookmarkStart", "w:id" = 90, "w:name" = "_GoBack")
  expect_warning(bkm_insert_doc(target, "empty_bookmark", source), "dup -> dup_2")
  s <- bkm_list(target)
  expect_equal(s$text[s$name == "dup_2"], "Source")
  expect_equal(sum(s$name == "_GoBack"), 1)
  x <- .xml(target, target$main)
  expect_equal(.attr(.find1(x, "//w:hyperlink"), "w:anchor"), "dup_2")
  expect_equal(.attr(.find1(x, "//w:fldSimple"), "w:instr"), " REF dup_2 \\h ")
  instr <- xml2::xml_text(.find_all(x, "//w:instrText"))
  expect_equal(instr, c(" PAGEREF dup_2 \\h ", " REF dup_x "))
  expect_false(any(duplicated(s$id)))
})

test_that("bkm_insert_docx splits a paragraph containing text", {
  doc <- doc_read(word_path("example.docx")) |>
    bkm_insert_docx("amount_2025", word_path("to_insert.docx"))
  x <- .xml(doc, doc$main)
  texts <- xml2::xml_text(.find_all(x, "//w:body/w:p"))
  i <- which(texts == "The order amount is ")
  expect_length(i, 1)
  expect_equal(texts[i + 1], "Document to insert.")
  expect_true(" euros in 2025." %in% texts)
  expect_false(grepl("{{amount_2025}}", paste(texts, collapse = ""), fixed = TRUE))
})

test_that("position = 'after' keeps the bookmark content", {
  doc <- doc_read(word_path("example.docx")) |>
    bkm_insert_docx("my_bookmark", word_path("to_insert.docx"), position = "after")
  expect_match(bkm_text(doc, "my_bookmark"), "^Middle\nDocument to insert")
})

test_that("bkm_insert_doc inserts a document object without modifying it", {
  src <- doc_read(word_path("to_insert.docx"))
  before <- .serialize(.xml(src, src$main))
  doc <- doc_read(word_path("example.docx")) |>
    bkm_insert_doc("empty_bookmark", src)
  expect_equal(.serialize(.xml(src, src$main)), before)
  expect_match(bkm_text(doc, "empty_bookmark"), "This title will be inserted")
})

test_that("a document can be inserted into itself", {
  doc <- doc_read(word_path("to_insert.docx")) |> doc_add_paragraph("", bookmark = "here")
  expect_warning(bkm_insert_doc(doc, "here", doc), "here -> here_1")
  expect_match(bkm_text(doc, "here"), "^Document to insert")
})

test_that("keep_bookmark = FALSE removes the bookmark after insertion", {
  doc <- doc_read(word_path("example.docx")) |>
    bkm_insert_docx("empty_bookmark", word_path("to_insert.docx"), keep_bookmark = FALSE)
  expect_false("empty_bookmark" %in% bkm_list(doc)$name)
})

test_that("styles, numbering, image, note and link are merged", {
  doc <- doc_read(word_path("example.docx")) |>
    bkm_insert_docx("empty_bookmark", word_path("complex.docx"))
  d2 <- round_trip(doc)
  x <- .xml(d2, d2$main)
  styles <- .attr(.find_all(.xml(d2, "word/styles.xml"), "//w:style"), "w:styleId")
  expect_true("CustomSourceStyle" %in% styles)
  expect_equal(sum(styles == "Heading1"), 1)
  # numbering: the numId used exists in numbering.xml
  num <- .attr(.find1(x, "//w:numPr/w:numId"), "w:val")
  p_num <- .part_by_type(d2, d2$main, "numbering")
  expect_false(is.na(p_num))
  expect_equal(.count(.xml(d2, p_num), sprintf("count(//w:num[@w:numId='%s'])", num)), 1)
  # image: the relationship points to an existing file
  rid <- .attr(.find1(x, "//a:blip"), "r:embed")
  rels <- .rels_df(d2, d2$main)
  target <- .absolute_target(d2$main, rels$target[rels$id == rid])
  expect_true(file.exists(file.path(d2$dir, target)))
  # external link
  rid_link <- .attr(.find1(x, "//w:hyperlink"), "r:id")
  expect_equal(rels$target[rels$id == rid_link], "https://www.r-project.org")
  # footnote copied
  note_id <- .attr(.find1(x, "//w:footnoteReference"), "w:id")
  notes <- .xml(d2, .part_by_type(d2, d2$main, "footnotes"))
  expect_match(xml2::xml_text(.find1(notes, sprintf("//w:footnote[@w:id='%s']", note_id))), "Note content")
  # the bookmark of the inserted document is kept
  expect_true("source_bookmark" %in% bkm_list(d2)$name)
})

test_that("two successive insertions produce distinct identifiers", {
  doc <- doc_read(word_path("example.docx")) |>
    bkm_insert_docx("empty_bookmark", word_path("complex.docx"))
  expect_warning(bkm_insert_docx(doc, "my_bookmark", word_path("complex.docx")),
                 "source_bookmark -> source_bookmark_1")
  s <- bkm_list(doc)
  expect_true(all(c("source_bookmark", "source_bookmark_1") %in% s$name))
  expect_false(any(duplicated(s$id)))
  x <- .xml(doc, doc$main)
  docpr_ids <- xml2::xml_attr(.find_all(x, "//wp:docPr"), "id")
  expect_false(any(duplicated(docpr_ids)))
  note_ids <- .attr(.find_all(x, "//w:footnoteReference"), "w:id")
  expect_length(unique(note_ids), 2)
})
