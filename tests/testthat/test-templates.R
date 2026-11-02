p_xml <- function(...) paste0("<w:p>", paste0(...), "</w:p>")
r_xml <- function(t, rpr = "") sprintf('<w:r>%s<w:t xml:space="preserve">%s</w:t></w:r>', rpr, t)
s_xml <- function(id, name) sprintf('<w:bookmarkStart w:id="%s" w:name="%s"/>', id, name)
e_xml <- function(id) sprintf('<w:bookmarkEnd w:id="%s"/>', id)
paragraph_texts <- function(doc) xml2::xml_text(.find_all(.xml(doc, doc$main), "/w:document/w:body/w:p"))

# ---- doc_fill ----------------------------------------------------------------

test_that("doc_fill fills bookmarks and tags and reports missing names", {
  doc <- doc_read(word_path("example.docx"))
  expect_warning(
    doc_fill(doc, list(amount_2025 = 2000, amount_2024 = "1 000", unknown = "x")),
    "unknown")
  expect_equal(bkm_text(doc, "amount_2025"), "2000")
  expect_true("The order amount is 1 000 euros in 2024." %in% paragraph_texts(doc))
  r <- attr(doc, "fill_report")
  expect_equal(r$bookmark, c(TRUE, FALSE, FALSE))
  expect_equal(r$tags, c(0L, 1L, 0L))
})

test_that("doc_fill counts the tags of each name in a single pass", {
  doc <- doc_from_xml(paste0(p_xml(r_xml("{{a}} and {{b}}, {{a}} again")), p_xml(r_xml("{{b}}"))))
  doc_fill(doc, list(a = "A", b = "B", c = "C"), warn = FALSE)
  expect_equal(paragraph_texts(doc), c("A and B, A again", "B"))
  expect_equal(attr(doc, "fill_report")$tags, c(2L, 2L, 0L))
})

test_that("doc_fill accepts a one-row data.frame, custom tags and NA", {
  doc <- doc_from_xml(p_xml(r_xml("Name: [name], city: [city]")))
  doc_fill(doc, data.frame(name = "Smith", city = NA), tags = c("[", "]"))
  expect_equal(paragraph_texts(doc), "Name: Smith, city: ")
  expect_error(doc_fill(doc, list("unnamed")), "named")
  expect_error(doc_fill(doc, list(a = 1:2)), "length 1")
  expect_error(doc_fill(doc, list(a = 1), tags = "{{"), "two")
})

# ---- bkm_conditional ---------------------------------------------------------

test_that("bkm_conditional removes whole paragraphs without leaving a gap", {
  doc <- doc_new() |>
    doc_add_paragraph("A") |>
    doc_add_paragraph("Clause", bookmark = "clause") |>
    doc_add_paragraph("B")
  bkm_conditional(doc, "clause", FALSE)
  expect_equal(paragraph_texts(doc), c("A", "B"))
  expect_false("clause" %in% bkm_list(doc)$name)
})

test_that("bkm_conditional handles a bookmark over several paragraphs and an end outside a paragraph", {
  doc <- doc_from_xml(paste0(
    p_xml(r_xml("A")), p_xml(s_xml(1, "c"), r_xml("C1")), p_xml(r_xml("C2")), e_xml(1), p_xml(r_xml("B"))))
  bkm_conditional(doc, "c", FALSE)
  expect_equal(paragraph_texts(doc), c("A", "B"))
  # case produced by Word: bookmark end at the start of the next paragraph
  doc2 <- doc_from_xml(paste0(p_xml(s_xml(1, "c"), r_xml("C")), p_xml(e_xml(1), r_xml("B"))))
  bkm_conditional(doc2, "c", FALSE)
  expect_equal(paragraph_texts(doc2), "B")
})

test_that("bkm_conditional removes inline content and preserves other bookmarks", {
  doc <- doc_from_xml(p_xml(r_xml("Before "), s_xml(1, "c"), r_xml("middle"), s_xml(2, "n"),
                            r_xml("x"), e_xml(2), e_xml(1), r_xml(" after")))
  bkm_conditional(doc, "c", FALSE)
  expect_equal(paragraph_texts(doc), "Before  after")
  expect_equal(bkm_list(doc)$name, "n")
})

test_that("bkm_conditional keeps the block if the condition is true", {
  doc <- doc_read(word_path("example.docx")) |> bkm_conditional("my_bookmark", TRUE)
  expect_equal(bkm_text(doc, "my_bookmark"), "Middle")
  bkm_conditional(doc, "my_bookmark", TRUE, keep_bookmark = FALSE)
  expect_false("my_bookmark" %in% bkm_list(doc)$name)
  expect_true("Middle" %in% paragraph_texts(doc))
  expect_error(bkm_conditional(doc, "amount_2025", NA), "TRUE or FALSE")
})

test_that("bkm_conditional on a document produced by Word", {
  doc <- doc_read(word_path("complex.docx"))
  n <- length(paragraph_texts(doc))
  bkm_conditional(doc, "source_bookmark", FALSE)
  expect_equal(length(paragraph_texts(doc)), n - 1)
  expect_false("First item" %in% paragraph_texts(doc))
  expect_true("Second item" %in% paragraph_texts(doc))
})

test_that("bkm_conditional removes table rows, or the whole table", {
  make_table <- function(rows) paste0("<w:tbl>", paste0(rows, collapse = ""), "</w:tbl>", p_xml())
  row <- function(...) paste0("<w:tr>", paste0("<w:tc>", c(...), "</w:tc>", collapse = ""), "</w:tr>")
  doc <- doc_from_xml(make_table(c(row(p_xml(r_xml("Header"))), row(p_xml(s_xml(1, "l"), r_xml("L"), e_xml(1))))))
  bkm_conditional(doc, "l", FALSE)
  x <- .xml(doc, doc$main)
  expect_equal(.count(x, "count(//w:tr)"), 1)
  doc2 <- doc_from_xml(make_table(row(p_xml(s_xml(1, "l"), r_xml("L"), e_xml(1)))))
  bkm_conditional(doc2, "l", FALSE)
  expect_equal(.count(.xml(doc2, doc2$main), "count(//w:tbl)"), 0)
})

# ---- bkm_repeat --------------------------------------------------------------

test_that("bkm_repeat repeats paragraphs", {
  people <- data.frame(name = c("Ana", "Bob", "Cy"), age = c(30, 41, NA))
  base <- paste0(p_xml(r_xml("List:")),
                 p_xml(s_xml(1, "line"), r_xml("- {{name}} ("), r_xml("{{age}}", "<w:rPr><w:b/></w:rPr>"),
                       r_xml(" years)"), e_xml(1)),
                 p_xml(r_xml("End")))
  doc <- doc_from_xml(base)
  bkm_repeat(doc, "line", people)
  expect_equal(paragraph_texts(doc), c("List:", "- Ana (30 years)", "- Bob (41 years)", "- Cy ( years)", "End"))
  expect_equal(bkm_text(doc, "line"), "- Ana (30 years)\r- Bob (41 years)\r- Cy ( years)")
  # the NA age gives an empty text: the corresponding bold run disappears
  expect_equal(.count(.xml(doc, doc$main), "count(//w:b)"), 2)
  doc2 <- doc_from_xml(base)
  bkm_repeat(doc2, "line", people, keep_bookmark = FALSE)
  expect_equal(nrow(bkm_list(doc2)), 0)
  doc3 <- doc_from_xml(base)
  bkm_repeat(doc3, "line", people[0, ])
  expect_equal(paragraph_texts(doc3), c("List:", "End"))
})

test_that("bkm_repeat repeats table rows", {
  data <- data.frame(product = c("Apples", "Pears"), price = c("2.50", "3.10"))
  cell <- function(x) paste0("<w:tc>", x, "</w:tc>")
  doc <- doc_from_xml(paste0(
    "<w:tbl>",
    "<w:tr>", cell(p_xml(r_xml("Product"))), cell(p_xml(r_xml("Price"))), "</w:tr>",
    "<w:tr>", cell(p_xml(s_xml(1, "rows"), r_xml("{{product}}"))), cell(p_xml(r_xml("{{price}} \u20ac"), e_xml(1))), "</w:tr>",
    "<w:tr>", cell(p_xml(r_xml("Total"))), cell(p_xml(r_xml("5.60 \u20ac"))), "</w:tr>",
    "</w:tbl>", p_xml()))
  bkm_repeat(doc, "rows", data)
  x <- .xml(doc, doc$main)
  rows <- vapply(as.list(.find_all(x, "//w:tr")), function(tr) paste(xml2::xml_text(.find_all(tr, ".//w:p")), collapse = "|"), "")
  expect_equal(rows, c("Product|Price", "Apples|2.50 \u20ac", "Pears|3.10 \u20ac", "Total|5.60 \u20ac"))
  expect_equal(bkm_text(doc, "rows"), "Apples\r2.50 \u20ac\rPears\r3.10 \u20ac")
  d2 <- round_trip(doc)
  expect_equal(nrow(bkm_list(d2)), 1)
})

test_that("bkm_repeat checks its arguments", {
  doc <- doc_read(word_path("example.docx"))
  expect_error(bkm_repeat(doc, "my_bookmark", list(a = 1)), "data.frame")
  expect_error(bkm_repeat(doc, "my_bookmark", data.frame(a = 1), tags = NULL), "NULL")
})

# ---- doc_mail_merge ----------------------------------------------------------

test_that("doc_mail_merge produces one document per row", {
  dir <- file.path(tempdir(), "mail_merge")
  unlink(dir, recursive = TRUE)
  data <- data.frame(name = c("Smith", "Jo/nes"), amount_2025 = c("100", "200"),
                     amount_2024 = c("10", "20"), with_bkm = c(TRUE, FALSE))
  template <- doc_read(word_path("example.docx"))
  expect_warning(
    files <- doc_mail_merge(template, data, dir, file_name = "letter_{name}_{.row}.docx",
                            process = function(doc, row) bkm_conditional(doc, "my_bookmark", row$with_bkm)),
    "with_bkm")
  expect_equal(basename(files), c("letter_Smith_1.docx", "letter_Jo_nes_2.docx"))
  expect_true(all(file.exists(files)))
  d1 <- doc_read(files[1])
  d2 <- doc_read(files[2])
  expect_equal(bkm_text(d1, "amount_2025"), "100")
  expect_equal(bkm_text(d2, "amount_2025"), "200")
  expect_true("The order amount is 20 euros in 2024." %in% paragraph_texts(d2))
  expect_true("my_bookmark" %in% bkm_list(d1)$name)
  expect_false("my_bookmark" %in% bkm_list(d2)$name)
  # the template is unchanged
  expect_equal(bkm_text(template, "amount_2025"), "{{amount_2025}}")
  expect_error(doc_mail_merge(template, data, dir, file_name = "same.docx"), "duplicate")
})
