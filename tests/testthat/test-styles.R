test_that("doc_add_style creates a paragraph style with the right XML", {
  doc <- doc_new() |>
    doc_add_style("My quote", font = "Georgia", size = 11, bold = TRUE, italic = FALSE, color = "#c00000",
                  align = "justify", spacing_before = 6, spacing_after = 12, line_spacing = 1.5,
                  indent_left = 1, indent_right = 0.5, indent_first_line = -0.5,
                  keep_with_next = TRUE, keep_lines_together = TRUE)
  expect_equal(attr(doc, "style_id"), "Myquote")
  p <- .part_by_type(doc, doc$main, "styles")
  s <- .find1(.xml(doc, p), "/w:styles/w:style[@w:styleId='Myquote']")
  expect_equal(.attr(s, "w:type"), "paragraph")
  expect_equal(.attr(.find1(s, "w:basedOn"), "w:val"), "Normal")
  sp <- .find1(s, "w:pPr/w:spacing")
  expect_equal(c(.attr(sp, "w:before"), .attr(sp, "w:after"), .attr(sp, "w:line"), .attr(sp, "w:lineRule")),
               c("120", "240", "360", "auto"))
  ind <- .find1(s, "w:pPr/w:ind")
  expect_equal(c(.attr(ind, "w:left"), .attr(ind, "w:right"), .attr(ind, "w:hanging")), c("567", "284", "284"))
  expect_equal(.attr(.find1(s, "w:pPr/w:jc"), "w:val"), "both")
  expect_equal(.count(s, "count(w:pPr/w:keepNext | w:pPr/w:keepLines)"), 2)
  expect_equal(.attr(.find1(s, "w:rPr/w:color"), "w:val"), "C00000")
  expect_equal(.attr(.find1(s, "w:rPr/w:sz"), "w:val"), "22")
  expect_equal(.attr(.find1(s, "w:rPr/w:i"), "w:val"), "0")
  # schema order of the children of w:style and w:pPr
  expect_equal(vapply(xml2::xml_children(s), .name, ""), c("w:name", "w:basedOn", "w:next", "w:qFormat", "w:pPr", "w:rPr"))
  expect_equal(vapply(xml2::xml_children(.find1(s, "w:pPr")), .name, ""),
               c("w:keepNext", "w:keepLines", "w:spacing", "w:ind", "w:jc"))
  expect_no_error(round_trip(doc))
})

test_that("styles created by doc_add_style can be used by the insertion functions", {
  doc <- doc_new() |>
    doc_add_style("Quote", italic = TRUE, indent_left = 1) |>
    doc_add_style("Highlight", type = "character", bold = TRUE)
  doc_add_paragraph(doc, "q", style = "Quote")
  doc_add_paragraph(doc, "x", bookmark = "bk")
  bkm_replace_text(doc, "bk", "replaced", style = "Highlight")
  x <- .xml(doc, doc$main)
  expect_equal(.count(x, "count(//w:p[w:pPr/w:pStyle[@w:val='Quote']])"), 1)
  expect_equal(.count(x, "count(//w:r[w:rPr/w:rStyle[@w:val='Highlight']][w:t='replaced'])"), 1)
  bkm_replace_text(doc, "bk", "para", style = "Quote")
  expect_equal(.count(.xml(doc, doc$main), "count(//w:p[w:pPr/w:pStyle[@w:val='Quote']])"), 2)
  bkm_insert_text(doc, "bk", "!", position = "after", style = "highlight")
  expect_equal(bkm_text(doc, "bk"), "para!")
  expect_error(bkm_replace_text(doc, "bk", "z", style = "unknown"), "not found")
})

test_that("doc_add_style: errors and overwrite", {
  doc <- doc_new() |> doc_add_style("S1", bold = TRUE)
  expect_error(doc_add_style(doc, "S1"), "already exists")
  expect_error(doc_add_style(doc, "s1"), "already exists")
  doc_add_style(doc, "S1", italic = TRUE, overwrite = TRUE)
  p <- .part_by_type(doc, doc$main, "styles")
  expect_equal(.count(.xml(doc, p), "count(/w:styles/w:style[@w:styleId='S1'])"), 1)
  expect_equal(.count(.xml(doc, p), "count(//w:style[@w:styleId='S1']/w:rPr/w:b)"), 0)
  expect_error(doc_add_style(doc, "C", type = "character", align = "left"), "paragraph")
  expect_error(doc_add_style(doc, "C", color = "red"), "hexadecimal")
  expect_error(doc_add_style(doc, "C", based_on = "Heading1", type = "character"), "same type")
  expect_error(doc_add_style(doc, "C", spacing_before = -1), "spacing_before")
  expect_error(doc_add_style(doc, "!!!"), "letter or digit")
  doc_add_style(doc, "Sub", based_on = "heading 1", size = 20)
  p <- .part_by_type(doc, doc$main, "styles")
  expect_equal(.attr(.find1(.xml(doc, p), "//w:style[@w:styleId='Sub']/w:basedOn"), "w:val"), "Heading1")
})

test_that("doc_add_style: bullets defined in the numbering part of the document", {
  doc <- doc_new() |>
    doc_add_style("List", bullet = "disc", size = 10) |>
    doc_add_style("Arrows", bullet = "\u2192", bullet_font = "Segoe UI Symbol", bullet_indent = 0, bullet_spacing = 0.5)
  pn <- .part_by_type(doc, doc$main, "numbering")
  expect_false(is.na(pn))
  xn <- .xml(doc, pn)
  expect_equal(.count(xn, "count(/w:numbering/w:abstractNum)"), 2)
  expect_equal(vapply(xml2::xml_children(xml2::xml_root(xn)), .name, ""),
               c("w:abstractNum", "w:abstractNum", "w:num", "w:num"))
  ps <- .xml(doc, .part_by_type(doc, doc$main, "styles"))
  num_of <- function(id) .attr(.find1(ps, sprintf("//w:style[@w:styleId='%s']/w:pPr/w:numPr/w:numId", id)), "w:val")
  lvl_of <- function(num) {
    abs <- .attr(.find1(xn, sprintf("/w:numbering/w:num[@w:numId='%s']/w:abstractNumId", num)), "w:val")
    .find1(xn, sprintf("/w:numbering/w:abstractNum[@w:abstractNumId='%s']/w:lvl", abs))
  }
  l1 <- lvl_of(num_of("List"))
  expect_equal(.attr(.find1(l1, "w:numFmt"), "w:val"), "bullet")
  expect_equal(.attr(.find1(l1, "w:lvlText"), "w:val"), "\uf0b7")
  expect_equal(.attr(.find1(l1, "w:rPr/w:rFonts"), "w:ascii"), "Symbol")
  expect_equal(c(.attr(.find1(l1, "w:pPr/w:ind"), "w:left"), .attr(.find1(l1, "w:pPr/w:ind"), "w:hanging")), c("714", "357"))
  l2 <- lvl_of(num_of("Arrows"))
  expect_equal(.attr(.find1(l2, "w:lvlText"), "w:val"), "\u2192")
  expect_equal(.attr(.find1(l2, "w:rPr/w:rFonts"), "w:ascii"), "Segoe UI Symbol")
  # Same indentation in the style, and schema order of w:pPr
  s2 <- .find1(ps, "//w:style[@w:styleId='Arrows']")
  expect_equal(c(.attr(.find1(s2, "w:pPr/w:ind"), "w:left"), .attr(.find1(s2, "w:pPr/w:ind"), "w:hanging")), c("284", "284"))
  expect_equal(vapply(xml2::xml_children(.find1(s2, "w:pPr")), .name, ""), c("w:numPr", "w:ind"))
  # Usable, saved, and kept when inserted into another document
  doc_add_paragraph(doc, "One\rTwo", style = "List")
  d2 <- round_trip(doc)
  expect_equal(.part_by_type(d2, d2$main, "numbering"), pn)
  target <- doc_new() |> doc_add_paragraph("", bookmark = "here")
  bkm_insert_doc(target, "here", d2)
  pt <- .part_by_type(target, target$main, "numbering")
  expect_false(is.na(pt))
  num_t <- .attr(.find1(.xml(target, .part_by_type(target, target$main, "styles")), "//w:style[@w:styleId='List']/w:pPr/w:numPr/w:numId"), "w:val")
  expect_equal(.attr(.find1(.xml(target, pt), sprintf("/w:numbering/w:num[@w:numId='%s']", num_t)), "w:numId"), num_t)
})

test_that("doc_add_style: bullet errors", {
  doc <- doc_new()
  expect_error(doc_add_style(doc, "B", bullet = "disc", indent_left = 1), "bullet_indent")
  expect_error(doc_add_style(doc, "B", bullet = "disc", type = "character"), "character style")
  expect_error(doc_add_style(doc, "B", bullet_indent = 1), "require `bullet`")
  expect_error(doc_add_style(doc, "B", bullet = ""), "bullet")
  expect_error(doc_add_style(doc, "B", bullet = "disc", bullet_spacing = -1), "bullet_spacing")
  doc_add_style(doc, "Dash", bullet = "dash")
  lvl <- .find1(.xml(doc, .part_by_type(doc, doc$main, "numbering")), "//w:lvl")
  expect_equal(.attr(.find1(lvl, "w:lvlText"), "w:val"), "\u2013")
  expect_equal(.count(lvl, "count(w:rPr)"), 0)
})
