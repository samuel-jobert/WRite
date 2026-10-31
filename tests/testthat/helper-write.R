word_path <- function(file) testthat::test_path("..", "word", file)

# Creates a document whose body contains the given WordprocessingML XML.
doc_from_xml <- function(body_content) {
  doc <- doc_new()
  body <- .find1(.xml(doc, doc$main), "/w:document/w:body")
  frag <- xml2::read_xml(sprintf('<w:body xmlns:w="%s">%s</w:body>', .ns[["w"]], body_content))
  sect <- .find1(body, "w:sectPr")
  for (n in xml2::xml_children(frag)) xml2::xml_add_sibling(sect, n, .where = "before")
  doc
}

body_xml <- function(doc) {
  .serialize(.find1(.xml(doc, doc$main), "/w:document/w:body"))
}

# Saves then reads the document again (checks that the produced file is readable).
round_trip <- function(doc) {
  f <- tempfile(fileext = ".docx")
  doc_save(doc, f)
  doc_read(f)
}
