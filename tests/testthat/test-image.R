png_image <- function(width = 200, height = 100) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = width, height = height)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  grDevices::dev.off()
  f
}

test_that("image dimensions are read from the header", {
  expect_equal(.pixel_dimensions(png_image(200, 100)), c(200, 100))
  logo <- file.path(R.home("doc"), "html", "logo.jpg")
  skip_if_not(file.exists(logo))
  px <- .pixel_dimensions(logo)
  expect_length(px, 2)
  expect_true(all(px > 0))
})

test_that("bkm_insert_image replaces the bookmark content with the image", {
  img <- png_image(200, 100)
  doc <- doc_read(word_path("example.docx")) |>
    bkm_insert_image("my_bookmark", img, width = 5)
  d2 <- round_trip(doc)
  x <- .xml(d2, d2$main)
  expect_equal(bkm_text(d2, "my_bookmark"), "")
  expect_false(grepl("Middle", body_xml(d2), fixed = TRUE))
  ext <- .find1(x, "//wp:inline/wp:extent")
  expect_equal(xml2::xml_attr(ext, "cx"), "1800000")
  expect_equal(xml2::xml_attr(ext, "cy"), "900000")
  rid <- .attr(.find1(x, "//a:blip"), "r:embed")
  rels <- .rels_df(d2, d2$main)
  target <- .absolute_target(d2$main, rels$target[rels$id == rid])
  expect_true(file.exists(file.path(d2$dir, target)))
  expect_equal(.ct_default(d2, "png"), "image/png")
  # the image is between the bookmark markers
  expect_equal(.count(x, "count(//w:bookmarkStart[@w:name='my_bookmark']/following-sibling::*[1][self::w:r/w:drawing])"), 1)
})

test_that("bkm_insert_image: position, native size, JPEG, several images", {
  logo <- file.path(R.home("doc"), "html", "logo.jpg")
  skip_if_not(file.exists(logo))
  doc <- doc_read(word_path("example.docx")) |>
    bkm_insert_image("my_bookmark", png_image(96, 48), position = "after") |>
    bkm_insert_image("empty_bookmark", logo, height = 2, keep_bookmark = FALSE)
  expect_equal(bkm_text(doc, "my_bookmark"), "Middle")
  expect_false("empty_bookmark" %in% bkm_list(doc)$name)
  x <- .xml(doc, doc$main)
  ext <- .find_all(x, "//wp:extent")
  expect_equal(xml2::xml_attr(ext[[1]], "cx"), as.character(96 * 9525))
  expect_equal(xml2::xml_attr(ext[[2]], "cy"), "720000")
  ids <- xml2::xml_attr(.find_all(x, "//wp:docPr"), "id")
  expect_false(any(duplicated(ids)))
  expect_length(list.files(file.path(doc$dir, "word", "media")), 2)
})

test_that("bkm_insert_image checks its arguments", {
  doc <- doc_read(word_path("example.docx"))
  expect_error(bkm_insert_image(doc, "my_bookmark", "missing.png"), "not found")
  f <- tempfile(fileext = ".svg")
  writeLines("<svg/>", f)
  expect_error(bkm_insert_image(doc, "my_bookmark", f), "Unsupported")
  expect_error(bkm_insert_image(doc, "my_bookmark", png_image(), width = -2), "positive")
})
