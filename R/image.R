#' Insert an image on a bookmark
#'
#' Inserts an inline image (PNG, JPEG, GIF or BMP) in the text, on a
#' bookmark. By default, the image replaces the content of the bookmark.
#'
#' If only the width or only the height is given, the other dimension is
#' computed to keep the proportions. With no dimension, the image is inserted
#' at its native size (96 points per inch).
#'
#' @param doc WRite document object.
#' @param name Bookmark name.
#' @param image Path of the image file.
#' @param width,height Dimensions in centimeters (optional).
#' @param position `"replace"` (replaces the content of the bookmark),
#'   `"before"` (at the start of the bookmark) or `"after"` (at the end of the
#'   bookmark).
#' @param alt_text Alternative text of the image (accessibility).
#' @param keep_bookmark Keep the bookmark around the image?
#' @return `doc`, invisibly.
#' @section Illustration:
#' ```
#' Before                      : Logo: [logo: (logo here)]
#' position = "replace"        : Logo: [logo: <image>]
#' position = "after"          : Logo: [logo: (logo here)<image>]
#' ```
#' @examples
#' img <- file.path(R.home("doc"), "html", "logo.jpg")   # R logo shipped with R
#' doc <- doc_read(write_example("example"))
#'
#' # Imposed width (5 cm), height computed to keep the proportions
#' bkm_insert_image(doc, "my_bookmark", img, width = 5)
#'
#' # Imposed height, image added after the text of the bookmark
#' bkm_insert_image(doc, "amount_2025", img, height = 0.5, position = "after")
#' bkm_text(doc, "amount_2025")   # the text is kept
#'
#' # R plot inserted as an image
#' png_tmp <- tempfile(fileext = ".png")
#' png(png_tmp, width = 800, height = 500, res = 120)
#' plot(cars, main = "Stopping distance")
#' dev.off()
#' bkm_insert_image(doc, "empty_bookmark", png_tmp, width = 15,
#'                  alt_text = "Distance / speed plot")
#' doc_save(doc, tempfile(fileext = ".docx"))
#' @export
bkm_insert_image <- function(doc, name, image, width = NULL, height = NULL,
                             position = c("replace", "before", "after"),
                             alt_text = basename(image), keep_bookmark = TRUE) {
  .check_doc(doc)
  position <- match.arg(position)
  if (!is.character(image) || length(image) != 1 || !file.exists(image)) stop("Image not found: ", image, call. = FALSE)
  ext <- tolower(tools::file_ext(image))
  types <- c(png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg", gif = "image/gif", bmp = "image/bmp")
  if (!ext %in% names(types)) stop("Unsupported image format (png, jpg, jpeg, gif, bmp): ", ext, call. = FALSE)
  for (d in list(width, height)) {
    if (!is.null(d) && !(is.numeric(d) && length(d) == 1 && d > 0)) stop("`width` and `height` must be positive numbers (cm).", call. = FALSE)
  }
  emu <- .emu_dimensions(image, width, height)
  .bkm_guard({
  bkm <- .bkm_find(doc, name)

  path <- .free_path(doc, paste0("word/media/image1.", ext))
  dir.create(file.path(doc$dir, "word", "media"), recursive = TRUE, showWarnings = FALSE)
  file.copy(image, file.path(doc$dir, path))
  .ct_ensure_default(doc, ext, types[[ext]])
  rid <- .add_rel(doc, bkm$part, .rel_type("image"), .relative_path(bkm$part, path))
  id <- .docpr_max(doc) + 1L

  run <- .fragment_run(sprintf(paste0(
    '<w:drawing><wp:inline distT="0" distB="0" distL="0" distR="0">',
    '<wp:extent cx="%1$.0f" cy="%2$.0f"/><wp:effectExtent l="0" t="0" r="0" b="0"/>',
    '<wp:docPr id="%3$d" name="Image %3$d" descr="%4$s"/>',
    '<wp:cNvGraphicFramePr><a:graphicFrameLocks noChangeAspect="1"/></wp:cNvGraphicFramePr>',
    '<a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture">',
    '<pic:pic><pic:nvPicPr><pic:cNvPr id="%3$d" name="%5$s"/><pic:cNvPicPr/></pic:nvPicPr>',
    '<pic:blipFill><a:blip r:embed="%6$s"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill>',
    '<pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="%1$.0f" cy="%2$.0f"/></a:xfrm>',
    '<a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr></pic:pic>',
    '</a:graphicData></a:graphic></wp:inline></w:drawing>'),
    emu[1], emu[2], id, .xml_escape(alt_text), .xml_escape(basename(path)), rid))

  if (position == "replace") .empty_range(bkm)
  if (position == "after") .insert_run_node(bkm$end, "before", run) else .insert_run_node(bkm$start, "after", run)
  if (!isTRUE(keep_bookmark)) .remove_markers(bkm)
  }, NULL)
  invisible(doc)
}

.xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

# Builds a w:r (in a temporary document) from its XML content.
.fragment_run <- function(content) {
  ns <- c(w = .ns[["w"]], r = .ns[["r"]], wp = .ns[["wp"]], a = .ns[["a"]],
          pic = "http://schemas.openxmlformats.org/drawingml/2006/picture")
  ns <- ns[names(ns) == "w" | vapply(paste0(names(ns), ":"), grepl, TRUE, content, fixed = TRUE)]
  decl <- paste(sprintf('xmlns:%s="%s"', names(ns), ns), collapse = " ")
  xml2::xml_root(xml2::read_xml(sprintf("<w:r %s>%s</w:r>", decl, content)))
}

# Copies the run `run` next to the marker (in a new paragraph if the marker is outside a paragraph).
.insert_run_node <- function(anchor, where, run) {
  if (is.null(.paragraph_of(anchor))) {
    p <- .add(anchor, "w:p", sibling = where)
    xml2::xml_add_child(p, run)
  } else {
    xml2::xml_add_sibling(anchor, run, .where = where)
  }
  invisible()
}

# Dimensions (width, height) in EMU: 1 cm = 360000 EMU, 1 pixel at 96 dpi = 9525 EMU.
.emu_dimensions <- function(image, width, height) {
  if (!is.null(width) && !is.null(height)) return(c(width, height) * 360000)
  px <- .pixel_dimensions(image)
  if (is.null(px)) stop("Unreadable image dimensions: specify `width` and `height`.", call. = FALSE)
  if (!is.null(width)) return(c(width, width * px[2] / px[1]) * 360000)
  if (!is.null(height)) return(c(height * px[1] / px[2], height) * 360000)
  px * 9525
}

# Width and height in pixels read from the file header (NULL if the format is not recognized).
.pixel_dimensions <- function(image) {
  b <- as.integer(readBin(image, "raw", n = min(file.size(image), 2e6)))
  be <- function(i, n) sum(b[i:(i + n - 1)] * 256^((n - 1):0))
  le <- function(i, n) sum(b[i:(i + n - 1)] * 256^(0:(n - 1)))
  if (length(b) > 24 && all(b[1:4] == c(0x89, 0x50, 0x4E, 0x47))) return(c(be(17, 4), be(21, 4)))
  if (length(b) > 10 && all(b[1:4] == c(0x47, 0x49, 0x46, 0x38))) return(c(le(7, 2), le(9, 2)))
  if (length(b) > 26 && all(b[1:2] == c(0x42, 0x4D))) {
    h <- le(23, 4)
    if (h >= 2^31) h <- 2^32 - h
    return(c(le(19, 4), h))
  }
  if (length(b) > 4 && all(b[1:2] == c(0xFF, 0xD8))) {
    i <- 3
    while (i + 8 <= length(b)) {
      if (b[i] != 0xFF) return(NULL)
      type <- b[i + 1]
      if (type %in% c(0xC0:0xC3, 0xC5:0xC7, 0xC9:0xCB, 0xCD:0xCF)) return(c(be(i + 7, 2), be(i + 5, 2)))
      i <- i + 2 + be(i + 2, 2)
    }
  }
  NULL
}
