#' Add a style to the document
#'
#' Creates a real Word style (stored in the document) that can then be used
#' by name in the `style` argument of [doc_add_paragraph()],
#' [bkm_insert_text()] and [bkm_replace_text()], and that appears in Word's
#' style gallery. Arguments left to `NULL` are inherited from the base style.
#'
#' A paragraph style applies to the whole paragraph; a character style
#' (`type = "character"`) only to the text it is applied to, and accepts only
#' the character arguments (`font` to `color`). Direct formatting given to the
#' insertion functions (`bold = TRUE`...) takes precedence over the style.
#'
#' "Keep with previous" is not a style property in Word (it is "keep with
#' next" set on the previous paragraph): use the `keep_with_previous` argument
#' of the insertion functions for it.
#'
#' @param doc WRite document object.
#' @param name Name of the style (shown in Word). Its identifier is derived
#'   from it (letters and digits only).
#' @param font Font name.
#' @param size Font size in points.
#' @param bold,italic,strike `TRUE`/`FALSE`.
#' @param underline `TRUE`, `FALSE` or Word underline type (`"double"`...).
#' @param color Text color as hexadecimal RGB (`"FF0000"` or `"#FF0000"`).
#' @param align Paragraph alignment: `"left"`, `"center"`, `"right"` or
#'   `"justify"`.
#' @param spacing_before,spacing_after Space before / after the paragraph, in
#'   points.
#' @param line_spacing Line spacing as a multiple of the line height (`1` =
#'   single, `1.5`, `2`...).
#' @param indent_left,indent_right Indentation in cm.
#' @param indent_first_line Indentation of the first line in cm (negative =
#'   hanging indent).
#' @param keep_with_next Keep the paragraph on the same page as the next one?
#' @param keep_lines_together Keep all the lines of the paragraph on the same
#'   page?
#' @param bullet Bullet of the paragraphs of the style (paragraph style only):
#'   a predefined name (`"disc"`, `"circle"`, `"square"`, `"dash"`, `"arrow"`,
#'   `"check"`, see the "Bullets" section) or any character (`"→"`,
#'   `"★"`...). `NULL` (default) = no bullet.
#' @param bullet_font Font of the bullet (by default the font of the predefined
#'   bullet, or that of the text for another character).
#' @param bullet_indent Position of the bullet from the left margin, in cm
#'   (default 0.63, as in Word).
#' @param bullet_spacing Distance between the bullet and the start of the text,
#'   in cm (default 0.63, as in Word). The text (and the following lines) start
#'   at `bullet_indent + bullet_spacing`.
#' @param type `"paragraph"` (default) or `"character"`.
#' @param based_on Style (identifier or name) the new one is based on (default
#'   `"Normal"` for a paragraph style; none for a character style).
#' @param overwrite Replace an existing style of the same name?
#' @return `doc`, invisibly. The identifier of the style is available in the
#'   `"style_id"` attribute of the returned value.
#' @section Bullets:
#' A style with a `bullet` is a bulleted list style: each paragraph of this
#' style starts with the bullet. The bullet is defined in the document (list
#' definition in its numbering part, created if needed), so it is kept when
#' the document is opened in Word and when paragraphs of this style are
#' inserted in another document with [bkm_insert_doc()].
#'
#' | `bullet` | Bullet | Character, font |
#' |---|---|---|
#' | `"disc"` | black disc (Word default) | U+F0B7, Symbol |
#' | `"circle"` | white circle | `"o"`, Courier New |
#' | `"square"` | black square | U+F0A7, Wingdings |
#' | `"dash"` | en dash | U+2013, font of the text |
#' | `"arrow"` | arrowhead | U+F0D8, Wingdings |
#' | `"check"` | check mark | U+F0FC, Wingdings |
#'
#' The position of the bullet and of the text are set with `bullet_indent` and
#' `bullet_spacing` (like "Adjust list indents" in Word), which replace
#' `indent_left` and `indent_first_line` for a bulleted style:
#' ```
#' margin
#'  |<- bullet_indent ->*<- bullet_spacing ->Text of the first line
#'  |                                        following lines, aligned on the text
#' ```
#' @examples
#' doc <- doc_new() |>
#'   doc_add_style("Quote", font = "Georgia", size = 11, italic = TRUE,
#'                 color = "444444", indent_left = 1, spacing_after = 12,
#'                 line_spacing = 1.5, keep_lines_together = TRUE) |>
#'   doc_add_style("Highlight", type = "character", bold = TRUE, color = "C00000")
#'
#' doc_add_paragraph(doc, "To be or not to be.", style = "Quote")
#' doc_add_paragraph(doc, "Result: ", bookmark = "res")
#' bkm_insert_text(doc, "res", "important", position = "after", style = "Highlight")
#' doc_text(doc)
#'
#' # Bulleted lists
#' doc_add_style(doc, "List", bullet = "disc", size = 10)
#' doc_add_style(doc, "Sub-list", bullet = "dash", bullet_indent = 1.27,
#'               bullet_spacing = 0.5)
#' doc_add_style(doc, "Arrows", bullet = "\u2192", bullet_font = "Segoe UI Symbol",
#'               bullet_indent = 0)
#' doc_add_paragraph(doc, "First point\rSecond point", style = "List")
#' doc_add_paragraph(doc, "Detail", style = "Sub-list")
#' @export
doc_add_style <- function(doc, name, font = NULL, size = NULL, bold = NULL, italic = NULL,
                          underline = NULL, strike = NULL, color = NULL, align = NULL,
                          spacing_before = NULL, spacing_after = NULL, line_spacing = NULL,
                          indent_left = NULL, indent_right = NULL, indent_first_line = NULL,
                          keep_with_next = NULL, keep_lines_together = NULL,
                          bullet = NULL, bullet_font = NULL, bullet_indent = NULL,
                          bullet_spacing = NULL,
                          type = c("paragraph", "character"), based_on = NULL,
                          overwrite = FALSE) {
  .check_doc(doc)
  type <- match.arg(type)
  .check_flag(overwrite, "overwrite")
  if (!is.character(name) || length(name) != 1 || is.na(name) || !nzchar(trimws(name))) {
    stop("`name` must be a non-empty string.", call. = FALSE)
  }
  if (!is.null(color)) {
    color <- sub("^#", "", color)
    if (!is.character(color) || length(color) != 1 || !grepl("^[0-9A-Fa-f]{6}$", color)) {
      stop("`color` must be an hexadecimal RGB color (e.g. \"FF0000\").", call. = FALSE)
    }
    color <- toupper(color)
  }
  f <- .format(bold, italic, underline, size, font, strike = strike, color = color)
  num <- function(x, nm, min = -Inf) {
    if (!is.null(x) && !(is.numeric(x) && length(x) == 1 && !is.na(x) && x >= min)) {
      stop(sprintf("`%s` must be a number%s.", nm, if (min == 0) " >= 0" else ""), call. = FALSE)
    }
  }
  num(spacing_before, "spacing_before", 0)
  num(spacing_after, "spacing_after", 0)
  num(line_spacing, "line_spacing", 0)
  num(indent_left, "indent_left")
  num(indent_right, "indent_right")
  num(indent_first_line, "indent_first_line")
  if (!is.null(align)) {
    align <- match.arg(align, c("left", "center", "right", "justify"))
  }
  if (!is.null(keep_with_next)) .check_flag(keep_with_next, "keep_with_next")
  if (!is.null(keep_lines_together)) .check_flag(keep_lines_together, "keep_lines_together")
  num(bullet_indent, "bullet_indent")
  num(bullet_spacing, "bullet_spacing", 0)
  if (!is.null(bullet_font) && !(is.character(bullet_font) && length(bullet_font) == 1 && !is.na(bullet_font) && nzchar(bullet_font))) {
    stop("`bullet_font` must be a font name.", call. = FALSE)
  }
  blt <- NULL
  if (is.null(bullet)) {
    if (!is.null(bullet_font) || !is.null(bullet_indent) || !is.null(bullet_spacing)) {
      stop("`bullet_font`, `bullet_indent` and `bullet_spacing` require `bullet`.", call. = FALSE)
    }
  } else {
    if (type == "character") stop("A character style cannot have a bullet.", call. = FALSE)
    if (!is.null(indent_left) || !is.null(indent_first_line)) {
      stop("With `bullet`, use `bullet_indent` and `bullet_spacing` instead of `indent_left` and `indent_first_line`.", call. = FALSE)
    }
    blt <- .bullet_spec(bullet, bullet_font)
    blt$spacing <- .or(bullet_spacing, 0.63)
    blt$left <- .or(bullet_indent, 0.63) + blt$spacing
  }
  para_args <- list(align, spacing_before, spacing_after, line_spacing, indent_left, indent_right,
                    indent_first_line, keep_with_next, keep_lines_together, blt)
  has_para <- !all(vapply(para_args, is.null, TRUE))
  if (type == "character" && has_para) {
    stop("A character style cannot have paragraph arguments (align, spacing, indent, keep...).", call. = FALSE)
  }

  p <- .part_by_type(doc, doc$main, "styles")
  if (is.na(p)) stop("The document does not contain any styles.", call. = FALSE)
  x <- .xml(doc, p)
  root <- .find1(x, "/w:styles")
  id <- gsub("[^A-Za-z0-9]", "", name)
  if (!nzchar(id)) stop("`name` must contain at least one letter or digit.", call. = FALSE)
  existing <- .find_all(x, sprintf("/w:styles/w:style[@w:styleId=%s or translate(w:name/@w:val, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')=%s]",
                                   .xpath_string(id), .xpath_string(tolower(name))))
  if (length(existing) > 0) {
    if (!overwrite) stop(sprintf("Style '%s' already exists (use `overwrite = TRUE`).", name), call. = FALSE)
    xml2::xml_remove(existing)
  }
  base <- NULL
  if (!is.null(based_on)) {
    st <- .resolve_style(doc, based_on)
    if (st$type != type) stop("`based_on` must be a style of the same type.", call. = FALSE)
    base <- st$id
  } else if (type == "paragraph") {
    base <- if (.count(x, "count(/w:styles/w:style[@w:styleId='Normal'])") > 0) "Normal" else NULL
  }

  s <- .add(root, "w:style", "w:type" = type, "w:styleId" = id)
  .add(s, "w:name", "w:val" = name)
  if (!is.null(base)) .add(s, "w:basedOn", "w:val" = base)
  if (type == "paragraph") .add(s, "w:next", "w:val" = id)
  .add(s, "w:qFormat")
  if (has_para) {
    ppr <- .add(s, "w:pPr")
    if (!is.null(keep_with_next)) .add(ppr, "w:keepNext", "w:val" = if (keep_with_next) "1" else "0")
    if (!is.null(keep_lines_together)) .add(ppr, "w:keepLines", "w:val" = if (keep_lines_together) "1" else "0")
    twips <- function(cm) as.character(round(cm * 567))
    if (!is.null(blt)) {
      num_id <- .add_bullet_numbering(doc, blt$char, blt$font, twips(blt$left), twips(blt$spacing))
      np <- .add(ppr, "w:numPr")
      .add(np, "w:ilvl", "w:val" = "0")
      .add(np, "w:numId", "w:val" = num_id)
      indent_left <- blt$left
      indent_first_line <- -blt$spacing
    }
    if (!is.null(spacing_before) || !is.null(spacing_after) || !is.null(line_spacing)) {
      sp <- .add(ppr, "w:spacing")
      if (!is.null(spacing_before)) .set_attr(sp, "w:before", as.character(round(spacing_before * 20)))
      if (!is.null(spacing_after)) .set_attr(sp, "w:after", as.character(round(spacing_after * 20)))
      if (!is.null(line_spacing)) {
        .set_attr(sp, "w:line", as.character(round(line_spacing * 240)))
        .set_attr(sp, "w:lineRule", "auto")
      }
    }
    if (!is.null(indent_left) || !is.null(indent_right) || !is.null(indent_first_line)) {
      ind <- .add(ppr, "w:ind")
      if (!is.null(indent_left)) .set_attr(ind, "w:left", twips(indent_left))
      if (!is.null(indent_right)) .set_attr(ind, "w:right", twips(indent_right))
      if (!is.null(indent_first_line)) {
        if (indent_first_line >= 0) {
          .set_attr(ind, "w:firstLine", twips(indent_first_line))
        } else {
          .set_attr(ind, "w:hanging", twips(-indent_first_line))
        }
      }
    }
    if (!is.null(align)) {
      .add(ppr, "w:jc", "w:val" = c(left = "left", center = "center", right = "right", justify = "both")[[align]])
    }
  }
  .add_rpr(s, f, where = NULL)
  attr(doc, "style_id") <- id
  invisible(doc)
}

# Predefined bullets: character and font (NA = font of the text).
.bullets <- list(
  disc = list(char = "\uf0b7", font = "Symbol"),
  circle = list(char = "o", font = "Courier New"),
  square = list(char = "\uf0a7", font = "Wingdings"),
  dash = list(char = "\u2013", font = NA_character_),
  arrow = list(char = "\uf0d8", font = "Wingdings"),
  check = list(char = "\uf0fc", font = "Wingdings")
)

.bullet_spec <- function(bullet, font) {
  if (!is.character(bullet) || length(bullet) != 1 || is.na(bullet) || !nzchar(bullet)) {
    stop("`bullet` must be a predefined bullet name or a character.", call. = FALSE)
  }
  spec <- if (bullet %in% names(.bullets)) .bullets[[bullet]] else list(char = bullet, font = NA_character_)
  if (!is.null(font)) spec$font <- font
  spec
}

# Path of the numbering part (lists), created if needed.
.numbering_part <- function(doc) {
  p <- .part_by_type(doc, doc$main, "numbering")
  if (is.na(p)) {
    p <- .free_path(doc, "word/numbering.xml")
    .xml_set(doc, p, xml2::read_xml(sprintf('<w:numbering xmlns:w="%s"/>', .ns[["w"]])))
    .add_rel(doc, doc$main, .rel_type("numbering"), .relative_path(doc$main, p))
    .ct_add_override(doc, p, .ct[["numbering"]])
  }
  p
}

# Adds a one-level bullet list definition (abstractNum + num); returns its numId.
# `left`, `hanging`: indentation of the text and of the bullet, in twips.
.add_bullet_numbering <- function(doc, char, font, left, hanging) {
  x <- .xml(doc, .numbering_part(doc))
  root <- xml2::xml_root(x)
  max_num <- max(c(0L, as.integer(.attr(.find_all(x, "/w:numbering/w:num"), "w:numId"))), na.rm = TRUE)
  max_abs <- max(c(-1L, as.integer(.attr(.find_all(x, "/w:numbering/w:abstractNum"), "w:abstractNumId"))), na.rm = TRUE)
  # Schema order: all the abstractNum, then the num, then numIdMacAtCleanup.
  abs_all <- .find_all(x, "/w:numbering/w:abstractNum")
  first_num <- .find1(x, "/w:numbering/w:num")
  cleanup <- .find1(x, "/w:numbering/w:numIdMacAtCleanup")
  abs <- if (length(abs_all) > 0) {
    .add(abs_all[[length(abs_all)]], "w:abstractNum", sibling = "after")
  } else if (!.is_missing(first_num)) {
    .add(first_num, "w:abstractNum", sibling = "before")
  } else if (!.is_missing(cleanup)) {
    .add(cleanup, "w:abstractNum", sibling = "before")
  } else {
    .add(root, "w:abstractNum")
  }
  .set_attr(abs, "w:abstractNumId", as.character(max_abs + 1L))
  .add(abs, "w:multiLevelType", "w:val" = "singleLevel")
  lvl <- .add(abs, "w:lvl", "w:ilvl" = "0")
  .add(lvl, "w:start", "w:val" = "1")
  .add(lvl, "w:numFmt", "w:val" = "bullet")
  .add(lvl, "w:lvlText", "w:val" = char)
  .add(lvl, "w:lvlJc", "w:val" = "left")
  .add(.add(lvl, "w:pPr"), "w:ind", "w:left" = left, "w:hanging" = hanging)
  if (!is.na(font)) {
    .add(.add(lvl, "w:rPr"), "w:rFonts", "w:ascii" = font, "w:hAnsi" = font, "w:cs" = font, "w:hint" = "default")
  }
  nums <- .find_all(x, "/w:numbering/w:num")
  n <- if (length(nums) > 0) {
    .add(nums[[length(nums)]], "w:num", sibling = "after")
  } else if (!.is_missing(cleanup)) {
    .add(cleanup, "w:num", sibling = "before")
  } else {
    .add(root, "w:num")
  }
  .set_attr(n, "w:numId", as.character(max_num + 1L))
  .add(n, "w:abstractNumId", "w:val" = as.character(max_abs + 1L))
  as.character(max_num + 1L)
}
