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
#' @param type `"paragraph"` (default) or `"character"`.
#' @param based_on Style (identifier or name) the new one is based on (default
#'   `"Normal"` for a paragraph style; none for a character style).
#' @param overwrite Replace an existing style of the same name?
#' @return `doc`, invisibly. The identifier of the style is available in the
#'   `"style_id"` attribute of the returned value.
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
#' @export
doc_add_style <- function(doc, name, font = NULL, size = NULL, bold = NULL, italic = NULL,
                          underline = NULL, strike = NULL, color = NULL, align = NULL,
                          spacing_before = NULL, spacing_after = NULL, line_spacing = NULL,
                          indent_left = NULL, indent_right = NULL, indent_first_line = NULL,
                          keep_with_next = NULL, keep_lines_together = NULL,
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
  para_args <- list(align, spacing_before, spacing_after, line_spacing, indent_left, indent_right,
                    indent_first_line, keep_with_next, keep_lines_together)
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
      twips <- function(cm) as.character(round(cm * 567))
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
