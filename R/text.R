# Building formatted text runs and handling styles

# Validated formatting parameters.
.format <- function(bold = NULL, italic = NULL, underline = NULL, size = NULL,
                    font = NULL, char_style = NULL, strike = NULL, color = NULL) {
  if (!is.null(bold) && !(is.logical(bold) && length(bold) == 1 && !is.na(bold))) stop("`bold` must be TRUE or FALSE.", call. = FALSE)
  if (!is.null(italic) && !(is.logical(italic) && length(italic) == 1 && !is.na(italic))) stop("`italic` must be TRUE or FALSE.", call. = FALSE)
  if (!is.null(underline) && !((is.logical(underline) || is.character(underline)) && length(underline) == 1 && !is.na(underline))) {
    stop("`underline` must be TRUE, FALSE or a Word underline type (\"single\", \"double\"...).", call. = FALSE)
  }
  if (!is.null(size) && !(is.numeric(size) && length(size) == 1 && size > 0)) stop("`size` must be a positive number of points.", call. = FALSE)
  if (!is.null(font) && !(is.character(font) && length(font) == 1)) stop("`font` must be a font name.", call. = FALSE)
  list(bold = bold, italic = italic, underline = underline, size = size,
       font = font, char_style = char_style, strike = strike, color = color)
}

# Adds a w:rPr (order imposed by the OOXML schema) to the run `r` if needed.
.add_rpr <- function(r, f, where = 0) {
  if (all(vapply(f, is.null, TRUE))) return(invisible())
  rpr <- .add(r, "w:rPr", .where = where)
  if (!is.null(f$char_style)) .add(rpr, "w:rStyle", "w:val" = f$char_style)
  if (!is.null(f$font)) {
    .add(rpr, "w:rFonts", "w:ascii" = f$font, "w:hAnsi" = f$font,
         "w:eastAsia" = f$font, "w:cs" = f$font)
  }
  if (!is.null(f$bold)) {
    v <- if (f$bold) "1" else "0"
    .add(rpr, "w:b", "w:val" = v)
    .add(rpr, "w:bCs", "w:val" = v)
  }
  if (!is.null(f$italic)) {
    v <- if (f$italic) "1" else "0"
    .add(rpr, "w:i", "w:val" = v)
    .add(rpr, "w:iCs", "w:val" = v)
  }
  if (!is.null(f$strike)) .add(rpr, "w:strike", "w:val" = if (f$strike) "1" else "0")
  if (!is.null(f$color)) .add(rpr, "w:color", "w:val" = f$color)
  if (!is.null(f$size)) {
    v <- as.character(round(f$size * 2))
    .add(rpr, "w:sz", "w:val" = v)
    .add(rpr, "w:szCs", "w:val" = v)
  }
  if (!is.null(f$underline)) {
    v <- if (isTRUE(f$underline)) "single" else if (isFALSE(f$underline)) "none" else f$underline
    .add(rpr, "w:u", "w:val" = v)
  }
  invisible(rpr)
}

# Fills the run `r` with the text (\n -> line break, \t -> tab).
.fill_text <- function(r, text) {
  text <- gsub("\r\n?", "\n", paste(text, collapse = "\n"))
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (endsWith(text, "\n")) lines <- c(lines, "")
  for (i in seq_along(lines)) {
    if (i > 1) .add(r, "w:br")
    segs <- strsplit(lines[i], "\t", fixed = TRUE)[[1]]
    if (endsWith(lines[i], "\t")) segs <- c(segs, "")
    for (j in seq_along(segs)) {
      if (j > 1) .add(r, "w:tab")
      if (nzchar(segs[j])) {
        t <- .add(r, "w:t", "xml:space" = "preserve")
        xml2::xml_text(t) <- segs[j]
      }
    }
  }
  invisible(r)
}

# Creates a run next to `anchor` (or as the last child of `parent`).
.new_run <- function(text, f, anchor = NULL, where = "after", parent = NULL) {
  r <- if (!is.null(parent)) .add(parent, "w:r") else .add(anchor, "w:r", sibling = where)
  .add_rpr(r, f)
  .fill_text(r, text)
  r
}

.html_decode <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&nbsp;", "\u00a0", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

# Splits `text` into list(text, f) segments. With `html`, the tags b/strong, i/em,
# u, s/strike/del (nestable) and br are interpreted; other tags stay as text.
.rich_segments <- function(text, f, html) {
  text <- paste(text, collapse = "\n")
  if (!html) return(list(list(text = text, f = f)))
  m <- gregexpr("(?i)<(/?)(b|strong|i|em|u|s|strike|del)\\s*>|<br\\s*/?>", text, perl = TRUE)[[1]]
  tags <- if (m[1] == -1) character() else regmatches(text, list(m))[[1]]
  depth <- c(b = 0, i = 0, u = 0, s = 0)
  keys <- c(b = "b", strong = "b", i = "i", em = "i", u = "u", s = "s", strike = "s", del = "s")
  segs <- list()
  emit <- function(s) {
    if (!nzchar(s)) return()
    g <- f
    if (depth[["b"]] > 0) g$bold <- TRUE
    if (depth[["i"]] > 0) g$italic <- TRUE
    if (depth[["u"]] > 0 && !is.character(g$underline)) g$underline <- TRUE
    if (depth[["s"]] > 0) g$strike <- TRUE
    segs[[length(segs) + 1]] <<- list(text = .html_decode(s), f = g)
  }
  pos <- 1
  for (k in seq_along(tags)) {
    emit(substr(text, pos, m[k] - 1))
    tag <- tolower(tags[k])
    if (startsWith(tag, "<br")) {
      emit("\n")
    } else {
      key <- keys[[gsub("[</>\\s]", "", tag, perl = TRUE)]]
      depth[[key]] <- max(0, depth[[key]] + if (startsWith(tag, "</")) -1 else 1)
    }
    pos <- m[k] + attr(m, "match.length")[k]
  }
  emit(substring(text, pos))
  if (length(segs) == 0) segs <- list(list(text = "", f = f))
  segs
}

# Creates the runs of a text (one per formatting segment); returns the list of runs.
.new_runs <- function(text, f, html = FALSE, anchor = NULL, where = "after", parent = NULL) {
  runs <- list()
  for (s in .rich_segments(text, f, html)) {
    r <- .new_run(s$text, s$f, anchor = anchor, where = where, parent = parent)
    runs[[length(runs) + 1]] <- r
    if (is.null(parent) && where == "after") anchor <- r
  }
  runs
}

# Order imposed by the OOXML schema for the children of w:rPr.
.rpr_order <- c("w:rStyle", "w:rFonts", "w:b", "w:bCs", "w:i", "w:iCs", "w:caps", "w:smallCaps",
                "w:strike", "w:dstrike", "w:outline", "w:shadow", "w:emboss", "w:imprint",
                "w:noProof", "w:snapToGrid", "w:vanish", "w:webHidden", "w:color", "w:spacing",
                "w:w", "w:kern", "w:position", "w:sz", "w:szCs", "w:highlight", "w:u",
                "w:effect", "w:bdr", "w:shd", "w:fitText", "w:vertAlign", "w:rtl", "w:cs",
                "w:em", "w:lang", "w:eastAsianLayout", "w:specVanish", "w:oMath")

# Gives the run `r` the properties `base` (a w:rPr copied), overridden by its own.
.merge_rpr <- function(r, base) {
  if (is.null(base) || .is_missing(base)) return(invisible(r))
  own <- .find1(r, "w:rPr")
  new <- xml2::xml_add_child(r, base, .where = 0, .copy = TRUE)
  if (!.is_missing(own)) {
    for (e in xml2::xml_children(own)) {
      nm <- .name(e)
      old <- .find1(new, nm)
      if (!.is_missing(old)) xml2::xml_remove(old)
      rank <- match(nm, .rpr_order)
      before <- Filter(function(x) {
        k <- match(.name(x), .rpr_order)
        !is.na(rank) && !is.na(k) && k < rank
      }, xml2::xml_children(new))
      if (length(before) > 0) {
        xml2::xml_add_sibling(before[[length(before)]], e, .where = "after", .copy = TRUE)
      } else {
        xml2::xml_add_child(new, e, .where = 0, .copy = TRUE)
      }
    }
    xml2::xml_remove(own)
  }
  invisible(r)
}

# Resolves a style by identifier or by name; returns list(id, type).
.resolve_style <- function(doc, style) {
  if (is.null(style)) return(NULL)
  p <- .part_by_type(doc, doc$main, "styles")
  x <- if (is.na(p)) NULL else .xml(doc, p)
  if (is.null(x)) stop("The document does not contain any styles.", call. = FALSE)
  n <- .find1(x, sprintf("/w:styles/w:style[@w:styleId=%s]", .xpath_string(style)))
  if (.is_missing(n)) {
    styles <- .find_all(x, "/w:styles/w:style")
    names <- .attr(.find1(styles, "w:name"), "w:val")
    i <- which(tolower(names) == tolower(style))
    if (length(i) == 0) {
      ids <- .attr(styles, "w:styleId")
      stop(sprintf("Style '%s' not found. Available styles: %s", style,
                   paste(ids, collapse = ", ")), call. = FALSE)
    }
    n <- styles[[i[1]]]
  }
  type <- .attr(n, "w:type")
  if (!type %in% c("paragraph", "character")) {
    stop(sprintf("Style '%s' is of type '%s': only paragraph or character styles are accepted.", style, type), call. = FALSE)
  }
  list(id = .attr(n, "w:styleId"), type = type)
}

.set_paragraph_style <- function(p, id) {
  .set_attr(.paragraph_property(p, "w:pStyle"), "w:val", id)
  invisible(p)
}

# Order imposed by the OOXML schema for the children of w:pPr.
.ppr_order <- c("w:pStyle", "w:keepNext", "w:keepLines", "w:pageBreakBefore", "w:framePr",
                "w:widowControl", "w:numPr", "w:suppressLineNumbers", "w:pBdr", "w:shd", "w:tabs",
                "w:suppressAutoHyphens", "w:kinsoku", "w:wordWrap", "w:overflowPunct",
                "w:topLinePunct", "w:autoSpaceDE", "w:autoSpaceDN", "w:bidi", "w:adjustRightInd",
                "w:snapToGrid", "w:spacing", "w:ind", "w:contextualSpacing", "w:mirrorIndents",
                "w:suppressOverlap", "w:jc", "w:textDirection", "w:textAlignment",
                "w:textboxTightWrap", "w:outlineLvl", "w:divId", "w:cnfStyle", "w:rPr",
                "w:sectPr", "w:pPrChange")

# Element `name` of the w:pPr of `p`, created at its place (schema order) if absent.
.paragraph_property <- function(p, name) {
  ppr <- .find1(p, "w:pPr")
  if (.is_missing(ppr)) ppr <- .add(p, "w:pPr", .where = 0)
  el <- .find1(ppr, name)
  if (!.is_missing(el)) return(el)
  rank <- match(name, .ppr_order)
  before <- Filter(function(e) {
    r <- match(.name(e), .ppr_order)
    !is.na(r) && r < rank
  }, xml2::xml_children(ppr))
  if (length(before) > 0) .add(before[[length(before)]], name, sibling = "after") else .add(ppr, name, .where = 0)
}

# Enables a boolean paragraph property (w:keepNext, w:keepLines...).
.enable_property <- function(p, name) {
  .set_attr(.paragraph_property(p, name), "w:val", NULL)
  invisible(p)
}

.check_flag <- function(x, name) {
  if (!(is.logical(x) && length(x) == 1 && !is.na(x))) stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
  invisible(x)
}

# Applies the pagination options to paragraph `p`: `keep_with_previous` sets
# w:keepNext on the previous paragraph (or on the last row of the previous table).
.set_keep_options <- function(p, keep_with_previous, keep_lines_together) {
  if (keep_lines_together) .enable_property(p, "w:keepLines")
  if (keep_with_previous) {
    prev <- .find1(p, "preceding-sibling::*[self::w:p or self::w:tbl][1]")
    if (.is_missing(prev)) {
      warning("No previous paragraph: `keep_with_previous` is ignored.", call. = FALSE)
    } else if (.name(prev) == "w:p") {
      .enable_property(prev, "w:keepNext")
    } else {
      for (q in .find_all(prev, "w:tr[last()]//w:p")) .enable_property(q, "w:keepNext")
    }
  }
  invisible(p)
}

#' Insert a page break
#'
#' `bkm_insert_page_break()` inserts a page break on a bookmark;
#' `doc_add_page_break()` adds one at the end of the document.
#'
#' @param doc WRite document object.
#' @param name Bookmark name.
#' @param position `"replace"` (replaces the content of the bookmark),
#'   `"before"` (at the start of the bookmark) or `"after"` (at the end of the
#'   bookmark).
#' @param keep_bookmark Keep the bookmark around the page break?
#' @return `doc`, invisibly.
#' @section Illustration:
#' ```
#' Before                  : Part 1 [next: ]
#'                           Part 2
#' After                   : Part 1 [next: <page break>]
#'                           Part 2                (at the top of the next page)
#' ```
#' @examples
#' # Page break on a bookmark
#' doc <- doc_read(write_example("example"))
#' bkm_insert_page_break(doc, "empty_bookmark")
#' bkm_text(doc, "empty_bookmark")   # "\f" = page break
#'
#' # Page break after the content of a bookmark, keeping it
#' bkm_insert_page_break(doc, "my_bookmark", position = "after")
#' bkm_text(doc, "my_bookmark")          # "Middle\f"
#'
#' # Building a document page by page
#' doc2 <- doc_new() |>
#'   doc_add_paragraph("Cover page", style = "Heading1") |>
#'   doc_add_page_break() |>
#'   doc_add_paragraph("Table of contents") |>
#'   doc_add_page_break() |>
#'   doc_add_paragraph("Chapter 1", style = "Heading1")
#' doc_text(doc2)
#' @export
bkm_insert_page_break <- function(doc, name, position = c("replace", "before", "after"),
                                  keep_bookmark = TRUE) {
  .check_doc(doc)
  position <- match.arg(position)
  .bkm_guard({
  bkm <- .bkm_find(doc, name)
  run <- .fragment_run('<w:br w:type="page"/>')
  if (position == "replace") .empty_range(bkm)
  if (position == "after") .insert_run_node(bkm$end, "before", run) else .insert_run_node(bkm$start, "after", run)
  if (!isTRUE(keep_bookmark)) .remove_markers(bkm)
  }, NULL)
  invisible(doc)
}

#' @rdname bkm_insert_page_break
#' @export
doc_add_page_break <- function(doc) {
  .check_doc(doc)
  p <- .new_final_paragraph(doc)
  .add(.add(p, "w:r"), "w:br", "w:type" = "page")
  invisible(doc)
}

.new_final_paragraph <- function(doc) {
  body <- .find1(.xml(doc, doc$main), "/w:document/w:body")
  sect <- .find1(body, "w:sectPr")
  if (.is_missing(sect)) .add(body, "w:p") else .add(sect, "w:p", sibling = "before")
}

#' Add a paragraph to the document
#'
#' Adds a paragraph at the end of the document body (or next to a given text),
#' with optional formatting, and can surround it with a bookmark (useful to
#' prepare a document created by [doc_new()]).
#'
#' @param doc WRite document object.
#' @param text Paragraph text. `"\n"` = line break, `"\n\n"` = new paragraph,
#'   `"\t"` = tab.
#' @param bold,italic `TRUE`/`FALSE` (`NULL` = inherited from the style).
#' @param underline `TRUE`, `FALSE` or Word underline type (`"double"`, `"dotted"`...).
#' @param size Font size in points.
#' @param font Font name.
#' @param style Predefined style (identifier or name). A paragraph style is
#'   applied to the paragraph, a character style to the text.
#' @param bookmark Name of a bookmark to create around the text (optional).
#' @param keep_with_previous Keep the previous paragraph on the same page as
#'   this one (Word "Keep with next" option set on the previous paragraph):
#'   in case of a page break, the previous paragraph also moves to the next
#'   page.
#' @param keep_lines_together Keep all the lines of the paragraph on the same
#'   page (Word "Keep lines together" option).
#' @param html Interpret HTML tags in `text`? Supported: `<b>`/`<strong>`,
#'   `<i>`/`<em>`, `<u>`, `<s>`/`<strike>`/`<del>` (can be nested) and `<br>`
#'   (line break). Other tags are kept as text; `&lt;`, `&gt;`, `&amp;` and
#'   `&nbsp;` are decoded.
#' @param at_text Text of the document next to which the paragraph is inserted
#'   (`NULL` = end of the document). The search covers the body, headers,
#'   footers and notes, and is literal and case-sensitive. If the text is not
#'   found, a warning is issued and nothing is inserted.
#' @param where With `at_text`: `"after"` (after the paragraph containing the
#'   text), `"before"` (before that paragraph) or `"replace"` (the text is
#'   replaced by the new paragraph(s); the paragraph containing it is split
#'   around them if it contains other text).
#' @param occurrence With `at_text`: `"first"` (first occurrence, with a
#'   warning if the text appears several times) or `"all"` (every occurrence;
#'   with `where` = `"before"`/`"after"`, once per paragraph containing the text).
#'   `"all"` cannot be combined with `bookmark`.
#' @return `doc`, invisibly.
#' @section Illustration:
#' ```
#' doc_new() |>
#'   doc_add_paragraph("Report", style = "Heading1") |>
#'   doc_add_paragraph("Summary:", bold = TRUE) |>
#'   doc_add_paragraph("", bookmark = "summary")
#'
#' Report               (style Heading 1)
#' **Summary:**
#' [summary: ]
#' ```
#' @examples
#' doc <- doc_new() |>
#'   doc_add_paragraph("Annual report", style = "heading 1") |>
#'   doc_add_paragraph("Normal text") |>
#'   doc_add_paragraph("Arial 9 italic text", font = "Arial",
#'                     size = 9, italic = TRUE) |>
#'   doc_add_paragraph("Column 1\tColumn 2\nSecond line") |>
#'   doc_add_paragraph("", bookmark = "to_fill")
#' doc_text(doc)
#' bkm_list(doc)
#'
#' # Paragraph attached to the previous one (they stay on the same page)
#' doc_add_paragraph(doc, "Conclusion", style = "Heading2")
#' doc_add_paragraph(doc, "Conclusion text", keep_with_previous = TRUE,
#'                   keep_lines_together = TRUE)
#'
#' # Rich text (HTML tags) and several paragraphs
#' doc_add_paragraph(doc, "Normal, <b>bold</b>, <i>italic</i> and <s>struck</s>",
#'                   html = TRUE)
#' doc_add_paragraph(doc, "First paragraph\n\nSecond paragraph")
#'
#' # Next to an existing text
#' doc_add_paragraph(doc, "Inserted after the title", at_text = "Annual report")
#' doc_add_paragraph(doc, "Inserted before the title", at_text = "Annual report",
#'                   where = "before")
#' doc_add_paragraph(doc, "Replaces the text", at_text = "Normal text",
#'                   where = "replace")
#' doc_text(doc)
#' @export
doc_add_paragraph <- function(doc, text = "", bold = NULL, italic = NULL, underline = NULL,
                              size = NULL, font = NULL, style = NULL, bookmark = NULL,
                              keep_with_previous = FALSE, keep_lines_together = FALSE,
                              html = FALSE, at_text = NULL,
                              where = c("after", "before", "replace"),
                              occurrence = c("first", "all")) {
  .check_doc(doc)
  .check_flag(keep_with_previous, "keep_with_previous")
  .check_flag(keep_lines_together, "keep_lines_together")
  .check_flag(html, "html")
  where <- match.arg(where)
  occurrence <- match.arg(occurrence)
  if (!is.null(at_text) && !(is.character(at_text) && length(at_text) == 1 && !is.na(at_text) && nzchar(at_text))) {
    stop("`at_text` must be a non-empty string.", call. = FALSE)
  }
  st <- .resolve_style(doc, style)
  f <- .format(bold, italic, underline, size, font,
               char_style = if (!is.null(st) && st$type == "character") st$id)
  if (!is.null(bookmark) && bookmark %in% bkm_list(doc)$name) {
    stop(sprintf("Bookmark '%s' already exists.", bookmark), call. = FALSE)
  }
  if (!is.null(bookmark) && !is.null(at_text) && occurrence == "all") {
    stop("`bookmark` cannot be used with `occurrence = \"all\"`.", call. = FALSE)
  }
  pieces <- .paragraph_pieces(text)
  build <- function() .build_paragraphs(doc, pieces, f, st, html, bookmark)
  finish <- function(ps) {
    if (keep_lines_together) for (p in ps) .enable_property(p, "w:keepLines")
    .set_keep_options(ps[[1]], keep_with_previous, FALSE)
  }
  if (is.null(at_text)) {
    finish(build())
  } else {
    .add_at_text(doc, at_text, where, occurrence, build, finish)
  }
  invisible(doc)
}

# Paragraph texts: "\n\n" separates paragraphs.
.paragraph_pieces <- function(text) {
  text <- gsub("\r\n?", "\n", paste(text, collapse = "\n"))
  if (!nzchar(text)) return("")
  strsplit(text, "\n\n", fixed = TRUE)[[1]]
}

# Creates the paragraphs at the end of the body; the bookmark spans all of them.
.build_paragraphs <- function(doc, pieces, f, st, html, bookmark) {
  id <- if (!is.null(bookmark)) .bookmark_max_id(doc) + 1
  n <- length(pieces)
  lapply(seq_len(n), function(i) {
    p <- .new_final_paragraph(doc)
    if (!is.null(st) && st$type == "paragraph") .set_paragraph_style(p, st$id)
    if (i == 1 && !is.null(bookmark)) .add(p, "w:bookmarkStart", "w:id" = id, "w:name" = bookmark)
    if (nzchar(pieces[i])) .new_runs(pieces[i], f, html, parent = p)
    if (i == n && !is.null(bookmark)) .add(p, "w:bookmarkEnd", "w:id" = id)
    p
  })
}

# Paragraph text, without the text boxes it contains.
.paragraph_text <- function(p) {
  k <- .count(p, "count(ancestor::w:txbxContent)")
  ts <- .find_all(p, sprintf(".//w:t[parent::w:r][count(ancestor::w:txbxContent) = %d]", k))
  paste(xml2::xml_text(ts), collapse = "")
}

# Paragraphs containing `text` (literal) and total number of occurrences.
.find_text <- function(doc, text) {
  pars <- list()
  n <- 0L
  for (part in .content_parts(doc)) {
    for (par in .find_all(.xml(doc, part), "//w:p")) {
      m <- gregexpr(text, .paragraph_text(par), fixed = TRUE)[[1]]
      k <- sum(m > 0)
      if (k > 0) {
        pars[[length(pars) + 1]] <- par
        n <- n + k
      }
    }
  }
  list(pars = pars, n = n)
}

# Inserts the paragraphs created by `build()` next to / in place of `at_text`.
.add_at_text <- function(doc, at_text, where, occurrence, build, finish) {
  hits <- .find_text(doc, at_text)
  if (hits$n == 0) {
    warning(sprintf("Text '%s' not found: nothing inserted.", at_text), call. = FALSE)
    return(invisible())
  }
  if (occurrence == "first" && hits$n > 1) {
    warning(sprintf("Text '%s' found %d times: only the first occurrence is used.", at_text, hits$n), call. = FALSE)
  }
  if (where != "replace") {
    targets <- if (occurrence == "first") hits$pars[1] else hits$pars
    for (target in targets) {
      ps <- build()
      ref <- target
      for (p in ps) {
        if (where == "before") .move(p, target, "before") else .move(p, ref, "after")
        ref <- p
      }
      finish(ps)
    }
    return(invisible())
  }
  token <- "\uE000WRite-paragraph\uE000"
  name <- "_WRite_par_tmp"
  if (occurrence == "first") {
    .replace_in_paragraph(hits$pars[[1]], at_text, NULL, token, FALSE, first_only = TRUE)
  } else {
    for (par in hits$pars) .replace_in_paragraph(par, at_text, NULL, token, FALSE)
  }
  while (.token_to_bookmark(doc, token, name)) {
    ps <- build()
    bkm <- .bkm_find(doc, name)
    res <- .insert_blocks(bkm, ps, "replace")
    for (p in ps) xml2::xml_remove(p)
    .finalize_split(res)
    .remove_markers(bkm)
    finish(res$ins)
  }
  invisible()
}
