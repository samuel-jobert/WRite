#' Replace text anywhere in the document
#'
#' Searches for `text` in the body, headers, footers and notes, and replaces
#' it with `replacement`. `text` may be a vector: the replacements are then
#' applied in order (like successive calls), in a single pass over the document.
#'
#' Before the first search on a document, adjacent runs with identical
#' formatting are merged (like `doc_clean(doc, bookmark_ids = FALSE)`); this
#' merge is done only once per object, even if the function is called several
#' times (or if [doc_clean()] has already been called). The search is then
#' done paragraph by paragraph, even when the text remains spread over runs
#' with different formatting: the replacement text then takes the formatting
#' of the start of the match. Bookmarks located inside a match are kept.
#'
#' @param doc WRite document object.
#' @param text Text(s) to search for (or regular expression(s) if `regex = TRUE`).
#' @param replacement Replacement text. With `regex = TRUE`, it may contain
#'   references `\\1`, `\\2`... Length 1 (same replacement for all the texts) or
#'   the length of `text`.
#' @param regex Is `text` a regular expression (PCRE syntax)?
#' @param ignore_case Ignore the difference between upper and lower case?
#' @param html Interpret the HTML tags of the replacement (`<b>`, `<i>`, `<u>`,
#'   `<s>`, `<br>`...)? See [doc_add_paragraph()]. The formatting of the tags
#'   is added to the formatting of the replaced text.
#' @return `doc`, invisibly. The number of replacements is available in the
#'   `"replacements"` attribute of the returned value.
#' @section Illustration:
#' ```
#' Before: The contract of {{client}} starts on **{{da**te}}.   (tag spread over 2 runs)
#' doc_replace_text(doc, "{{client}}", "Mr Martin")
#' doc_replace_text(doc, "{{date}}", "March 1st")
#' After : The contract of Mr Martin starts on **March 1st**.
#' ```
#' @examples
#' doc <- doc_read(write_example("example"))
#'
#' # Literal replacement
#' doc_replace_text(doc, "{{amount_2024}}", "1,000,000")
#' attr(doc, "replacements")   # 1
#' doc_text(doc)[3]
#'
#' # Regular expression with a reference: "in 2024" -> "in the year 2024"
#' doc_replace_text(doc, "in (\\d{4})", "in the year \\1", regex = TRUE)
#' doc_text(doc)[3:4]
#'
#' # Ignoring case
#' doc_replace_text(doc, "sTART", "Beginning", ignore_case = TRUE)
#' doc_text(doc)[1]
#'
#' # Text spread over runs with different formatting
#' doc2 <- doc_read(write_example("to_insert"))
#' doc_replace_text(doc2, "Normal text, bold text, normal text.", "corrected text")
#' doc_text(doc2)
#'
#' # Several texts at once, with HTML tags
#' doc3 <- doc_new() |> doc_add_paragraph("Hello {{a}}, you owe {{b}}.")
#' doc_replace_text(doc3, c("{{a}}", "{{b}}"), c("<b>Ann</b>", "<i>10</i> euros"), html = TRUE)
#' doc_text(doc3)
#' @export
doc_replace_text <- function(doc, text, replacement, regex = FALSE, ignore_case = FALSE,
                             html = FALSE) {
  .check_doc(doc)
  if (!is.character(text) || length(text) < 1 || anyNA(text) || !all(nzchar(text))) {
    stop("`text` must be a vector of non-empty strings.", call. = FALSE)
  }
  if (!is.character(replacement) || anyNA(replacement) || !length(replacement) %in% c(1, length(text))) {
    stop("`replacement` must be a string or a vector of the same length as `text`.", call. = FALSE)
  }
  .check_flag(html, "html")
  store <- if (html) new.env() else NULL
  n <- sum(.replace_everywhere(doc, text, replacement, regex, ignore_case, store))
  if (html) .apply_html(doc, store)
  attr(doc, "replacements") <- n
  invisible(doc)
}

# Replaces each temporary token by the runs of the HTML replacement.
.apply_html <- function(doc, store) {
  tokens <- sort(ls(store))
  for (token in tokens) {
    repl <- get(token, envir = store)
    .token_to_bookmark(doc, token, "_WRite_html_tmp", callback = function(start, end, rpr) {
      runs <- .new_runs(repl, .format(), html = TRUE, anchor = start, where = "after")
      for (r in runs) .merge_rpr(r, rpr)
      xml2::xml_remove(start)
      xml2::xml_remove(end)
    })
  }
  invisible()
}

.merge_runs_once <- function(doc) {
  if (isTRUE(doc$runs_merged)) return(invisible())
  for (p in .content_parts(doc)) .merge_runs(.xml(doc, p))
  doc$runs_merged <- TRUE
  invisible()
}

.pattern <- function(text, regex, ignore_case) {
  if (regex) return(text)
  pattern <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", text)
  if (ignore_case) pattern else NULL
}

# Returns the number of replacements for each element of `text`.
.replace_everywhere <- function(doc, text, replacement, regex = FALSE, ignore_case = FALSE,
                                store = NULL) {
  .merge_runs_once(doc)
  patterns <- lapply(text, .pattern, regex = regex, ignore_case = ignore_case)
  replacement <- rep_len(replacement, length(text))
  n <- integer(length(text))
  for (p in .content_parts(doc)) {
    for (par in .find_all(.xml(doc, p), "//w:p")) {
      full <- xml2::xml_text(par)
      for (i in seq_along(text)) {
        found <- if (is.null(patterns[[i]])) {
          grepl(text[i], full, fixed = TRUE)
        } else {
          grepl(patterns[[i]], full, perl = TRUE, ignore.case = ignore_case)
        }
        if (!found) next
        k <- .replace_in_paragraph(par, text[i], patterns[[i]], replacement[i], ignore_case,
                                   store = store)
        if (k > 0) {
          n[i] <- n[i] + k
          full <- xml2::xml_text(par)
        }
      }
    }
  }
  n
}

# `pattern` NULL = literal search of `text`, otherwise PCRE regular expression.
.replace_in_paragraph <- function(p, text, pattern, replacement, ignore_case, first_only = FALSE,
                                 store = NULL) {
  k <- .count(p, "count(ancestor::w:txbxContent)")
  ts <- .find_all(p, sprintf(".//w:t[parent::w:r][count(ancestor::w:txbxContent) = %d]", k))
  if (length(ts) == 0) return(0L)
  txt <- xml2::xml_text(ts)
  full <- paste(txt, collapse = "")
  m <- if (is.null(pattern)) {
    gregexpr(text, full, fixed = TRUE)[[1]]
  } else {
    gregexpr(pattern, full, perl = TRUE, ignore.case = ignore_case)[[1]]
  }
  lengths <- attr(m, "match.length")
  keep <- m > 0 & lengths > 0
  if (!any(keep)) return(0L)
  if (first_only) keep[-which(keep)[1]] <- FALSE
  match_starts <- m[keep]
  match_ends <- match_starts + lengths[keep] - 1
  node_start <- c(1, cumsum(nchar(txt))[-length(txt)] + 1)
  modified <- rep(FALSE, length(txt))
  # Process the last match first: earlier positions remain valid.
  for (j in rev(seq_along(match_starts))) {
    s <- match_starts[j]
    e <- match_ends[j]
    repl <- if (is.null(pattern)) replacement else {
      sub(pattern, replacement, substr(full, s, e), perl = TRUE, ignore.case = ignore_case)
    }
    if (!is.null(store)) {
      token <- sprintf("\ue000WRite-html%d\ue000", length(ls(store)) + 1L)
      assign(token, repl, envir = store)
      repl <- token
    }
    i1 <- max(which(node_start <= s))
    i2 <- max(which(node_start <= e))
    before <- substr(txt[i1], 1, s - node_start[i1])
    if (i1 == i2) {
      txt[i1] <- paste0(before, repl, substring(txt[i1], e - node_start[i1] + 2))
    } else {
      txt[i1] <- paste0(before, repl)
      if (i2 - i1 > 1) txt[(i1 + 1):(i2 - 1)] <- ""
      txt[i2] <- substring(txt[i2], e - node_start[i2] + 2)
      modified[(i1 + 1):i2] <- TRUE
    }
    modified[i1] <- TRUE
  }
  for (i in which(modified)) .set_text(ts[[i]], txt[i])
  length(match_starts)
}

# Sets the text of a w:t, or removes it if empty (and its run if it became empty).
.set_text <- function(t, value) {
  if (nzchar(value)) {
    xml2::xml_text(t) <- value
    .set_attr(t, "xml:space", "preserve")
  } else {
    r <- xml2::xml_parent(t)
    xml2::xml_remove(t)
    if (.count(r, "count(*[not(self::w:rPr)])") == 0) xml2::xml_remove(r)
  }
  invisible()
}

# Replaces the first occurrence of the token (contained in a single w:t) by an empty bookmark.
.token_to_bookmark <- function(doc, token, name, callback = NULL) {
  for (p in .content_parts(doc)) {
    t <- .find1(.xml(doc, p), sprintf("//w:t[contains(., %s)]", .xpath_string(token)))
    if (.is_missing(t)) next
    txt <- xml2::xml_text(t)
    pos <- regexpr(token, txt, fixed = TRUE)
    before <- substr(txt, 1, pos - 1)
    after <- substring(txt, pos + nchar(token))
    # The run is split into two copies around the token.
    r <- xml2::xml_parent(t)
    children <- xml2::xml_children(r)
    i <- which(vapply(children, identical, TRUE, t))
    r2 <- xml2::xml_add_sibling(r, r, .where = "after")
    children2 <- xml2::xml_children(r2)
    for (k in seq_along(children)) {
      if (k > i) xml2::xml_remove(children[[k]])
      if (k < i && .name(children2[[k]]) != "w:rPr") xml2::xml_remove(children2[[k]])
    }
    id <- .bookmark_max_id(doc) + 1
    start <- .add(r, "w:bookmarkStart", sibling = "after", "w:id" = id, "w:name" = name)
    end <- .add(start, "w:bookmarkEnd", sibling = "after", "w:id" = id)
    if (!is.null(callback)) callback(start, end, .find1(r, "w:rPr"))
    .set_text(t, before)
    .set_text(children2[[i]], after)
    return(TRUE)
  }
  FALSE
}
