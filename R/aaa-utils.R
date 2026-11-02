# Internal utility functions (XML, paths, relationships, content types)

.ns <- c(
  w   = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
  r   = "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
  wp  = "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing",
  a   = "http://schemas.openxmlformats.org/drawingml/2006/main",
  m   = "http://schemas.openxmlformats.org/officeDocument/2006/math",
  mc  = "http://schemas.openxmlformats.org/markup-compatibility/2006",
  w14 = "http://schemas.microsoft.com/office/word/2010/wordml",
  xml = "http://www.w3.org/XML/1998/namespace"
)

.ns_rels <- "http://schemas.openxmlformats.org/package/2006/relationships"
.ns_ct <- "http://schemas.openxmlformats.org/package/2006/content-types"

.rel_type <- function(x) {
  paste0("http://schemas.openxmlformats.org/officeDocument/2006/relationships/", x)
}

.ct <- c(
  numbering = "application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml",
  footnotes = "application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml",
  endnotes  = "application/vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml",
  styles    = "application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml",
  document  = "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"
)

# ---- XML access ------------------------------------------------------------

.find_all <- function(x, xpath) xml2::xml_find_all(x, xpath, ns = .ns)
.find1 <- function(x, xpath) xml2::xml_find_first(x, xpath, ns = .ns)
.count <- function(x, xpath) xml2::xml_find_num(x, xpath, ns = .ns)
.attr <- function(x, name) xml2::xml_attr(x, name, ns = .ns)
.set_attr <- function(x, name, value) xml2::xml_set_attr(x, name, value, ns = .ns)
.name <- function(x) xml2::xml_name(x, ns = .ns)
.is_missing <- function(x) inherits(x, "xml_missing") || length(x) == 0

# Creates a child element (or a sibling if `sibling` is given) and sets prefixed attributes on it.
.add <- function(parent, name, ..., .where = NULL, sibling = NULL) {
  n <- if (!is.null(sibling)) {
    xml2::xml_add_sibling(parent, name, .where = sibling)
  } else if (is.null(.where)) {
    xml2::xml_add_child(parent, name)
  } else {
    xml2::xml_add_child(parent, name, .where = .where)
  }
  attrs <- list(...)
  for (a in names(attrs)) .set_attr(n, a, as.character(attrs[[a]]))
  n
}

# Moves a node within the same document (no copy).
# The node is detached first: xmlAddChild (libxml2) does not remove it from its old parent.
.move <- function(node, target, where = c("child", "before", "after")) {
  where <- match.arg(where)
  xml2::xml_remove(node, free = FALSE)
  if (where == "child") {
    xml2::xml_add_child(target, node, .copy = FALSE)
  } else {
    xml2::xml_add_sibling(target, node, .where = where, .copy = FALSE)
  }
  invisible(node)
}

# Position (0-based) of an element in document order (equivalent to its rank in //*).
.index <- function(n) xml2::xml_find_num(n, "count(preceding::*) + count(ancestor::*)")

# Closest paragraph containing the node (NULL if none).
.paragraph_of <- function(n) {
  p <- .find1(n, "ancestor::w:p[1]")
  if (.is_missing(p)) NULL else p
}

.xpath_string <- function(x) {
  if (!grepl("'", x, fixed = TRUE)) return(paste0("'", x, "'"))
  if (!grepl('"', x, fixed = TRUE)) return(paste0('"', x, '"'))
  pieces <- strsplit(x, "'", fixed = TRUE)[[1]]
  paste0("concat('", paste(pieces, collapse = "', \"'\", '"), "')")
}

.or <- function(x, y) if (is.null(x)) y else x

.check_doc <- function(doc) {
  if (!inherits(doc, "write_doc")) {
    stop("The object is not a WRite document (see doc_read() or doc_new()).", call. = FALSE)
  }
  invisible(TRUE)
}

# ---- Paths in the OPC package ----------------------------------------------

.normalize_path <- function(path) {
  segs <- strsplit(path, "/", fixed = TRUE)[[1]]
  res <- character()
  for (s in segs) {
    if (s == "" || s == ".") next
    if (s == "..") res <- utils::head(res, -1) else res <- c(res, s)
  }
  paste(res, collapse = "/")
}

.dir_of <- function(part) {
  d <- dirname(part)
  if (d == ".") "" else d
}

.absolute_target <- function(part, target) {
  if (startsWith(target, "/")) return(.normalize_path(target))
  .normalize_path(paste(.dir_of(part), target, sep = "/"))
}

.relative_path <- function(part, abs_target) {
  from <- if (.dir_of(part) == "") character() else strsplit(.dir_of(part), "/", fixed = TRUE)[[1]]
  to <- strsplit(abs_target, "/", fixed = TRUE)[[1]]
  k <- 0
  while (k < length(from) && k < length(to) - 1 && from[k + 1] == to[k + 1]) k <- k + 1
  paste(c(rep("..", length(from) - k), to[(k + 1):length(to)]), collapse = "/")
}

.rels_path <- function(part) {
  if (part == "") return("_rels/.rels")
  d <- .dir_of(part)
  paste0(if (d == "") "" else paste0(d, "/"), "_rels/", basename(part), ".rels")
}

# ---- XML part cache --------------------------------------------------------

.xml <- function(doc, path) {
  if (exists(path, envir = doc$parts, inherits = FALSE)) {
    return(get(path, envir = doc$parts, inherits = FALSE))
  }
  f <- file.path(doc$dir, path)
  if (!file.exists(f)) return(NULL)
  x <- xml2::read_xml(f, options = "HUGE")
  assign(path, x, envir = doc$parts)
  x
}

.xml_set <- function(doc, path, x) {
  assign(path, x, envir = doc$parts)
  .reset_content_parts(doc)
  invisible(x)
}

.part_exists <- function(doc, path) {
  exists(path, envir = doc$parts, inherits = FALSE) || file.exists(file.path(doc$dir, path))
}

# Free path in the package, suffixing the name if needed.
.free_path <- function(doc, path) {
  if (!.part_exists(doc, path)) return(path)
  ext <- tools::file_ext(path)
  base <- if (nzchar(ext)) substr(path, 1, nchar(path) - nchar(ext) - 1) else path
  k <- 2
  repeat {
    cand <- paste0(base, "_", k, if (nzchar(ext)) paste0(".", ext) else "")
    if (!.part_exists(doc, cand)) return(cand)
    k <- k + 1
  }
}

# Serializes an XML document, removing namespace declarations that are
# redundant with those of the root (created by node copies).
.serialize <- function(x) {
  txt <- as.character(x, options = character())
  pos <- regexpr("<[^?!][^>]*>", txt)
  if (pos < 0) return(txt)
  end <- pos + attr(pos, "match.length") - 1
  root <- substr(txt, pos, end)
  decls <- regmatches(root, gregexpr('xmlns(:[A-Za-z0-9_.-]+)?="[^"]*"', root))[[1]]
  rest <- substring(txt, end + 1)
  for (d in decls) rest <- gsub(paste0(" ", d), "", rest, fixed = TRUE)
  paste0(substr(txt, 1, end), rest)
}

.write_xml <- function(x, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw(enc2utf8(.serialize(x))), file)
}

# ---- Relationships ---------------------------------------------------------

.rels_df <- function(doc, part) {
  x <- .xml(doc, .rels_path(part))
  empty <- data.frame(id = character(), type = character(), target = character(),
                      mode = character(), stringsAsFactors = FALSE)
  if (is.null(x)) return(empty)
  n <- xml2::xml_find_all(x, "/*/*[local-name()='Relationship']")
  if (length(n) == 0) return(empty)
  data.frame(id = xml2::xml_attr(n, "Id"), type = xml2::xml_attr(n, "Type"),
             target = xml2::xml_attr(n, "Target"), mode = xml2::xml_attr(n, "TargetMode"),
             stringsAsFactors = FALSE)
}

# Absolute path of the first part linked to `part` by a relationship of the given type.
.part_by_type <- function(doc, part, type) {
  r <- .rels_df(doc, part)
  r <- r[r$type == .rel_type(type) & is.na(r$mode), , drop = FALSE]
  if (nrow(r) == 0) return(NA_character_)
  .absolute_target(part, r$target[1])
}

.add_rel <- function(doc, part, type, target, external = FALSE, id = NULL) {
  path <- .rels_path(part)
  x <- .xml(doc, path)
  if (is.null(x)) {
    x <- xml2::read_xml(sprintf('<Relationships xmlns="%s"/>', .ns_rels))
    .xml_set(doc, path, x)
  }
  if (is.null(id)) {
    ids <- xml2::xml_attr(xml2::xml_find_all(x, "/*/*[local-name()='Relationship']"), "Id")
    i <- 1
    while (paste0("rId", i) %in% ids) i <- i + 1
    id <- paste0("rId", i)
  }
  n <- xml2::xml_add_child(xml2::xml_root(x), "Relationship")
  xml2::xml_set_attr(n, "Id", id)
  xml2::xml_set_attr(n, "Type", type)
  xml2::xml_set_attr(n, "Target", target)
  if (external) xml2::xml_set_attr(n, "TargetMode", "External")
  .reset_content_parts(doc)
  id
}

# ---- Content types ([Content_Types].xml) -----------------------------------

.ct_xml <- function(doc) .xml(doc, "[Content_Types].xml")

.ct_override <- function(doc, part) {
  n <- xml2::xml_find_first(.ct_xml(doc), sprintf(
    "/*/*[local-name()='Override' and @PartName=%s]", .xpath_string(paste0("/", part))))
  if (.is_missing(n)) NA_character_ else xml2::xml_attr(n, "ContentType")
}

.ct_default <- function(doc, ext) {
  nodes <- xml2::xml_find_all(.ct_xml(doc), "/*/*[local-name()='Default']")
  exts <- tolower(xml2::xml_attr(nodes, "Extension"))
  i <- match(tolower(ext), exts)
  if (is.na(i)) NA_character_ else xml2::xml_attr(nodes[[i]], "ContentType")
}

.ct_add_override <- function(doc, part, type) {
  if (!is.na(.ct_override(doc, part))) return(invisible())
  n <- xml2::xml_add_child(xml2::xml_root(.ct_xml(doc)), "Override")
  xml2::xml_set_attr(n, "PartName", paste0("/", part))
  xml2::xml_set_attr(n, "ContentType", type)
  invisible()
}

.ct_ensure_default <- function(doc, ext, type) {
  if (!is.na(.ct_default(doc, ext)) || is.na(type)) return(invisible())
  n <- xml2::xml_add_child(xml2::xml_root(.ct_xml(doc)), "Default", .where = 0)
  xml2::xml_set_attr(n, "Extension", ext)
  xml2::xml_set_attr(n, "ContentType", type)
  invisible()
}

# ---- Content parts ---------------------------------------------------------

# Parts that can contain bookmarks: body, headers, footers and notes.
# Cached in `doc`; the cache is reset by .add_rel() and .xml_set().
.content_parts <- function(doc, notes = TRUE) {
  key <- if (notes) "content_parts" else "content_parts_no_notes"
  if (!is.null(doc[[key]])) return(doc[[key]])
  r <- .rels_df(doc, doc$main)
  types <- c("header", "footer", if (notes) c("footnotes", "endnotes"))
  r <- r[r$type %in% .rel_type(types) & is.na(r$mode), , drop = FALSE]
  others <- vapply(r$target, function(cb) .absolute_target(doc$main, cb), "")
  others <- others[vapply(others, function(p) .part_exists(doc, p), TRUE)]
  res <- unique(c(doc$main, unname(others)))
  assign(key, res, envir = doc)
  res
}

.reset_content_parts <- function(doc) {
  doc$content_parts <- NULL
  doc$content_parts_no_notes <- NULL
  invisible()
}

# Evaluates `expr`; if it fails, warns and returns `value` instead of stopping.
.bkm_guard <- function(expr, value) {
  tryCatch(expr, error = function(e) {
    warning(sprintf("%s Operation ignored.", conditionMessage(e)), call. = FALSE)
    value
  })
}
