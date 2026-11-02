#' Insert a Word document on a bookmark
#'
#' Inserts the content (body) of a Word document on a bookmark of the target
#' document. `bkm_insert_docx()` takes the path of a `.docx` file,
#' `bkm_insert_doc()` a WRite document object (left unchanged).
#'
#' The content is really merged (no "altChunk"):
#' * the styles used that are missing from the target document are imported
#'   (a style that exists in the target document, by identifier or by name,
#'   is kept as is);
#' * numbering (lists), images, hyperlinks and footnotes / endnotes are
#'   copied with new identifiers;
#' * the bookmarks of the inserted document are renumbered; a bookmark whose
#'   name already exists in the target is renamed with a suffix (`name_1`, or
#'   `name_2` if `name_1` also exists...), with a warning listing the
#'   renamings. References to this bookmark in the inserted content
#'   (internal links, `REF` / `PAGEREF` fields) are updated. The `_GoBack`
#'   bookmark (cursor position) of the inserted document is removed if it
#'   already exists;
#' * section breaks and comments of the inserted document are ignored.
#'
#' If the paragraph containing the bookmark contains only the bookmark, it is
#' replaced by the inserted content; otherwise it is split in two around the
#' inserted content.
#'
#' @param doc WRite document object (target).
#' @param name Bookmark name.
#' @param file Path of the `.docx` file to insert.
#' @param source WRite document object to insert.
#' @param position `"replace"` (replaces the content of the bookmark),
#'   `"before"` (at the start of the bookmark) or `"after"` (at the end of the
#'   bookmark).
#' @param keep_bookmark Keep the bookmark around the inserted content?
#' @return `doc`, invisibly.
#' @section Illustration:
#' ```
#' Target document              Document to insert      Result
#' Introduction                 Clause 1                Introduction
#' [clauses: ]                  Clause 2                [clauses: Clause 1
#' Conclusion                                                     Clause 2]
#'                                                      Conclusion
#'
#' Paragraph with text: "See [appendix: ] above" becomes
#'   See
#'   [appendix: Clause 1
#'              Clause 2]
#'    above
#' ```
#' @examples
#' doc <- doc_read(write_example("example"))
#'
#' # Insert a .docx file on an empty bookmark
#' bkm_insert_docx(doc, "empty_bookmark", write_example("to_insert"))
#' cat(bkm_text(doc, "empty_bookmark"))
#'
#' # Rich document: styles, bulleted list, image, note, link, table are imported
#' bkm_insert_docx(doc, "my_bookmark", write_example("complex"), position = "after")
#'
#' # Insert an already opened (and modified) document; the source is unchanged
#' appendix <- doc_read(write_example("to_insert")) |>
#'   doc_replace_text("First paragraph.", "Custom text")
#' doc2 <- doc_read(write_example("example")) |>
#'   bkm_insert_doc("empty_bookmark", appendix, keep_bookmark = FALSE)
#' bkm_list(doc2)$name
#'
#' # Bookmark with the same name already present: renamed with a warning
#' doc3 <- doc_read(write_example("complex"))
#' doc3 <- doc_add_paragraph(doc3, "", bookmark = "here")
#' withCallingHandlers(
#'   bkm_insert_docx(doc3, "here", write_example("complex")),
#'   warning = function(w) {
#'     message("Warning: ", conditionMessage(w))
#'     invokeRestart("muffleWarning")
#'   })
#' bkm_list(doc3)$name   # "source_bookmark" and "source_bookmark_1"
#' @export
bkm_insert_docx <- function(doc, name, file, position = c("replace", "before", "after"),
                            keep_bookmark = TRUE) {
  source <- doc_read(file)
  bkm_insert_doc(doc, name, source, position = position, keep_bookmark = keep_bookmark)
}

#' @rdname bkm_insert_docx
#' @export
bkm_insert_doc <- function(doc, name, source, position = c("replace", "before", "after"),
                           keep_bookmark = TRUE) {
  .check_doc(doc)
  .check_doc(source)
  position <- match.arg(position)
  if (identical(doc, source)) source <- doc_copy(source)
  .bkm_guard(.insert_source(doc, .bkm_find(doc, name), source, position, keep_bookmark), NULL)
  invisible(doc)
}

# Inserts on bookmark `bkm` the blocks of the body of `source` (all except sectPr if `blocks` is NULL).
.insert_source <- function(doc, bkm, source, position, keep_bookmark, blocks = NULL) {
  ctx <- new.env(parent = emptyenv())
  ctx$bkm_max <- .bookmark_max_id(doc)
  ctx$bkm_names <- .bkm_names(doc)
  ctx$docpr_max <- .docpr_max(doc)
  ctx$parts <- new.env(parent = emptyenv())
  ctx$rels <- list()
  ctx$renamed <- character()

  x_src <- .xml(source, source$main)
  .merge_root_ns(bkm$xml, x_src)
  if (is.null(blocks)) blocks <- .find_all(x_src, "/w:document/w:body/*[not(self::w:sectPr)]")

  if (position == "replace") .empty_range(bkm)
  res <- .insert_blocks(bkm, blocks, position)
  ins <- res$ins
  if (length(ins) > 0) {
    groups <- list(list(src = source$main, dst = bkm$part, nodes = ins))
    for (type in c("footnote", "endnote")) {
      g <- .copy_notes(doc, source, ins, type)
      if (!is.null(g)) groups[[length(groups) + 1]] <- g
    }
    styles <- character()
    num_ids <- character()
    for (g in groups) {
      .clean_inserted(g$nodes)
      .renumber_bookmarks(g$nodes, ctx)
      .renumber_docpr(g$nodes, ctx)
      .remap_rels(doc, source, g$src, g$dst, g$nodes, ctx)
      styles <- c(styles, .values(g$nodes, "w:pStyle | descendant-or-self::w:rStyle | descendant-or-self::w:tblStyle"))
    }
    all_nodes <- do.call(c, lapply(groups, function(g) g$nodes))
    .rename_references(all_nodes, ctx$renamed)
    copies <- .merge_styles(doc, source, unique(styles), all_nodes)
    num_ids <- .values(c(all_nodes, copies), "w:numId")
    .merge_numbering(doc, source, unique(num_ids), c(all_nodes, copies))
    .finalize_split(res)
  }
  if (!isTRUE(keep_bookmark)) .remove_markers(bkm)
  if (length(ctx$renamed) > 0) {
    warning(sprintf("Bookmark(s) of the inserted document already present in the target document, renamed: %s",
                    paste(names(ctx$renamed), ctx$renamed, sep = " -> ", collapse = ", ")), call. = FALSE)
  }
  invisible(doc)
}

# w:val values of the elements matching `xpath` (relative, descendant-or-self) in `nodes`.
.values <- function(nodes, xpath) {
  unlist(lapply(nodes, function(n) .attr(.find_all(n, paste0("descendant-or-self::", xpath)), "w:val")))
}

.docpr_max <- function(doc) {
  ids <- unlist(lapply(.content_parts(doc), function(p) {
    xml2::xml_attr(.find_all(.xml(doc, p), "//wp:docPr"), "id")
  }))
  max(c(0L, suppressWarnings(as.integer(ids))), na.rm = TRUE)
}

# Adds to the target root the namespaces (and mc:Ignorable prefixes) of the source root.
.merge_root_ns <- function(x_dst, x_src) {
  decls <- function(x) {
    txt <- substr(as.character(x, options = character()), 1, 20000)
    pos <- regexpr("<[^?!][^>]*>", txt)
    root <- regmatches(txt, pos)
    m <- regmatches(root, gregexpr('xmlns:([A-Za-z0-9_.-]+)="([^"]*)"', root))[[1]]
    uris <- sub('^xmlns:[^=]+="([^"]*)"$', "\\1", m)
    names(uris) <- sub('^xmlns:([^=]+)=.*$', "\\1", m)
    uris
  }
  d_dst <- decls(x_dst)
  d_src <- decls(x_src)
  root <- xml2::xml_root(x_dst)
  missing <- setdiff(names(d_src), names(d_dst))
  for (pfx in missing) xml2::xml_set_attr(root, paste0("xmlns:", pfx), d_src[[pfx]])
  ign_src <- strsplit(.or(.attr(xml2::xml_root(x_src), "mc:Ignorable"), ""), " +")[[1]]
  ign_dst <- strsplit(.or(.attr(root, "mc:Ignorable"), ""), " +")[[1]]
  ign_dst <- ign_dst[!is.na(ign_dst) & nzchar(ign_dst)]
  to_add <- setdiff(ign_src[!is.na(ign_src) & nzchar(ign_src)], ign_dst)
  to_add <- to_add[to_add %in% c(names(d_dst), missing)]
  if (length(to_add) > 0 && "mc" %in% c(names(d_dst), missing)) {
    .set_attr(root, "mc:Ignorable", paste(c(ign_dst, to_add), collapse = " "))
  }
  invisible()
}

.is_empty_paragraph <- function(p) {
  .count(p, paste0(
    "count(descendant::w:t[string-length() > 0] | descendant::w:drawing | descendant::w:pict | ",
    "descendant::w:object | descendant::w:tab[parent::w:r] | descendant::w:br | descendant::w:fldChar | ",
    "descendant::w:fldSimple | descendant::w:sym | descendant::w:footnoteReference | ",
    "descendant::w:endnoteReference | descendant::w:instrText | descendant::m:oMath | ",
    "w:pPr/w:sectPr)")) == 0
}

# Moves the bookmark markers of a paragraph to the start or the end of a block.
.move_markers <- function(p, block, where = c("start", "end")) {
  where <- match.arg(where)
  markers <- as.list(.find_all(p, "w:bookmarkStart | w:bookmarkEnd"))
  if (length(markers) == 0) return(invisible())
  if (.name(block) == "w:p") {
    if (where == "end") {
      for (m in markers) .move(m, block, "child")
    } else {
      ppr <- .find1(block, "w:pPr")
      first <- xml2::xml_child(block)
      if (!.is_missing(ppr)) {
        ref <- ppr
        for (m in markers) { .move(m, ref, "after"); ref <- m }
      } else if (!.is_missing(first)) {
        for (m in markers) .move(m, first, "before")
      } else {
        for (m in markers) .move(m, block, "child")
      }
    }
  } else if (where == "start") {
    for (m in markers) .move(m, block, "before")
  } else {
    ref <- block
    for (m in markers) { .move(m, ref, "after"); ref <- m }
  }
  invisible()
}

# Copies the source blocks next to `ref`; returns the list of inserted nodes.
.copy_sequence <- function(ref, where, blocks) {
  ins <- list()
  cur <- ref
  for (b in blocks) {
    nv <- xml2::xml_add_sibling(cur, b, .where = if (length(ins) == 0) where else "after")
    ins[[length(ins) + 1]] <- nv
    cur <- nv
  }
  ins
}

# Copies the blocks at the insertion point, splitting the paragraph of the bookmark if needed.
# Returns list(ins = inserted nodes, p, p2 = halves of the split paragraph or NULL).
.insert_blocks <- function(bkm, blocks, position) {
  anchor <- if (position == "after") bkm$end else bkm$start
  p <- .paragraph_of(anchor)
  if (is.null(p)) {
    ins <- .copy_sequence(anchor, if (position == "after") "before" else "after", blocks)
    return(list(ins = ins, p = NULL, p2 = NULL))
  }
  if (!identical(xml2::xml_parent(anchor), p)) {
    stop("The bookmark is placed in a structure (hyperlink, field, content control...) that is not supported for inserting a document.", call. = FALSE)
  }
  if (length(blocks) == 0) return(list(ins = list(), p = NULL, p2 = NULL))
  # Paragraph split: p keeps what precedes the insertion point, p2 the rest.
  p2 <- .add(p, "w:p", sibling = "after")
  ppr <- .find1(p, "w:pPr")
  if (!.is_missing(ppr)) {
    xml2::xml_add_child(p2, ppr)
    xml2::xml_remove(.find_all(p, "w:pPr/w:sectPr"))
  }
  to_move <- as.list(xml2::xml_find_all(anchor, "following-sibling::*"))
  if (position == "after") to_move <- c(list(anchor), to_move)
  for (n in to_move) .move(n, p2, "child")
  ins <- .copy_sequence(p, "after", blocks)
  list(ins = ins, p = p, p2 = p2)
}

# Removes the empty halves of the split paragraph, moving their bookmarks into the inserted content.
.finalize_split <- function(res) {
  ins <- res$ins
  if (is.null(res$p) || length(ins) == 0) return(invisible())
  if (.is_empty_paragraph(res$p)) {
    .move_markers(res$p, ins[[1]], "start")
    xml2::xml_remove(res$p)
  }
  if (.is_empty_paragraph(res$p2)) {
    .move_markers(res$p2, ins[[length(ins)]], "end")
    xml2::xml_remove(res$p2)
  }
  invisible()
}

# Removes from the inserted nodes what cannot be reused as is.
.clean_inserted <- function(nodes) {
  for (n in nodes) {
    for (p in .find_all(n, "descendant-or-self::w:p")) {
      .set_attr(p, "w14:paraId", NULL)
      .set_attr(p, "w14:textId", NULL)
    }
    xml2::xml_remove(.find_all(n, "descendant::w:pPr/w:sectPr"))
    xml2::xml_remove(.find_all(n, paste(
      "descendant-or-self::w:commentRangeStart", "descendant-or-self::w:commentRangeEnd",
      "descendant::w:r[w:commentReference]", sep = " | ")))
  }
  invisible()
}

.renumber_bookmarks <- function(nodes, ctx) {
  mapping <- list()
  for (n in nodes) {
    for (m in .find_all(n, "descendant-or-self::w:bookmarkStart | descendant-or-self::w:bookmarkEnd")) {
      old <- .or(.attr(m, "w:id"), "NA")
      if (.name(m) == "w:bookmarkStart") {
        name <- .attr(m, "w:name")
        # Word cursor-position bookmark: only one per document.
        if (identical(name, "_GoBack") && name %in% ctx$bkm_names) {
          mapping[[old]] <- "remove"
          xml2::xml_remove(m)
          next
        }
        ctx$bkm_max <- ctx$bkm_max + 1L
        mapping[[old]] <- as.character(ctx$bkm_max)
        .set_attr(m, "w:id", as.character(ctx$bkm_max))
        if (!is.na(name) && name %in% ctx$bkm_names) {
          new_name <- .free_bookmark_name(name, ctx$bkm_names)
          .set_attr(m, "w:name", new_name)
          ctx$renamed[name] <- new_name
          name <- new_name
        }
        ctx$bkm_names <- c(ctx$bkm_names, name)
      } else if (identical(mapping[[old]], "remove")) {
        xml2::xml_remove(m)
      } else if (!is.null(mapping[[old]])) {
        .set_attr(m, "w:id", mapping[[old]])
      }
    }
  }
  invisible()
}

# First name `name_1`, `name_2`... absent from `existing` (Word limits names to 40 characters).
.free_bookmark_name <- function(name, existing) {
  k <- 1
  repeat {
    suffix <- paste0("_", k)
    cand <- paste0(substr(name, 1, 40 - nchar(suffix)), suffix)
    if (!cand %in% existing) return(cand)
    k <- k + 1
  }
}

# Updates the references (internal links, REF/PAGEREF fields...) to renamed bookmarks.
.rename_references <- function(nodes, renamed) {
  if (length(renamed) == 0) return(invisible())
  rename <- function(x) {
    for (a in names(renamed)) {
      x <- gsub(paste0("(?<![\\w])\\Q", a, "\\E(?![\\w])"), renamed[[a]], x, perl = TRUE)
    }
    x
  }
  for (n in nodes) {
    for (h in .find_all(n, "descendant-or-self::w:hyperlink[@w:anchor]")) {
      a <- .attr(h, "w:anchor")
      if (a %in% names(renamed)) .set_attr(h, "w:anchor", renamed[[a]])
    }
    for (t in .find_all(n, "descendant::w:instrText")) xml2::xml_text(t) <- rename(xml2::xml_text(t))
    for (f in .find_all(n, "descendant-or-self::w:fldSimple[@w:instr]")) .set_attr(f, "w:instr", rename(.attr(f, "w:instr")))
  }
  invisible()
}

.renumber_docpr <- function(nodes, ctx) {
  for (n in nodes) {
    for (d in .find_all(n, "descendant-or-self::wp:docPr")) {
      ctx$docpr_max <- ctx$docpr_max + 1L
      xml2::xml_set_attr(d, "id", as.character(ctx$docpr_max))
    }
  }
  invisible()
}

# Copies the relationships (r:id, r:embed...) of the inserted nodes to the target part.
.remap_rels <- function(doc, source, p_src, p_dst, nodes, ctx) {
  rels <- .rels_df(source, p_src)
  key <- paste(p_src, p_dst, sep = "|")
  map <- .or(ctx$rels[[key]], character())
  xp <- sprintf("descendant-or-self::*[@*[namespace-uri()='%s']]", .ns[["r"]])
  for (n in nodes) {
    for (el in .find_all(n, xp)) {
      at <- xml2::xml_attrs(el, ns = .ns)
      at <- at[startsWith(names(at), "r:")]
      for (nm in names(at)) {
        old <- at[[nm]]
        if (is.na(map[old])) {
          rel <- rels[rels$id == old, , drop = FALSE]
          if (nrow(rel) == 0) {
            warning(sprintf("Relationship '%s' not found in the inserted document.", old), call. = FALSE)
            next
          }
          map[old] <- if (!is.na(rel$mode[1]) && rel$mode[1] == "External") {
            .add_rel(doc, p_dst, rel$type[1], rel$target[1], external = TRUE)
          } else {
            nv <- .copy_part(source, .absolute_target(p_src, rel$target[1]), doc, ctx)
            .add_rel(doc, p_dst, rel$type[1], .relative_path(p_dst, nv))
          }
        }
        .set_attr(el, nm, map[[old]])
      }
    }
  }
  ctx$rels[[key]] <- map
  invisible()
}

# Copies a part (image, chart...) and, recursively, its own relationships.
.copy_part <- function(source, path, doc, ctx) {
  if (exists(path, envir = ctx$parts, inherits = FALSE)) return(get(path, envir = ctx$parts))
  nv <- .free_path(doc, path)
  assign(path, nv, envir = ctx$parts)
  dest <- file.path(doc$dir, nv)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (exists(path, envir = source$parts, inherits = FALSE)) {
    .write_xml(get(path, envir = source$parts), dest)
  } else {
    file.copy(file.path(source$dir, path), dest)
  }
  ct <- .ct_override(source, path)
  if (!is.na(ct)) {
    .ct_add_override(doc, nv, ct)
  } else {
    ext <- tools::file_ext(path)
    .ct_ensure_default(doc, ext, .ct_default(source, ext))
  }
  rels <- .rels_df(source, path)
  for (i in seq_len(nrow(rels))) {
    external <- !is.na(rels$mode[i]) && rels$mode[i] == "External"
    target <- rels$target[i]
    if (!external) target <- .relative_path(nv, .copy_part(source, .absolute_target(path, target), doc, ctx))
    .add_rel(doc, nv, rels$type[i], target, external = external, id = rels$id[i])
  }
  nv
}

# Creates the notes part (footnotes/endnotes) if it does not exist.
.notes_part <- function(doc, type) {
  p <- .part_by_type(doc, doc$main, paste0(type, "s"))
  if (!is.na(p)) return(p)
  p <- .free_path(doc, paste0("word/", type, "s.xml"))
  sep <- function(id, t) sprintf(paste0(
    '<w:%s w:type="%s" w:id="%d"><w:p><w:pPr><w:spacing w:after="0" w:line="240" w:lineRule="auto"/></w:pPr>',
    '<w:r><w:%s/></w:r></w:p></w:%s>'), type, t, id, t, type)
  x <- xml2::read_xml(paste0(sprintf('<w:%ss xmlns:w="%s" xmlns:r="%s">', type, .ns[["w"]], .ns[["r"]]),
                             sep(-1, "separator"), sep(0, "continuationSeparator"),
                             sprintf("</w:%ss>", type)))
  .xml_set(doc, p, x)
  .add_rel(doc, doc$main, .rel_type(paste0(type, "s")), .relative_path(doc$main, p))
  .ct_add_override(doc, p, .ct[[paste0(type, "s")]])
  p
}

# Copies the notes referenced by the inserted nodes; returns a group list(src, dst, nodes).
.copy_notes <- function(doc, source, nodes, type) {
  refs <- do.call(c, lapply(nodes, function(n) as.list(.find_all(n, sprintf("descendant-or-self::w:%sReference", type)))))
  if (length(refs) == 0) return(NULL)
  p_src <- .part_by_type(source, source$main, paste0(type, "s"))
  if (is.na(p_src)) {
    for (r in refs) xml2::xml_remove(r)
    return(NULL)
  }
  x_src <- .xml(source, p_src)
  p_dst <- .notes_part(doc, type)
  x_dst <- .xml(doc, p_dst)
  .merge_root_ns(x_dst, x_src)
  id_max <- max(c(0L, suppressWarnings(as.integer(.attr(.find_all(x_dst, sprintf("/w:%ss/w:%s", type, type)), "w:id")))), na.rm = TRUE)
  copies <- list()
  for (r in refs) {
    note <- .find1(x_src, sprintf("/w:%ss/w:%s[@w:id=%s]", type, type, .xpath_string(.attr(r, "w:id"))))
    if (.is_missing(note)) {
      xml2::xml_remove(r)
      next
    }
    id_max <- id_max + 1L
    nv <- xml2::xml_add_child(xml2::xml_root(x_dst), note)
    .set_attr(nv, "w:id", as.character(id_max))
    .set_attr(r, "w:id", as.character(id_max))
    copies[[length(copies) + 1]] <- nv
  }
  list(src = p_src, dst = p_dst, nodes = copies)
}

# Imports the missing styles (and their dependencies); returns the copied styles.
.merge_styles <- function(doc, source, ids, nodes) {
  p_dst <- .part_by_type(doc, doc$main, "styles")
  p_src <- .part_by_type(source, source$main, "styles")
  if (is.na(p_dst) || is.na(p_src) || length(ids) == 0) return(list())
  x_dst <- .xml(doc, p_dst)
  x_src <- .xml(source, p_src)
  .merge_root_ns(x_dst, x_src)
  names_dst <- .attr(.find1(.find_all(x_dst, "/w:styles/w:style"), "w:name"), "w:val")
  ids_dst <- .attr(.find_all(x_dst, "/w:styles/w:style"), "w:styleId")
  copies <- list()
  map <- character()
  done <- character()
  queue <- ids
  while (length(queue) > 0) {
    id <- queue[1]
    queue <- queue[-1]
    if (is.na(id) || id %in% done) next
    done <- c(done, id)
    if (id %in% ids_dst) next
    st <- .find1(x_src, sprintf("/w:styles/w:style[@w:styleId=%s]", .xpath_string(id)))
    if (.is_missing(st)) next
    name <- .attr(.find1(st, "w:name"), "w:val")
    i <- match(tolower(name), tolower(names_dst))
    if (!is.na(i)) {
      map[id] <- ids_dst[i]
      next
    }
    nv <- xml2::xml_add_child(xml2::xml_root(x_dst), st)
    .set_attr(nv, "w:default", NULL)
    copies[[length(copies) + 1]] <- nv
    queue <- c(queue, .attr(.find_all(nv, "w:basedOn | w:next | w:link"), "w:val"))
  }
  if (length(map) > 0) {
    xp <- "descendant-or-self::w:pStyle | descendant-or-self::w:rStyle | descendant-or-self::w:tblStyle | descendant-or-self::w:basedOn | descendant-or-self::w:next | descendant-or-self::w:link"
    for (n in c(nodes, copies)) {
      for (el in .find_all(n, xp)) {
        v <- .attr(el, "w:val")
        if (!is.na(v) && !is.na(map[v])) .set_attr(el, "w:val", map[[v]])
      }
    }
  }
  copies
}

# Copies the numbering definitions used and renumbers the w:numId.
.merge_numbering <- function(doc, source, num_ids, nodes) {
  num_ids <- num_ids[!is.na(num_ids) & num_ids != "0"]
  if (length(num_ids) == 0) return(invisible())
  p_src <- .part_by_type(source, source$main, "numbering")
  if (is.na(p_src)) return(invisible())
  x_src <- .xml(source, p_src)
  p_dst <- .part_by_type(doc, doc$main, "numbering")
  if (is.na(p_dst)) {
    p_dst <- .free_path(doc, "word/numbering.xml")
    .xml_set(doc, p_dst, xml2::read_xml(sprintf('<w:numbering xmlns:w="%s"/>', .ns[["w"]])))
    .add_rel(doc, doc$main, .rel_type("numbering"), .relative_path(doc$main, p_dst))
    .ct_add_override(doc, p_dst, .ct[["numbering"]])
  }
  x_dst <- .xml(doc, p_dst)
  .merge_root_ns(x_dst, x_src)
  root <- xml2::xml_root(x_dst)
  max_num <- max(c(0L, as.integer(.attr(.find_all(x_dst, "/w:numbering/w:num"), "w:numId"))), na.rm = TRUE)
  max_abs <- max(c(-1L, as.integer(.attr(.find_all(x_dst, "/w:numbering/w:abstractNum"), "w:abstractNumId"))), na.rm = TRUE)
  map <- character()
  map_abs <- character()
  for (old in num_ids) {
    num <- .find1(x_src, sprintf("/w:numbering/w:num[@w:numId=%s]", .xpath_string(old)))
    if (.is_missing(num)) next
    abs_old <- .attr(.find1(num, "w:abstractNumId"), "w:val")
    if (is.na(map_abs[abs_old])) {
      abs <- .find1(x_src, sprintf("/w:numbering/w:abstractNum[@w:abstractNumId=%s]", .xpath_string(abs_old)))
      if (.is_missing(abs)) next
      last <- .find_all(x_dst, "/w:numbering/w:abstractNum")
      first_num <- .find1(x_dst, "/w:numbering/w:num")
      nv_abs <- if (length(last) > 0) {
        xml2::xml_add_sibling(last[[length(last)]], abs, .where = "after")
      } else if (!.is_missing(first_num)) {
        xml2::xml_add_sibling(first_num, abs, .where = "before")
      } else {
        xml2::xml_add_child(root, abs)
      }
      max_abs <- max_abs + 1L
      .set_attr(nv_abs, "w:abstractNumId", as.character(max_abs))
      xml2::xml_remove(.find_all(nv_abs, "w:nsid"))
      map_abs[abs_old] <- as.character(max_abs)
    }
    lasts <- .find_all(x_dst, "/w:numbering/w:num")
    cleanup <- .find1(x_dst, "/w:numbering/w:numIdMacAtCleanup")
    nv_num <- if (length(lasts) > 0) {
      xml2::xml_add_sibling(lasts[[length(lasts)]], num, .where = "after")
    } else if (!.is_missing(cleanup)) {
      xml2::xml_add_sibling(cleanup, num, .where = "before")
    } else {
      xml2::xml_add_child(root, num)
    }
    max_num <- max_num + 1L
    .set_attr(nv_num, "w:numId", as.character(max_num))
    .set_attr(.find1(nv_num, "w:abstractNumId"), "w:val", map_abs[[abs_old]])
    map[old] <- as.character(max_num)
  }
  for (n in nodes) {
    for (el in .find_all(n, "descendant-or-self::w:numId")) {
      v <- .attr(el, "w:val")
      if (!is.na(v) && !is.na(map[v])) .set_attr(el, "w:val", map[[v]])
    }
  }
  invisible()
}
