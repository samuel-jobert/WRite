#' Pipe operator
#'
#' Re-export of the `%>%` operator from the magrittr package: it allows
#' chaining WRite functions without loading magrittr (the native pipe `|>`
#' works too). See the magrittr documentation for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value (for example a WRite document).
#' @param rhs A function call to which `lhs` is passed as first argument.
#' @return The result of the call `rhs(lhs)`.
#' @examples
#' doc <- doc_read(write_example()) %>%
#'   bkm_replace_text("my_bookmark", "New text") %>%
#'   doc_fill(list(amount_2025 = "1,500"))
#' bkm_text(doc, "my_bookmark")
#' @export
NULL
