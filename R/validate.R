#' Validate a classification list.
#'
#' Ensures the provided JSON-based classification data follows the expected
#' structure and contains valid fields.
#'
#' @param x list. The classification object loaded via
#'   `jsonlite::fromJSON(..., simplifyVector = FALSE)`.
#'
#' @return Logical, `TRUE` if valid, otherwise an error is raised.
#' @examples
#' path <- system.file("extdata", "classification.json", package = "rconvertu")
#' cls <- jsonlite::fromJSON(path, simplifyVector = FALSE)
#' check_classification(cls)
#'
#' @export
check_classification <- function(x) {
  # --- find metadata & sources and collect records for both shapes ---
  md <- NULL; src <- NULL; records <- NULL
  
  # Shape B: named top-level
  if (is.list(x) && (! is.null(x$records) || ! is.null(x$metadata)          ||
                     ! is.null(x$sources))) {
    if (! is.null(x$metadata)) md      <- x$metadata
    if (! is.null(x$sources))  src     <- x$sources
    if (! is.null(x$records))  records <- x$records
  }
  
  # Shape A: tail wrappers inside an array
  if (is.null(md) || is.null(src) || is.null(records)) {
    if (! is.list(x)) stop("`data` must be a list/array", call. = FALSE)
    i_meta <- which(vapply(x, function(d) is.list(d) && ! is.null(d$metadata),
                           logical(1)))
    i_src  <- which(vapply(x, function(d) is.list(d) && ! is.null(d$sources),
                           logical(1)))
    if (! identical(length(i_meta), 1L)) stop("exactly one `metadata`",
                                              call. = FALSE)
    if (! identical(length(i_src), 1L))  stop("exactly one `sources`",
                                              call. = FALSE)
    md      <- x[[i_meta]]$metadata
    src     <- x[[i_src]]$sources
    rec_ix  <- setdiff(seq_along(x), c(i_meta, i_src))
    records <- x[rec_ix]
  }
  
  # type checks (no enforced keys)
  if (! is.list(md) || length(md) == 0L      ) {
    stop("`metadata` must be a non-empty list",   call. = FALSE)
  }
  if (  is.list(src)) {
    src <- unlist(src, use.names = FALSE)
  }
  if (! is.character(src) || ! length(src)   ) {
    stop("`sources` must be non-empty character", call. = FALSE)
  }
  if (! is.list(records) || ! length(records)) {
    stop("need at least one record",              call. = FALSE)
  }
  
  # at least one record has non-empty regex
  has_regex <- any(vapply(records, function(d) {
    is.list(d) && is.character(d$regex) && length(d$regex) == 1L            &&
      nzchar(d$regex)
  }, logical(1)))
  if (! has_regex) stop("at least one record needs non-empty `regex`",
                        call. = FALSE)
  
  TRUE
}
