.validate.data <- function(obj) {
  # Top-level must be a list/array
  if (!is.list(obj)) {
    stop("`data` must be a list/array", call. = FALSE)
  }
  
  # Locate the single metadata and sources wrappers
  i_meta <- which(vapply(
    obj, function(d) is.list(d) && !is.null(d$metadata), logical(1)
  ))
  i_src  <- which(vapply(
    obj, function(d) is.list(d) && !is.null(d$sources),  logical(1)
  ))
  
  if (! identical(length(i_meta), 1L)) {
    stop("`data` must contain exactly one `metadata` entry", call. = FALSE)
  }
  if (! identical(length(i_src), 1L)) {
    stop("`data` must contain exactly one `sources` entry",  call. = FALSE)
  }
  
  md  <- obj[[i_meta]]$metadata
  src <- obj[[i_src]]$sources
  
  # metadata: non-empty named list
  if (!is.list(md) || length(md) == 0L) {
    stop("`metadata` must be a non-empty named list", call. = FALSE)
  }
  
  # sources: allow character or list-of-character; normalize to character
  if (is.list(src)) src <- unlist(src, use.names = FALSE)
  if (!is.character(src) || length(src) == 0L) {
    stop("`sources` must be a non-empty character vector", call. = FALSE)
  }
  
  # Records are all other elements (may be unnamed)
  rec_ix <- setdiff(seq_along(obj), c(i_meta, i_src))
  if (length(rec_ix) == 0L) {
    stop("`data` must contain at least one record element", call. = FALSE)
  }
  
  # At least one record has a non-empty regex
  has_regex <- any(vapply(obj[rec_ix], function(d) {
    is.list(d) && is.character(d$regex) &&
      length(d$regex) == 1L && nzchar(d$regex)
  }, logical(1)))
  if (!has_regex) {
    stop("`data` must contain at least one entry with a non-empty `regex`",
         call. = FALSE)
  }
  
  invisible(NULL)
}

#' Convert text into target classifications (e.g., ISO 3166-1) using a JSON
#' mapping with regular expressions.
#'
#' Pure-R implementation of the **convertu** API.
#' Converts text into a target classification using a JSON mapping, or
#' returns mapping/metadata (`info` / `dump` modes).
#'
#' Behavior:
#' * `info = TRUE` → returns only metadata and sources entries (no conversion).
#' * `dump = TRUE` → returns the full classification (no metadata/sources).
#' * Otherwise → converts `text` using regex-based matching and returns the
#'   value from the requested field `to`.
#'
#' @param data list of named lists (optional). A complete classification
#'   mapping provided directly. If supplied without `json_file`, this data will
#'   be used in-memory for conversions without reading from disk. If both
#'   `data` and `json_file` are supplied, the data is written to `json_file`
#'   and the file path is returned.
#' @param json_file character(1). Path to the classification JSON file.
#'   If not provided, the default bundled `classification.json` is used
#'   (resolved via `system.file("extdata", "classification.json",
#'   package="rconvertu")`). When `data` is not supplied, this file is loaded
#'   and used as the source mapping. When `data` is supplied along with 
#'   `json_file`, the data is written to `json_file`.
#' @param info logical(1). If `TRUE`, return only metadata/sources entries.
#'   No conversion is performed.
#' @param dump logical(1). If `TRUE`, return the full mapping (filtered of
#'   metadata/sources). No conversion is performed.
#' @param to character(1). Target field name to return from matched records
#'   (e.g., `"iso3"`).
#' @param text character(). One or more input strings to convert. A single
#'   string input yields a single string output; a vector yields a character
#'   vector of converted results.
#'
#' @return
#' If `info = TRUE` or `dump = TRUE`, returns a list of records.
#' Otherwise, returns a character vector of converted values:
#' \itemize{
#'   \item If `length(text) == 1`, returns a length-one character scalar.
#'   \item If no match is found for an input, the original value is returned.
#' }
#'
#' @examples
#' # Single conversion
#' cconv(to = "iso3", text = "Czech Republic")
#'
#' # Multiple conversions
#' cconv(to = "iso3", text = c("Czech Republic", "Slovakia"))
#'
#' # Show bundled metadata
#' cconv(info = TRUE)
#'
#' # Dump classification mapping only
#' cconv(dump = TRUE)
#'
#' @section Data template (list of named lists):
#' The classification is a top-level list with three kinds of elements:
#'
#' 1) Many record elements (unnamed or named) with fields:
#'    \itemize{
#'      \item \code{regex}   (chr): pattern matching the input text.
#'      \item \code{name_en} (chr): English short name.
#'      \item \code{name_fr} (chr): French short name (optional).
#'      \item \code{iso3}    (chr): alpha-3 code (example field).
#'      \item \code{iso2}    (chr): alpha-2 code (example field).
#'      \item \code{isoN}    (chr): numeric code (example field).
#'    }
#'
#' 2) One element \code{metadata} (named list) mapping field names to their
#'    human-readable descriptions:
#'    \preformatted{
#'    metadata = list(
#'      name_en = "English short name",
#'      name_fr = "French short name",
#'      iso3    = "alpha-3 code",
#'      iso2    = "alpha-2 code",
#'      isoN    = "numeric"
#'    )
#'    }
#'
#' 3) One element \code{sources} (character vector) with references:
#'    \preformatted{
#'    sources = c(
#'      "https://www.iso.org/iso-3166-country-codes.html",
#'      "https://en.wikipedia.org/wiki/List_of_alternative_country_names"
#'    )
#'    }
#'
#' @export
cconv          <- function(data = NULL, json_file = NULL, info = FALSE,
                           dump = FALSE, to = NULL, text = character()) {
  # retrieve the metadata/sources and classification
  if (! is.null(data)) {                             # read from the argument
    .validate.data(data)
    if (! is.null(json_file)) {
      jsonlite::write_json(data, path = json_file, auto_unbox = TRUE,
                           pretty = TRUE, digits = NA)
      return(invisible(json_file))
    }
  } else {                                           # read from the file
    if (is.null(json_file)) {
      json_file <- system.file("extdata", "classification.json",
                               package = "rconvertu", mustWork = TRUE )
    }
    data <- jsonlite::read_json(json_file, simplifyVector = FALSE)
  }
  if (! is.list(data)) {
    stop("Mapping JSON must be a list of objects",  call. = FALSE)
  }
  metadata       <- Filter(function(d) is.list(d) && (! is.null(d$metadata) ||
                                                        ! is.null(d$sources)),
                           data)
  classification <- Filter(function(d) is.list(d) && (  is.null(d$metadata) &&
                                                          is.null(d$sources)),
                           data)
  
  # return metadata/sources or classification
  if (isTRUE(info)         ) return(metadata      )
  if (isTRUE(dump)         ) return(classification)
  
  # process arguments
  if (!is.character(to) || length(to) != 1L || !nzchar(to)) {
    stop("'to' must be a non-empty character(1)", call. = FALSE)
  }
  if (length(text)     == 0) return(character())
  if (is.character(text)) {
  } else if (is.list(text) && length(text) > 0L &&
             all(vapply(text, function(z) is.character(z) && length(z) == 1L,
                        logical(1)))) {
    text <- unlist(text, use.names = FALSE)
  } else {
    stop("'text' must be a character vector or a list of character(1)",
         call. = FALSE)
  }  
  
  # precompile patterns once
  compiled <- Filter(function(d) is.character(d$regex) && nzchar(d$regex)   &&
                       ! is.null(d[[to]]),
                     classification)
  if (length(compiled) == 0) return(as.character(text))
  
  # convert compiled
  convert_one <- function(s) {
    if (is.na(s)) return(NA_character_)              # keep NA as NA
    s <- as.character(s)
    for (d in compiled) {
      if (grepl(d$regex, s, perl = TRUE, ignore.case = TRUE)) {
        val <- d[[to]]
        return(if (is.null(val) || is.na(val)) s else as.character(val))
      }
    }
    s
  }
  result <- vapply(text, convert_one, character(1), USE.NAMES = FALSE)
  if (length(text) == 1) result[1] else result
}

#' @rdname cconv
#' @export
convertu       <- cconv                                # alias
