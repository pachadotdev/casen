#' Normalized path and metadata extraction
#' @keywords internal
#' @export
archive <- function(path) {
  assert(
    "{path} is not a readable file path",
    is_readable(path)
  )

  path <- normalizePath(path)

  res <- archive_metadata(path)
  class(res) <- c("archive", class(res))
  res
}

#' Error messages
#' @importFrom glue glue
#' @keywords internal
#' @export
assert <- function(msg, ...) {
  tests <- unlist(list(...))

  if (!all(tests)) {
    stop(structure(list(
      message = glue::glue(msg, .envir = parent.frame()),
      .call = sys.call(-1)
    ), class = c("error", "condition")))
  }
}

as_archive <- function(x) {
  if (inherits(x, "archive")) {
    return(x)
  }
  archive(x)
}

is_readable <- function(path) {
  is_string(path) &&
    file.exists(path)
  # file.access fails on some NFS, such as shared folders on virtualbox
  # https://stat.ethz.ch/pipermail/r-devel/2008-December/051461.html
  # file.access(path, mode = 4)[[1]] == 0
}

is_number <- function(x) {
  is.numeric(x) && length(x) == 1
}

is_string <- function(x) {
  is.character(x) && length(x) == 1
}
