#' Get path to countenance example data
#'
#' @param file Name of file. If NULL, the example files will be listed.
#'
#' @return path and file name
#' @export
#'
#' @examples
#' countenance_example("femg_raw_exported_sample.txt")
countenance_example <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "countenance"))
  }
  else {
    system.file("extdata", file, package = "countenance", mustWork = TRUE)
  }
}
