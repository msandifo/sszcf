#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
pkg_resource <- function(...) {
  system.file("resources", ..., package = "sszcf", mustWork = TRUE)
}


