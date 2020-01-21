#' Add JS paths to shiny
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
.onLoad <- function(...) {
  shiny::addResourcePath('shintodashboard', system.file("assets", package = "shintodashboard"))
}

