#' Attach dependencies
#'
#' @noRd
#' @importFrom utils packageVersion
#' @importFrom htmltools htmlDependency attachDependencies
attachDependencies <- function(tag){

  version <- as.character(packageVersion("shintodashboard")[[1]])

  dep <- list(
    htmltools::htmlDependency(
      "tooltip", version,
      src = c(href = "shintodashboard/tooltip"),
      script = "tooltip.js",
      stylesheet = "tooltip.css"
    ),
    htmltools::htmlDependency(
      "plotorder", version,
      src = c(href = "shintodashboard/plotorder"),
      script = "plotsdashboard.js"
    ),
    htmltools::htmlDependency(
      "fleetingMessage", version,
      src = c(href = "shintodashboard/fleetingMessage"),
      stylesheet = "fleetingMessage.css"
    ),
    htmltools::htmlDependency(
      "dashboardstyle", version,
      src = c(href = "shintodashboard/dashboardstyle"),
      stylesheet = "style.css"
    )
  )

htmltools::attachDependencies(tag, dep, append = TRUE)
}


