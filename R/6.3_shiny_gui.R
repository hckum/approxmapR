#' Shiny interface for approxmap
#'
#' This function calls a shiny interface for approxmap.
#'
#' @param launch.browser Option will be passed on to \code{\link[shiny]{runApp}}
#' @export

gui_approxmap <- function(launch.browser=TRUE) {
  shiny::runApp(system.file('approxmap_shiny', package='approxmap'),
                launch.browser=launch.browser)
}
