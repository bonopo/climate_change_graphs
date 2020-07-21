#' @export
runExample <- function() {
  appDir <- system.file("shiny-CCgraphs/", "myapp", package = "CCgraphs")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `CCgraphs`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
