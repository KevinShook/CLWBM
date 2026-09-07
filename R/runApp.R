#' @export
runExample <- function() {
  appDir <- system.file("myapp", package = "CLWBM")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `CLWBM`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
