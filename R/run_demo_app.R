#' Run the demo Shiny app
#'
#' This function launches a simple Shiny application that demonstrates
#' the core reUnderwriteR workflow: loading a motor portfolio, computing
#' portfolio KPIs and evaluating a quota share treaty.
#'
#' @export
run_demo_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the demo app.")
  }
  
  app_dir <- system.file("shiny", package = "reUnderwriteR")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. ",
         "Please make sure inst/shiny/app.R exists in the package.")
  }
  
  shiny::runApp(app_dir, display.mode = "normal")
}
