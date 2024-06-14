#' Shiny Application
#'
#' @return This function does not return anything. It initiates a Shiny app
#' @import shiny
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom shinyjs useShinyjs hide show
#' @importFrom shinythemes shinytheme
#' @importFrom dplyr filter bind_rows
#' @import magrittr
#' @import fcaR
#' @importFrom igraph graph add_vertices add_edges simplify as.undirected
#' @import visNetwork
#' @export
#'
#' @examples
#' \dontrun{
#' launchApp()
#' }
launchApp <- function(){
  appDir <- system.file("shinyapp", package = "sysRecfcaR")
  if (appDir == "") {
    stop("The shiny app could not be found in the package")
  }
  shiny::runApp(appDir)
}
