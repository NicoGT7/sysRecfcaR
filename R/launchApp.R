#' Shiny Application
#'
#' @return This function does not return anything. It initiates a Shiny app
#' @import shiny
#' @import shinyWidgets
#' @importFrom shinyjs useShinyjs hide show
#' @importFrom shinythemes shinytheme
#' @import dplyr
#' @import magrittr
#' @import fcaR
#' @importFrom igraph graph add_vertices add_edges simplify as.undirected
#' @import visNetwork
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(magrittr)
#' library(fcaR)
#' library(shiny)
#' library(shinyWidgets)
#' library(shinyjs)
#' library(shinythemes)
#' library(igraph)
#' library(visNetwork)
#' launchApp()
#' }
launchApp <- function(){
  appDir <- system.file("shinyapp", package = "sysRecfcaR")
  if (appDir == "") {
    stop("The shiny app could not be found in the package")
  }
  shiny::runApp(appDir)
}
