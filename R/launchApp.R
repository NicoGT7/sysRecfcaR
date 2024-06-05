#' Shiny Application
#'
#' @return This function does not return anything. It initiates a Shiny app
#' @import shiny
#' @import shinyWidgets
#' @import dplyr
#' @import magrittr
#' @import fcaR
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(magrittr)
#' library(fcaR)
#' library(shiny)
#' library(shinyWidgets)
#' launchApp()
#' }
launchApp <- function(){
  appDir <- system.file("shinyapp", package = "sysRecfcaR")
  if (appDir == "") {
    stop("The shiny app could not be found in the package")
  }
  shiny::runApp(appDir)
}
