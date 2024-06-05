#' Returns a graph given a sublattice from fcaR
#'
#' @param sublattice sublattice from package fcaR
#'
#' @return graph type igraph
#' @importFrom fcaR FormalContext
#' @importFrom sysRecfcaR getIdx
#' @importFrom igraph graph add_vertices add_edges simplify as.undirected
#' @export
#'
#' @examples
#' library(fcaR)
#' library(igraph)
#' fc <- FormalContext$new(vegas)
#' fc$find_concepts()
#' concepts <- fc$concepts
#' idx <- which(fc$concepts$support() > 0.9)
#' sublattice <- fc$concepts$sublattice(idx)
#' graph_sublattice(sublattice)
graph_sublattice <- function(sublattice) {
  num <- sublattice$size()

  grafo <- graph(c())

  grafoRes <- add_vertices(grafo, num)

  for (i in 1:num) {
    if(i != 1){

      upN <- sublattice$upper_neighbours(sublattice$sub(i))
      upN <- upN$to_list()
      for (j in 1:length(upN)) {
        atr <- upN[[j]]$get_intent()
        arista <- getIdx(sublattice, atr)

        grafoRes <- add_edges(grafoRes, c(i, arista))
      }

    }

    if(i < num){

      loN <- sublattice$lower_neighbours(sublattice$sub(i))
      loN <- loN$to_list()
      for (j in 1:length(loN)) {
        atr <- loN[[j]]$get_intent()
        arista <- getIdx(sublattice, atr)

        grafoRes <- add_edges(grafoRes, c(i, arista))
      }
    }
  }

  grafoRes <- simplify(grafoRes)
  grafoRes <- as.undirected(grafoRes, mode = "collapse")
  return(grafoRes)
}
