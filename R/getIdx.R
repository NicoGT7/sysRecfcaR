#' Returns the index corresponding to the given set of attributes
#'
#' @param concepts Concept Lattice by fcaR
#' @param atr Set of attributes
#'
#' @return index of the Concept Lattice
#' @importFrom Matrix which
#' @importFrom fcaR FormalContext
#' @export
#'
#' @examples
#' library(fcaR)
#' fc <- FormalContext$new(vegas)
#' fc$find_concepts()
#' concepts <- fc$concepts
#' atr <- concepts$sub(122)
#' getIdx(concepts, atr$get_intent())
getIdx <- function(concepts, atr){
  return(Matrix::which(fcaR:::.equal_sets(atr$get_vector(), concepts$intents())))
}
