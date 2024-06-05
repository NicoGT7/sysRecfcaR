#' Returns the attributes of the concept in character format
#'
#' @param concepts Concept Lattice by fcaR
#' @param idx number of a concept
#'
#' @return attributes of the concept
#' @importFrom fcaR FormalContext
#' @export
#'
#' @examples
#' library(fcaR)
#' fc <- FormalContext$new(vegas)
#' fc$find_concepts()
#' concepts <- fc$concepts
#' getAttributes(concepts,59)
getAttributes <- function(concepts, idx){
  intent <- concepts$sub(idx)$get_intent()
  attributes <- intent$get_attributes()[which(as.vector(intent$get_vector()) == 1)]
  return(attributes)
}
