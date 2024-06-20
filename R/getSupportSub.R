#' Calculation of subconcepts
#'
#' @param concepts Concept Lattice by fcaR
#' @param idx number of a concept
#'
#' @return matrix ordered by confidence and with all the attributes it has
#' @importFrom fcaR FormalContext
#' @importFrom sysRecfcaR getIdx
#' @export
#'
#' @examples
#' library(fcaR)
#' fc <- FormalContext$new(vegas)
#' fc$find_concepts()
#' concepts <- fc$concepts
#' getSupportSub(concepts, 502)
getSupportSub <- function(concepts,idx){

  C <- concepts$sub(idx)

  suppC <- concepts$support()[idx]

  subconcepts <- concepts$subconcepts(C)

  matrix <- matrix(nrow = subconcepts$size(), ncol = length(concepts$sub(1)$get_intent()$get_attributes()) + 2)
  colNames <- c("idx", "Confidence", concepts$sub(1)$get_intent()$get_attributes())
  colnames(matrix) <- colNames

  for (i in 1:subconcepts$size()) {
    suppD <- subconcepts$support()[i]

    confidence <- suppD/suppC

    subConcept <- subconcepts$sub(i)
    matrix[i,1] <- getIdx(concepts, subConcept$get_intent())
    matrix[i,2] <- round(confidence,2)

    aux <- subconcepts$sub(i)$get_intent()
    aux2 <- as.vector(t(as.matrix(aux$get_vector())))

    matrix[i, 3:ncol(matrix)] <- aux2
  }
  return(matrix[order(matrix[, 2], decreasing = TRUE), ])
}
