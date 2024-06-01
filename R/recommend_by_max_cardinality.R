#' Recommendation Based on Maximum Cardinality with Minimum Confidence
#'
#' @param fc a FormalContext using fcaR
#' @param selectedAttributes the desired attributes
#' @param conf_value a value between 0 and 1 to filter concepts by that confidence
#'
#' @return a data frame with the best recommendations sorted by the number of attributes the concept possesses
#' @importFrom fcaR FormalContext
#' @importFrom sysRecfcaR getIdx getSupportSub
#' @export
#'
#' @examples
#' library(fcaR)
#' fc <- FormalContext$new(vegas)
#' recommend_by_max_cardinality(fc, c("Traveler type=Friends","Gym"), 0.5)
#' recommend_by_max_cardinality(fc, c("Pool", "Spa", "Score=4"), 0.6)
recommend_by_max_cardinality <- function(fc, selectedAttributes, conf_value){
  fc$find_concepts()
  concepts <- fc$concepts

  set_attributes <- Set$new(fc$attributes)
  set_attributes$assign(attributes = selectedAttributes, values = rep(1,length(selectedAttributes)))

  s <- fc$closure(set_attributes)

  idxConcept <- getIdx(concepts, s)

  dfSubconceptos <- as.data.frame(getSupportSub(concepts,idxConcept))

  dfSubconceptos <- dfSubconceptos[dfSubconceptos$confidence > conf_value, ]

  result <- dfSubconceptos[,1:2]

  for (col in 3:ncol(dfSubconceptos)) {
    if (any(dfSubconceptos[,col] == 1)) {
      result <- cbind(result, dfSubconceptos[,col, drop = FALSE])
    }
  }

  result$ones <- rowSums(result[, 3:ncol(result)] == 1)

  result <- result[order(-result$ones), ]

  result$ones <- NULL

  return(result)
}
