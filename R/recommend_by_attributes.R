#' Recommendation Based on the Best Confidence Match for One or Multiple Attributes
#'
#' @description
#' This method applies closure to the selected attributes in order to
#' perform a search in the concept lattice and retain the best possible
#' subconcept that contains the desired attribute.
#'
#' @param fc a FormalContext using fcaR
#' @param selectedAttributes the desired attributes
#' @param probAttributes the attributes to check
#'
#' @return a data frame with the best possible recommendation for each attribute to check
#' @importFrom fcaR FormalContext
#' @importFrom sysRecfcaR getIdx getSupportSub
#' @import dplyr
#' @import magrittr
#' @export
#'
#' @examples
#' library(fcaR)
#' fc <- FormalContext$new(vegas)
#' recommend_by_attributes(fc, c("Traveler type=Friends","Gym"), c("Score=5"))
#' recommend_by_attributes(fc,
#'                         c("Traveler type=Friends","Pool", "Spa"),
#'                         c("Score=3", "Score=4", "Score=5"))
recommend_by_attributes <- function(fc, selectedAttributes, probAttributes){
  fc$find_concepts()
  concepts <- fc$concepts

  set_attributes <- Set$new(fc$attributes)
  set_attributes$assign(attributes = selectedAttributes, values = rep(1,length(selectedAttributes)))

  s <- fc$closure(set_attributes)

  idxConcept <- getIdx(concepts, s)

  dfSubconceptos <- as.data.frame(getSupportSub(concepts,idxConcept))

  final_result <- data.frame()

  for (i in 1:length(probAttributes)) {
    res <- dfSubconceptos %>% filter(.data[[probAttributes[i]]] == 1)

    result <- res[1,1:2]

    result$atr <- probAttributes[i]

    for (col in 3:ncol(res)) {
      if (any(res[1,col] == 1)) {
        result <- cbind(result, res[1,col, drop = FALSE])
      }
    }

    final_result <- bind_rows(final_result, result)
    final_result[is.na(final_result)] <- 0

  }

  return(final_result)
}
