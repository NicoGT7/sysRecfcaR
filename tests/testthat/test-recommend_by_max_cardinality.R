library(testthat)
library(fcaR)
library(Matrix)

test_that("recommend_by_max_cardinality returns correct recommends", {

  fc <- FormalContext$new(vegas)
  fc$find_concepts()

  result <- recommend_by_max_cardinality(fc, c("Traveler type=Solo",
                                               "Tennis court", "Free internet"), 0.5)

  matrix <- matrix(c(
    198, 1, 1, 1, 1, 1, 1, 1, 1, 0,
    201, 0.67, 1, 1, 1, 1, 1, 1, 1, 1
  ), nrow = 2, byrow = TRUE)

  colnames(matrix) <- c("idx", "confidence", "Traveler type=Solo", "Pool", "Gym", "Tennis court",
                        "Spa", "Casino", "Free internet", "Stars=4")

  df <- as.data.frame(matrix, stringsAsFactors = FALSE)

  df <- df[order(df$confidence), ]

  expect_equal(result, df)
})

test_that("recommend_by_max_cardinality handles incorrect data values", {
  fc <- FormalContext$new(vegas)
  fc$find_concepts()

  expect_error(recommend_by_max_cardinality(NULL, c("Traveler type=Solo"), 0.75))
  expect_error(recommend_by_max_cardinality(fc, c("Tennis court", "Free internet"), 1.2))
})
