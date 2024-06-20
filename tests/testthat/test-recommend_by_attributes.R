library(testthat)
library(fcaR)
library(Matrix)

test_that("recommend_by_attributes returns correct recommends", {

  fc <- FormalContext$new(vegas)
  fc$find_concepts()

  result <- recommend_by_attributes(fc, c("Traveler type=Couples","Pool"), c("Stars=4"))

  matrix <- matrix(c(
    498, 0.28, "Stars=4", 1, 1, 1, 1, 1, 1
  ), nrow = 1, byrow = TRUE)

  colnames(matrix) <- c("idx", "Confidence", "Attribute", "Traveler type=Couples",
                        "Pool", "Gym", "Spa", "Casino", "Stars=4")

  expected <- as.data.frame(matrix, stringsAsFactors = FALSE)

  expected[,1:2] <- as.numeric(expected[,1:2])
  expected[,4:9] <- as.numeric(expected[,4:9])

  result2 <- recommend_by_attributes(fc,
                                     c("Traveler type=Families", "Spa", "Free internet"),
                                     c("Score=3", "Score=4", "Score=5"))

  matrix2 <- matrix(c(
    383, 0.17, "Score=3", 1, 1, 1, 1, 1, 1, 1, 0, 0,
    382, 0.24, "Score=4", 1, 1, 1, 1, 1, 1, 0, 1, 0,
    381, 0.49, "Score=5", 1, 1, 1, 1, 1, 1, 0, 0, 1
  ), nrow = 3, byrow = TRUE)

  colnames(matrix2) <- c("idx", "Confidence", "Attribute", "Traveler type=Families", "Pool", "Gym", "Spa", "Casino",
                        "Free internet", "Score=3", "Score=4", "Score=5")

  df2 <- as.data.frame(matrix2, stringsAsFactors = FALSE)

  df_numeric <- df2[, c(1:2, 4:12)]

  df_numeric <- apply(df_numeric, 2, as.numeric)

  df2[, c(1:2, 4:12)] <- df_numeric

  expect_equal(result, expected)
  expect_equal(result2, df2)
})

test_that("recommend_by_attributes handles incorrect data values", {
  fc <- FormalContext$new(vegas)
  fc$find_concepts()

  expect_error(recommend_by_attributes(NULL, c("Traveler type=Couples","Pool"), c("Stars=4")))
  expect_error(recommend_by_attributes(fc, c("Traveler type=Coupless","Pool"), c("Stars=43")))
})
