library(testthat)
library(fcaR)
library(Matrix)

test_that("getSupportSub returns correct matrix", {

  fc <- FormalContext$new(vegas)
  fc$find_concepts()
  concepts <- fc$concepts

  result <- getSupportSub(concepts, 502)

  expected <- matrix(c(
    502, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
    518, 0.6, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
    931, 0.4, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
    1987, 0.4, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
    945, 0.2, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
    1308, 0.2, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
    2002, 0.2, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
    2082, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
  ), nrow = 8, byrow = TRUE)
  colnames(expected) <- c(
    "idx", "Confidence", "Period of stay=Dec-Feb", "Period of stay=Jun-Aug", "Period of stay=Mar-May",
    "Period of stay=Sep-Nov", "Traveler type=Business", "Traveler type=Couples", "Traveler type=Families",
    "Traveler type=Friends", "Traveler type=Solo", "Pool", "Gym", "Tennis court", "Spa", "Casino", "Free internet",
    "Stars=3", "Stars=3.5", "Stars=4", "Stars=4.5", "Stars=5", "Score=1", "Score=2", "Score=3", "Score=4", "Score=5"
  )

  expect_equal(result, expected)
})

test_that("getSupportSub handles incorrect data values", {
  fc <- FormalContext$new(vegas)
  fc$find_concepts()
  concepts <- fc$concepts

  expect_error(getSupportSub(concepts, 5000))
  expect_error(getSupportSub(NULL, 100))
})
