library(testthat)
library(fcaR)
library(Matrix)

test_that("getAttributes works correctly", {

  fc <- FormalContext$new(vegas)
  fc$find_concepts()
  concepts <- fc$concepts

  result <- getAttributes(concepts, 59)
  expect <- c("Pool","Score=2")

  result2 <- getAttributes(concepts, 348)
  expect2 <- c("Traveler type=Families", "Pool", "Casino", "Free internet", "Stars=4.5")

  expect_equal(result, expect)
  expect_equal(result2, expect2)
})

test_that("getAttributes handles incorrect data types", {
  fc <- FormalContext$new(vegas)
  fc$find_concepts()
  concepts <- fc$concepts

  expect_error(getAttributes(concepts, NULL))
  expect_error(getAttributes("not concepts", 65))
})
