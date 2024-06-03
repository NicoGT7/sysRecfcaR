library(testthat)
library(fcaR)
library(Matrix)

test_that("getIdx works correctly", {

  fc <- FormalContext$new(vegas)
  fc$find_concepts()
  concepts <- fc$concepts

  atr <- concepts$sub(122)
  result <- getIdx(concepts, atr$get_intent())

  atr2 <- concepts$sub(623)
  result2 <- getIdx(concepts, atr2$get_intent())

  expect_equal(result, 122)
  expect_equal(result2, 623)
})

test_that("getIdx handles incorrect data types", {
  fc <- FormalContext$new(vegas)
  fc$find_concepts()
  concepts <- fc$concepts
  atr <- concepts$sub(54)

  expect_error(getIdx(concepts, NULL))
  expect_error(getIdx("not concepts", atr$get_intent()))
})
