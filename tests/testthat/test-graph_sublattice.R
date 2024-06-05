library(testthat)
library(fcaR)
library(Matrix)

test_that("graph_sublattice works correctly", {

  fc <- FormalContext$new(vegas)
  fc$find_concepts()
  concepts <- fc$concepts
  idx <- which(fc$concepts$support() > 0.9)
  sublattice <- fc$concepts$sublattice(idx)
  grafo <- graph_sublattice(sublattice)

  expect_true(igraph::is_igraph(grafo))

  expect_equal(igraph::vcount(grafo), 16)
  expect_equal(igraph::ecount(grafo), 32)

  expect_true(igraph::are_adjacent(grafo, 1, 2))
})

test_that("graph_sublattice handles incorrect data types", {

  expect_error(graph_sublattice(NULL))
})
