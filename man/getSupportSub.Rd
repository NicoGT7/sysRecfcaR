% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSupportSub.R
\name{getSupportSub}
\alias{getSupportSub}
\title{Calculation of subconcepts}
\usage{
getSupportSub(concepts, idx)
}
\arguments{
\item{concepts}{Concept Lattice by fcaR}

\item{idx}{number of a concept}
}
\value{
matrix ordered by confidence and with all the attributes it has
}
\description{
Calculation of subconcepts
}
\examples{
library(fcaR)
fc <- FormalContext$new(vegas)
fc$find_concepts()
concepts <- fc$concepts
getSupportSub(concepts, 502)
}
