% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{splines2_basis}
\alias{splines2_basis}
\title{Create B-Spline basis}
\usage{
splines2_basis(x, knots, deg, periodic = FALSE, intercept = TRUE)
}
\arguments{
\item{x}{Vector of values.}

\item{knots}{Vector of knots.}

\item{deg}{Degree of the Spline functions.}

\item{periodic}{Whether the basis should be periodic or not.}

\item{intercept}{Whether the firs column should be kept.}
}
\value{
Returns a matrix of B-Spline basis functions.
}
\description{
This function creates a B-Spline matrix.
}
\examples{
n <- 9
deg <- 3
mu <- 0.35
x <- 0:1000 / 1000

knots <- make_knots(n, mu = mu, deg = deg)

B <- splines2_basis(x, knots, deg)
ts.plot(B, col = 1:dim(B)[2])

# Periodic Case
B <- splines2_basis(x, knots, deg, periodic = TRUE)
ts.plot(B, col = 1:dim(B)[2])

}
