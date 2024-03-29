% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{post_process_model}
\alias{post_process_model}
\title{Post Process Data from conline Class}
\usage{
post_process_model(model_instance, names)
}
\arguments{
\item{model_instance}{An instance of conline.}

\item{names}{A named list with dimnames of \code{y} and \code{experts}.}
}
\description{
This function works in conjunction with the conline class.
After the main learning task, it takes the output of the
conline class and returns an object suitable for, visualization,
further, and deployment.
analysis.
}
