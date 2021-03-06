% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normals4.R
\docType{data}
\name{normals4}
\alias{normals4}
\title{Data generated from a mixture of two multivariate Normal distributions, 3 dimensions.
A dataset containing 150 observations generated from a mixture of two multivariate
Normal distributions. 75 observations come from a distribution with mean vector
(1, 3, 2) and 75 observations come from a distribution with mean vector (4, 6, 0).
For each distribution, the variables each have unit variance and are uncorrelated.
The dataset is clusterable.}
\format{A data frame with 150 rows and 4 variables:

\describe{
   \item{x}{x variable}
   \item{y}{y variable}
   \item{z}{z variable}
   \item{cluster}{Distribution from which the observation was sampled}
}}
\usage{
normals4
}
\description{
Remove the cluster variable before using the dataset in any tests.
}
\keyword{datasets}
