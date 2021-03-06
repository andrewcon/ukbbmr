% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biobank.r
\name{shapiro.dt}
\alias{shapiro.dt}
\title{Load a Matrix}
\usage{
shapiro.dt(x, t = c(NA), s = 5000, iter = 100)
}
\arguments{
\item{x}{Path to the input file}

\item{t}{Path to the input file}

\item{s}{Path to the input file}

\item{iter}{Path to the input file}
}
\value{
A matrix of the infile
}
\description{
This function loads a file as a matrix. It assumes that the first column
contains the rownames and the subsequent columns are the sample identifiers.
Any rows with duplicated row names will be dropped with the first one being
kepted.
}
