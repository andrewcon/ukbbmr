% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1smr.r
\name{w.score}
\alias{w.score}
\title{Load a Matrix}
\usage{
w.score(data, org_beta, org_cols = c(), org_trait = NA)
}
\arguments{
\item{data}{Path to the input file}

\item{org_beta}{Path to the input file}

\item{org_cols}{Path to the input file}

\item{org_trait}{Path to the input file}
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
