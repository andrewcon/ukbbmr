% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biobank.r
\name{extract.time}
\alias{extract.time}
\title{Load a Matrix}
\usage{
extract.time(
  data,
  datafield,
  datafield_name,
  keep.time = TRUE,
  keep.date = TRUE
)
}
\arguments{
\item{data}{Path to the input file}

\item{datafield}{Path to the input file}

\item{datafield_name}{Path to the input file}

\item{keep.time}{Path to the input file}

\item{keep.date}{Path to the input file}
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
