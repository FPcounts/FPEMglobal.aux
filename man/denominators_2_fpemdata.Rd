% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_data.R
\name{denominators_2_fpemdata}
\alias{denominators_2_fpemdata}
\title{Convert input file to fpemdata format}
\usage{
denominators_2_fpemdata(output_dir = NULL)
}
\arguments{
\item{output_dir}{Path to directory containing outputs. See
Section \dQuote{Specifying results directory} in the help file
for \code{\link{get_output_dir}}.}
}
\value{
A \code{\link[tibble]{tibble}}.
}
\description{
Takes the raw input file from an \pkg{FPEMglobal} run and returns
it in \pkg{fpemdata} format. \code{\link{get_used_input_data}} is
called to get the input file.
}
\seealso{
get_used_input_data

Other FPEM data converters: 
\code{\link{input_data_2_fpemdata}()}
}
\author{
Mark Wheldon
}
\concept{FPEM data converters}
