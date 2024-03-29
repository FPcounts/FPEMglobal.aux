##' Storage format of years
##'
##' @description
##' Years are stored in many external files and R objects that are
##' used and created by \pkg{FPEMglobal} and \pkg{FPEMglobal.aux}. Two
##' different formats currently exist, defined as follows:
##' \describe{
##' \item{whole-number}{The year is stored as a whole number, e.g., \dQuote{1970}, \dQuote{1971},
##' etc.}
##' \item{mid-year}{The year is stored with the decimal \dQuote{0.5} appended, e.g., \dQuote{1970.5}, \dQuote{1971.5},
##' etc.}}
##'
##' @details
##' The \dQuote{whole-number} format is a general format that is
##' ambiguous regarding the exact point in the year the associated
##' value was recorded or is referring to.
##'
##' The \dQuote{mid-year} format is a specific format that indicates
##' the associated value was recorded, or refers to, the middle of the
##' year, i.e., 1st July. For example, in a population count dataset
##' this format would indicate the counts are 1st July
##' \dQuote{mid-year} counts.
##'
##' @section Specifying year storage format in FPEMglobal.aux:
##' When created by \pkg{FPEMglobal.aux} functions,
##' the format can be controlled with the argument
##' \code{years_as_midyear}. This must be passed one of the following:
##' \describe{
##' \item{\code{TRUE}}{\dQuote{mid-year} format.}
##' \item{\code{FALSE}}{\dQuote{whole-number} format.}}
##'
##' @section Useage in package FPEMglobal:
##' Package \pkg{FPEMglobal} uses the formats inconsistently.
##'
##' \subsection{Denominator counts}{
##' In the input file for the denominator counts, years are coded into
##' the column names and are in \dQuote{whole-number} format. This is
##' despite the fact that denominator populations have been, and are
##' expected to be, mid-year population counts.
##'
##' Conversely, in the various \pkg{FPEMglobal} outputs they are in \dQuote{mid-year} format.
##'
##' In the csv outputs, e.g., \dQuote{Country_perc_Modern.csv}, the years are
##' stored in the column names as in the snippet below:
##' \preformatted{
##' Name         Iso  Percentile  1970.5    1971.5    1972.5
##' Afghanistan  4    0.025       0.004564  0.005108  0.005733}
##'
##' In the the R output objects in, e.g., \dQuote{res.country.rda}
##' that stores the posterior summaries, years are stored in the
##' \code{dimnames} as in the following snippet:
##' \preformatted{
##' str(res.country$CIprop.Lg.Lcat.qt$France$Modern)
##' num [1:5, 1:61] 0.0109 0.0703 0.3663 0.6492 0.756 ...
##' - attr(*, "dimnames")=List of 2
##'   ..$ : chr [1:5] "0.025" "0.1" "0.5" "0.9" ...
##'   ..$ : chr [1:61] "1970.5" "1971.5" "1972.5" "1973.5" ...}
##' }
##'
##' @name year_storage_format
##' @rdname year_storage_format
##' @author Mark Wheldon
NULL
