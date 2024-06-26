% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/countries_aggregates.R
\name{get_country_classifications}
\alias{get_country_classifications}
\title{Get country geographic classifications}
\usage{
get_country_classifications(
  M49_region_names_names = TRUE,
  clean_col_names = TRUE
)
}
\arguments{
\item{M49_region_names_names}{Logical; should
\code{\link{convert_M49_region_names}(..., convert_from = "M49_region_names")} be
run to standardize country and area names?}

\item{clean_col_names}{Logical; when \code{TRUE}, the column names
of the result are \sQuote{cleaned} by applying
\code{\link{clean_col_names}}. See \dQuote{Details} for a note
about \dQuote{percentile} vs. \dQuote{quantile}.}
}
\value{
A \code{\link[tibble]{tibble}} with the aggregates.
}
\description{
Returns a table of geographic country groupings. The file is read
using \code{\link[readr]{read_csv}}. See \dQuote{Note} in the
documentation for \code{\link{get_195_countries}}.
}
\seealso{
Other Country and region/aggregate names: 
\code{\link{get_185_countries}()},
\code{\link{get_195_countries}()},
\code{\link{get_aggregate_names}()},
\code{\link{get_used_special_aggregates}()},
\code{\link{get_used_unpd_regions}()}
}
\author{
Mark Wheldon
}
\concept{Country and region/aggregate names}
