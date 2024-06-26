% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_manipulation.R
\name{clean_indic_names}
\alias{clean_indic_names}
\title{Clean indicator names}
\usage{
clean_indic_names(indicator)
}
\arguments{
\item{indicator}{Character string to \dQuote{clean} up.}
}
\value{
Character string.
}
\description{
This function takes out spaces and makes the whole string lower
case. This is not quite the same as lower snake case as spaces are
not replaced with underscores, rather they are just removed
resulting in a single unbroken string.
}
\examples{

## Known indicators are:
list_all_std_indicator_names()

}
\seealso{
Other String utility functions: 
\code{\link{clean_col_names}()},
\code{\link{convert_M49_region_names}()},
\code{\link{convert_country_names_2_latex}()},
\code{\link{convert_param_name}()},
\code{\link{shorten_reg_names_manus}()}
}
\author{
Mark Wheldon
}
\concept{String utility functions}
