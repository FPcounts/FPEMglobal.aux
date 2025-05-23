% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constants.R
\name{get_std_indicator_names}
\alias{get_std_indicator_names}
\alias{list_all_std_indicator_names}
\title{Report family planning indicator names}
\usage{
get_std_indicator_names(
  stat = c("prop", "count", "ratio", "age_ratio"),
  marital_group = get_std_marital_group_names(),
  adjusted = c("orig", "adj"),
  aw_set = c("all", "only_common", "only_extra"),
  indicator_name_format = c("traj_array", "csv_results_file_names", "clean")
)

list_all_std_indicator_names()
}
\arguments{
\item{stat}{Which statistic(s) should be loaded? See
\dQuote{Details}.}

\item{marital_group}{Character; marital group for which names are
required.}

\item{adjusted}{Character; returns names for original
(\dQuote{orig}) or adjusted medians results.}

\item{aw_set}{Character; the set of all women names required; see
\dQuote{Details}.}

\item{indicator_name_format}{Character; return the indicator names as used in
trajectory array dimnames or as used in the file names of the
csv results files?}
}
\value{
Character vector of indicator names.
}
\description{
These functions return character vectors containing indicator
names as used in either the \code{dimnames} attribute of the
country and aggregate trajectory arrays (e.g., see
\code{\link{get_country_traj_muw}}), or in the file names of the
csv results files. \code{list_all_std_indicator_names} returns
all valid names, \code{get_std_indicator_names}
provides functionality to subet by statistic and marital group.
}
\details{
The indicator names stored in the country and aggregate trajectory
arrays differ by statistic (\code{stat} argument) and marital
group (\code{marital_group}) argument.

In addition, more ratio indicators are available in the all women
results if \code{adjusted = "orig"}. The \code{aw_set} argument
provides control over whether all all-women ratio names are
returned (\code{"all"}), only those in common with
married/unmarried results (\code{"only_common"q}), or only those
that are unique to all women results (\code{"only_extra"}).

The extra all women indicators are not available if \code{adjusted
= "adj"}. In this case, \code{aw_set = "all"} and \code{aw_set =
"only_common"} return the same thing, and \code{aw_set =
"only_extra"} is an error.
}
\seealso{
Other String constants: 
\code{\link{get_aggregate_names}()},
\code{\link{get_std_marital_group_names}()}
}
\author{
Mark Wheldon
}
\concept{String constants}
