% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trajectories.R
\name{get_country_traj_muw}
\alias{get_country_traj_muw}
\title{Load and return country-level trajectories for married and unmarried women}
\usage{
get_country_traj_muw(
  run_name = NULL,
  output_dir = NULL,
  root_dir = NULL,
  iso_code
)
}
\arguments{
\item{run_name}{The run name.}

\item{output_dir}{Path to directory containing outputs. See
Section \dQuote{Specifying results directory} in the help file
for \code{\link{get_output_dir}}. Note that \code{root_dir} is
ignored if \code{output_dir} is supplied.}

\item{root_dir}{A root directory if needed, i.e., the location of
the directory called \dQuote{output}.}

\item{iso_code}{Numeric ISO code of country to get trajectories
for.}
}
\value{
The loaded country trajectory object.
}
\description{
This function \code{\link{load}}s the MCMC sample from the
posterior sample for married or unmarried for a single country and returns it as an R
object. Trajectories are loaded from \file{.rda} files found in
the subdirectory \file{countrytrajectories} of the results
directory (see below). The filename for the given country is
determined by reference to an index which must be in the file
\code{file.path(output_dir, "iso.Ptp3s.key.csv")}.
}
\details{
\code{\link{load}} is called is such a way that ensures nothing is
added to the global environment; the only copy of the loaded
object is that returned by the function.

Country trajectories are 3D arrays:
\preformatted{str(...)
num [1:61, 1:3, 1:13344] 0.0622 0.0725 0.0879 0.0633 0.1078 ...
- attr(*, "dimnames")=List of 3
 ..$ : chr [1:61] "1970.5" "1971.5" "1972.5" "1973.5" ...
 ..$ : chr [1:3] "Traditional" "Modern" "Unmet"
 ..$ : NULL}

\subsection{Married/unmarried vs all women trajectories}{
The country trajectories for all women are stored differently. Do
not use this function for all women country trajectories. Use
\code{\link{get_country_traj_aw}} instead.

The country trajectories for all women are \emph{counts}. Those
for married and unmarried are \emph{proportions}.}
}
\section{Specifying results directory}{

See the section in \code{\link{get_csv_res}}.
}

\seealso{
Other trajectory functions: 
\code{\link{get_agg_traj}()},
\code{\link{get_country_traj_aw}()},
\code{\link{get_model_traj}()}
}
\author{
Mark Wheldon
}
\concept{trajectory functions}
