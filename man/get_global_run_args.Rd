% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta_data.R
\name{get_global_run_args}
\alias{get_global_run_args}
\title{Get arguments used to produce a global run}
\usage{
get_global_run_args(run_name = NULL, output_dir = NULL, root_dir = NULL)
}
\arguments{
\item{run_name}{The run name.}

\item{output_dir}{Path to directory containing outputs. See
Section \dQuote{Specifying results directory} in the help file
for \code{\link{get_output_dir}}. Note that \code{root_dir} is
ignored if \code{output_dir} is supplied.}

\item{root_dir}{A root directory if needed, i.e., the location of
the directory called \dQuote{output}.}
}
\value{
A named list.
}
\description{
This function is a convenience wrapper that calls
\code{\link{get_global_mcmc_args}} for married and unmarried runs,
and \code{\link{get_combine_runs_args}} for all women runs.
}
\seealso{
Other Model run meta info functions: 
\code{\link{get_combine_runs_args}()},
\code{\link{get_country_index}()},
\code{\link{get_global_mcmc_args}()},
\code{\link{get_global_post_process_args}()},
\code{\link{get_marital_group}()},
\code{\link{get_model_meta_info}()},
\code{\link{get_run_name}()}
}
\author{
Mark Wheldon
}
\concept{Model run meta info functions}
