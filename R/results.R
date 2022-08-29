###-----------------------------------------------------------------------------
### * Work With Main Model Results

##' Load and return quantiles of country-specific model parameters.
##'
##' This function \code{\link{load}}s and returns the object in the
##' file \file{par.ciq.rda} found in
##' \code{file.path{output_dir}}. These are the quantiles of the
##' country-specific model parameters, such as the asymptotes, rate
##' paratmeters, timing parameters, etc.
##'
##' The specific parameters loaded are, \dQuote{omega.c}, \dQuote{T.c},
##' \dQuote{pmax.c}, \dQuote{Romega.c}, \dQuote{RT.c}, \dQuote{Rmax.c},
##' \dQuote{unmet.intercept.c}.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##' @return The \code{\link{load}}ed object.
##' @author Mark Wheldon
##' @export
get_model_quantiles <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                         verbose = FALSE) {

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)
    tmp_env <- new.env()
    if (verbose) on.exit(message("Loaded '", file.path(output_dir, "par.ciq.rda"), "'."),
                         add = TRUE, after = FALSE)
    return(get(load(file.path(output_dir, "par.ciq.rda"), envir = tmp_env)[1], envir = tmp_env))
}



##' DEPRECATED
##'
##' Use \code{\link{get_model_quantiles}} instead.
##'
##' @author Mark Wheldon
##' @export
get_countries_model_params_q <- function(...) {
    warning("'get_countries_model_params_q' is DEPRACATED and will soon be removed! Please use 'get_model_quantiles' instead.")
    get_model_quantiles(...)
}
