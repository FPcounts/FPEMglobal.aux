
###-----------------------------------------------------------------------------
### * Interpret Character Strings

##' Attempt to guess marital group from run name
##'
##' @inheritParams get_output_dir
##' @return Returns \dQuote{married}, \dQuote{unmarried}, or
##'     \dQuote{all women} if only one of them was found in
##'     \code{run_name}. Otherwise returns \code{NULL}.
##' @author Mark Wheldon
guess_marital_group <- function(run_name) {
    m <- grepl("[^u][^n]married", run_name)
    u <- grepl("unmarried", run_name)
    a <- grepl("all[_ -.]women", run_name)
    if (identical(m + u + a, 1L)) {
        if (m) "married"
        else if (u) "unmarried"
        else if (a) "all_women"
    } else NULL
}


##' Lists unique parameter names in mcmc array
##'
##' Parameters in the mcmc.array object are named with country and
##' region indices, e.g., \code{theta.ci[98,7]}. This function
##' strips the indices in the square brackets, and the square brackets
##' themselves, and returns the unique strings in what's left. This is
##' helpful for listing the \emph{names} of the parameters.
##'
##' @param mcmc_array An mcmc array from a completed model run, e.g.,
##'     of the kind retrieved by \code{\link{get_model_traj}}.
##' @return Character vector of unique names.
##' @author Mark Wheldon
##' @export
extract_model_param_names <- function(mcmc_array) {
    unique(gsub("\\[[0-9,]+\\]", "", dimnames(mcmc_array)[[3]]))
}
