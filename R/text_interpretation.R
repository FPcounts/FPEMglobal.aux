
###-----------------------------------------------------------------------------
### * Interpret Character Strings

##' Attempt to guess marital group from run name
##'
##' @inheritParams get_output_dir
##' @return Returns \dQuote{married}, \dQuote{unmarried}, or
##'     \dQuote{all_women} if only one of them was found in
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


## Get 'long' marital group name from the acronym
switch_marr_group_names <- function(x, return_case = c("lower", "title", "sentence", "upper")) {
    x <- tolower(x)
    x <- match.arg(x, choices = c("mwra", "uwra", "wra", "awra"))
    basis <- get_std_marr_group_names(return_case = return_case)
    return(as.character(basis[x])) # Need 'as.character' to remove names.
}


##' Standard marital group names
##'
##' Returns a \emph{named} character vector with standard marital
##' group names. The names are the abbreviations in lower case.
##'
##' @param return_case Case of the return value.
##' @return Character string.
##' @author Mark Wheldon
##' @export
get_std_marr_group_names <- function(return_case = c("lower", "sentence", "title", "upper")) {
    return_case <- match.arg(return_case)
    basis <- c("mwra" = "Married", "uwra" = "Unmarried", "wra" = "All Women")
    basis2 <- c("mwra" = "Married", "uwra" = "Unmarried", "wra" = "All women")
    if (identical(return_case, "lower")) return(tolower(basis))
    else if (identical(return_case, "upper")) return(toupper(basis))
    else if (identical(return_case, "sentence")) return(basis2)
    else if (identical(return_case, "title")) return(basis)
    return(stop("Nothing selected"))
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
