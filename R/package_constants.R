###-----------------------------------------------------------------------------
### * Constants

###-----------------------------------------------------------------------------
### ** Sting Constants

##' Standard marital group names
##'
##' Returns a \emph{named} character vector with standard marital
##' group names. The names are the abbreviations in lower case.
##'
##' @param return_case Case of the return value.
##' @return Character string.
##' @author Mark Wheldon
##' @export
get_std_marr_group_names <- function(return_case = c("lower", "sentence", "title", "upper"), named = FALSE) {
    return_case <- match.arg(return_case)
    basis <- c("mwra" = "Married", "uwra" = "Unmarried", "wra" = "All Women")
    basis2 <- c("mwra" = "Married", "uwra" = "Unmarried", "wra" = "All women")
    if (identical(return_case, "lower")) out <- tolower(basis)
    else if (identical(return_case, "upper")) out <- toupper(basis)
    else if (identical(return_case, "sentence")) out <- basis2
    else if (identical(return_case, "title")) out <- basis
    else stop("Nothing selected")
    if (!named) out <- unname(out)
    return(out)
}
