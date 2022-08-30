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
        else if (a) "all women"
    } else NULL
}


## Conversion from short/acronym marital group names to long names.
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


## Get 'long' marital group name from the acronym
switch_marr_group_names <- function(x = c("mwra", "uwra", "wra", "awra"),
                                    return_case = c("lower", "title", "sentence", "upper")) {
    x <- tolower(x)
    x <- match.arg(x, choices = c("mwra", "uwra", "wra", "awra"))
    return_case <- match.arg(return_case)
    basis <- get_std_marr_group_names(return_case = return_case)
    return(as.character(basis[x])) # Need 'as.character' to remove names.
}
