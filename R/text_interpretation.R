###-----------------------------------------------------------------------------
### * Interpret Character Strings


##' Attempt to guess marital group from run name
##'
##' @param run_name
##' @return Returns \dQuote{married}, \dQuote{unmarried}, or
##'     \dQuote{all_women} if only one of them was found in
##'     \code{run_name}. Otherwise returns \code{NULL}.
##' @author Mark Wheldon
guess_marital_group <- function(run_name) {
    m <- grepl("[^u][^n]married", run_name)
    u <- grepl("unmarried", run_name)
    a <- grepl("all[_ -.]women", run_name)
    if(identical(m + u + a, 1L)) {
        if(m) "married"
        else if(u) "unmarried"
        else if(a) "all_women"
    } else NULL
}
