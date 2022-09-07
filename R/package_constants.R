###-----------------------------------------------------------------------------
### * Constants

###-----------------------------------------------------------------------------
### ** String Constants

##' Standard marital group names
##'
##' Returns a \emph{named} character vector with standard marital
##' group names. The names are the abbreviations in lower case.
##'
##' @param return_case Case of the return value.
##' @return Character string.
##' @author Mark Wheldon
##'
##' @family String constants
##'
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


##' Report all possible names of family planning indicators used in FPEMglobal
##'
##' This function returns a character vector containing indicator
##' names as used in the \code{dimnames} attribute of the country and
##' aggregate trajectory arrays (e.g., see
##' \code{\link{get_country_traj_muw}}).
##'
##' The indicator names stored in the country and aggregate trajectory
##' arrays differ by statistic (\code{stat} argument) and marrital
##' group (\code{marr_group}) argument. In addition, more ratio
##' indicators available in the all women results. The \code{aw_set}
##' argument provides control over whether all all women ratio names
##' are returned (\code{"all"}), only those in common with
##' married/unmarried results (\code{"only common"}), or only those
##' that are unique to all women results (\code{"only extra"}).
##'
##' @inheritParams get_csv_res
##' @param marr_group Marrital group for which names are required.
##' @param aw_set The set of all women names required; see \dQuote{Details}.
##' @return Character vector of indicator names.
##' @author Mark Wheldon
##'
##' @family String constants
##'
##' @export
get_std_indicator_names <- function(stat = c("prop", "count", "ratio"),
                                marr_group = get_std_marr_group_names(),
                                aw_set = c("all", "only common", "only extra")) {
    stat <- match.arg(stat)
    marr_group <- match.arg(stat)
    aw_set <- match.arg(aw_set)

    ## Name Strings
    names_prop_count <-
        c("Total", "Traditional", "Modern", "Unmet", "TotalPlusUnmet", "TradPlusUnmet")
    names_ratio_common <-
        c("Modern/Total", "Met Demand", "Z", "Met Demand with Modern Methods")
    names_ratio_aw_extra <-
        c("Modern Married Over All", "Trad Married Over All", "Unmet Married Over All",
          "Modern Unmarried Over All", "Trad Unmarried Over All", "Unmet Unmarried Over All")

    ## Output
    if (stat %in% c("prop", "count")) return(names_prop_count)
    else { # stat must == "ratio"
        if (!identical(marr_group, "all women")) return(names_ratio_common)
        else { # marr_group must == "all women"
            if (identical(aw_set, "all")) return(c(names_ratio_common, names_ratio_aw_extra))
            else if (identical(aw_set, "only common")) return(names_ratio_common)
            else return(names_ratio_aw_extra)
        }
    }
}
