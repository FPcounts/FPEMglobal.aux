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
##' @param snake_case Logical; use \dQuote{_} instead of spaces?
##' @return Character string.
##' @author Mark Wheldon
##'
##' @family String constants
##'
##' @export
get_std_marital_group_names <- function(return_case = c("lower", "sentence", "title", "upper"),
                                        snake_case = FALSE,
                                        named = FALSE) {
    return_case <- match.arg(return_case)
    stopifnot(is.logical(snake_case))
    basis <- c("mwra" = "Married", "uwra" = "Unmarried", "wra" = "All Women")
    basis2 <- c("mwra" = "Married", "uwra" = "Unmarried", "wra" = "All women")
    if (identical(return_case, "lower")) out <- tolower(basis)
    else if (identical(return_case, "upper")) out <- toupper(basis)
    else if (identical(return_case, "sentence")) out <- basis2
    else if (identical(return_case, "title")) out <- basis
    else stop("Nothing selected")
    if (snake_case) out <- gsub(" ", "_", out)
    if (!named) out <- unname(out)
    return(out)
}


##' Report family planning indicator names as used in trajectory arrays
##'
##' These functions return character vectors containing indicator
##' names as used in the \code{dimnames} attribute of the country and
##' aggregate trajectory arrays (e.g., see
##' \code{\link{get_country_traj_muw}}). \code{get_traj_array_indicator_names}
##' returns all valid names, \code{get_traj_array_indicator_names_select}
##' provides functionality to subet by statistic and marital group.
##'
##' The indicator names stored in the country and aggregate trajectory
##' arrays differ by statistic (\code{stat} argument) and marital
##' group (\code{marital_group}) argument. In addition, more ratio
##' indicators available in the all women results. The \code{aw_set}
##' argument provides control over whether all all women ratio names
##' are returned (\code{"all"}), only those in common with
##' married/unmarried results (\code{"only common"}), or only those
##' that are unique to all women results (\code{"only extra"}).
##'
##' @inheritParams get_csv_res
##' @param marital_group Marital group for which names are required.
##' @param aw_set The set of all women names required; see \dQuote{Details}.
##' @return Character vector of indicator names.
##' @author Mark Wheldon
##'
##' @family String constants
##'
##' @name get_traj_array_indicator_names
##' @export
get_traj_array_indicator_names_select <- function(stat = c("prop", "count", "ratio", "age ratio"),
                                marital_group = get_std_marital_group_names(),
                                aw_set = c("all", "only common", "only extra")) {

### NOTE !!! THESE ARE THE NAMES IN THE TRAJECTORY ARRAYS, NOT THE .CSV RESULTS FILES!!

    stat <- match.arg(stat)
    marital_group <- match.arg(marital_group)
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
    if (stat %in% c("prop", "count", "age ratio")) return(names_prop_count)
    else { # stat must == "ratio"
        if (!identical(marital_group, "all women")) return(names_ratio_common)
        else { # marital_group must == "all women"
            if (identical(aw_set, "all")) return(c(names_ratio_common, names_ratio_aw_extra))
            else if (identical(aw_set, "only common")) return(names_ratio_common)
            else return(names_ratio_aw_extra)
        }
    }
}

##' @rdname get_traj_array_indicator_names
##' @export
get_traj_array_indicator_names <- function() {
    out <- character()
    for (i in c("prop", "count", "ratio", "age ratio")) {
        for (j in get_std_marital_group_names()) {
            out <- c(out, get_traj_array_indicator_names_select(stat = i, marital_group = j, aw_set = "all"))
        }
    }
    return(unique(out))
}
