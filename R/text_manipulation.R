###-----------------------------------------------------------------------------
### * Text Manipulation

###-----------------------------------------------------------------------------
### ** Indicators and Columns

##' Clean indicator names
##'
##' This function takes out spaces and makes the whole string lower
##' case. This is not quite the same as lower snake case as spaces are
##' not replaced with underscores, rather they are just removed
##' resulting in a single unbroken string.
##'
##' @param indicator Character string to \dQuote{clean} up.
##' @return Character string.
##' @author Mark Wheldon
##'
##' @family String utility functions
##'
##' @examples
##'
##' ## Known indicators are:
##' get_traj_array_indicator_names()
##'
##' @export
clean_indic_names <- function(indicator) {
    clean <- function(x) {
        tolower(gsub("[ ]+", "_", x))
    }
    ## regexp <- paste(get_traj_array_indicator_names(), collapse = "|")
    ## match_list <- gregexpr(pattern = regexp, text = indicator)
    ## mapply(function(as.list(ind), mat) {

    ## },
    ## indicator, match_list,
    ## SIMPLIFY = TRUE, USE.NAMES = FALSE)
    clean(indicator)
}


## Round down years to nearest integer?
round_down_years <- function(x) {
    out <- floor(as.numeric(x))
    if (is.character(x)) return(as.character(out))
    else return(out)
}

## Put years in mid-year format
put_years_in_mid_year_fmt <- function(x) {
    out <- as.numeric(round_down_years(x)) + 0.5
    if (is.character(x)) return(as.character(out))
    else return(out)
}



## Turn strings into lower_snake_case.  Spaces and full stops are
## replaced with underscores and the whole thing is put in lower case.
lower_snake_casify <- function(x, use_make.names = TRUE) {
    if (use_make.names) x <- base::make.names(x)
    x <- gsub("[ .\n\r]+", "_", x)
    x <- gsub("_+", "_", x)
    tolower(x)
}


##' Clean names of columns in data frames
##'
##' Reformats names to lower snake case by replacing spaces and other
##' non-letter characters (e.g., \dQuote{.}) with underscores. If
##' \code{use_make.names = TRUE}, \code{\link[base]{make.names}} is
##' run \emph{first}. In addition, the following substitutions are
##' made \emph{after} all re-formats:
##' \describe{
##' \item{\code{iso_code}}{replaced with \code{iso}}
##' \item{\code{country}, \code{country_or_area}}{replaced with \code{name}}
##' }
##'
##' If \code{clean_indicator_names} is \code{FALSE}, any strings
##' matching the known standard indicator names are \emph{not}
##' cleaned. The known standard indicator names are defined by
##' \code{\link{get_traj_array_indicator_names}}.
##'
##' @param x Object for which a method is defined (e.g., character
##'     string or data frame).
##' @param use_make.names Logical; apply
##'     \code{\link[base]{make.names}} to \code{x} \emph{before} doing
##'     anything else?
##' @param clean_indicator_names Logical; should the procedure be
##'     applied to standard indicator names as well? See
##'     \dQuote{Details} for more information.
##' @return Formatted version of \code{x}.
##' @author Mark Wheldon
##'
##' @family String utility functions
##'
##' @export
clean_col_names <- function(x, ...) {
    UseMethod("clean_col_names")
}

##' @rdname clean_col_names
##' @export
clean_col_names.character <- function(x, use_make.names = TRUE, clean_indicator_names = FALSE) {
    if (!clean_indicator_names) {
        idx <- !(x %in% get_traj_array_indicator_names())
    } else { idx <- rep.int(TRUE, length(x)) }
    if (sum(idx)) x[idx] <- lower_snake_casify(x[idx], use_make.names = use_make.names)
    x[x == "iso_code"] <- "iso"
    x[x == "iso_country"] <- "iso"
    x[x %in% c("country", "country_or_area")] <- "name"
    return(x)
}

##' @rdname clean_col_names
##' @export
clean_col_names.data.frame <- function(x, use_make.names = TRUE, clean_indicator_names = FALSE) {
    colnames(x) <- clean_col_names(colnames(x), use_make.names = use_make.names,
                                   clean_indicator_names = clean_indicator_names)
    return(x)
}

##' @rdname clean_col_names
##' @export
clean_col_names.list <- function(x, use_make.names = TRUE, clean_indicator_names = FALSE) {
    avail_methods <-
        gsub("clean_col_names\\.", "",
             row.names(attr(methods("clean_col_names"), "info")))
    lapply(x, function(z, umn = use_make.names, cin = clean_indicator_names, am = avail_methods) {
        if (class(z) %in% am)
            clean_col_names(z, use_make.names = umn, clean_indicator_names = cin)
    })
}
