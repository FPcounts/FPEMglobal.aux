###-----------------------------------------------------------------------------
### * Convert and Expand Trajectories

###-----------------------------------------------------------------------------
### ** Support Functions

## traj_array Indicator names in countrytrajectories arrays
get_traj_array_indicator_names_count <- function() {
    c("Total", "Traditional", "Modern", "Unmet", "TotalPlusUnmet",
       "TradPlusUnmet")
}

get_traj_array_indicator_names_prop <- function() {
    get_traj_array_indicator_names_count()
}

get_traj_array_indicator_names_ratio <- function() {
    c("Modern/Total", "Met Demand", "Z", "Met Demand with Modern Methods")
}


## Checks inputs to the various converter functions.
traj_converters_arg_check <- function(traj_array, denominator_counts_df, iso = NULL) {
    ## Colnames of denominators
    if (!all(c("iso", "name", "count", "year") %in% colnames(denominator_counts)))
        stop("'denominator_counts_df' must have column names ",
             toString(c("iso", "name", "count", "year")),
             ". Use 'get_used_denominators()' with 'clean_col_names = TRUE' and 'table_format = \"long\"'.")

    ## Denominators must be for one country
    if (length(unique(denominator_counts_df$iso)) > 1) {
        if (is.null(iso))
            stop("'denominator_counts_df' has counts for more than one location (ISO). Supply the 'iso' of 'traj_array' or subset 'denominator_counts_df' before using.")
        else denominator_counts_df <- denominator_counts_df[denominator_counts_df$iso == iso, ]
    }

    ## Years are same in traj and denom
    if (!identical(dim(traj_array)[1], nrow(denominator_counts_df)))
        stop("'traj_array' and 'denominator_counts_df' do not have the same number of years.")

    ## Return denom in case it was subset on iso
    return(denominator_counts_df)
}


###-----------------------------------------------------------------------------
### ** Conversion Functions


##' Transform MCMC trajectories
##'
##' These functions transform country trajectories between counts and
##' proportions, and from counts to ratios. Country trajectories for
##' married and unmarried women runs are prevalence proportions (see
##' \code{\link{get_country_traj_muw}}); country trajectories for all
##' women runs are counts (see \code{link{get_country_traj_aw}}).
##'
##' Conversions between counts and proportions require denominator
##' counts. Since each trajectory array is only for a single country
##' or area, the denominators must match. Argument \code{iso} can be
##' used to subset \code{denominator_counts_df} if needed. In that
##' case, the ISOs must be in column \code{"iso"}. See
##' \code{\link{get_used_denominators}}.
##'
##' @param traj_array_props Trajectory array of proportions (for
##'     \code{convert_country_traj_to_counts}).
##' @param traj_array_counts Trajectory array of counts (for
##'     \code{convert_country_traj_to_props} and
##'     \code{convert_country_traj_to_ratios}).
##' @param denominator_counts_df Data frame of denominator counts (as
##'     produced by \code{\link{get_used_denominators}}).
##' @param iso Numeric ISO code of the country to select from
##'     \code{denominator_counts_df}; see \dQuote{Details}.
##' @return Trajectory array.
##' @author Mark Wheldon
##'
##' @family Trajectory conversion functions
##' @seealso \code{\link{get_country_traj_muw}}, \code{link{get_country_traj_aw}}
##'
##' @export
convert_country_traj_to_counts <- function(traj_array_props, denominator_counts_df, iso = NULL) {

    ## -------* Check Inputs

    stopifnot(is.array(traj_array_props))
    stopifnot(is.data.frame(denominator_counts))

    if (any(traj_array_props) > 1)
        stop("Some elements of 'traj_array_props' are > 1. They should all be proportions. Did you pass in an all women trajectory array or an array of counts?")

    denominator_counts_df <-
        traj_converters_arg_check(traj_array = traj_array_props, denominator_counts_df = denominator_counts_df, iso = iso)

    ## -------* Convert to Counts

    ## Year index
    year_vec <- floor(as.numeric(dimnames(traj_array_props)[[1]]))
    year_match <- match(year_vec, denominator_counts_df$year)
    if (any(is.na(year_match)))
        stop("Cannot match years in 'traj_array_props' and 'denominator_counts_df'.")

    ## Put denominators into an array and multiply
    return(traj_array_props * array(denominator_counts_df[year_match, "count"],
                              dim = c(length(unique(denominator_counts_df$year)), dim(traj_array_props)[[2]], dim(traj_array_props)[[3]])))
}


##' @rdname convert_country_traj_to_counts
##' @export
convert_country_traj_to_props <- function(traj_array_counts, denominator_counts_df, iso = NULL) {

    ## -------* Check Inputs

    stopifnot(is.array(traj_array_counts))
    stopifnot(is.data.frame(denominator_counts))

    if (all(traj_array_counts <= 1))
        warning("*All* elements of 'traj_array_counts' are <= 1. They should all be counts. Did you pass in a trajectory array of proportions?")

    denominator_counts_df <-
        traj_converters_arg_check(traj_array = traj_array_counts, denominator_counts_df = denominator_counts_df, iso = iso)

    ## -------* Convert to Proportions

    ## Year index
    year_vec <- floor(as.numeric(dimnames(traj_array_counts)[[1]]))
    year_match <- match(year_vec, denominator_counts_df$year)
    if (any(is.na(year_match)))
        stop("Cannot match years in 'traj_array_counts' and 'denominator_counts_df'.")

    ## Put denominators into an array and divide
    return(traj_array_counts / array(denominator_counts_df[year_match, "count"],
                              dim = c(length(unique(denominator_counts_df$year)), 3, dim(traj_array_counts)[[3]])))
}


##' @rdname convert_country_traj_to_counts
##' @export
convert_country_traj_to_ratios <- function(traj_array_counts) {

    ## -------* Check Inputs

    stopifnot(is.array(traj_array_counts))

    needed_indicators <- c("Total", "Traditional", "Modern", "Unmet", "TotalPlusUnmet", "TradPlusUnmet", "NoUse")
    if (!all(needed_indicators %in% dimnames(traj_array_counts[[2]])))
        stop("'traj_array_counts' must have at least the indicators ", toString(needed_indicators), ". If you are missing 'NoUse', use 'expand_country_traj_count(..., incl_no_use = TRUE, ...)'.")

    if (all(traj_array_counts <= 1))
        warning("*All* elements of 'traj_array_counts' are <= 1. They should all be counts. Did you pass in a trajectory array of proportions?")

    ## -------* Convert to Ratios

    return(abind::abind(traj_array_counts[, "Modern", ] / traj_array_counts[, "Total", ],
                        traj_array_counts[, "Total", ] / (traj_array_counts[, "Total", ] + traj_array_counts[, "Unmet", ]),
                        traj_array_counts[, "Unmet", ] / (traj_array_counts[, "Unmet", ] + traj_array_counts[, "NoUse", ]),
                        traj_array_counts[, "Modern", ] / (traj_array_counts[, "Total", ] + traj_array_counts[, "Unmet", ]),
                        along = 3,
                        new.names = list(dimnames(traj_array_counts)[[1]],
                                         c("Modern/Total", "Met Demand", "Z", "Met Demand with Modern Methods"),
                                         NULL)))
}


##' Expand country count trajectory arrays by adding indicators
##'
##' Country trajectory arrays for married and unmarried women only
##' contain trajectories for \code{"Traditional"}, \code{"Modern"},
##' and \code{"Total"}. This function will add \code{"Total"},
##' \code{"TotalPlusUnmet"}, \code{"TradPlusUnmet"}, and (optionally)
##' \code{"NoUse"}.
##'
##' @inheritParams convert_country_traj_to_counts
##' @param incl_no_use Logical; should \code{"NoUse"} be added? If so,
##'     you must supply \code{denominator_counts_df}.
##' @return An expanded trajectory array of counts.
##' @author Mark Wheldon
##'
##' @family Trajectory conversion functions
##' @seealso \code{\link{get_country_traj_muw}}, \code{link{get_country_traj_aw}}
##'
##' @export
expand_country_traj_count <- function(traj_array_counts, incl_no_use = !is.null(denominator_counts_df),
                                      denominator_counts_df = NULL, iso = NULL) {

    ## -------* Check Inputs

    stopifnot(is.array(traj_array_counts))

    if (!all(c("Modern", "Traditional", "Unmet") %in% dimnames(traj_array_counts)[[2]]))
        stop("'traj_array_counts' must have indicators ", toString(c("Modern", "Traditional", "Unmet")), ".")

    all_ind_names <- get_traj_array_indicator_names_count()
    if (incl_no_use) all_ind_names <- c(all_ind_names, "NoUse")
    if (all(all_ind_names %in% dimnames(traj_array_counts)[[2]]))
        warning("'traj_array_counts' already expanded; did you supply an all women array?")

    if (incl_no_use) {

        ## -------* Calculate 'No Use' Category

        if (is.null(denominator_counts_df))
            stop("If 'incl_no_use' is 'TRUE', you must supply 'denominator_counts_df'.")

        denominator_counts_df <-
            traj_converters_arg_check(traj_array = traj_array_counts, denominator_counts_df = denominator_counts_df, iso = iso)

        ## Year index
        year_vec <- floor(as.numeric(dimnames(traj_array_counts)[[1]]))
        year_match <- match(year_vec, denominator_counts_df$year)
        if (any(is.na(year_match)))
            stop("Cannot match years in 'traj_array_counts' and 'denominator_counts_df'.")

        denom_counts_matrix <-
            matrix(denominator_counts_df[year_match, "count"],
                  nrow = length(unique(denominator_counts_df$year)), ncol = dim(traj_array_counts)[[3]])

    }

    ## -------* Expand

    if (!incl_no_use) {

        return(abind::abind(traj_array_counts[, "Modern", ] + traj_array_counts[, "Traditional", ],
                            traj_array_counts[, "Traditional", ],
                            traj_array_counts[, "Modern", ],
                            traj_array_counts[, "Unmet", ],
                            traj_array_counts[, "Modern", ] + traj_array_counts[, "Traditional", ] + traj_array_counts[, "Unmet", ],
                            traj_array_counts[, "Traditional", ] + traj_array_counts[, "Unmet", ],
                            along = 3,
                            new.names = list(dimnames(traj_array_counts)[[1]],
                                             c("Total", "Traditional", "Modern", "Unmet", "TotalPlusUnmet", "TradPlusUnmet"),
                                             NULL)))

    } else {

        return(abind::abind(traj_array_counts[, "Modern", ] + traj_array_counts[, "Traditional", ],
                            traj_array_counts[, "Traditional", ],
                            traj_array_counts[, "Modern", ],
                            traj_array_counts[, "Unmet", ],
                            traj_array_counts[, "Modern", ] + traj_array_counts[, "Traditional", ] + traj_array_counts[, "Unmet", ],
                            traj_array_counts[, "Traditional", ] + traj_array_counts[, "Unmet", ],
                            denom_counts_matrix - traj_array_counts[, "Total", ] - traj_array_counts[, "Unmet", ],
                            along = 3,
                            new.names = list(dimnames(traj_array_counts)[[1]],
                                             c("Total", "Traditional", "Modern", "Unmet", "TotalPlusUnmet", "TradPlusUnmet", "NoUse"),
                                             NULL)))

    }
}


