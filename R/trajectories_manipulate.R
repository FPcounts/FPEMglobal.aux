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
traj_converters_arg_check <- function(traj_array, denominator_counts_df, iso = NULL, safe = FALSE) {
    ## Colnames of denominators
    if (!all(c("iso", "name", "count", "year") %in% colnames(denominator_counts_df)))
        stop("'denominator_counts_df' must have column names ",
             toString(c("iso", "name", "count", "year")),
             ". Use 'get_csv_denominators()' with 'clean_col_names = TRUE' and 'table_format = \"long\"'.")

    ## Denominators must be for one country
    ## Subset on 'iso' if necessary
    if (length(unique(denominator_counts_df$iso)) > 1) {
        if (is.null(iso))
            stop("'denominator_counts_df' has counts for more than one location (ISO). Supply the 'iso' of 'traj_array' or subset 'denominator_counts_df' before using.")
        else denominator_counts_df <- denominator_counts_df[denominator_counts_df$iso == iso, ]
    }

    ## Years are same in traj and denom
    if (!identical(dim(traj_array)[1], nrow(denominator_counts_df)))
        stop("'traj_array' and 'denominator_counts_df' do not have the same number of years.")

    ## Trajectory counts are in the thousands; try to warn if it seems
    ## they are in millions
    if (!is.null(iso) && !iso %in% c(356, 1456)) # India, China
        count_threshold <- 10e3
    else count_threshold <- 20e3
    if (any(denominator_counts_df$count > count_threshold)) {
        msg <- "Some denominator counts exceed 20,000. Trajectory counts are stored in units of 1000; did you supply denominators in units of 1?"
        if (safe) stop(msg)
        else warning(msg)
    }

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
##' \code{\link{get_csv_denominators}}.
##'
##' Ratio indicators for all women include ratios of married/unmarried
##' over all women. To compute these from an all women trajectory
##' array, the married \emph{and} unmarried women trajectory arrays
##' must be supplied via \code{traj_array_counts_married} and
##' \code{traj_array_counts_unmarried}.
##'
##' @param traj_array_props Trajectory array of proportions (for
##'     \code{convert_country_traj_to_counts}).
##' @param traj_array_counts Trajectory array of counts (for
##'     \code{convert_country_traj_to_props} and
##'     \code{convert_country_traj_to_ratios}).
##' @param traj_array_counts_married,traj_array_counts_unmarried For
##'     \code{convert_country_traj_to_ratios} only: trajectory array
##'     counts for married and unmarried women. See \dQuote{Details}.
##' @param denominator_counts_df Data frame of denominator counts (as
##'     produced by \code{\link{get_csv_denominators}}).
##' @param iso Numeric ISO code of the country to select from
##'     \code{denominator_counts_df}; see \dQuote{Details}.
##' @param safe Logical; elevate some warnings to errors?
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
    stopifnot(is.data.frame(denominator_counts_df))

    if (any(traj_array_props > 1))
        stop("Some elements of 'traj_array_props' are > 1. They should all be proportions. Did you pass in an all women trajectory array or an array of counts?")

    denominator_counts_df <-
        as.data.frame(traj_converters_arg_check(traj_array = traj_array_props,
                                                denominator_counts_df = denominator_counts_df, iso = iso))

    ## -------* Convert to Counts

    ## Year index
    year_vec <- round_down_years(as.numeric(dimnames(traj_array_props)[[1]]))
    denominator_counts_df$year <- round_down_years(denominator_counts_df$year)
    year_match <- match(year_vec, denominator_counts_df$year)
    if (any(is.na(year_match)))
        stop("Cannot match years in 'traj_array_props' and 'denominator_counts_df'.")

    ## Put denominators into an array and multiply
    return(traj_array_props * array(denominator_counts_df[year_match, "count"],
                                    dim = c(length(unique(denominator_counts_df$year)), dim(traj_array_props)[[2]], dim(traj_array_props)[[3]])))
}


##' @rdname convert_country_traj_to_counts
##' @export
convert_country_traj_to_props <- function(traj_array_counts, denominator_counts_df, iso = NULL, safe = TRUE) {

    ## -------* Check Inputs

    stopifnot(is.array(traj_array_counts))
    stopifnot(is.data.frame(denominator_counts_df))

    if (all(traj_array_counts <= 1)) {
        msg <- "*All* elements of 'traj_array_counts' are <= 1. They should all be counts. Did you pass in a trajectory array of proportions?"
        if (safe) stop(msg)
        else warning(msg)
    }

    denominator_counts_df <-
        as.data.frame(traj_converters_arg_check(traj_array = traj_array_counts,
                                                denominator_counts_df = denominator_counts_df, iso = iso))

    ## -------* Convert to Proportions

    ## Year index
    year_vec <- round_down_years(as.numeric(dimnames(traj_array_counts)[[1]]))
    denominator_counts_df$year <- round_down_years(denominator_counts_df$year)
    year_match <- match(year_vec, denominator_counts_df$year)
    if (any(is.na(year_match)))
        stop("Cannot match years in 'traj_array_counts' and 'denominator_counts_df'.")

    ## Put denominators into an array and divide
    return(traj_array_counts / array(denominator_counts_df[year_match, "count"],
                                     dim = c(length(unique(denominator_counts_df$year)),
                                             dim(traj_array_counts)[[2]],
                                             dim(traj_array_counts)[[3]])))
}


##' @rdname convert_country_traj_to_counts
##' @export
convert_country_traj_to_ratios <- function(traj_array_counts,
                                           traj_array_counts_married = NULL,
                                           traj_array_counts_unmarried = NULL,
                                           safe = TRUE) {

    ## -------* Functions

    assert_needed_indicators <- function(arr, incl_no_use = FALSE) {
        needed_indicators <- c("Total", "Traditional", "Modern", "Unmet", "TotalPlusUnmet", "TradPlusUnmet")
        if (incl_no_use) needed_indicators <- c(needed_indicators, "NoUse")
        if (!all(needed_indicators %in% dimnames(arr)[[2]]))
            stop("'", deparse(substitute(arr)), "' must have at least the indicators ", toString(needed_indicators), ". If you are missing 'NoUse', use 'expand_country_traj_count(..., incl_no_use = TRUE, ...)'.")
    }

    check_array_lt_1 <- function(arr, safe) {
        if (all(arr <= 1)) {
            msg <- c("*All* elements of '", deparse(substitute(arr)), "' are <= 1. They should all be counts. Did you pass in a trajectory array of proportions?")
            if (safe) stop(msg)
            else warning(msg)
        }
    }

    ## -------* Check Inputs

    stopifnot(is.array(traj_array_counts))

    muw_sum <- is.null(traj_array_counts_married) + is.null(traj_array_counts_unmarried)
    if (identical(muw_sum, 1L))
        stop("'traj_array_counts_married' and 'traj_array_counts_unmarried' must either be both 'NULL' or both non-'NULL'.")
    muw_arrays <- identical(muw_sum, 0L)

    assert_needed_indicators(traj_array_counts, incl_no_use = TRUE)
    check_array_lt_1(traj_array_counts, safe)

    if (muw_arrays) {
        assert_needed_indicators(traj_array_counts_married, incl_no_use = FALSE)
        check_array_lt_1(traj_array_counts_married, safe)

        assert_needed_indicators(traj_array_counts_unmarried, incl_no_use = FALSE)
        check_array_lt_1(traj_array_counts_unmarried, safe)
    }

    ## -------* Convert to Ratios

    if (!muw_arrays) {
        return(aperm(abind::abind(traj_array_counts[, "Modern", ] / traj_array_counts[, "Total", ],
                                  traj_array_counts[, "Total", ] / (traj_array_counts[, "Total", ] + traj_array_counts[, "Unmet", ]),
                                  traj_array_counts[, "Unmet", ] / (traj_array_counts[, "Unmet", ] + traj_array_counts[, "NoUse", ]),
                                  traj_array_counts[, "Modern", ] / (traj_array_counts[, "Total", ] + traj_array_counts[, "Unmet", ]),
                                  along = 3,
                                  new.names = list(dimnames(traj_array_counts)[[1]],
                                                   NULL,
                                                   c("Modern/Total", "Met Demand", "Z", "Met Demand with Modern Methods"))),
                     c(1, 3, 2)))
    } else {
        return(aperm(abind::abind(traj_array_counts[, "Modern", ] / traj_array_counts[, "Total", ],
                                  traj_array_counts[, "Total", ] / (traj_array_counts[, "Total", ] + traj_array_counts[, "Unmet", ]),
                                  traj_array_counts[, "Unmet", ] / (traj_array_counts[, "Unmet", ] + traj_array_counts[, "NoUse", ]),
                                  traj_array_counts[, "Modern", ] / (traj_array_counts[, "Total", ] + traj_array_counts[, "Unmet", ]),
                                  traj_array_counts_married[, "Modern", ] / traj_array_counts[, "Modern", ],
                                  traj_array_counts_married[, "Traditional", ] / traj_array_counts[, "Traditional", ],
                                  traj_array_counts_married[, "Unmet", ] / traj_array_counts[, "Unmet", ],
                                  traj_array_counts_unmarried[, "Modern", ] / traj_array_counts[, "Modern", ],
                                  traj_array_counts_unmarried[, "Traditional", ] / traj_array_counts[, "Traditional", ],
                                  traj_array_counts_unmarried[, "Unmet", ] / traj_array_counts[, "Unmet", ],
                                  along = 3,
                                  new.names = list(dimnames(traj_array_counts)[[1]],
                                                   NULL,
                                                   c("Modern/Total", "Met Demand", "Z", "Met Demand with Modern Methods",
                                                     "Modern Married Over All", "Trad Married Over All", "Unmet Married Over All",
                                                     "Modern Unmarried Over All", "Trad Unmarried Over All", "Unmet Unmarried Over All"))),
                     c(1, 3, 2)))
    }
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
            as.data.frame(traj_converters_arg_check(traj_array = traj_array_counts,
                                      denominator_counts_df = denominator_counts_df, iso = iso))

        ## Year index
        year_vec <- round_down_years(as.numeric(dimnames(traj_array_counts)[[1]]))
        denominator_counts_df$year <- round_down_years(denominator_counts_df$year)
        year_match <- match(year_vec, denominator_counts_df$year)
        if (any(is.na(year_match)))
            stop("Cannot match years in 'traj_array_counts' and 'denominator_counts_df'.")
        denom_counts_matrix <-
            matrix(denominator_counts_df[year_match, "count"],
                   nrow = length(unique(denominator_counts_df$year)), ncol = dim(traj_array_counts)[[3]])

    }

    ## -------* Expand

    if (!incl_no_use) {

        return(aperm(abind::abind(traj_array_counts[, "Modern", ] + traj_array_counts[, "Traditional", ],
                            traj_array_counts[, "Traditional", ],
                            traj_array_counts[, "Modern", ],
                            traj_array_counts[, "Unmet", ],
                            traj_array_counts[, "Modern", ] + traj_array_counts[, "Traditional", ] + traj_array_counts[, "Unmet", ],
                            traj_array_counts[, "Traditional", ] + traj_array_counts[, "Unmet", ],
                            along = 3,
                            new.names = list(dimnames(traj_array_counts)[[1]],
                                             NULL,
                                             c("Total", "Traditional", "Modern", "Unmet", "TotalPlusUnmet", "TradPlusUnmet"))),
                     c(1, 3, 2)))

    } else {

        return(aperm(abind::abind(traj_array_counts[, "Modern", ] + traj_array_counts[, "Traditional", ],
                            traj_array_counts[, "Traditional", ],
                            traj_array_counts[, "Modern", ],
                            traj_array_counts[, "Unmet", ],
                            traj_array_counts[, "Modern", ] + traj_array_counts[, "Traditional", ] + traj_array_counts[, "Unmet", ],
                            traj_array_counts[, "Traditional", ] + traj_array_counts[, "Unmet", ],
                            denom_counts_matrix - traj_array_counts[, "Modern", ] - traj_array_counts[, "Traditional", ] - traj_array_counts[, "Unmet", ],
                            along = 3,
                            new.names = list(dimnames(traj_array_counts)[[1]],
                                             NULL,
                                             c("Total", "Traditional", "Modern", "Unmet", "TotalPlusUnmet", "TradPlusUnmet", "NoUse"))),
                     c(1, 3, 2)))

    }
}
