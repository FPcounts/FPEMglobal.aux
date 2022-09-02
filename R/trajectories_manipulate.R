
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



convert_country_traj_to_counts <- function(traj_array, denominator_counts_df, iso = NULL) {

    ## -------* Check Inputs

    stopifnot(is.array(traj_array))
    stopifnot(is.data.frame(denominator_counts))

    if (any(traj_array) > 1)
        stop("Some elements of 'traj_array' are > 1. They should all be proportions. Did you pass in an all women trajectory array?")

    denominator_counts_df <-
        traj_converters_arg_check(traj_array = traj_array, denominator_counts_df = denominator_counts_df, iso = iso)

    ## -------* Convert to Counts

    ## Year index
    year_vec <- floor(as.numeric(dimnames(traj_array)[[1]]))
    year_match <- match(year_vec, denominator_counts_df$year)
    if (any(is.na(year_match)))
        stop("Cannot match years in 'traj_array' and 'denominator_counts_df'.")

    ## Put denominators into an array and multiply
    return(traj_array * array(denominator_counts_df[year_match, "count"],
                              dim = c(length(unique(denominator_counts_df$year)), dim(traj_array)[[2]], dim(traj_array)[[3]])))
}


convert_country_traj_to_props <- function(traj_array, denominator_counts_df, iso = NULL) {

    ## -------* Check Inputs

    stopifnot(is.array(traj_array))
    stopifnot(is.data.frame(denominator_counts))

    denominator_counts_df <-
        traj_converters_arg_check(traj_array = traj_array, denominator_counts_df = denominator_counts_df, iso = iso)

    ## -------* Convert to Proportions

    ## Year index
    year_vec <- floor(as.numeric(dimnames(traj_array)[[1]]))
    year_match <- match(year_vec, denominator_counts_df$year)
    if (any(is.na(year_match)))
        stop("Cannot match years in 'traj_array' and 'denominator_counts_df'.")

    ## Put denominators into an array and divide
    return(traj_array / array(denominator_counts_df[year_match, "count"],
                              dim = c(length(unique(denominator_counts_df$year)), 3, dim(traj_array)[[3]])))
}


expand_country_traj <- function(traj_array, denominator_counts_df,
                                    stat_in = c("prop", "count"),
                                    stat_out = c("prop", "count", "ratio"),
                                    iso = NULL) {

    ## -------* Check Inputs

    stopifnot(is.array(traj_array))
    stopifnot(is.data.frame(denominator_counts))
    stat_in <- match.arg(stat_in)
    stat_out <- match.arg(stat_out)

    if (!all(c("Modern", "Traditional", "Unmet") %in% dimnames(traj_array)[[2]]))
        stop("'traj_array' must have indicators ", toString(c("Modern", "Traditional", "Unmet")), ".")

    if (identical(stat_in, "prop")) {
        if (any(traj_array > 1))
            stop("Some elements of 'traj_array' are > 1. They should all be proportions. Did you pass in an all women trajectory array?")
    }

    if (identical(stat_in, "count")) {
        if (all(traj_array <= 1))
            warning("*All* elements of 'traj_array' are <= 1. They should all be counts. Did you pass in a trajectory array of proportions?")
    }

    denominator_counts_df <-
        traj_converters_arg_check(traj_array = traj_array, denominator_counts_df = denominator_counts_df, iso = iso)

    ## -------* Convert to Counts

    ## Must be counts to expand

    if (identical(stat_in, "prop")) {
        traj_array <-
            convert_country_traj_to_counts(traj_array = traj_array,
                                               denominator_counts_df = denominator_counts_df,
                                               iso = iso)
    }

    ## -------* Expand

    traj_array <- abind::abind(traj_array[, "Modern", ] + traj_array[, "Traditional", ],
                               traj_array[, "Modern", ],
                               traj_array[, "Traditional", ],
                               traj_array[, "Unmet", ],
                               traj_array[, "Modern", ] + traj_array[, "Traditional", ] + traj_array[, "Unmet", ],
                               traj_array[, "Traditional", ] + traj_array[, "Unmet", ],
                               along = 3,
                               new.names = list(dimnames(traj_array)[[1]],
                                                c("Total", "Modern", "Traditional", "Unmet", "TotalPlusUnmet", "TradPlusUnmet"),
                                                NULL))

    ## -------* Convert for Output

    if (identical(stat_out, "prop"))
        traj_array <-
            convert_country_traj_to_props(traj_array = traj_array,
                                          denominator_counts_df = denominator_counts_df,
                                          iso = iso)

    ## -------* END

    return(traj_array)
}


