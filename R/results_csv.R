###-----------------------------------------------------------------------------
### * Work with .csv Results Files

##' Read results csv files containing percentages, counts, and ratios
##'
##' Reads \file{.csv} files from a results directory of an
##' \pkg{FPEMglobal} run for a single marital group. The geography
##' (countries or aggregates) and statistic(s) loaded (proportions,
##' counts, ratios) can be specified. The files are read using
##' \code{\link[readr]{read_csv}}. You need to supply the results
##' directory via the \code{output_dir} argument. Arguments
##' \code{output_dir}, \code{aggregate}, \code{stat} can be any
##' combination of scalars and vectors to allow retrieval of results
##' for multiple marital groups, aggregates, and statistics; see
##' \dQuote{Details} for important requirements and restrictions.
##'
##' To get results for multiple marital groups, passing a named vector
##' (or list) to \code{output_dir}. The names \emph{must} be taken
##' from \code{get_std_marital_group_names(return_case = "lower",
##' snake_case = TRUE, named = TRUE)}; any other names will result in
##' an error. The directories are further checked to ensure they
##' contain results for the marital group indicated.
##'
##' Argument \code{aggregate} also accepts a vector, in which case the
##' result is a \code{\link[tibble]{tibble}} with all requested
##' aggregates. A column \code{"aggregate"} is added to identify the
##' aggregate of each row. This is necessary in the event that areas
##' within aggregates have the same name.
##'
##' The \code{stat} argument specifies the type of results to
##' return. Results are stored separately for prevalence proportions,
##' ratio indicators, and number of users. These can be requested by,
##' respectively, \code{"prop"}, \code{"ratio"}, \code{"count"}. Note
##' that prevalence \emph{proportions} are stored in \file{.csv} files
##' with \dQuote{perc} in their names.
##'
##' Finally, you can also supply a vector to \code{stat} to request
##' multiple result types in one go. In this case, however, you must
##' also specify \code{table_format = "long"}. The output will have an
##' additional column named \code{"stat"} indicating the result type
##' for each row. This is required because the indicator names for the
##' \code{"prop"} and \code{"count"} result types are the same.
##'
##' \code{clean_col_names} applies \code{\link{clean_col_names}} to
##' the column names. This tidies up and standardizes column
##' names. \emph{Note:} An additional step is performed after calling
##' \code{\link{clean_col_names}} the function: the column name
##' \dQuote{\code{percentile}} is changed to
##' \dQuote{\code{quantile}}. This makes the column name match the
##' actual values in the column. This column is misnamed in the raw
##' \file{.csv} output files.
##'
##' Country classifications are loaded via
##' \code{\link{get_used_unpd_regions}},
##' \code{\link{get_used_world_bank_regions}}, and
##' \code{\link{get_used_special_aggregates}}.
##'
##' \code{table_format} determines the \dQuote{shape} of the output
##' table. It takes values in c("long", "wide", "raw"),
##' which have the following effects:
##' \describe{
##' \item{\code{"raw"}}{No reshaping of the input file is done. This
##' format has a column for each year and an ID column for indicator
##' (\code{"indicator"}).}
##' \item{\code{"wide"}}{The output has a column for each indicator
##' and an ID column for years (\code{"year"}).}
##' \item{\code{"long"}}{Like \code{"wide"} but the indicators are
##' also transposed. It has a single \code{"value"} colum for all
##' indicators and an ID column for indicator (\code{"indicator"})}
##' The default is \code{"long"}.
##' }
##'
##' @param output_dir Path(s) to director(y/ies) containing
##'     outputs. See \dQuote{Details}.
##' @param verbose Logical; report the path, filename, and object name
##'     in a message?
##' @param aggregate Name of the aggregate(s) to return. Note: use
##'     \code{aggregate = "country"} to load country results. See
##'     \dQuote{Details}.
##' @param stat Which statistic(s) should be loaded? See
##'     \dQuote{Details}.
##' @param indicator_name_format Character; the format to use for
##'     indicator names. It is highly recommended to use the first
##'     (default) option, maybe the second.
##' @param adjusted Loads original results (\dQuote{orig}), adjusted
##'     medians only (\dQuote{adj}), or original results with medians
##'     substituted with adjusted medians (\dQuote{sub_adj}).
##' @param years_as_midyear Logical; should years be stored in
##'     \dQuote{mid-year} format, e.g., 1970.5, 1971.5, etc.? See
##'     \dQuote{Specifying year storage format in
##'     \pkg{FPEMglobal.aux}} in \code{\link{year_storage_format}}.
##' @param clean_col_names Logical; when \code{TRUE}, the column names
##'     of the result are \sQuote{cleaned} by applying
##'     \code{\link{clean_col_names}}. See \dQuote{Details} for a note
##'     about \dQuote{percentile} vs. \dQuote{quantile}.
##' @param add_country_classifications Logical; add on columns with
##'     geographic country classifications? Only has an effect if
##'     \code{aggregate} is \code{"country"}. See also
##'     \dQuote{Details}.
##' @param table_format Logical or character. Should the table be
##'     returned in long format? See \dQuote{Details}.
##' @param sort Logical. Sort by stat, name, year, percentile?
##' @param verbose Logical. Print lots of messages? See
##'     \code{\link{FPEMglobal.aux}} for a note about \pkg{readr}
##'     messages.
##'
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##'
##' @family Get results from csv files
##'
##' @seealso \code{\link{get_output_dir}} for instructions on how to
##'     specify output directories and run names;
##'     \code{\link{FPEMglobal.aux}} for a note about \pkg{readr}
##'     messages; \code{\link{year_storage_format}} for background on
##'     storage format of year values.
##'
##' @author Mark Wheldon
##' @export
get_csv_res <- function(output_dir = NULL,
                        aggregate = "country",
                        stat = c("prop", "count", "ratio"),  #Could add 'age_ratio' here later
                        adjusted = c("orig", "adj", "sub_adj"),
                        clean_col_names = TRUE,
                        indicator_name_format = c("clean", "traj_array", "csv_results_file_names"),
                        years_as_midyear = TRUE,
                        add_country_classifications = FALSE,
                        table_format = c("long", "wide", "raw"),
                        sort = TRUE) {

    ## -------* Check arguments

    verbose <- getOption("FPEMglobal.aux.verbose")

    ## -------** Simple checks

    indicator_name_format <- match.arg(indicator_name_format)
    stopifnot(is.logical(years_as_midyear))
    adjusted <- match.arg(adjusted)
    table_format <- match.arg(table_format)

    if (add_country_classifications && !identical(aggregate, "country")) {
        warning("'add_country_classifications' only has an effect if 'aggregate' == '\"country\"'. No classifications will be added.")
        add_country_classifications <- FALSE
    }

    ## -------** Checks for output_dir

    mar_groups <-
        get_std_marital_group_names(return_case = "lower", snake_case = TRUE, named = TRUE)
    if (length(output_dir) > 1) {
        if (!all(names(output_dir) %in% mar_groups))
            stop("'output_dir' has multiple elements but wrong names; use names '",
                 toString(mar_groups))
        if (mar_groups["mwra"] %in% names(output_dir))
            stopifnot(is_married_women_run(output_dir[mar_groups["mwra"]]))
        if (mar_groups["uwra"] %in% names(output_dir))
            stopifnot(is_unmarried_women_run(output_dir[mar_groups["uwra"]]))
        if (mar_groups["wra"] %in% names(output_dir))
            stopifnot(is_all_women_run(output_dir[mar_groups["wra"]]))
    }

    ## -------** Checks for stat

    stat <- match.arg(stat, several.ok = TRUE)
    if (length(stat) > 1 && !identical(table_format, "long"))
        stop("'stat' has more than one element; 'table_format' must be '\"long\"'.")

    ## -------* Recursive Calls

    ## -------** Multiple output directories?

    if (length(output_dir) > 1) {
        return(dplyr::bind_rows(lapply(names(output_dir), function(z) {
            data.frame(get_csv_res(output_dir = unlist(output_dir[z]), ## <<<
                                   aggregate = aggregate,
                                   stat = stat,
                                   adjusted = adjusted,
                                   clean_col_names = clean_col_names,
                                   indicator_name_format = indicator_name_format,
                                   years_as_midyear = years_as_midyear,
                                   add_country_classifications = add_country_classifications,
                                   table_format = table_format,
                                   sort = sort),
                       marital_group = z)
        })))
    }

    ## -------** Multiple "aggregates"s requested?

    if (length(aggregate) > 1) {
        return(dplyr::bind_rows(lapply(aggregate, function(z) {
            data.frame(get_csv_res(output_dir = output_dir,
                                   aggregate = z,  ## <<<<<
                                   stat = stat,
                                   adjusted = adjusted,
                                   clean_col_names = clean_col_names,
                                   indicator_name_format = indicator_name_format,
                                   years_as_midyear = years_as_midyear,
                                   add_country_classifications = add_country_classifications,
                                   table_format = table_format,
                                   sort = sort),
                       aggregate = z)
        })))
    }

    ## -------** Multiple "stat"s requested?

    if (length(stat) > 1) {
        return(dplyr::bind_rows(lapply(stat, function(z) {
            data.frame(get_csv_res(output_dir = output_dir,
                                   aggregate = aggregate,
                                   stat = z, ## <<<<<
                                   adjusted = adjusted,
                                   clean_col_names = clean_col_names,
                                   indicator_name_format = indicator_name_format,
                                   years_as_midyear = years_as_midyear,
                                   add_country_classifications = add_country_classifications,
                                   table_format = table_format,
                                   sort = sort),
                       stat = z)
        })))
    }

    ## -------* Set Up

    ## -------** Constants

    marital_group <- get_marital_group(output_dir = output_dir,
                                       lower_snake_casify = FALSE)
    ind_names_filenames <-
        get_std_indicator_names(stat = stat, marital_group = marital_group,
                                adjusted = c(orig = "orig", adj = "adj", sub_adj = "adj")[adjusted],
                                indicator_name_format = "csv_results_file_names")
    ind_names_df <-
        get_std_indicator_names(stat = stat, marital_group = marital_group,
                                adjusted = c(orig = "orig", adj = "adj", sub_adj = "adj")[adjusted],
                                indicator_name_format = indicator_name_format)

    ## -------** Directories

    output_dir <-
        output_dir_wrapper(output_dir = output_dir,
                           post_processed = TRUE, made_results = TRUE,
                           age_ratios = FALSE,
                           adjusted_medians = any(c("adj", "sub_adj") %in% adjusted))

    data_dir_name <- "data"
    table_dir_name <- "table"

    tbl_dir0 <- file.path(output_dir, table_dir_name)

    run_name <- get_run_name(output_dir = output_dir)
    fname <- paste(run_name, aggregate, sep = "_")

    ## -------* Load the 'orig' results

    if (adjusted %in% c("orig", "sub_adj")) {

        tbl_dir <-
            table_orig_adj_dir(tbl_dir = tbl_dir0, adj = FALSE)

        if (stat %in% c("prop", "count")) {

            if (identical(stat, "prop")) stat_in_fname <- "perc"
            else stat_in_fname <- stat

            ## Read the '_Total' file first
            fname1 <- paste0(fname, "_", stat_in_fname, "_", ind_names_filenames["total"], ".csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname1), "'.")
            res <- readr::read_csv(file.path(tbl_dir, fname1))
            res$stat <- stat
            res$indicator <- ind_names_df["total"]

            ## Read in rest of indicators
            for (indicator in names(ind_names_filenames)[names(ind_names_filenames) != "total"]) {
                fname_ind <- paste0(fname, "_", stat_in_fname, "_",
                                    ind_names_filenames[indicator], ".csv")
                if (verbose) message("Reading '", file.path(tbl_dir, fname_ind), "'.")
                res_ind <- try(readr::read_csv(file.path(tbl_dir, fname_ind)), silent = TRUE)
                if (identical(class(res_ind), "try-error")) {
                    if (verbose) message("Note: '", fname_ind, "' does not exist.")
                } else {
                    res_ind$stat <- stat
                    res_ind$indicator <- ind_names_df[indicator]
                    res <- dplyr::bind_rows(res, res_ind)
                }
            }

        } else if (identical(stat, "ratio")) {

            ## Read in 'ratio'

            ## Read the '_Met Demand' file first
            fname1 <- paste0(fname, "_", stat, "_", ind_names_filenames["met_demand"], ".csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname1), "'.")
            res <- readr::read_csv(file.path(tbl_dir, fname1))
            res$stat <- stat
            res$indicator <- ind_names_df["met_demand"]

            for (indicator in names(ind_names_filenames)[names(ind_names_filenames) != "met_demand"]) {
                fname_ind <- paste0(fname, "_", "ratio", "_",
                                    ind_names_filenames[indicator], ".csv")
                if (verbose) message("Reading '", file.path(tbl_dir, fname_ind), "'.")
                res_ind <- try(readr::read_csv(file.path(tbl_dir, fname_ind)))
                if (identical(class(res_ind), "try-error")) {
                    if (verbose) message("Note: '", fname_ind, "' does not exist.")
                } else {
                    res_ind$stat <- stat
                    res_ind$indicator <- ind_names_df[indicator]
                    res <- dplyr::bind_rows(res, res_ind)
                }
            }
        }
    }

    ## -------* Load adjusted results

    if (adjusted %in% c("adj", "sub_adj")) {

        tbl_dir <-
            table_orig_adj_dir(tbl_dir = tbl_dir0, adj = TRUE)

        if (stat %in% c("prop", "count")) {

            if (identical(stat, "prop")) stat_in_fname <- "perc"
            else stat_in_fname <- stat

            ## Read the '_Total' file first
            fname1 <- paste0(fname, "_", stat_in_fname, "_Total_Adj.csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname1), "'.")
            res_adj <- readr::read_csv(file.path(tbl_dir, fname1))
            res_adj$stat <- stat
            res_adj$indicator <- ind_names_df["total"]

            ## Read in rest of indicators
            for (indicator in names(ind_names_filenames)[names(ind_names_filenames) != "total"]) {
                fname_ind <- paste0(fname, "_", stat_in_fname, "_",
                                    ind_names_filenames[indicator], "_Adj.csv")
                if (verbose) message("Reading '", file.path(tbl_dir, fname_ind), "'.")
                res_adj_ind <- try(readr::read_csv(file.path(tbl_dir, fname_ind)))
                if (identical(class(res_adj_ind), "try-error")) {
                    if (verbose) message("Note: '", fname_ind, "' does not exist.")
                } else {
                    res_adj_ind$stat <- stat
                    res_adj_ind$indicator <- ind_names_df[indicator]
                    res_adj <- dplyr::bind_rows(res_adj, res_adj_ind)
                }
            }

        } else if (identical(stat, "ratio")) {

            ## Read the '_Met Demand' file first
            fname1 <- paste0(fname, "_", stat, "_Met Demand_Adj.csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname1), "'.")
            res_adj <- readr::read_csv(file.path(tbl_dir, fname1))
            res_adj$stat <- stat
            res_adj$indicator <- ind_names_df["met_demand"]

            ## Read in 'ratio'
            for (indicator in names(ind_names_filenames)[names(ind_names_filenames) != "met_demand"]) {
                fname_ind <- paste0(fname, "_", "ratio", "_",
                                    ind_names_filenames[indicator], "_Adj.csv")
                if (verbose) message("Reading '", file.path(tbl_dir, fname_ind), "'.")
                res_adj_ind <- try(readr::read_csv(file.path(tbl_dir, fname_ind)))
                if (identical(class(res_adj_ind), "try-error")) {
                    if (verbose) message("Note: '", fname_ind, "' does not exist.")
                } else {
                    res_adj_ind$stat <- stat
                    res_adj_ind$indicator <- ind_names_df[indicator]
                    res_adj <- dplyr::bind_rows(res_adj, res_adj_ind)
                }
            }
        }
    }

    ## -------* Modify, Merge, Etc.

    ## Years columns
    if (!years_as_midyear) {
        if (adjusted %in% c("orig", "sub_adj")) {
            yr_cols_idx <- !colnames(res) %in% c("Name", "Iso", "Percentile", "indicator", "stat")
            colnames(res)[yr_cols_idx] <- round_down_years(colnames(res)[yr_cols_idx])
        }
        if (adjusted %in% c("adj", "sub_adj")) {
            yr_cols_idx <- !colnames(res_adj) %in% c("Name", "Iso", "Percentile", "indicator", "stat")
            colnames(res_adj)[yr_cols_idx] <- round_down_years(colnames(res_adj)[yr_cols_idx])
        }
    } else {
        if (adjusted %in% c("orig", "sub_adj")) {
            yr_cols_idx <- !colnames(res) %in% c("Name", "Iso", "Percentile", "indicator", "stat")
            colnames(res)[yr_cols_idx] <- put_years_in_mid_year_fmt(colnames(res)[yr_cols_idx])
        }
        if (adjusted %in% c("adj", "sub_adj")) {
            yr_cols_idx <- !colnames(res_adj) %in% c("Name", "Iso", "Percentile", "indicator", "stat")
            colnames(res_adj)[yr_cols_idx] <- put_years_in_mid_year_fmt(colnames(res_adj)[yr_cols_idx])
        }
    }

    ## Reshape
    if (adjusted == "orig") {
        if (table_format %in% c("wide", "long")) {
            res <- tidyr::gather(res, -Name, -Iso, -Percentile, -stat, -indicator,
                                 key = "Year", value = "Value")
            res$Year <- as.numeric(res$Year)
        }
        res$adjusted <- FALSE
    }

    if (adjusted == "adj") {
        ## Only output res_adj
        res <- res_adj
        if (table_format %in% c("wide", "long")) {
            res <- tidyr::gather(res, -Name, -Iso, -Percentile, -stat, -indicator,
                                 key = "Year", value = "Value")
            res$Year <- as.numeric(res$Year)
        }
        res$adjusted <- TRUE

    } else if (adjusted == "sub_adj") {
        if (table_format %in% c("wide", "long")) {
            res <- tidyr::gather(res, -Name, -Iso, -Percentile, -stat, -indicator,
                                 key = "Year", value = "Value")
            res_adj <- tidyr::gather(res_adj, -Name, -Iso, -Percentile, -stat, -indicator,
                                     key = "Year", value = "Value")
            res <- dplyr::left_join(res, res_adj,
                                    by = c("Name", "Iso", "Percentile", "stat",
                                           "indicator", "Year"))
            res[res$Percentile == 0.5,]$Value.x <- res[res$Percentile == 0.5,]$Value.y
            res <- dplyr::select(res, -Value.y) %>% dplyr::rename(Value = Value.x)

            ## Substitute medians for adjusted medians
            res_med <- res$Percentile == 0.5
            res$adjusted <- FALSE
            res$adjusted[res_med] <- TRUE
            res$Year <- as.numeric(res$Year)
        } else {
            if (!identical(sort(colnames(res)), sort(colnames(res_adj))))
                stop("Column names in 'orig' and 'adj' results are not the same; cannot concatenate.")
            res$adjusted <- FALSE
            res_adj$adjusted <- TRUE
            res <- dplyr::bind_rows(res[res$Percentile != 0.5,], res_adj)
        }
    }

    if (identical(table_format, "wide")) {
        ind_vals <- unique(res$indicator)
        res <- res %>%
            tidyr::spread(key = c("indicator"), value = "Value")
        if ("adj" %in% colnames(res))
            res <- res[,c("stat", "Name", "Iso", "Year", "Percentile", "adjusted", ind_vals)]
        else res <- res[,c("stat", "Name", "Iso", "Year", "Percentile", ind_vals)]
    }

    ## Discard the 'stat' column.
    ## This is here for historical reasons; could probably just not
    ## create it in the first place.
    res <- res %>% dplyr::select(-stat)

    ## add classifications?
    if (add_country_classifications) {
        class_unpd_agg <-
            get_used_unpd_regions(output_dir = output_dir,
                                  clean_col_names = FALSE)

        ## Problem with these because they are not exclusive
        ## classifications; countries can be in more than one
        ## aggregate within the classification.
        ##
        ## class_spec <-
        ##     get_used_special_aggregates(output_dir = output_dir,
        ##                                 clean_col_names = FALSE)

        res <- res %>%
            dplyr::left_join(dplyr::select(class_unpd_agg, -`Country or area`),
                             by = c("Iso" = "ISO Code"))##  %>%
            ## dplyr::left_join(class_spec, by = c("Iso" = "iso.country"))
    }

    ## Sort
    if (sort) {
        if (identical(table_format, "raw"))
            res <- res %>% dplyr::arrange(indicator, Iso, Percentile)
        else if (identical(table_format, "wide"))
            res <- res %>% dplyr::arrange(Iso, Year, Percentile)
        else if (identical(table_format, "long"))
            res <- res %>% dplyr::arrange(indicator, Iso, Year, Percentile)
    }

    ## Clean column names
    if (clean_col_names) {
        res <- clean_col_names(res)
        ## Fix the 'percentile' / 'quantile' mess (only if 'clean_col_names' = TRUE)
        colnames(res)[colnames(res) == "percentile"] <- "quantile"
    }

    ## END
    return(tibble::as_tibble(res))
}


##' Read results csv files containing percentages, etc., for all three marital groups
##'
##' A wrapper for \code{\link{get_csv_res}}. Calls
##' \code{\link{get_csv_res}} sequentially on first elements of
##' \code{output_dir_list}, second elements
##' of \code{output_dir_list}, etc., to load
##' \emph{all} results for all marital groups. Binds the three into
##' single object.
##'
##' @family Get results from csv files
##'
##' @section Specifying results directory:
##' See the section in \code{\link{get_csv_res}}.
##'
##' @param output_dir_list List of output directories for married,
##'     unmarried, and all women. Element names must be \dQuote{married},
##'     \dQuote{unmarried}, and \dQuote{all_women}.
##' @param index_col_name Character. Name of column added to output
##'     that identifies observations for each marital group.
##' @param ... Passed to \code{\link{get_csv_res}}. Must not include
##'     argument \code{output_dir}.
##' @return A \code{\link[tibble]{tibble}}.
##' @author Mark Wheldon
##' @export
get_csv_all_mar_res <- function(output_dir_list = NULL,
                                index_col_name = "marital_group",
                                ...) {

    warning("!! 'get_csv_all_mar_res()' is currently UN-TESTED. Use 'get_csv_res()' instead.")

    verbose <- getOption("FPEMglobal.aux.verbose")

    ## !!!!!!!!!!! NEEDS WORK !!!!!!!!!!!!!

    stopifnot(is.list(output_dir_list))
    stopifnot(!is.null(names(output_dir_list)))
    stopifnot(identical(sort(names(output_dir_list)),
                        sort(c("married", "unmarried", "all_women"))))

    stopifnot(is.character(index_col_name))
    stopifnot(identical(length(index_col_name), 1L))

    out <- data.frame()

    for (i in seq_along(output_dir_list)) {
        x <- get_csv_res(output_dir = output_dir_list[[i]], ...)
        if (index_col_name %in% colnames(x))
            stop("'index_col_name' is a column name in csv results table. Choose a different name.")
        x[, index_col_name] <- names(output_dir_list)[[i]]
        out <- dplyr::bind_rows(out, x)
    }

    cols <- colnames(out)[colnames(out) != "marital_group"]
    return(tibble::as_tibble(out[,c("marital_group", cols)]))
}


##' Convert csv results to \pkg{fpemdata} format.
##'
##' This function provides for interaction with the \pkg{fpemdata}
##' package. The csv results are loaded \emph{via}
##' \code{\link{get_csv_res}} and then modified accordingly.
##'
##' \pkg{fpemdata} expects input data in a data frame with the
##' following columns:
##' \describe{
##' \item{Name}{Country name}
##' \item{Iso}{Numeric ISO code}
##' \item{Percentile}{\emph{Quantile}, e.g., 0.025, 0.1, etc.}
##' \item{par}{Indicator in lower case; can take values "modern", "traditional", "unmet"}
##' \item{1970.5, etc.}{Mid-year; multiple columns from 1970.5 up to 2030.5}}
##'
##' @family fpemdata converters
##' @seealso get_csv_res
##'
##' @param stat Which statistics should be loaded? See
##'     \code{\link{get_csv_res}} for details, but note that, unlike
##'     with \code{\link{get_csv_res}}, you can only specifiy one
##'     \dQuote{\code{stat}} at a time with this function.
##' @inheritParams get_csv_res
##' @return A \code{\link[tibble]{tibble}}.
##' @author Mark C Wheldon
##' @export
convert_csv_res_2_fpemdata <- function(output_dir = NULL,
                                       stat = c("prop", "count", "ratio"),
                               adjusted = c("orig", "adj", "sub_adj")) {

    verbose <- getOption("FPEMglobal.aux.verbose")

    stat <- match.arg(stat, several.ok = FALSE)
    adjusted <- match.arg(adjusted, several.ok = FALSE)

    output_dir <-
        output_dir_wrapper(output_dir = output_dir,
                           post_processed = TRUE, made_results = TRUE,
                           age_ratios = FALSE,
                           adjusted_medians = any(c("adj", "sub_adj") %in% adjusted))

    out <- get_csv_res(output_dir = output_dir,
                       stat = stat,
                       adjusted = adjusted,
                       table_format = "raw", clean_col_names = FALSE)

    out <- out %>%
        dplyr::mutate(indicator = tolower(indicator)) %>%
        dplyr::rename(par = indicator) %>%
        dplyr::arrange(par, Iso, Percentile)
    return(tibble::as_tibble(out))
}

