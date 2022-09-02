###-----------------------------------------------------------------------------
### * Work with .csv Results Files

##' Read results csv files containing percentages, counts, and ratios
##'
##' Reads \emph{all} results \file{.csv} files from a results
##' directory for a given country or region that contain proportions,
##' counts, and ratios. The files are read using
##' \code{\link[readr]{read_csv}}.
##'
##' The \code{stat} argument specifies the type of results to
##' return. Results are stored separately for prevalence proportions,
##' ratio indicators, and number of users. These can be requested by,
##' respectively, \code{"prop"}, \code{"ratio"}, \code{"count"}. Note
##' that prevalence \emph{proportions} are stored in \file{.csv} files
##' with \dQuote{perc} in their names.
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
##' @param output_dir Path to directory containing outputs. See
##'     Section \dQuote{Specifying results directory} in the help file
##'     for \code{\link{get_output_dir}}. Note that \code{root_dir} is
##'     ignored if \code{output_dir} is supplied.
##' @param verbose Logical; report the path, filename, and object name
##'     in a message?
##' @param aggregate Name of the 'aggregate' to load. Note: use
##'     \code{aggregate = "country"} to load country results.
##' @param stat Which statistics should be loaded? Allowable values
##'     are \code{c("orig", "adj", "sub_adj")}.
##' @param add_stat_column Logical. Add a column \dQuote{\code{stat}}
##'     with value \code{stat} to the result?
##' @param adjusted Loads original results (\dQuote{orig}), adjusted
##'     medians only (\dQuote{adj}), or original results with medians
##'     substituted with adjusted medians (\dQuote{sub_adj}).
##' @param add_adjusted_column Add a column \dQuote{\code{adjusted}}?
##'     Cells indicate which rows are adjusted (\code{TRUE}) and which
##'     are not (\code{FALSE}).
##' @param round_down_years Should years in \dQuote{yyyy.5} format be
##'     rounded down to integer values? (e.g., 1970.5 becomes 1970).
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
##'     \code{link{FPEMglobal.aux}} for a note about \pkg{readr}
##'     messages.
##' @param ... passed to \code{\link{read_csv}}.
##' @inheritParams get_output_dir
##'
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##'
##' @family csv results functions
##'
##' @seealso \code{\link{get_output_dir}} for instructions on how to
##'     specify output directories and run
##'     names. \code{link{FPEMglobal.aux}} for a note about
##'     \pkg{readr} messages.
##'
##' @author Mark Wheldon
##' @export
get_csv_res <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                             aggregate = "country",
                             stat = c("prop", "count", "ratio"),
                             add_stat_column = FALSE,
                             adjusted = c("orig", "adj", "sub_adj"),
                             add_adjusted_column = identical(adjusted, "sub_adj"),
                             clean_col_names = TRUE,
                             round_down_years = FALSE,
                             add_country_classifications = FALSE,
                             table_format = c("long", "wide", "raw"),
                             sort = TRUE,
                             verbose = FALSE,
                             ...) {

    if (!verbose) { op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
    on.exit(options(op), add = TRUE, after = FALSE) }

    stat <- match.arg(stat)

    if (add_country_classifications && !identical(aggregate, "country")) {
        warning("'add_country_classifications' only has an effect if 'aggregate' == '\"country\"'. No classifications will be added.")
        add_country_classifications <- FALSE
    }

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = TRUE, made_results = TRUE)

    data_dir_name <- "data"
    table_dir_name <- "table"

    tbl_dir0 <- file.path(output_dir, table_dir_name)

    if (is.null(run_name)) run_name <- get_run_name(output_dir = output_dir, verbose = verbose)
    fname <- paste(run_name, aggregate, sep = "_")

    adjusted <- match.arg(adjusted)

    table_format <- match.arg(table_format)

    ## -------* Load the 'orig' results

    if (adjusted %in% c("orig", "sub_adj")) {

        tbl_dir <-
            table_orig_adj_dir(tbl_dir = tbl_dir0, adj = FALSE,
                               verbose = verbose)

        if (stat %in% c("prop", "count")) {

            if (identical(stat, "prop")) stat_in_fname <- "perc"
            else stat_in_fname <- stat

            ## Read the '_Total' file first
            fname1 <- paste0(fname, "_", stat_in_fname, "_Total.csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname1), "'.")
            res <- readr::read_csv(file.path(tbl_dir, fname1))
            res$stat <- stat
            res$indicator <- "Total"

            ## Read in rest of indicators
            for (indicator in c("Modern", "TotalPlusUnmet", "Traditional", "TradPlusUnmet",
                               "Unmet")) {
                fname_ind <- paste0(fname, "_", stat_in_fname, "_", indicator, ".csv")
                if (verbose) message("Reading '", file.path(tbl_dir, fname_ind), "'.")
                res_ind <- readr::read_csv(file.path(tbl_dir, fname_ind))
                res_ind$stat <- stat
                res_ind$indicator <- indicator
                res <- dplyr::bind_rows(res, res_ind)
            }

        } else if (identical(stat, "ratio")) {

            ## Read in 'ratio'

            ## Read the '_Met Demand' file first
            fname1 <- paste0(fname, "_", stat, "_Met Demand.csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname1), "'.")
            res <- readr::read_csv(file.path(tbl_dir, fname1))
            res$stat <- stat
            res$indicator <- "Met Demand"

            ind_values <- c("MetDemModMeth", "ModernOverTotal")
            if (adjusted == "orig") ind_values <- c(ind_values, "Z")
            for (indicator in ind_values) {
                fname_ind <- paste0(fname, "_", "ratio", "_", indicator, ".csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname_ind), "'.")
                res_ind <- readr::read_csv(file.path(tbl_dir, fname_ind))
                res_ind$stat <- stat
                res_ind$indicator <- indicator
                res <- dplyr::bind_rows(res, res_ind)
            }
        }
    }

    ## -------* Load adjusted results

    if (adjusted %in% c("adj", "sub_adj")) {

        tbl_dir <-
            table_orig_adj_dir(tbl_dir = tbl_dir0, adj = TRUE,
                               verbose = verbose)

        if (stat %in% c("prop", "count")) {

            if (identical(stat, "prop")) stat_in_fname <- "perc"
            else stat_in_fname <- stat

            ## Read the '_Total' file first
            fname1 <- paste0(fname, "_", stat_in_fname, "_Total_Adj.csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname1), "'.")
            res_adj <- readr::read_csv(file.path(tbl_dir, fname1))
            res_adj$stat <- stat
            res_adj$indicator <- "Total"

            ## Read in rest of indicators
            for (indicator in c("Modern", "TotalPlusUnmet", "Traditional", "TradPlusUnmet",
                               "Unmet")) {
                fname_ind <- paste0(fname, "_", stat_in_fname, "_", indicator, "_Adj.csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname_ind), "'.")
                res_adj_ind <- readr::read_csv(file.path(tbl_dir, fname_ind))
                res_adj_ind$stat <- stat
                res_adj_ind$indicator <- indicator
                res_adj <- dplyr::bind_rows(res_adj, res_adj_ind)
            }

        } else if (identical(stat, "ratio")) {

            ## Read the '_Met Demand' file first
            fname1 <- paste0(fname, "_", stat, "_Met Demand_Adj.csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname1), "'.")
            res_adj <- readr::read_csv(file.path(tbl_dir, fname1))
            res_adj$stat <- stat
            res_adj$indicator <- "Met Demand"

            ## Read in 'ratio'
            ind_values <- c("MetDemModMeth", "ModernOverTotal")
            for (indicator in ind_values) {
                fname_ind <- paste0(fname, "_", "ratio", "_", indicator, "_Adj.csv")
            if (verbose) message("Reading '", file.path(tbl_dir, fname_ind), "'.")
                res_adj_ind <- readr::read_csv(file.path(tbl_dir, fname_ind))
                res_adj_ind$stat <- stat
                res_adj_ind$indicator <- indicator
                res_adj <- dplyr::bind_rows(res_adj, res_adj_ind)
            }
        }
    }

    ## -------* Modify, Merge, Etc.

    ## Years columns
    if (round_down_years) {
        if (adjusted %in% c("orig", "sub_adj")) {
            yr_cols_idx <- !colnames(res) %in% c("Name", "Iso", "Percentile", "indicator", "stat")
            colnames(res)[yr_cols_idx] <- round_down_years(colnames(res)[yr_cols_idx])
        }
        if (adjusted %in% c("adj", "sub_adj")) {
            yr_cols_idx <- !colnames(res_adj) %in% c("Name", "Iso", "Percentile", "indicator", "stat")
            colnames(res_adj)[yr_cols_idx] <- round_down_years(colnames(res_adj)[yr_cols_idx])
        }
    }

    ## Reshape
    if (adjusted == "orig") {
        if (table_format %in% c("wide", "long")) {
            res <- tidyr::gather(res, -Name, -Iso, -Percentile, -stat, -indicator,
                                 key = "Year", value = "Value")
            res$Year <- as.numeric(res$Year)
        }
        if (add_adjusted_column) res$adjusted <- FALSE
    }

    if (adjusted == "adj") {
        ## Only output res_adj
        res <- res_adj
        if (table_format %in% c("wide", "long")) {
            res <- tidyr::gather(res, -Name, -Iso, -Percentile, -stat, -indicator,
                                 key = "Year", value = "Value")
            res$Year <- as.numeric(res$Year)
        }
        if (add_adjusted_column) res$adjusted <- TRUE

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
            if (add_adjusted_column) {
                res$adjusted <- FALSE
                res$adjusted[res_med] <- TRUE
            }
            res$Year <- as.numeric(res$Year)
        } else {
            if (!identical(sort(colnames(res)), sort(colnames(res_adj))))
                stop("Column names in 'orig' and 'adj' results are not the same; cannot concatenate.")
            if (add_adjusted_column) {
                res$adjusted <- FALSE
                res_adj$adjusted <- TRUE
            }
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

    ## add classifications?
    if (add_country_classifications) {
        class_unpd_agg <-
            get_used_unpd_regions(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                                  clean_col_names = FALSE,
                                  verbose = verbose)

        class_spec <-
            get_used_special_aggregates(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                                        clean_col_names = FALSE,
                                        verbose = verbose)

        res <- res %>%
            dplyr::left_join(dplyr::select(class_unpd_agg, -`Country or area`),
                             by = c("Iso" = "ISO Code")) %>%
            dplyr::left_join(class_spec, by = c("Iso" = "iso.country"))
    }

    ## Stat column
    if (add_stat_column && !("stat" %in% colnames(res))) res$stat <- stat
    else if (!add_stat_column && "stat" %in% colnames(res))
        res <- res[, colnames(res) != "stat"]

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
        ## Fix the 'percentile' / 'quantile' mess (only if 'clean_col_names' = TRUE
        colnames(res)[colnames(res) == "percentile"] <- "quantile"
    }

    ## END
    return(tibble::as_tibble(res))
}


##' Read results csv files containing percentages, etc., for all three marital groups
##'
##' Calls \code{\link{get_csv_res}} sequentially on first
##' elements of \code{run_name_list} and \code{output_dir_list}, second
##' elements of \code{run_name_list} and \code{output_dir_list}, etc.,
##' to load \emph{all} results for all marital groups. Binds the three
##' into data frame.
##'
##' @family csv results functions
##'
##' @section Specifying results directory:
##' See the section in \code{\link{get_csv_res}}.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##' @param run_name_list List of run names for married, unmarried, and
##'     all women. Element names must be \dQuote{married},
##'     \dQuote{unmarried}, and \dQuote{all_women}.
##' @param output_dir_list List of output directories for married,
##'     unmarried, and all women. See \code{run_name_list}.
##' @param index_col_name Character. Name of column added to output
##'     that identifies observations for each marital group.
##' @param ... Passed to \code{\link{get_csv_res}}.
##' @return A \code{\link[tibble]{tibble}}.
##' @author Mark Wheldon
##' @export
get_csv_all_marr_res <- function(run_name_list = NULL, output_dir_list = NULL,
                                            root_dir = NULL,
                                            index_col_name = "marital_group",
                                            ...) {

    stopifnot(is.list(run_name_list))
    stopifnot(!is.null(names(run_name_list)))
    stopifnot(identical(sort(names(run_name_list)),
                             sort(c("married", "unmarried", "all_women"))))

    stopifnot(is.list(output_dir_list))
    stopifnot(!is.null(names(output_dir_list)))
    stopifnot(identical(sort(names(output_dir_list)),
                        sort(c("married", "unmarried", "all_women"))))

    stopifnot(is.character(index_col_name))
    stopifnot(identical(length(index_col_name), 1L))

    out <- data.frame()

    for (i in seq_along(output_dir_list)) {
        x <- get_csv_res(run_name = run_name_list[[i]],
                         output_dir = output_dir_list[[i]], root_dir = root_dir,
                         sort = FALSE, #< Don't sort twice
                         ...)
        if (index_col_name %in% colnames(x))
            stop("'index_col_name' is a column name in csv results table. Choose a different name.")
        x[, index_col_name] <- names(output_dir_list)[[i]]
        out <- dplyr::bind_rows(out, x)
    }

    cols <- colnames(out)[colnames(out) != "marital_group"]
    out <- out[,c("marital_group", cols)]

    return(tibble::as_tibble(out))
}


##' Convert loaded csv results to \pkg{fpemdata} format.
##'
##' This function provides for interaction with the \pkg{fpemdata}
##' package.
##'
##' @section Note:
##' \pkg{fpemdata} only requires the proportions which
##'     can be read via \code{get_csv_res(..., stat = "prop",
##'     ...)}.
##'
##' @family csv results functions
##'
##' @param csv_tbl Results as loaded by \code{\link{get_csv_res}}.
##' @return A \code{\link[tibble]{tibble}}.
##' @author Mark C Wheldon
##' @export
csv_res_2_fpemdata <- function(csv_tbl) {

    csv_tbl <- clean_col_names(csv_tbl)

    ## Need to account for possible renaming of 'percentile' column.
    col_quant <- grep("[Qq]antile", colnames(csv_tbl))
    if (length(col_quant))
        colnames(csv_tbl)[col_quant] <- "percentile"

    out <- csv_tbl %>%
        dplyr::select(c("name", "iso", "year", "percentile",
                        "modern", "traditional", "unmet")) %>%
        tidyr::pivot_longer(cols = c("modern", "traditional", "unmet"),
                            names_to = "par") %>%
        tidyr::pivot_wider(values_from = "value",
                           names_from = "year") %>%
        dplyr::rename(Name = name, Iso = iso, Percentile = percentile) %>%
        dplyr::arrange(par, Iso, Percentile)
    return(tibble::as_tibble(out))
}

