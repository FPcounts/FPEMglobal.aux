###-----------------------------------------------------------------------------
### * Work with .csv Results Files

##' Read results csv files containing percentages, counts, and ratios
##'
##' Reads \emph{all} results \file{.csv} files from a results
##' directory for a given country or region that contain percentages,
##' counts, and ratios.
##'
##' @family csv results functions
##'
##' @param output_dir Path to directory containing outputs. See
##'     Section \dQuote{Specifying results directory} in the help file
##'     for \code{\link{get_output_dir}}. Note that \code{root_dir} is
##'     ignored if \code{output_dir} is supplied.
##' @param verbose Logical; report the path, filename, and object name
##'     in a message?
##' @param aggregate Name of the 'aggregate' to load. Note: use
##'     \code{aggregate = "Country"} to load country results.
##' @param stat Which statistics should be loaded? Allowable values
##'     are \code{c("orig", "adj", "sub_adj")}. A column \dQuote{stat}
##'     is added to the output with value \code{stat}.
##' @param adj Loads original results (\dQuote{orig}), adjusted
##'     medians only (\dQuote{adj}), or original results with medians
##'     substituted with adjusted medians (\dQuote{sub_adj}).
##' @param clean_indicator_names Logical. Make indicator names lower
##'     case and remove spaces?
##' @param clean_col_names Logical. Make column names lower snake
##'     case?
##' @param add_country_classifications Add on columns giving country
##'     classifications? If \dQuote{TRUE}, then
##'     \code{\link{get_used_unpd_regions}},
##'     \code{\link{get_used_world_bank_regions}}, and
##'     \code{\link{get_used_special_aggregates}} are called to load
##'     the mappings from ISO codes to classifications. In the first
##'     and third case, these are loaded from \code{data_input_dir}.
##' @param long_format Logical. Should the table be returned in long
##'     format?
##' @param sort Logical. Sort by stat, name, year, percentile?
##' @param verbose Logical. Print lots of messages?
##' @param ... passed to \code{\link{read_csv}}.
##' @inheritParams get_output_dir
##'
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##'
##' @seealso \code{\link{get_output_dir}} for instructions on how to
##'     specify output directories and run names.
##'
##' @author Mark Wheldon
##' @export
get_FPEMglobal_csv_res <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
                             aggregate = "Country",
                             stat = c("perc", "count", "ratio"),
                             adj = c("orig", "adj", "sub_adj"),
                             clean_indicator_names = TRUE,
                             clean_col_names = TRUE,
                             add_country_classifications = TRUE,
                             long_format = FALSE,
                             sort = TRUE,
                             verbose = FALSE,
                             ...) {

    stat <- match.arg(stat)

    c_add_class <- identical(aggregate, "Country") & add_country_classifications
    if(c_add_class & !clean_col_names) {
        clean_col_names <- TRUE
        message("'clean_col_names' set to 'TRUE' because 'add_country_classifications' is 'TRUE'.")
    }

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)

    data_dir_name <- "data"
    table_dir_name <- "table"

    tbl_dir0 <- file.path(output_dir, table_dir_name)

    if (is.null(run_name)) run_name <- get_run_name(output_dir = output_dir, verbose = verbose)
    fname <- paste(run_name, aggregate, sep = "_")

    adj <- match.arg(adj)

    ## -------* Load the 'orig' results

    if(adj %in% c("orig", "sub_adj")) {

        tbl_dir <-
            table_orig_adj_dir(tbl_dir = tbl_dir0, adj = FALSE,
                               verbose = verbose)

        if(stat %in% c("perc", "count")) {

            ## Read the '_Total' file first
            fname1 <- paste0(fname, "_", stat, "_Total.csv")
            if(verbose) {
                res <- readr::read_csv(file.path(tbl_dir, fname1), ...)
            } else {
                suppressMessages({
                    res <- readr::read_csv(file.path(tbl_dir, fname1), ...)
                })
            }
            res$stat <- stat
            res$indicator <- "total"

            ## Read in rest of indicators
            for(indicator in c("Modern", "TotalPlusUnmet", "Traditional", "TradPlusUnmet",
                               "Unmet")) {
                fname_ind <- paste0(fname, "_", stat, "_", indicator, ".csv")
                if(verbose) {
                    res_ind <- readr::read_csv(file.path(tbl_dir, fname_ind), ...)
                } else {
                    suppressMessages({
                        res_ind <- readr::read_csv(file.path(tbl_dir, fname_ind), ...)
                    })
                }
                res_ind$stat <- stat
                if(clean_indicator_names) res_ind$indicator <- clean_indic_name(indicator)
                res <- dplyr::bind_rows(res, res_ind)
            }

        } else if(identical(stat, "ratio")) {

            ## Read in 'ratio'

            ## Read the '_Met Demand' file first
            fname1 <- paste0(fname, "_", stat, "_Met Demand.csv")
            if(verbose) {
                res <- readr::read_csv(file.path(tbl_dir, fname1), ...)
            } else {
                suppressMessages({
                    res <- readr::read_csv(file.path(tbl_dir, fname1), ...)
                })
            }
            res$stat <- stat
            res$indicator <- "Met Demand"

            ind_values <- c("MetDemModMeth", "ModernOverTotal")
            if(adj == "orig") ind_values <- c(ind_values, "Z")
            for(indicator in ind_values) {
                fname_ind <- paste0(fname, "_", "ratio", "_", indicator, ".csv")
                if(verbose) {
                    res_ind <- readr::read_csv(file.path(tbl_dir, fname_ind), ...)
                } else {
                    suppressMessages({
                        res_ind <- readr::read_csv(file.path(tbl_dir, fname_ind), ...)
                    })
                }
                res_ind$stat <- stat
                if(clean_indicator_names) res_ind$indicator <- clean_indic_name(indicator)
                res <- dplyr::bind_rows(res, res_ind)
            }
        }
    }

    ## -------* Load adjusted results

    if(adj %in% c("adj", "sub_adj")) {

        tbl_dir <-
            table_orig_adj_dir(tbl_dir = tbl_dir0, adj = TRUE,
                               verbose = verbose)

        if(stat %in% c("perc", "count")) {

            ## Read the '_Total' file first
            fname1 <- paste0(fname, "_", stat, "_Total_Adj.csv")
            if(verbose) {
                res_adj <- readr::read_csv(file.path(tbl_dir, fname1), ...)
            } else {
                suppressMessages({
                    res_adj <- readr::read_csv(file.path(tbl_dir, fname1), ...)
                })
            }
            res_adj$stat <- stat
            res_adj$indicator <- "total"

            ## Read in rest of indicators
            for(indicator in c("Modern", "TotalPlusUnmet", "Traditional", "TradPlusUnmet",
                               "Unmet")) {
                fname_ind <- paste0(fname, "_", stat, "_", indicator, "_Adj.csv")
                if(verbose) {
                    res_adj_ind <- readr::read_csv(file.path(tbl_dir, fname_ind), ...)
                } else {
                    suppressMessages({
                        res_adj_ind <- readr::read_csv(file.path(tbl_dir, fname_ind), ...)
                    })
                }
                res_adj_ind$stat <- stat
                if(clean_indicator_names) res_adj_ind$indicator <- clean_indic_name(indicator)
                res_adj <- dplyr::bind_rows(res_adj, res_adj_ind)
            }

        } else if(identical(stat, "ratio")) {

            ## Read the '_Met Demand' file first
            fname1 <- paste0(fname, "_", stat, "_Met Demand_Adj.csv")
            if(verbose) {
                res_adj <- readr::read_csv(file.path(tbl_dir, fname1), ...)
            } else {
                suppressMessages({
                    res_adj <- readr::read_csv(file.path(tbl_dir, fname1), ...)
                })
            }
            res_adj$stat <- stat
            res_adj$indicator <- "Met Demand"

            ## Read in 'ratio'
            ind_values <- c("MetDemModMeth", "ModernOverTotal")
            for(indicator in ind_values) {
                fname_ind <- paste0(fname, "_", "ratio", "_", indicator, "_Adj.csv")
                if(verbose) {
                    res_adj_ind <- readr::read_csv(file.path(tbl_dir, fname_ind), ...)
                } else {
                    suppressMessages({
                        res_adj_ind <- readr::read_csv(file.path(tbl_dir, fname_ind), ...)
                    })
                }
                res_adj_ind$stat <- stat
                if(clean_indicator_names) res_adj_ind$indicator <- clean_indic_name(indicator)
                res_adj <- dplyr::bind_rows(res_adj, res_adj_ind)
            }
        }
    }

    if(adj == "orig") {
        res <- tidyr::gather(res, -Name, -Iso, -Percentile, -stat, -indicator,
                             key = "year", value = "value")
        }
    if(adj == "adj") {
        ## Only output res_adj
        res <- res_adj
        res$adjusted <- TRUE
        res <- tidyr::gather(res, -Name, -Iso, -Percentile, -stat, -indicator, -adjusted,
                         key = "year", value = "value")
    } else if(adj == "sub_adj") {
        res <- tidyr::gather(res, -Name, -Iso, -Percentile, -stat, -indicator,
                             key = "year", value = "value")

        res_adj <- tidyr::gather(res_adj, -Name, -Iso, -Percentile, -stat, -indicator,
                                 key = "year", value = "value")

        res <- dplyr::left_join(res, res_adj,
                                by = c("Name", "Iso", "Percentile", "stat",
                                       "indicator", "year"))
        res[res$Percentile == 0.5,]$value.x <-
            res[res$Percentile == 0.5,]$value.y
        res <- dplyr::select(res, -value.y) %>% dplyr::rename(value = value.x)

        ## Substitute medians for adj medians
        res_med <- res$Percentile == 0.5
        res$adjusted <- FALSE
        res$adjusted[res_med] <- TRUE
    }

    if(!long_format) {
        ind_vals <- unique(res$indicator)
        res <- res %>%
            tidyr::spread(key = c("indicator"), value = "value")
        if(adj == "adj") {
            res <- res[,c("stat", "Name", "Iso", "year", "Percentile", "adjusted", ind_vals)]
        } else {
            res <- res[,c("stat", "Name", "Iso", "year", "Percentile", ind_vals)]
        }
    }

    ## Lower case column names
    if(clean_col_names) colnames(res) <- lower_snake_casify(colnames(res))

    if(c_add_class) {           #add classifications?
        class_unpd_agg <-
            get_used_unpd_regions(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                                  clean_col_names = TRUE,
                                  verbose = verbose)

        class_spec <-
            get_used_special_aggregates(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                                        clean_col_names = TRUE,
                                        verbose = verbose)

        res <- res %>%
            dplyr::left_join(dplyr::select(class_unpd_agg, -name), by = "iso") %>%
            dplyr::left_join(class_spec, by = "iso")
    }

    ## Sort
    if(sort) res <- res %>% dplyr::arrange(stat, name, year, percentile)

    return(res)
}


##' Read results csv files containing percentages, etc., for all three marital groups
##'
##' Calls \code{\link{get_FPEMglobal_csv_res}} sequentially on first
##' elements of \code{run_name_list} and \code{output_dir_list}, second
##' elements of \code{run_name_list} and \code{output_dir_list}, etc.,
##' to load \emph{all} results for all marital groups. Binds the three
##' into data frame.
##'
##' @family csv results functions
##'
##' @section Specifying results directory:
##' See the section in \code{\link{get_FPEMglobal_csv_res}}.
##'
##' @param run_name_list List of run names for married, unmarried, and
##'     all women. Element names must be \dQuote{married},
##'     \dQuote{unmarried}, and \dQuote{all_women}.
##' @param output_dir_list List of output directories for married,
##'     unmarried, and all women. See \code{run_name_list}.
##' @param index_col_name Character. Name of column added to output
##'     that identifies observations for each marital group.
##' @param sort Logical. Sort by marital status, stat, name, year, percentile?
##' @inheritParams get_output_dir
##' @inheritParams get_FPEMglobal_csv_res
##' @inheritParams get_FPEMglobal_csv_res
##' @export
##' @return A \code{\link[tibble]{tibble}}.
##' @author Mark Wheldon
get_FPEMglobal_csv_all_marr_res <- function(run_name_list = NULL, output_dir_list = NULL,
                                      root_dir = ".",
                             aggregate = "Country", stat = "perc",
                             adj = c("orig", "adj", "sub_adj"),
                             clean_indicator_names = TRUE,
                             verbose = FALSE,
                             index_col_name = "marital_group",
                             long_format = FALSE,
                             sort = TRUE,
                             ...) {

    stopifnot(is.list(run_name_list))
    stopifnot(!is.null(names(run_name_list)))
    stopifnot(identical(sort(names(run_name_list)),
                             sort(c("married", "unmarried", "all_women"))))

    stopifnot(is.list(output_dir_list))
    stopifnot(!is.null(names(output_dir_list)))
    stopifnot(identical(sort(names(output_dir_list)),
                             sort(c("married", "unmarried", "all_women"))))

    out <- data.frame()

    for(i in seq_along(output_dir_list)) {
        x <- get_FPEMglobal_csv_res(run_name = run_name_list[[i]],
                         output_dir = output_dir_list[[i]], root_dir = root_dir,
                         aggregate = aggregate, stat = stat, adj = adj,
                         clean_indicator_names = clean_indicator_names,
                         long_format = long_format,
                         verbose = FALSE,
                         sort = FALSE, #< Don't sort twice
                         ...)
        x[, index_col_name] <- names(output_dir_list)[[i]]
        out <- dplyr::bind_rows(out, x)
    }

    cols <- colnames(out)[colnames(out) != "marital_group"]
    out <- out[,c("marital_group", cols)]

    if(sort) out <- out %>% arrange(marital_group, stat, name, year, percentile)

    tibble::as_tibble(out)
}


##' Convert loaded csv results to \pkg{fpemdata} format.
##'
##' @section Note:
##' \pkg{fpemdata} only requires the proportions which
##'     can be read via \code{get_FPEMglobal_csv_res(..., stat = "perc",
##'     ...)}.
##'
##' @family csv results functions
##'
##' @param csv_tbl Results as loaded by \code{\link{get_FPEMglobal_csv_res}}.
##' @return A \code{\link[tibble]{tibble}}.
##' @author Mark C Wheldon
##' @export
csv_res_2_fpemdata <- function(csv_tbl) {
    out <- csv_tbl %>%
        dplyr::select(c("name", "iso", "year", "percentile",
                        "modern", "traditional", "unmet")) %>%
        tidyr::pivot_longer(cols = c("modern", "traditional", "unmet"),
                            names_to = "par") %>%
        tidyr::pivot_wider(values_from = "value",
                           names_from = "year") %>%
        dplyr::rename(Name = name, Iso = iso, Percentile = percentile) %>%
        dplyr::arrange(par, Iso, Percentile)
    return(out)
}

