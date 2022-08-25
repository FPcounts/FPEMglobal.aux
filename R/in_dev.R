###-----------------------------------------------------------------------------
### * Functions that need work

###-----------------------------------------------------------------------------
### ** Make Consistent

### These need making consistent with the other functions---i.e., same
### argument names as much as possible, same way of working.



##' Read results csv files containing percentage point changes and PPPCs
##'
##' \emph{**WARNING**} FUNCTION IN DEVELOPMENT ...
##'
##' Reads \emph{all} results \file{.csv} files from a results directory for a
##' given aggregate (dQuote{Country}, \dQuote{UNPD_aggregate}, etc.)
##' that contain percentage point changes and posterior probabilities
##' of positive change (PPPCs). These are the files that have
##' \dQuote{changes} in their filenames.
##'
##' @family csv results functions
##'
##' @section Note:
##' There are no \dQuote{adjusted} results for posterior estiamtes of changes.
##'
##' @param marital_group Marital group being loaded. This is needed
##'     because the ratio indicators for all women are different from
##'     those for married and unmarried. \dQuote{guess}, the default,
##'     will work if the \code{run_name} contains one (and only one)
##'     of the regular expressions \code{married}, \code{unmarried},
##'     or \code{all[_. ]women}.
##' @inheritParams get_csv_res
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##' @author Mark Wheldon
##' @noRd
get_csv_change_res <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
                                    aggregate = "Country",
                                    clean_indicator_names = TRUE,
                                    verbose = FALSE,
                                    marital_group = c("guess", "married", "unmarried" , "all_women"),
                               ...) {

    warning("===============================================================================\n",
            " 'get_csv_change_res' IS IN DEVELOPMENT AND NOT FULLY TESTED\n",
            "===============================================================================\n")

    if (!verbose) { op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
    on.exit(options(op), add = TRUE, after = FALSE) }

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)
    table_dir_name <- "table"
    tbl_dir0 <- file.path(output_dir, table_dir_name)
    fname <- paste(run_name, aggregate, sep = "_")

    tbl_dir <-
        table_orig_adj_dir(tbl_dir = tbl_dir0, adj = FALSE,
                           verbose = verbose)

    marital_group <- match.arg(marital_group)

    if (identical(marital_group, "guess")) {
        marital_group  <- guess_marital_group(run_name)
    }

    ## Read the 'perc_Total' file first
    fname1 <- paste0(fname, "_changes_perc_Total.csv")
    if (verbose) {
        res <- readr::read_csv(file.path(tbl_dir, fname1), ...)
    } else {
        suppressMessages({
            res <- readr::read_csv(file.path(tbl_dir, fname1), ...)
        })
    }
    res$stat <- "perc"
    res$indicator <- "total"

    ## Read the 'count_Total' file
    fname1 <- paste0(fname, "_changes_count_Total.csv")
    if (verbose) {
        res2 <- readr::read_csv(file.path(tbl_dir, fname1), ...)
    } else {
        suppressMessages({
            res2 <- readr::read_csv(file.path(tbl_dir, fname1), ...)
        })
    }
    res2$stat <- "count"
    res2$indicator <- "total"

    res <- rbind(res, res2)

    ## Read in 'count' and 'perc'
    for (stat in c("count", "perc")) {
        for (indicator in c("Modern", "TotalPlusUnmet", "Traditional", "TradPlusUnmet",
                           "Unmet")) {
            fname_ind <- paste0(fname, "_changes_", stat, "_", indicator, ".csv")
            if (verbose) {
                res_ind <- readr::read_csv(file.path(tbl_dir, fname_ind), ...)
            } else {
                suppressMessages({
                    res_ind <- readr::read_csv(file.path(tbl_dir, fname_ind), ...)
                })
            }
            res_ind$stat <- stat
            if (clean_indicator_names) res_ind$indicator <- clean_indic_name(indicator)
            res <- dplyr::bind_rows(res, res_ind)
        }
    }

    ## Read in 'ratio'
    if (marital_group %in% c("married", "unmarried")) {
        ind_values <- c("MetDemand", "MetDemModMeth", "ModernOverTotal", "Z")
    } else if (identical(marital_group, "all_women")) {
        ind_values <- c("Met Demand", "MetDemModMeth", "ModernOverTotal",
                        "Mod-MarriedOverAll", "Mod-UnmarriedOverAll",
                        "Trad-MarriedOverAll", "Trad-UnmarriedOverAll",
                        "Unmet-MarriedOverAll", "Unmet-UnmarriedOverAll",
                        "Z")
    }
    for (indicator in ind_values) {
        fname_ind <- paste0(fname, "_changes_", "ratio", "_", indicator, ".csv")
        if (verbose) {
            res_ind <- readr::read_csv(file.path(tbl_dir, fname_ind), ...)
        } else {
            suppressMessages({
                res_ind <- readr::read_csv(file.path(tbl_dir, fname_ind), ...)
            })
        }
        res_ind$stat <- "ratio"
        if (clean_indicator_names) res_ind$indicator <- clean_indic_name(indicator)
        res <- dplyr::bind_rows(res, res_ind)
    }

    ## Married and Unmarried have percentages in colnames so convert
    ## to proportions.
    perc_colnames <- c("10%", "2.5%", "50%", "90%", "97.5%")
    colnames_perc_idx <- colnames(res) %in% perc_colnames
    perc_colnames_present_idx <-
        perc_colnames %in% colnames(res)[colnames_perc_idx]
    colnames(res)[colnames_perc_idx] <-
        c("0.1", "0.025", "0.5", "0.9", "0.975")[perc_colnames_present_idx]

    ## Reshape
    res <- tidyr::gather(res, -Name, -Iso, -Change, -stat, -indicator, -PPPC,
                         key = "percentile", value = "prop_change")

    ## Lower case column names
    colnames(res) <- tolower(colnames(res))

    return(res)
}


##' Read results csv files containing changes for all three marital groups
##'
##' \emph{**WARNING**} FUNCTION IN DEVELOPMENT ...
##'
##' Calls \code{\link{get_csv_change_res}} sequentially on first
##' elements of \code{run_name_list} and \code{output_dir_list}, second
##' elements of \code{run_name_list} and \code{output_dir_list}, etc.,
##' to load \emph{all} results for all marital groups. Binds the three
##' into data frame.
##'
##' @family csv results functions
##'
##' @inheritParams get_csv_all_marr_res
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##' @inheritParams get_csv_res
##'
##' @return A \code{\link[tibble]{tibble}}.
##' @author Mark Wheldon
##'
##' @noRd
get_csv_change_all_mar_res <- function(run_name_list = NULL, output_dir_list = NULL,
                                            root_dir = ".",
                                            aggregate = "Country",
                                            clean_indicator_names = TRUE,
                                            verbose = FALSE,
                                            index_col_name = "marital_group",
                                       ...) {

        warning("===============================================================================\n",
            " 'get_csv_change_all_mar_res' IS IN DEVELOPMENT AND NOT FULLY TESTED\n",
            "===============================================================================\n")

    stopifnot(is.list(run_name_list))
    stopifnot(identical(length(setdiff(names(run_name_list),
                                       c("married", "unmarried", "all_women"))), 0L))

    stopifnot(is.list(output_dir_list))
    stopifnot(identical(length(setdiff(names(output_dir_list),
                                       c("married", "unmarried", "all_women"))), 0L))

    out <- data.frame()

    for (i in seq_along(output_dir_list)) {
        x <- get_csv_change_res(run_name = run_name_list[[i]],
                         output_dir = output_dir_list[[i]], root_dir = root_dir,
                         aggregate = aggregate,
                         clean_indicator_names = clean_indicator_names,
                         verbose = FALSE, ...)
        x[, index_col_name] <- names(output_dir_list)[[i]]
        out <- dplyr::bind_rows(out, x)
    }

    tibble::as_tibble(out)
}



##' Read results csv files for age ratios
##'
##' \emph{**WARNING**} FUNCTION IN DEVELOPMENT ...
##'
##' Reads \emph{all} results csv files from a results director for a given aggregate
##' (dQuote{Country}, \dQuote{UNPD_aggregate}, etc.)
##'
##' @family csv results functions
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##' @inheritParams read_csv_res
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##' @author Mark Wheldon
##'
##' @noRd
get_csv_res_age_ratios <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
                                         aggregate = "Country", adj = FALSE, clean_indicator_names = TRUE,
                                        verbose = FALSE,
                                   ...) {

        warning("===============================================================================\n",
            " 'get_csv_res_age_ratios' IS IN DEVELOPMENT AND NOT FULLY TESTED\n",
            "===============================================================================\n")

    if (!verbose) { op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
    on.exit(options(op), add = TRUE, after = FALSE) }

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = FALSE)
    table_dir_name <- "table"
    tbl_dir <- file.path(output_dir, table_dir_name)

    if (adj) {
        warning("'adj = TRUE' not yet implemented. 'adj' set to 'FALSE'.")
        adj <- FALSE
    }

    tbl_dir <-
        table_orig_adj_dir(tbl_dir = tbl_dir, adj = adj, verbose = verbose)

    fname <- paste(run_name, aggregate, sep = "_")

    ## Read the 'age_ratio_Total' file first
    fname1 <- paste0(fname, "_age_ratio_Total.csv")

    if (verbose) {
        res <- readr::read_csv(file.path(tbl_dir, fname1), ...)
    } else {
        suppressMessages({
            res <- readr::read_csv(file.path(tbl_dir, fname1), ...)
        })
    }
    res$stat <- "age_ratio"
    res$indicator <- "total"

    for (indicator in c("Modern", "TotalPlusUnmet", "Traditional", "TradPlusUnmet", "Unmet")) {
        fname_ind <- paste0(fname, "_", "age_ratio", "_", indicator, ".csv")
        if (verbose) {
            res_ind <- readr::read_csv(file.path(tbl_dir, fname_ind), ...)
        } else {
            suppressMessages({
                res_ind <- readr::read_csv(file.path(tbl_dir, fname_ind), ...)
            })
        }
            res_ind$stat <- "age_ratio"
            if (clean_indicator_names) res_ind$indicator <- clean_indic_name(indicator)
            res <- dplyr::bind_rows(res, res_ind)
        }

    ## Tidy
    res <- tidyr::gather(res, -Name, -Iso, -Percentile, -stat, -indicator,
                  key = "year", value = "value")

    ## Lower case column names
    colnames(res) <- tolower(colnames(res))

    return(res)

}



## Get results from the RDA files
get_indicator_quantiles <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
                             aggregate = "Country",
                             adj = c("orig", "adj", "sub_adj"),
                             verbose = FALSE) {

        warning("===============================================================================\n",
            " 'get_indicator_quantiles' IS IN DEVELOPMENT AND NOT FULLY TESTED\n",
            "===============================================================================\n")

    adj <- match.arg(adj)
    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)

    if (identical(adj, "orig")) {
        if (identical(aggregate, "Country")) fname <- "res.country.rda"
        else if (identical(aggregate, "UNPDaggregate")) fname <- "res.aggregate.rda"
    }

    tmp_env <- new.env()
    return(get(load(file.path(output_dir, fname), envir = tmp_env)[1], envir = tmp_env))
}
