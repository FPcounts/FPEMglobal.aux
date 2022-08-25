###-----------------------------------------------------------------------------
### * Work with Countries and Aggregates

##' Read table of the \dQuote{195} countries
##'
##' These are the 195 countries in the original married women
##' model. The file is read using \code{\link{[readr]{read_csv}}}.
##'
##' @section Note:
##' These are taken from the \pkg{FPEMglobal} package \emph{installed
##' on your system}. As such, they \emph{may} not correspond to the
##' aggregates used in a specific model run (although, as of
##' 2022-08-25, this is unlikely).
##'
##' @family countries, regions and aggregates functions
##'
##' @inheritParams get_csv_res
##' @return A \code{\link[tibble]{tibble}} with the countries.
##' @author Mark Wheldon
##'
##' @family country_aggregates
##'
##' @export
get_195_countries <- function(clean_col_names = TRUE, verbose = FALSE) {
    if (!verbose) { op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
        on.exit(options(op), add = TRUE, after = FALSE) }
    out <- readr::read_csv(system.file("extdata/countries_mwra_195.csv", package = "FPEMglobal"))
    if (clean_col_names) out <- clean_col_names(out)
    return(out)
}


##' Read table of the \dQuote{185} countries
##'
##' These are the 185 countries for which estimates are released by
##' UNPD. The file is read using \code{\link{[readr]{read_csv}}}. See
##' \dQuote{Note} in the documentation for
##' \code{\link{get_195_countries}}.
##'
##' @family countries, regions and aggregates functions
##'
##' @inheritParams get_csv_res
##' @return A \code{\link[tibble]{tibble}} with the countries.
##' @author Mark Wheldon
##'
##' @family country_aggregates
##' @export
get_185_countries <- function(clean_col_names = TRUE, verbose = FALSE) {
    if (!verbose) { op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
        on.exit(options(op), add = TRUE, after = FALSE) }
    out <- readr::read_csv(system.file("extdata/countries_unpd_185.csv", package = "FPEMglobal"))
    if (clean_col_names) out <- clean_col_names(out)
    return(out)
}


##' Get country geographic classifications
##'
##' Returns a table of geographic country groupings. The file is read
##' using \code{\link{[readr]{read_csv}}}. See \dQuote{Note} in the
##' documentation for \code{\link{get_195_countries}}.
##'
##' @family countries, regions and aggregates functions
##'
##' @inheritParams get_csv_res
##' @inheritParams get_used_unpd_regions
##' @return A \code{\link[tibble]{tibble}} with the aggregates.
##' @author Mark Wheldon
##'
##' @family country_aggregates
##' @export
get_country_classifications <- function(UNlocations_names = TRUE,
                                        clean_col_names = TRUE,
                                        verbose = FALSE) {
    if (!verbose) { op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
        on.exit(options(op), add = TRUE, after = FALSE) }
    out <- readr::read_csv(system.file("extdata/country_and_area_classification.csv", package = "FPEMglobal"))
    if (UNlocations_names)
            out[, "Country or area"] <-
                match_UNlocations(out[, "Country or area"], "UNlocations")
    if (clean_col_names) out <- clean_col_names(out)
    return(out)
}


##' Get UNPD aggregate country classifications
##'
##' Loads the country classifications actually used in a model
##' run. The files are read using \code{\link{[readr]{read_csv}}}.
##'
##' @family countries, regions and aggregates functions
##'
##' @param UNlocations_names Logical; should
##'     \code{\link{match_UNlocations}(..., return_names = "UNlocations")} be
##'     run to standardize country and area names?
##'
##' @inheritParams get_used_input_data
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##' @author Mark Wheldon
##'
##' @family country_aggregates
##' @export
get_used_unpd_regions <-
    function(run_name = NULL, output_dir = NULL, root_dir = NULL,
             clean_col_names = TRUE, UNlocations_names = TRUE,
             verbose = FALSE) {

        if (!verbose) { op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
        on.exit(options(op), add = TRUE, after = FALSE) }

        output_dir <-
            output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                               root_dir = root_dir, verbose = verbose)
        data_dir_name <- "data"
        data_dir <- file.path(output_dir, data_dir_name)

        out <- readr::read_csv(file.path(data_dir,
                                         "country_and_area_classification.csv"))
        if (UNlocations_names)
            out[, "Country or area"] <-
                match_UNlocations(out[, "Country or area"], "UNlocations")
        if (clean_col_names) out <- clean_col_names(out)

        return(out)
    }


## List World Bank aggregates
list_world_bank_aggregates_names <- function() {
    c("high income countries",
      "middle income countries",
      "upper middle income countries",
      "lower middle income countries",
      "low income countries",
      "no income group")
}


## List of special aggregates
list_special_aggregates_csv_filenames <- function() {
    ## This just has to be a hard-coded list of all possible special aggregate csv file names.
    c("WB_Income_2020-w_middle.csv",
      "WB_IncomeGroup_8June2022-w_middle.csv",
      "SDG_regions_LDCs.csv",
      "SDG_regions_LLDCs_SIDS.csv",
      "SDG_regions_level_1.csv",
      "SDG_regions_level_2.csv",
      "UNFPA_regions.csv",
      "UNICEF_regions_level_1.csv",
      "UNICEF_regions_level_2.csv",
      "WHO_regions.csv")
}
list_special_aggregates_names <- function() {
    gsub("\\.csv", "", list_special_aggregates_csv_filenames())
}

##' Get special aggregate country classifications
##'
##' Loads the special country classifications actually used to produce
##' the results. The files are read using
##' \code{\link{[readr]{read_csv}}}.
##'
##' @family countries, regions and aggregates functions
##'
##' @inheritParams get_used_input_data
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##' @author Mark Wheldon
##'
##' @family country_aggregates
##' @export
get_used_special_aggregates <-
    function(run_name = NULL, output_dir = NULL, root_dir = NULL,
             clean_col_names = TRUE,
             verbose = FALSE, ...) {

        if (!verbose) { op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
        on.exit(options(op), add = TRUE, after = FALSE) }

        output_dir <-
            output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                               root_dir = root_dir, verbose = verbose)
        data_dir_name <- "data"
        data_dir <- file.path(output_dir, data_dir_name)

        out <- tibble::tibble(iso = as.numeric(NA))

        agg_csv_names <- list_special_aggregates_csv_filenames()
        agg_names <- list_special_aggregates_names()

        for (i in seq_along(agg_csv_names)) {
            fpath <- file.path(data_dir, agg_csv_names[i])
            if (file.exists(fpath)) {
                if (verbose) {
                    y <- readr::read_csv(fpath)[,c("iso.country", "groupname")]
                } else {
                    suppressMessages({
                        y <- readr::read_csv(fpath)[,c("iso.country", "groupname")]
                    })
                }
                if (clean_col_names) colnames(y) <- c("iso", agg_names[i])
                out <- dplyr::full_join(out, y, by = "iso")
            }
        }
        out <- out[!is.na(out$iso),]

        if ("SDG_regions_LDCs" %in% colnames(out)) {
            out <- out %>%
                dplyr::mutate(SDG_LDC = SDG_regions_LDCs == "Least Developed Countries (LDCs)" &
                           !is.na(SDG_regions_LDCs)) %>%
                dplyr::select(-SDG_regions_LDCs)
        }
        if ("SDG_regions_LLDCs_SIDS" %in% colnames(out)) {
            out <- out %>%
                dplyr::mutate(SDG_LLDC = SDG_regions_LLDCs_SIDS == "Landlocked developing countries (LLDCs)" &
                           !is.na(SDG_regions_LLDCs_SIDS),
                       SDG_SIDS = SDG_regions_LLDCs_SIDS == "Small island developing States (SIDS)" &
                           !is.na(SDG_regions_LLDCs_SIDS)) %>%
                dplyr::select(-SDG_regions_LLDCs_SIDS)
        }

        if (clean_col_names) {
            out <- clean_col_names(out)
            }

        return(out)
    }


##' Get the country-index map
##'
##' The model outputs are not labelled with country names or ISO
##' codes. Instead there is an internal index for the countries. This
##' function returns a data frame with columns \dQuote{iso.c},
##' \dQuote{name.c}, and \dQuote{filename} containing the ISO codes,
##' country names, and filename of the country-level trajectories. The
##' internal country index corresponds to the country's row number in
##' this data frame.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##' @return A \code{\link[tibble]{tibble}} with country ISO codes, names, and trajectory
##'     file names, where row number corresponds to the internal index
##'     number.
##' @author Mark Wheldon
##' @export
get_country_index <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                verbose = FALSE) {

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)

    traj_index <- try(read.csv(file.path(output_dir, "iso.Ptp3s.key.csv"),
                               colClasses = c("character")))

    if (identical(class(traj_index), "try-error")) {
        stop("Error reading 'iso.Ptp3s.key.csv'. Did you supply a run name or output directory for an all women run?")
        } else return(tibble::as_tibble(traj_index))
    }
