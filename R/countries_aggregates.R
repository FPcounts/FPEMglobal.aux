###-----------------------------------------------------------------------------
### * Work with Countries and Aggregates

### NOTE: See also 'text_manipulation.R' for more functions that deal with region/aggregate names.




##' Read table of the \dQuote{195} countries
##'
##' These are the 195 countries in the original married women
##' model. The file is read using \code{\link[readr]{read_csv}}.
##'
##' @section Note:
##' These are taken from the \pkg{FPEMglobal} package \emph{installed
##' on your system}. As such, they \emph{may} not correspond to the
##' aggregates used in a specific model run (although, as of
##' 2022-08-25, this is unlikely).
##'
##' @inheritParams get_csv_res
##' @return A \code{\link[tibble]{tibble}} with the countries.
##' @author Mark Wheldon
##'
##' @family Country and region/aggregate names
##'
##' @export
get_195_countries <- function(clean_col_names = TRUE) {
    verbose <- getOption("FPEMglobal.aux.verbose")
    fname <- system.file("extdata",
                         get_FPEMglobal_extdata_filenames()[["countries_mwra_195"]],
                         package = "FPEMglobal")
    if (verbose) message("Reading '", fname, "'.")
    out <- readr::read_csv(fname, name_repair = "minimal")
    if (clean_col_names) out <- clean_col_names(out)
    return(out)
}


##' Read table of the \dQuote{185} countries
##'
##' These are the 185 countries for which estimates are released by
##' UNPD. The file is read using \code{\link[readr]{read_csv}}. See
##' \dQuote{Note} in the documentation for
##' \code{\link{get_195_countries}}.
##'
##' @inheritParams get_csv_res
##' @return A \code{\link[tibble]{tibble}} with the countries.
##' @author Mark Wheldon
##'
##' @family Country and region/aggregate names
##' @export
get_185_countries <- function(clean_col_names = TRUE) {
    verbose <- getOption("FPEMglobal.aux.verbose")
    fname <- system.file("extdata",
                         get_FPEMglobal_extdata_filenames()[["countries_unpd_185"]],
                         package = "FPEMglobal")
    out <- readr::read_csv(fname, name_repair = "minimal")
    if (clean_col_names) out <- clean_col_names(out)
    return(out)
}


##' Get country geographic classifications
##'
##' Returns a table of geographic country groupings. The file is read
##' using \code{\link[readr]{read_csv}}. See \dQuote{Note} in the
##' documentation for \code{\link{get_195_countries}}.
##' @inheritParams get_csv_res
##' @inheritParams get_used_unpd_regions
##' @return A \code{\link[tibble]{tibble}} with the aggregates.
##' @author Mark Wheldon
##'
##' @family Country and region/aggregate names
##' @export
get_country_classifications <- function(M49_region_names_names = TRUE,
                                        clean_col_names = TRUE) {
    verbose <- getOption("FPEMglobal.aux.verbose")
    fname <- system.file("extdata",
                         get_FPEMglobal_extdata_filenames()[["country_classifications"]],
                         package = "FPEMglobal")
    out <- readr::read_csv(fname, name_repair = "minimal")
    if (M49_region_names_names)
            out[, "Country or area"] <-
                convert_M49_region_names(out[, "Country or area"], "M49_region_names")
    if (clean_col_names) out <- clean_col_names(out)
    return(out)
}


##' Get UNPD aggregate country classifications
##'
##' Loads the country classifications actually used in a model
##' run. The files are read using \code{\link[readr]{read_csv}}.
##'
##' @param M49_region_names_names Logical; should
##'     \code{\link{convert_M49_region_names}(..., convert_from = "M49_region_names")} be
##'     run to standardize country and area names?
##'
##' @inheritParams get_used_input_data
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##' @author Mark Wheldon
##'
##' @family Country and region/aggregate names
##' @export
get_used_unpd_regions <-
    function(output_dir = NULL,
             clean_col_names = TRUE, M49_region_names_names = TRUE) {

        verbose <- getOption("FPEMglobal.aux.verbose")

        output_dir <- output_dir_wrapper(output_dir = output_dir)

        if (!is_all_women_run(output_dir = output_dir)) {
            file_path <- get_global_mcmc_args(output_dir = output_dir)$region_information_csv_filename
        } else {
            file_path <- get_combine_runs_args(output_dir = output_dir)$region_information_csv_filename
        }

        file_name <- basename(file_path)

        data_dir_name <- "data"
        data_dir <- file.path(output_dir, data_dir_name)

        fname <- file.path(data_dir, file_name)
        if (verbose) message("Reading '", fname, "'.")
        out <- readr::read_csv(fname, name_repair = "minimal")
        if (M49_region_names_names)
            out[, "Country or area"] <-
                convert_M49_region_names(out[, "Country or area"], "M49_region_names")
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
    unique(c("ECA_regions.csv",
      "ECE_regions.csv",
      "ECLAC_regions.csv",
      "ESCWA_regions.csv",
      "ESCWA_regions_extended.csv",
        "WB_Income_2020-w_middle.csv",
      "WB_IncomeGroup_8June2022-w_middle.csv",
      "SDG_regions_LDCs.csv",
      "SDG_regions_LLDCs_SIDS.csv",
      "SDG_regions_level_1.csv",
      "SDG_regions_level_2.csv",
      "UNFPA_regions.csv",
      "UNICEF_regions_level_1.csv",
      "UNICEF_regions_level_2.csv",
      "WHO_regions.csv",
      get_FPEMglobal_extdata_filenames(file_ext = TRUE)[["special_aggregates"]]))
}

make_special_aggregates_names <- function(csv_filenames) {
    gsub("\\.csv", "", csv_filenames)
}
list_special_aggregates_names <- function() {
    make_special_aggregates_names(list_special_aggregates_csv_filenames())
}

##' Get special aggregate country classifications
##'
##' Loads the special country classifications actually used to produce
##' the results. The files are read using
##' \code{\link[readr]{read_csv}}.
##'
##' @inheritParams get_used_input_data
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##' @author Mark Wheldon
##'
##' @family Country and region/aggregate names
##' @export
get_used_special_aggregates <-
    function(output_dir = NULL,
             clean_col_names = TRUE) {

        stop("THIS ISN'T WORKING PROPERLY")
        ## Problem with these because they are not exclusive
        ## classifications; countries can be in more than one
        ## aggregate within the classification.

        verbose <- getOption("FPEMglobal.aux.verbose")

        output_dir <-
            output_dir_wrapper(output_dir = output_dir)
        data_dir_name <- "data"
        data_dir <- file.path(output_dir, data_dir_name)

        out <- tibble::tibble(iso.country = as.numeric(NA))

        agg_csv_names <- list_special_aggregates_csv_filenames()
        agg_names <- list_special_aggregates_names()

        for (i in seq_along(agg_csv_names)) {
            fpath <- file.path(data_dir, agg_csv_names[i])
            if (file.exists(fpath)) {
                if (verbose) message("Reading '", file.path(tbl_dir, fpath), "'.")
                y <- readr::read_csv(fpath, name_repair = "minimal")[,c("iso.country", "groupname")]
                colnames(y)[colnames(y) == "groupname"] <- agg_names[i]
                out <- dplyr::full_join(out, y, by = "iso.country")
            }
        }
        out <- out[!is.na(out$iso.country),]

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
