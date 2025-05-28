###-----------------------------------------------------------------------------
### * Work with Countries and Aggregates

### NOTE: See also 'text_manipulation.R' for more functions that deal with region/aggregate names.


##' Get country geographic classifications
##'
##' Returns a table of geographic country groupings stored in the
##' \pkg{FPEMglobal} package. The file that is used depends on the
##' \code{version} argument. \code{version = "used"} refers to the country
##' classifications used in the run referenced by \code{output_dir} (which,
##' therefore, must be specified). \code{version = "installed"} refers to the
##' classifications file distributed with the version of \pkg{FPEMglobal}
##' installed on the system (in which case, \code{output_dir} need not be
##' specified, and is ignored in any case). In both cases, the classifications
##' are stored in \file{.csv} files. Once identified, the file is read using
##' \code{\link[readr]{read_csv}}.
##'
##' @section Note:
##'
##' The country classifications file is considered to be an \dQuote{internal}
##' file in \pkg{FPEMglobal}. As such, its filename is not passed in to the main
##' user functions such as \code{\link[FPEMglobal]{do_global_mcmc}},
##' \code{\link[FPEMglobal]{do_global_all_women_run()}}, etc. One consequence is
##' that the filename is not stored in the argument lists retrieved by, e.g.,
##' \code{\link{get_global_post_process_args}}. Contrast this with the main
##' input file and the denominator counts file, for example. For this reason,
##' there is no equivalent of \code{\link{get_used_csv_denominators_filepath}}
##' for the country classifications. To retrieve the file names of internal
##' files, including the country classifications,
##' \code{\link[FPEMglobal]{pkg_files_included}} must be used, which is what
##' this function does.
##'
##' @inheritParams get_csv_res
##' @inheritParams get_input_data
##' @param M49_region_names Logical; should
##'     \code{\link{convert_M49_region_names}(..., convert_to =
##'     "M49_region_names")} be run to standardize country and area names?
##' @return A \code{\link[tibble]{tibble}} with the aggregates.
##' @author Mark Wheldon
##'
##' @family Country and region/aggregate names
##' @export
get_country_classifications <- function(output_dir = NULL,
                                        M49_region_names = TRUE,
                                        clean_col_names = TRUE,
                                        version = c("used", "installed")) {

    ## Argument check
    version <- match.arg(version)
    if ((is.null(output_dir) || missing(output_dir)) && identical(version, "used"))
        stop("'output_dir' has not been specified: 'version' must be '\"installed\"'.")

    verbose <- getOption("FPEMglobal.aux.verbose")

    if (!verbose) {
        op <- options(); force(op)
        options(readr.show_col_types = FALSE)
        on.exit(options(op), add = TRUE, after = FALSE)
    }

    ## -------* Used

    if (identical(version, "used")) {

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

    } else {

        ## -------* Installed

        fname <- system.file("extdata",
                             get_FPEMglobal_extdata_filenames()[["country_classifications"]],
                             package = "FPEMglobal")

    }

    if (verbose) message("Reading '", fname, "'.")
    out <- readr::read_csv(fname, name_repair = "minimal")
    if (M49_region_names)
        out[, "Country or area"] <-
            convert_M49_region_names(out[, "Country or area"], "M49_region_names")
    if (clean_col_names) out <- clean_col_names(out)

    return(out)
}


##' Read table of the \dQuote{195} or \dQuote{185} countries
##'
##' \code{get_195_countries} returns the 195 countries included in the original
##' married women model. \code{get_185_countries} returns the 185 countries for
##' which estimates are released by UNPD. The file that is used depends on the
##' \code{version} argument. \code{version = "recorded"} refers to the country
##' classifications recorded in the \file{data} folder of the run referenced by
##' \code{output_dir} (which, therefore, must be specified). \code{version =
##' "installed"} refers to the classifications file distributed with the version
##' of \pkg{FPEMglobal} installed on the system (in which case,
##' \code{output_dir} need not be specified, and is ignored in any case). Once
##' identified, the file is read using \code{\link[readr]{read_csv}}.
##'
##' @section Note:
##'
##' Like the country classification file, the lists of 195 and 185 countries are
##' \emph{internal} files to \pkg{FPEMglobal}. See the \dQuote{Note} in
##' \code{\link{get_country_classifications}} for what this implies.
##'
##' @inheritParams get_country_classifications
##' @return A \code{\link[tibble]{tibble}} with the countries.
##' @author Mark Wheldon
##'
##' @family Country and region/aggregate names
##'
##' @export
get_195_countries <- function(output_dir = NULL,
                              M49_region_names = TRUE,
                              clean_col_names = TRUE,
                              version = c("recorded", "installed")) {

    get_XXX_countries("195", output_dir = output_dir,
                             M49_region_names = M49_region_names,
                             clean_col_names = clean_col_names,
                      version = version)
}


##' Read table of the \dQuote{185} countries
##'
##' These are the 185 countries for which estimates are released by
##' UNPD. The file is read using \code{\link[readr]{read_csv}}. See
##' \dQuote{Note} in the documentation for
##' \code{\link{get_195_countries}}.
##'
##' @inheritParams get_country_classifications
##' @return A \code{\link[tibble]{tibble}} with the countries.
##' @author Mark Wheldon
##'
##' @family Country and region/aggregate names
##' @export
##'
get_185_countries <- function(output_dir = NULL,
                              M49_region_names = TRUE,
                              clean_col_names = TRUE,
                              version = c("recorded", "installed")) {

    get_XXX_countries("185", output_dir = output_dir,
                      M49_region_names = M49_region_names,
                      clean_col_names = clean_col_names,
                      version = version)
}


## This is used inside `get_195_countries()` and `get_185_countries()`.
##
get_XXX_countries <- function(XXX = c("195", "185"),
                              output_dir = NULL,
                              M49_region_names = TRUE,
                              clean_col_names = TRUE,
                              version = c("recorded", "installed")) {

    XXX <- match.arg(XXX)

    if (identical(XXX, "195")) id_name <- paste0("countries_mwra_", XXX)
    else id_name <- paste0("countries_unpd_", XXX)

    ## Argument check
    version <- match.arg(version)
    if ((is.null(output_dir) || missing(output_dir)) && identical(version, "recorded"))
        stop("'output_dir' has not been specified: 'version' must be '\"installed\"'.")

    verbose <- getOption("FPEMglobal.aux.verbose")
    if (!verbose) {
        op <- options(); force(op)
        options(readr.show_col_types = FALSE)
        on.exit(options(op), add = TRUE, after = FALSE)
    }

    ## -------* Recorded

    if (identical(version, "recorded")) {

        output_dir <- output_dir_wrapper(output_dir = output_dir)

        file_path <- get_FPEMglobal_extdata_filenames()[[id_name]]
        file_name <- basename(file_path)

        data_dir_name <- "data"
        data_dir <- file.path(output_dir, data_dir_name)

        fname <- file.path(data_dir, file_name)

        if (!file.exists(fname))
            stop("File '", fname, "' does not exist. Try using 'version = \"installed\"' instead.")

    } else {

        ## -------* Installed

        fname <- system.file("extdata",
                             get_FPEMglobal_extdata_filenames()[[id_name]],
                             package = "FPEMglobal")
    }

    if (verbose) message("Reading '", fname, "'.")
    out <- readr::read_csv(fname, name_repair = "minimal")
    if (clean_col_names) out <- clean_col_names(out)
    if (M49_region_names)
        out[, "name"] <-
            convert_M49_region_names(out[, "name"], convert_to = "M49_region_names")
    return(out)
}


##' Get UNPD aggregate country classifications
##'
##' Loads the country classifications actually used in a model
##' run. The files are read using \code{\link[readr]{read_csv}}.
##'
##' This function is deprecated. Please use
##' \code{\link{get_country_classifications(..., version = "used")}}.
##'
##' @inheritParams get_country_classifications
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##' @author Mark Wheldon
##'
##' @keywords internal
##' @export
get_used_unpd_regions <-
    function(output_dir = NULL,
             clean_col_names = TRUE, M49_region_names = TRUE) {

        lifecycle::deprecate_soft(
                       when = "1.3.0",
                       what = "get_used_unpd_regions()",
                       with = "get_country_classifications(..., version = \"used\")")

        return(get_country_classifications(output_dir = output_dir,
                                           M49_region_names = M49_region_names,
                                           clean_col_names = clean_col_names,
                                           version = "used"))
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
##' @inheritParams get_input_data
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

        ## !! If you ever fix this, you'll need to make it consistent with the
        ## !! other 'get_' functions in this file by, e.g., maybe by adding a
        ## !! 'version' argument and the M49 conversion argument.

        verbose <- getOption("FPEMglobal.aux.verbose")
    if (!verbose) {
        op <- options(); force(op)
        options(readr.show_col_types = FALSE)
        on.exit(options(op), add = TRUE, after = FALSE)
    }

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
