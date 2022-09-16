###-----------------------------------------------------------------------------
### * Work with Input Data


##' Get country input data actually used
##'
##' Reads the '.csv' file containing the prevalence data used in the
##' run. The file is read using \code{\link[readr]{read_csv}}.
##'
##' @param processed Logical; get the input data after processing by
##'     \pkg{\link{FPEMglobal}}, or the raw input data?
##' @inheritParams get_csv_res
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##' @author Mark Wheldon
##'
##' @family input_data_functions
##'
##' @export
get_used_input_data <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
             processed = TRUE,
             verbose = FALSE, ...) {

    if (!verbose) { op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
    on.exit(options(op), add = TRUE, after = FALSE) }

        output_dir <-
            output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                               root_dir = root_dir, verbose = verbose)

        if (processed) {
            data_dir <- file.path(output_dir)
            fname <- "dataCPmodel_input_preprocessed.csv"
        } else {
            fname <- "dataCPmodel_input.csv"
            data_dir_name  <- "data"
            data_dir <- file.path(output_dir, data_dir_name)
        }
    if (verbose) message("Reading '", file.path(data_dir, fname), "'.")
        readr::read_csv(file.path(data_dir, fname), show_col_types = verbose)
}


##' Get denominator counts actually used
##'
##' @description
##' Reads denominators from the \file{res.country.rda} or
##' \file{res.country.all.women.rda} files. The output directory will
##' have to have been post-processed (i.e., the files loaded will need
##' to have been created by \pkg{FPEMglobal} functions. If this is not
##' the case, use \code{\link{get_csv_denominators}}.
##'
##' The counts appear in the source files in units of 1000 but are
##' returned are in units of the individual by default (\code{units =
##' "unit"}). Use the \code{units} argument to return counts in
##' multiples of 1000.
##'
##' The result is a \code{\link[tibble]{tibble}} with columns \code{"iso"},
##' \code{"name"}, \code{"year"}, \code{"count"}. The \code{"year"}
##' column will have years in \dQuote{mid-year} format because this is
##' the format in which they are stored in the \file{res....} files.
##'
##' @details
##' To match the format returned by \code{\link{get_csv_denominators}},
##' use \preformatted{
##' get_csv_denominators(...,
##'                      add_marital_group = FALSE, ..., add_age_group = FALSE,
##'                      clean_col_names = TRUE, table_format = "long")}
##' \code{get_csv_denominators} cannot produce a data frame with years
##' in \dQuote{mid-year} format.
##'
##' @inheritParams get_csv_res
##' @inheritParams get_output_dir
##' @param units Units in which to return the counts.
##' @return A \code{\link[tibble]{tibble}} with the denominators; see
##'     \dQuote{Details}.
##' @author Mark Wheldon
##'
##' @family input_data_functions
##'
##' @export
get_used_denominators <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                  units = c("units", "thousands"),
                                  years_as_midyear = TRUE,
                                  verbose = FALSE) {

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = TRUE, countrytrajectories = FALSE,
                           made_results = FALSE)

    stopifnot(is.logical(years_as_midyear))

    units <- match.arg(units)
    if (identical(units, "units")) unit_mult <- 1000
    else unit_mult <- 1

    res <- get_indicator_summary_results(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                                         stat = "std", adjusted = "orig", aggregate = "country")
    iso_names <- data.frame(iso = as.numeric(res$iso.g), name = names(res$CIprop.Lg.Lcat.qt))
    denom <- expand.grid(iso = as.numeric(res$iso.g),
                         year = as.numeric(dimnames(res$CIprop.Lg.Lcat.qt[[1]][[1]])[[2]]),
                         count = NA,
                         stringsAsFactors = FALSE)
    denom <- base::merge(denom, iso_names, all.x = TRUE)
    denom <- denom[with(denom, order(iso, year)), ]
    for (i in seq_along(res$W.Lg.t)) {
        nm <- names(res$W.Lg.t)[i]
        denom[denom$name == nm, "count"] <- res$W.Lg.t[[i]] * unit_mult
    }
    if (!years_as_midyear) denom$year <- round_down_years(denom$year)
    return(tibble::as_tibble(denom[, c("iso", "name", "year", "count")]))
}



##' Get denominator counts from csv files
##'
##' @description
##' Reads the '.csv' file containing the married and unmarried
##' denominator counts used in the run. These are in units of the
##' individual.
##'
##' The data frame returned has columns \code{"iso"}, \code{"name"},
##' \code{"year"}, \code{"count"}. The \code{"year"} column will have
##' years in \dQuote{1st Jan} format (i.e., whole number years, 1970,
##' 1971 etc.) because this is the format in which they are stored in
##' the \file{csv} files. This differs from
##' \code{\link{get_used_denominators}}. There is no option to return
##' \dQuote{mid-year} format years from \code{get_csv_denominators}.
##'
##' @details
##' One or more marital groups can be requested via argument
##' \code{marital_group}. If more than one, a column
##' \dQuote{\code{marital_group}} will be present in the output to
##' identify records accordingly (regardless the value of
##' \code{add_marital_group}). Value \code{"default"} will read the
##' the meta data (see \code{\link{get_global_mcmc_args}}) and ensure
##' the marital group matching that of the output is included. If
##' \code{"all women"} is included, values will be constructed by
##' summing the married and unmarried denominators.
##'
##' If \code{table_format} is \code{"long"}, the counts are in a
##' single column called \dQuote{\code{value}}, with the necessary
##' identifying columns added. Otherwise the result is in the same
##' format as the original counts files, which have one column per
##' year.
##'
##' The counts appear in the \file{.csv} files in units of 1 and, by
##' default, are returned as such (\code{units = "unit"}). Use the
##' \code{units} argument to return counts in multiples of 1000.
##'
##' @section Technical Note:
##' The poulation denominators are stored in \file{.csv} files in the
##' output directory. These are read in using an unexported function
##' from \pkg{FPEMglobal}, which is a thin wrapper to \code{read.csv}.
##'
##' @param filename Name of file with the counts (including
##'     extension). If \code{NULL}, this will be inferred from the
##'     meta data (see \code{\link{get_global_mcmc_args}}.
##' @param age_group Age group of the counts for the output data
##'     frame. If the column names are of the form
##'     'U/MW_\[aabb\]_year' this is ignored and the age group is
##'     taken from the column names.
##' @param marital_group Marital group to load denominators for. Can
##'     be more than one; see \dQuote{Details}. The default is the
##'     length 1 vector \code{"default"}.
##' @param add_marital_group Logical. Should a column
##'     \dQuote{\code{marital_group}} be added to the output to
##'     indicate the marital group? Such a column is always added if
##'     \code{marital_group} has more than one element.
##' @param units Units in which to return the counts.
##' @inheritParams get_csv_res
##' @inheritParams get_output_dir
##'
##' @return A \code{\link[tibble]{tibble}} with the denominators; see
##'     \dQuote{Details}.
##' @author Mark Wheldon
##'
##' @family input_data_functions
##'
##' @export
get_csv_denominators <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                  filename = NULL,
                                  marital_group = c("default", "married", "unmarried", "all women"),
                                  add_marital_group = length(marital_group) > 1L,
                                  age_group = NULL,
                                  add_age_group = TRUE,
                                  clean_col_names = TRUE,
                                 units = c("units", "thousands"),
                                 years_as_midyear = TRUE,
                                 table_format = c("long", "raw"),
                                 verbose = FALSE, ...) {

    ## -------* Sub-Functions

    ## MUST use this _inside_ this function.
    get_value_cols_colnames <- function(x) {

        ## Determine column name format

        ## There are three formats in use for the value columns:
        ## 1. E.g., MW_1549_1979 (i.e., marital group, age group, year)
        ## 2. E.g., 1549_1979 (i.e., age group, year)
        ## 3. E.g., 1979 (i.e., year)

        col_fmt1_regexp <- "^(M|U)W_[0-9]{4}_(19|20|21)[0-9]{2}$"
        col_fmt2_regexp <- "^X?[0-9]{4}_(19|20|21)[0-9]{2}$"
        col_fmt3_regexp <- "^X?(19|20|21)[0-9]{2}$"

        check_fmt1 <- grep(col_fmt1_regexp, colnames(x), value = TRUE)
        check_fmt2 <- grep(col_fmt2_regexp, colnames(x), value = TRUE)
        check_fmt3 <- grep(col_fmt3_regexp, colnames(x), value = TRUE)

        is_fmt1 <- length(check_fmt1)
        is_fmt2 <- length(check_fmt2)
        is_fmt3 <- length(check_fmt3)

        if (is_fmt1) value_cols <- check_fmt1
        else if (is_fmt2) value_cols <- check_fmt2
        else if (is_fmt3) value_cols <- check_fmt3
        else stop("Column format of '", fpath, "' cannot be determined.")

        if (is_fmt1 || is_fmt2) {
            if (is_fmt1) {
                age_group_from_cols <-
                    sapply(strsplit(gsub("X", "", value_cols), "_", fixed = TRUE), "[[", 2)
            } else if (is_fmt2) {
                age_group_from_cols <-
                    sapply(strsplit(gsub("X", "", value_cols), "_", fixed = TRUE), "[[", 1)
            }
            age_group_from_cols <- paste(substr(age_group_from_cols, 1, 2),
                                         substr(age_group_from_cols, 3,4), sep = "-")
            if (!identical(as.character(rep(age_group_from_cols[1], length(age_group_from_cols))),
                           as.character(age_group_from_cols)))
                stop("Age groups implied in column names of '", fpath, "' are not all the same: ",
                     toString(age_group_from_cols))
            if (!identical(as.character(age_group_from_cols[1]), as.character(age_group)))
                stop("Age group implied in column names is '", as.character(age_group_from_cols[1]),
                     "' but 'age_group' argument is '", age_group,
                     "'. Note: if you didn't specify 'age_group' it was read from the meta data.")
        }
        return(value_cols)
    }

    ## -------* Set-up

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = is.null(filename), countrytrajectories = FALSE,
                           made_results = FALSE,
                           assert_valid = FALSE #<<<<<<<<<<<< IF THIS IS TRUE TESTS WILL PROBABLY FAIL
                           )

    output_age_group <- get_age_group(output_dir = output_dir)

    if (missing(marital_group)) marital_group <- "default"
    else marital_group <- match.arg(marital_group, several.ok = TRUE)
    if ("default" %in% marital_group) {
        marital_group[marital_group == "default"] <- get_marital_group(output_dir = output_dir)
        marital_group <- unique(marital_group)
    }

    table_format <- match.arg(table_format)

    units <- match.arg(units)
    if (identical(units, "units")) unit_mult <- 1
    else unit_mult <- 1/1000

    data_dir_name <- "data"
    data_dir <- file.path(output_dir, data_dir_name)

    if (is.null(filename)) {
        if (is_all_women_run(output_dir = output_dir))
            denom_file_name_meta <- get_combine_runs_args(output_dir = output_dir)$denominator_counts_csv_filename
        else
            denom_file_name_meta <- get_global_post_process_args(output_dir = output_dir)$denominator_counts_csv_filename
        if (length(denom_file_name_meta) && nchar(denom_file_name_meta)) {
            filename <- basename(denom_file_name_meta)

            ## Since filename auto-determined, read the age group as well.
            age_group_from_args <- output_age_group
            if (!is.null(age_group) && !identical(as.character(age_group), as.character(age_group_from_args)))
                warning("'age_group' = '", age_group, "', but output directory is a run for age group '",
                        age_group_from_args, "'. Argument 'age_group' reset to the latter.\n\nIf you want to load an age group different from the age group of the output directory, you must specify 'filename'.")
            age_group <- age_group_from_args
        }
        else stop("'filename' could not be determined from meta data; you must be supply it via the 'filename' argument.")
    }

    fpath <- file.path(data_dir, filename)

    ## If 'age_group' still 'NULL', use mcmc args
    if (is.null(age_group)) age_group <- output_age_group

    ## -------* Load .csv

    if (any(marital_group %in% c("married", "all women"))) {
        denom_counts_m <-
            FPEMglobal:::extractDenominators(fpath, in_union = 1)
    }
    if (any(marital_group %in% c("unmarried", "all women"))) {
        denom_counts_u <-
            FPEMglobal:::extractDenominators(fpath, in_union = 0)
    }
    if ("all women" %in% marital_group) {
        if (!(identical(dim(denom_counts_m), dim(denom_counts_u)) &&
              identical(colnames(denom_counts_m), colnames(denom_counts_u)) &&
              identical(sort(denom_counts_m$ISO.code), sort(denom_counts_u$ISO.code))))
            stop("Cannot create all women denominators. The married and unmarried tables have different ISOs or different columns.")
        value_cols <- get_value_cols_colnames(denom_counts_m)
        denom_counts_a <- denom_counts_m
        denom_counts_a[, value_cols] <-
            denom_counts_m[, value_cols] +
            denom_counts_u[match(denom_counts_m$ISO.code, denom_counts_u$ISO.code), value_cols]
        if (all(grepl("^MW_", value_cols))) {
            value_cols_idx <- match(value_cols, colnames(denom_counts_a))
            colnames(denom_counts_a)[value_cols_idx] <-
                make.names(gsub("^MW_", "", colnames(denom_counts_a)[value_cols_idx]))
        }
    }

    denom_counts <- data.frame()
    if ("married" %in% marital_group) {
        denom_counts <- dplyr::bind_rows(denom_counts, denom_counts_m)
        if (add_marital_group) denom_counts$marital_group <- "married"
    }
    if ("unmarried" %in% marital_group) {
        denom_counts <- dplyr::bind_rows(denom_counts, denom_counts_u)
        if (add_marital_group) denom_counts$marital_group <- "unmarried"
    }
    if ("all women" %in% marital_group) {
        denom_counts <- dplyr::bind_rows(denom_counts, denom_counts_a)
        if (add_marital_group) denom_counts$marital_group <- "all women"
    }

    value_cols <- get_value_cols_colnames(denom_counts)
    value_cols_idx <- match(value_cols, colnames(denom_counts))

    denom_counts <- tibble::as_tibble(denom_counts)

    ## -------* Cleaning, Additional Columns, etc.

    denom_counts[, value_cols] <- denom_counts[, value_cols] * unit_mult

    if (years_as_midyear) {
        colnames(denom_counts)[value_cols_idx] <-
            paste0(colnames(denom_counts)[value_cols_idx], ".5")
        value_cols <- paste0(value_cols, ".5")
        }

    if (identical(table_format, "long")) {
        value_cols <- sapply(strsplit(value_cols, "_"), function(z) z[[length(z)]])
        colnames(denom_counts)[value_cols_idx] <- value_cols
        denom_counts <-
            tidyr::gather(denom_counts, tidyselect::all_of(value_cols),
                          key = "year", value = "count") %>%
        dplyr::mutate(year = gsub("X|x", "", year)) %>%
            dplyr::mutate(year = as.numeric(year))
        if (any(is.na(denom_counts$year))) stop("'year' is 'NA' for some rows.")
    }

    if (add_age_group && !("age_group" %in% colnames(denom_counts)) && identical(length(age_group), 1L))
        denom_counts$age_group <- age_group

    if (add_marital_group && !("marital_group" %in% colnames(denom_counts)) && identical(length(marital_group), 1L))
        denom_counts$marital_group <- marital_group

    if (clean_col_names) {
        denom_counts <- clean_col_names(denom_counts)
    }

    ## -------* END

   return(tibble::as_tibble(denom_counts))
}


##' Read contents of a named .csv file
##'
##' A thin wrapper to \code{\link[readr]{read_csv}} to read the
##' contents of an arbitrary \file{.csv} file in the output directory
##' of an FPEM run. The file is read using
##' \code{\link[readr]{read_csv}}.
##'
##' The file will be searched for in the output directory, so add any
##' subdirectories to \code{file_name} (e.g., \code{file_name =
##' "table/obj/myFile.csv"}).
##'
##' @inheritParams get_csv_res
##' @inheritParams get_output_dir
##' @param file_name Name of file to read, \emph{including} any
##'     subdirectory and file extension; see \dQuote{Details}.
##' @return A \code{\link[tibble]{tibble}} with the file contents.
##' @author Mark Wheldon
##'
##' @seealso get_used_input_data, get_csv_res
##' @export
read_named_csv_file <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                file_name, verbose = FALSE, ...) {

    if (!verbose) { op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
    on.exit(options(op), add = TRUE, after = FALSE) }

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = FALSE, countrytrajectories = FALSE,
                           made_results = FALSE)
    if (verbose) message("Reading '", file.path(output_dir, file_name), "'.")
    readr::read_csv(file = file.path(output_dir, file_name), show_col_types = verbose)
}
