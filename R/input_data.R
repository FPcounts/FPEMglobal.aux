###-----------------------------------------------------------------------------
### * Work with Input Data


##' Get country input data actually used
##'
##' Reads the '.csv' file containing the prevalence data used in a completed run
##' of FPEMglobal (\code{version = "used"}) or distributed with the installed
##' version of \pkg{FPEMglobal} (version = "installed"). The file is read using
##' \code{\link[readr]{read_csv}}.
##'
##' @inheritParams get_csv_res
##' @param variant Which variant of the input file to load? Only relevant if
##'     \code{version = "used"}.
##' @param version Character; retrieve the country classifications from a
##'     completed FPEMglobal run (\code{"used"}), or the version of
##'     \pkg{FPEMglobal} installed (\code{"installed"})?
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##' @author Mark Wheldon
##'
##' @family Input data functions
##'
##' @export
get_input_data <- function(output_dir = NULL,
                           variant = c("raw", "preprocessed", "to_model"),
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

    variant <- match.arg(variant)

    output_dir <-
        output_dir_wrapper(output_dir = output_dir)

    fname <- switch(variant,
                    raw = "dataCPmodel_input_raw.csv",
                    preprocessed = "dataCPmodel_input_preprocessed.csv",
                    to_model = "dataCPmodel_input_to_model.csv",
                    stop("'variant' is invalid"))

        fname <- file.path(output_dir, fname)

    }  else {

    ## -------* Installed

    fname <- system.file("extdata",
                         get_FPEMglobal_extdata_filenames()[["input_data_1549"]],
                         package = "FPEMglobal")

    }

    if (verbose) message("Reading '", file.path(output_dir, fname), "'.")
    out <- readr::read_csv(fname,
                           name_repair = "minimal")

    return(out)
}

get_used_input_data <- function(output_dir = NULL,
                                variant = c("raw", "preprocessed", "to_model")) {
    lifecycle::deprecate_soft(
                   when = "1.3.0",
                   what = "get_used_input_data()",
                   with = "get_input_data(..., version = \"used\")")
    return(get_input_data(output_dir = output_dir, variant = variant, version = "used"))
}


##' Get population denominators
##'
##' @description
##'
##' Returns the population denominators used to convert contraceptive
##' prevalences into counts of users. The source of the denominators is
##' controlled by the \code{version} argument. See \dQuote{Details} for how this
##' works.
##'
##' Note that the units in which the counts are recorded in the two
##' versions are different: in the \code{"rda"} file, they are in units of 1000
##' while in the \code{"csv"} file they are in units of 1. The \code{units}
##' argument can be used to specify the scale of the counts returned. By
##' default, this is \code{"units"} for \emph{both} the \code{"rda"} and
##' \code{"csv"} versions (i.e., the counts in the \code{"rda"} file will be
##' converted to units).
##'
##' @details
##'
##' \subsection{Version = "used_rda"}{
##' Reads denominators from the \file{.rda} file produced during a run
##' (e.g., \file{res.country.rda}, \file{res.country.all.women.rda},
##' etc.). The output directory must have been post-processed
##' otherwise these files will not exist (see \code{\link[FPEMglobal]{post_process_mcmc}}). If this is not the case, use
##' \code{\link{get_denominators}(..., version = "used_csv")}. The result is a
##' \code{\link[tibble]{tibble}} with columns \code{"iso"},
##' \code{"name"}, \code{"year"}, \code{"count"}.
##'
##' \strong{Note:} The counts appear in the source files
##' (\file{res.country.rda}, etc.) in units of 1000 but are returned according
##' to the value of the \code{units} argument, which is \code{"units"} by
##' default.
##' }
##'
##' \subsection{Version = "used_csv"}{
##' Reads a '.csv' file containing the married and unmarried
##' denominator counts used in the FPEMglobal run stored in
##' \code{output_dir}. The data frame returned has columns
##' \code{"iso"}, \code{"name"}, \code{"year"}, \code{"count"}. By
##' default, the filename will be taken from the meta info of the
##' run. Alternatively, a specific file can be read by specifying the
##' path via \code{filename}.
##'
##' One or more marital groups can be requested via argument
##' \code{marital_group}. Value \code{"default"} will read the
##' the meta data (see \code{\link{get_global_mcmc_args}}) and ensure
##' the marital group matching that of the output is included. If
##' \code{"all_women"} is included, values will be constructed by
##' summing the married and unmarried denominators.
##'
##' If \code{table_format} is \code{"long"}, the counts are in a
##' single column called \dQuote{\code{value}}, with the necessary
##' identifying columns added. Otherwise the result is in the same
##' format as the original counts files, which have one column per
##' year.
##'
##' The counts appear in the \file{.csv} files in units of 1 and, by
##' default, are returned as such (\code{units = "unit"}). Use
##' \code{units = "thousands"} to return counts in multiples of 1000.
##'
##' \subsection{Technical Note}{
##' The poulation denominators are stored in \file{.csv} files in the
##' output directory. These are read in using an unexported function
##' from \pkg{FPEMglobal}, which is a thin wrapper to \code{read.csv}.
##' }}
##'
##' @inheritParams get_csv_res
##' @inheritParams get_output_dir
##' @param units Units in which to return the counts.
##' @param version Character; version of denominators to retrieve. See
##'     \dQuote{Details}.
##' @return A \code{\link[tibble]{tibble}} with the denominators; see
##'     \dQuote{Details}.
##' @author Mark Wheldon
##'
##' @family Input data functions
##'
##' @seealso \code{\link{year_storage_format}} for background on
##'     storage format of year values.
##'
##' @export
get_denominators <- function(## used_rda & used_csv
                             output_dir = NULL,
                             units = c("units", "thousands"),
                             years_as_midyear = TRUE,

                             ## used_csv only
                             filename = NULL,
                             marital_group = c("default", "married", "unmarried", "all_women"),
                             age_group = NULL,
                             clean_col_names = TRUE,
                             table_format = c("long", "raw"),

                             ## Version selection
                             version = c("used_csv", "used_rda")) {

    verbose <- getOption("FPEMglobal.aux.verbose")

    ## Arg Check
    version <- match.arg(version)

    units <- match.arg(units)
    if (identical(units, "units")) unit_mult <- 1000
    else unit_mult <- 1

    stopifnot(is.logical(years_as_midyear))

    ## -------* Used RDA

    if (identical(version, "used_rda")) {

        output_dir <-
            output_dir_wrapper(output_dir = output_dir,
                               post_processed = TRUE, countrytrajectories = FALSE,
                               made_results = FALSE)

        res <- get_indicator_summary_results(output_dir = output_dir,
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

        if (!years_as_midyear) {
            denom$year <- round_down_years(denom$year)
        } else {
            denom$year <- put_years_in_mid_year_fmt(denom$year)
        }

        return(tibble::as_tibble(denom[, c("iso", "name", "year", "count")]))

    } else { # version = "used_csv"

        ## -------* Used CSV

        ## Verbose ........................................
        ##
        op <- options(FPEMglobal.verbose = getOption("FPEMglobal.aux.verbose"))
        on.exit(options(op), add = TRUE, after = FALSE)
        ## ................................................

        ## -------** Sub-Functions

        ## There are three formats in use for the value columns:
        ## 1. E.g., MW_1549_1979 (i.e., marital group, age group, year)
        ## 2. E.g., 1549_1979 (i.e., age group, year)
        ## 3. E.g., 1979 (i.e., year)

        col_fmt1_regexp <- "^(M|U)W_[0-9]{4}_(19|20|21)[0-9]{2}$"
        col_fmt2_regexp <- "^X?[0-9]{4}_(19|20|21)[0-9]{2}$"
        col_fmt3_regexp <- "^X?(19|20|21)[0-9]{2}$"

        ## MUST use these _inside_ this function.

        ## Return column name format
        get_value_cols_fmt <- function(x) {

            check_fmt1 <- grep(col_fmt1_regexp, colnames(x), value = TRUE)
            check_fmt2 <- grep(col_fmt2_regexp, colnames(x), value = TRUE)
            check_fmt3 <- grep(col_fmt3_regexp, colnames(x), value = TRUE)

            if (length(check_fmt1)) return(1L)
            else if (length(check_fmt2)) return(2L)
            else if (length(check_fmt3)) return(3L)
        }

        ## Return the character string with the colnames of the value columns
        get_value_cols_colnames <- function(x) {

            fmt <- get_value_cols_fmt(x)

            is_fmt1 <- identical(fmt, 1L)
            is_fmt2 <- identical(fmt, 2L)
            is_fmt3 <- identical(fmt, 3L)

            check_fmt1 <- grep(col_fmt1_regexp, colnames(x), value = TRUE)
            check_fmt2 <- grep(col_fmt2_regexp, colnames(x), value = TRUE)
            check_fmt3 <- grep(col_fmt3_regexp, colnames(x), value = TRUE)

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

        ## -------** Set-up

        output_dir <-
            output_dir_wrapper(output_dir = output_dir,
                               post_processed = is.null(filename), countrytrajectories = FALSE,
                               made_results = FALSE,
                               assert_valid = FALSE #<<<<<<<<<<<< IF THIS IS TRUE TESTS WILL PROBABLY FAIL
                               )

        if (!is.null(age_group) && !identical(length(age_group), 1L))
            stop("'age_group' must be of length '1'.")

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

        stopifnot(is.logical(years_as_midyear))

        data_dir_name <- "data"
        data_dir <- file.path(output_dir, data_dir_name)

        if (is.null(filename)) {
            try_fn <-
                try(get_used_csv_denominators_filepath(output_dir = output_dir),
                    silent = TRUE)

            if (inherits(try_fn, "try-error"))
                stop("'filename' could not be determined from meta data; you must be supply it via the 'filename' argument.")
            else fpath <- try_fn

            ## Since filename auto-determined, read the age group as well.
            age_group_from_args <- output_age_group
            if (!is.null(age_group) && !identical(as.character(age_group), as.character(age_group_from_args)))
                warning("'age_group' = '", age_group, "', but output directory is a run for age group '",
                        age_group_from_args, "'. Argument 'age_group' reset to the latter.\n\nIf you want to load an age group different from the age group of the output directory, you must specify 'filename'.")
            age_group <- age_group_from_args

        } else {
            fpath <- file.path(data_dir, filename)
        }

        ## If 'age_group' still 'NULL', use mcmc args
        if (is.null(age_group)) age_group <- output_age_group

        ## -------** Load .csv

        if (any(marital_group %in% c("married", "all_women"))) {
            denom_counts_m <-
                FPEMglobal:::extractDenominators(fpath, in_union = 1)
        }
        if (any(marital_group %in% c("unmarried", "all_women"))) {
            denom_counts_u <-
                FPEMglobal:::extractDenominators(fpath, in_union = 0)
        }
        if ("all_women" %in% marital_group) {
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
                    make.names(gsub("^MW_", "", colnames(denom_counts_a)[value_cols_idx]), unique = TRUE)
            }
        }

        denom_counts <- data.frame()
        if ("married" %in% marital_group) {
            denom_counts <-
                dplyr::bind_rows(denom_counts,
                                 data.frame(denom_counts_m, mar_gp = "married"))
        }
        if ("unmarried" %in% marital_group) {
            denom_counts <-
                dplyr::bind_rows(denom_counts,
                                 data.frame(denom_counts_u, mar_gp = "unmarried"))
        }
        if ("all_women" %in% marital_group) {
            denom_counts <-
                dplyr::bind_rows(denom_counts,
                                 data.frame(denom_counts_a, mar_gp = "all_women"))
        }
        mar_gp_col_nm <- switch(table_format,
                                long = "marital_group",
                                raw = "In.union",
                                stop("Error in 'mar_gp_col_nm'"))
        denom_counts <-
            gdata::rename.vars(denom_counts,
                               from = "mar_gp", to = mar_gp_col_nm,
                               info = FALSE)

        value_cols_fmt <- get_value_cols_fmt(denom_counts)
        value_cols <- get_value_cols_colnames(denom_counts)
        value_cols_idx <- match(value_cols, colnames(denom_counts))

        denom_counts <- tibble::as_tibble(denom_counts)

        ## -------** Cleaning, Additional Columns, etc.

        denom_counts[, value_cols] <- denom_counts[, value_cols] * unit_mult

        if (years_as_midyear) {
            colnames(denom_counts)[value_cols_idx] <-
                paste0(colnames(denom_counts)[value_cols_idx], ".5")
        } else {
            ## Nothing: none of the known formats use 'mid-year' format.
        }
        value_cols <- colnames(denom_counts)[value_cols_idx]

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

        denom_counts$age_group <- age_group

        if (clean_col_names) {
            denom_counts <- clean_col_names(denom_counts)
        }

        ## -------** END

        return(tibble::as_tibble(denom_counts))
    }
}

get_used_denominators <- function(output_dir = NULL,
                                  units = c("units", "thousands"),
                                  years_as_midyear = TRUE) {
    lifecycle::deprecate_soft(
                   when = "1.3.0",
                   what = "get_used_denominators()",
                   with = "get_denominators(..., version = \"used_rda\")")
    return(get_denominators(output_dir = output_dir,
                     units = units,
                     years_as_midyear = years_as_midyear,
                     version = "used"))
}

get_used_csv_denominators <- function(output_dir = NULL,
                                      filename = NULL,
                                      marital_group = c("default", "married", "unmarried", "all_women"),
                                      age_group = NULL,
                                      clean_col_names = TRUE,
                                      units = c("units", "thousands"),
                                      years_as_midyear = TRUE,
                                      table_format = c("long", "raw")) {

    lifecycle::deprecate_soft(
                   when = "1.3.0",
                   what = "get_used_csv_denominators()",
                   with = "get_denominators(..., version = \"use_csv\")")

    return(get_used_csv_denominators(output_dir = output_dir,
                                     filename = filename,
                                     marital_group = marital_group,
                                     age_group = age_group,
                                     clean_col_names = clean_col_names,
                                     units = units,
                                     years_as_midyear = years_as_midyear,
                                     table_format = table_format))
}


##' Get the file path to the denominator population .csv file
##'
##' Returns the path to the \file{.csv} file (including the extension)
##' containing the denominator counts that were used in the global run. The path
##' returned is relative to the directory containing \code{output_dir}; i.e., of
##' the form \file{<output-dir>/data/<filename>.csv}.
##'
##' @inheritParams get_csv_res
##' @inheritParams get_output_dir
##' @return Character string containing the file path.
##' @author Mark Wheldon
##' @export
get_used_csv_denominators_filepath <- function(output_dir = NULL) {
    output_dir <-
        output_dir_wrapper(output_dir = output_dir,
                           post_processed = TRUE, countrytrajectories = FALSE,
                           made_results = FALSE,
                           assert_valid = FALSE #<<<<<<<<<<<< IF THIS IS TRUE TESTS WILL PROBABLY FAIL
                           )

    if (is_all_women_run(output_dir = output_dir))
        denom_file_name_meta <- get_combine_runs_args(output_dir = output_dir)$denominator_counts_csv_filename
    else
        denom_file_name_meta <- get_global_post_process_args(output_dir = output_dir)$denominator_counts_csv_filename
    if (length(denom_file_name_meta) && nchar(denom_file_name_meta))
        return(file.path(output_dir, "data", basename(denom_file_name_meta)))
    else stop("File path to denominator .csv file could not be determined from meta data.")
}


##' Read contents of a named .csv file
##'
##' A thin wrapper to \code{\link[readr]{read_csv}} to read the
##' contents of an arbitrary \file{.csv} file in the output directory
##' of an FPEM run.
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
##' @seealso get_input_data, get_csv_res
##' @export
read_named_csv_file <- function(output_dir = NULL,
                                file_name, ...) {

    verbose <- getOption("FPEMglobal.aux.verbose")
    if (!verbose) {
        op <- options(); force(op)
        options(readr.show_col_types = FALSE)
        on.exit(options(op), add = TRUE, after = FALSE)
    }

    output_dir <-
        output_dir_wrapper(output_dir = output_dir,
                           post_processed = FALSE, countrytrajectories = FALSE,
                           made_results = FALSE)
    if (verbose) message("Reading '", file.path(output_dir, file_name), "'.")
    readr::read_csv(file = file.path(output_dir, file_name))
}


