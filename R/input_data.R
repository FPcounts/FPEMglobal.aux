###-----------------------------------------------------------------------------
### * Work with Input Data

##' Read main input file
##'
##' This is the \file{.csv} file with \emph{raw survey observations}
##' of CP, Unmet need, etc. observations.
##'
##' @param age_group Age group of interest. Only 15-49 and 15-19 are
##'     supported so far.
##' @param folder_path Path to input file.
##' @param filename Filename of input file.
##' @return Data frame with input data.
##' @author Mark Wheldon
##' @seealso \code{\link{get_used_input_data}}.
##' @export
get_FPEMglobal_package_input_file <- function(age_group = c("15-49", "15-19"),
                                folder_path = system.file("extdata", package = "FPEMglobal"),
                                filename = paste0("data_cp_model_all_women_", age_group, ".csv"),
                                verbose = FALSE) {

    op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
    on.exit(options(op), add = TRUE, after = FALSE)

    age_group <- match.arg(age_group)
    readr::read_csv(file.path(folder_path, filename))
}


##' Get country input data actually used
##'
##' Reads the '.csv' file containing the prevalence data used in the run.
##'
##' @param processed Logical; get the input data after processing by
##'     \pkg{\link{FPEMglobal}}, or the raw input data?
##' @inheritParams get_FPEMglobal_csv_res
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##' @author Mark Wheldon
##' @seealso \code{\link{get_main_input_file}}.
##' @export
get_used_input_data <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
             processed = TRUE,
             verbose = FALSE, ...) {

    op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
    on.exit(options(op), add = TRUE, after = FALSE)

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

        readr::read_csv(file.path(data_dir, fname), show_col_types = verbose)
}


##' Get denominator counts actually used
##'
##' Reads the '.csv' file containing the married and unmarried
##' denominator counts used in the run.
##'
##' One or more marital groups can be requested via argument
##' \code{marital_group}. If more than one, a column
##' \dQuote{\code{marital_group}} will be present in the output to
##' identify records accordingly (regardless the value of
##' \code{add_marital_group}). Value \code{"default"} will read the
##' the meta data (see \code{\link{get_global_mcmc_args}}) and ensure
##' the marital group matching that of the output is included. If
##' \code{"all_women"} is included, values will be constructed by
##' summing the married and unmarried denominators.
##'
##' If \code{long_format} is \code{TRUE}, the counts are in a single
##' column called \dQuote{\code{value}}, with the necessary
##' identifying columns added. Otherwise the result is in the same
##' format as the original counts files, which have one year per row.
##'
##' Note that if \code{long_format} is \code{FALSE}, the column names
##' of the year columns will be prefixed with an
##' \dQuote{\code{X}}. This is a result of the existing code in
##' \pkg{FPEMglobal} and cannot be undone with the
##' \code{clean_col_names} argument.
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
##' @param sort Logical. Sort by stat, name, year, percentile?
##' @inheritParams get_FPEMglobal_csv_res
##' @inheritParams get_output_dir
##' @return A \code{\link[tibble]{tibble}} with the requested results
##'     in \dQuote{long} format; see \dQuote{Details}.
##' @author Mark Wheldon
##' @seealso \code{\link{get_main_input_file}}.
##'
##' @examples
##'
##' @export
get_used_denominators <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
                                  filename = NULL,
                                  marital_group = c("default", "married", "unmarried", "all_women"),
                                  add_marital_group = TRUE,
                                  add_age_group = TRUE,
                                  age_group = NULL,
                                  clean_col_names = TRUE,
                                  long_format = TRUE,
                                  verbose = FALSE, ...) {

    ## -------* Set-up

    mcmc_args <- get_global_mcmc_args(output_dir = output_dir)
    pp_args <- get_global_post_process_args(run_name = run_name, output_dir = output_dir,
                                         root_dir = root_dir)

    if (missing(marital_group)) marital_group <- "default"
    else marital_group <- match.arg(marital_group, several.ok = TRUE)
    if ("default" %in% marital_group) {
        marital_group[marital_group == "default"] <- mcmc_args$marital_group
        marital_group <- unique(marital_group)
    }
    if (length(marital_group) > 1) add_marital_group <- TRUE

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)
    data_dir_name <- "data"
    data_dir <- file.path(output_dir, data_dir_name)

    if (is.null(filename)) {
        if (length(pp_args$denominator_counts_csv_filename) && nchar(pp_args$denominator_counts_csv_filename)) {
            filename <- basename(pp_args$denominator_counts_csv_filename)

            ## Since filename auto-determined, read the age group as well.
            age_group_from_args <- mcmc_args$age_group
            if (!is.null(age_group) && !identical(as.character(age_group), as.character(age_group_from_args)))
                warning("'age_group' = '", age_group, "', but output directory is a run for age group '",
                        age_group_from_args, "'. Argument 'age_group' reset to the latter.\n\nIf you want to load an age group different from the age group of the output directory, you must specify 'filename'.")
            age_group <- age_group_from_args
        }
        else stop("'filename' could not be determined from meta data; must be supplied.")
    }

    fpath <- file.path(data_dir, filename)

    ## If 'age_group' still 'NULL', use mcmc args
    if (is.null(age_group)) age_group <- mcmc_args$age_group

    ## -------* Load .csv

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
        value_cols <- colnames(denom_counts_m)[!colnames(denom_counts_m) %in% c("ISO.code", "Country")]
        denom_counts_a <- denom_counts_m
        denom_counts_a[, value_cols] <-
            denom_counts_m[, value_cols] +
            denom_counts_u[match(denom_counts_m$ISO.code, denom_counts_u$ISO.code), value_cols]
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
    if ("all_women" %in% marital_group) {
        denom_counts <- dplyr::bind_rows(denom_counts, denom_counts_a)
        if (add_marital_group) denom_counts$marital_group <- "all_women"
    }

    denom_counts <- tibble::as_tibble(denom_counts)

    ## -------** Determine column name format

    ## There are two formats in use for the value columns:
    ## 1. E.g., MW_1549_1979 (i.e., marrital group, age group, year)
    ## 2. E.g., 1549_1979 (i.e., age group, year)

    col_fmt1_regexp <- "^(M|U)W_[0-9]{4}_(19|20)[0-9]{2}$"
    col_fmt2_regexp <- "^X?[^0-9]{4}_(19|20)[0-9]{2}$"
    col_fmt3_regexp <- "^X?(19|20)[0-9]{2}$"

    check_fmt1 <- grep(col_fmt1_regexp, colnames(denom_counts), value = TRUE)
    check_fmt2 <- grep(col_fmt2_regexp, colnames(denom_counts), value = TRUE)
    check_fmt3 <- grep(col_fmt3_regexp, colnames(denom_counts), value = TRUE)

    is_fmt1 <- length(check_fmt1)
    is_fmt2 <- length(check_fmt2)
    is_fmt3 <- length(check_fmt3)

    if (is_fmt1) value_cols <- check_fmt1
    else if (is_fmt2) value_cols <- check_fmt2
    else if (is_fmt3) value_cols <- check_fmt3
    else stop("Column format of '", fpath, "' cannot be determined.")

    if (is_fmt1 || is_fmt2) {
        if (is_fmt1) {
            age_group_from_cols <- sapply(strsplit(value_cols, "_", fixed = TRUE), "[[", 2)
        } else if (is_fmt2) {
            age_group_from_cols <- sapply(strsplit(value_cols, "_", fixed = TRUE), "[[", 1)
        }
        age_group_from_cols <- paste(substr(age_group_from_cols, 1, 2),
                                     substr(age_group_from_cols, 3,4), sep = "-")
        if (!identical(as.character(rep(age_group_from_cols[1], length(age_group_from_cols))),
                       as.character(age_group_from_cols)))
            stop("Age groups implied in column names of '", fpath, "' are not all the same.")
        if (!identical(as.character(age_group_from_cols), as.character(age_group)))
            stop("Age group implied in column names is '", as.character(age_group_from_cols),
                 "' but 'age_group' argument is '", age_group,
                 "'. Note: if you didn't specify 'age_group' it was read from the meta data.")
    }

    ## -------* Cleaning, Additional Columns, etc.

    if (long_format) {
        denom_counts <-
            tidyr::gather(denom_counts, tidyselect::all_of(value_cols),
                          key = "key", value = "count") %>%
            dplyr::mutate(year = substr(key, start = nchar(key) - 3, stop = nchar(key)))

        denom_counts <- dplyr::select(denom_counts, -key) %>%
            dplyr::mutate(year = as.numeric(year))
        if (any(is.na(denom_counts$year))) stop("'year' is 'NA' for some rows.")
    }

    if (add_age_group && !("age_group" %in% colnames(denom_counts)) && identical(length(age_group), 1L))
        denom_counts$age_group <- age_group

    if (add_marital_group && !("marital_group" %in% colnames(denom_counts)) && identical(length(marital_group), 1L))
        denom_counts$marital_group <- marital_group

    if (clean_col_names) denom_counts <- clean_col_names(denom_counts)

    ## -------* END

   return(denom_counts)
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
##' @inheritParams get_FPEMglobal_csv_res
##' @inheritParams get_output_dir
##' @param file_name Name of file to read, \emph{including} any
##'     subdirectory and file extension; see \dQuote{Details}.
##' @return A \code{\link[tibble]{tibble}} with the file contents.
##' @author Mark Wheldon
##' @export
read_named_csv_file <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
                                file_name, verbose = FALSE, ...) {

    op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
    on.exit(options(op), add = TRUE, after = FALSE)

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)
    readr::read_csv(file = file.path(output_dir, file_name), show_col_types = verbose)
}
