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
get_main_input_file <- function(age_group = c("15-49", "15-19"),
                                folder_path = system.file("extdata", package = "FPEMglobal"),
                                filename = paste0("data_cp_model_all_women_", age_group, ".csv")) {
    age_group <- match.arg(age_group)
    read.csv(file.path(folder_path, filename))
}


##' Get country input data actually used
##'
##' Reads the '.csv' file containing the prevalence data used in the run.
##'
##' @param data_dir_name Name of subdirectory of \code{output_dir}
##'     holding input data files. If \code{processed} is \code{TRUE}
##'     this is set to \dQuote{data} by default, otherwise \dQuote{.}.
##' @param processed Get the input data after processing by
##'     \pkg{\link{FPEMglobal}}?
##' @inheritParams get_FPEMglobal_csv_res
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##' @author Mark Wheldon
##' @seealso \code{\link{get_main_input_file}}.
##' @export
get_used_input_data <-
    function(run_name = NULL, output_dir = NULL, root_dir = ".",
             data_dir_name = NULL, processed = TRUE,
             verbose = FALSE, ...) {

        output_dir <-
            output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                               root_dir = root_dir, verbose = verbose)

        if(processed) {
            data_dir <- file.path(output_dir)
            fname <- "dataCPmodel_input_preprocessed.csv"
        } else {
            fname <- "dataCPmodel_input.csv"
            if(is.null(data_dir_name)) data_dir_name  <- "data"
            data_dir <- file.path(output_dir, data_dir_name)
        }

        readr::read_csv(file.path(data_dir, fname))

    }


##' Get denominator counts actually used
##'
##' Reads the '.csv' file containing the married and unmarried
##' denominator counts used in the run.
##'
##' @param data_dir_name Name of subdirectory of \code{output_dir}
##'     holding input data files.
##' @param filename Name of file with the counts (including
##'     extension).
##' @param age_group Age group of the counts for the output data
##'     frame. If the column names are of the form 'U/MW_\[aabb\]_year'
##'     this is ignored and the age group is taken from the column
##'     names.
##' @inheritParams get_FPEMglobal_csv_res
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##' @author Mark Wheldon
##' @seealso \code{\link{get_main_input_file}}.
##' @export
get_used_denominators <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
                                  data_dir_name = "data", filename,
                                  marital_group = c("married", "unmarried", "all women"),
                                  age_group = "unknown",
                                  verbose = FALSE, ...) {

    marital_group <- match.arg(marital_group, several.ok = TRUE)

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)
    data_dir <- file.path(output_dir, data_dir_name)
    fpath <- file.path(data_dir, filename)

    denom_counts <- data.frame()
    if(any(marital_group %in% c("married", "all women"))) {
        denom_counts <-
            rbind(denom_counts,
                  FPEMglobal:::extractDenominators(fpath, in_union = 1))
        denom_counts$marital_group <- "married"
    }
    if(any(marital_group %in% c("unmarried", "all women"))) {
        denom_counts1 <-
            FPEMglobal:::extractDenominators(fpath, in_union = 0)
        denom_counts1$marital_group <- "unmarried"
        denom_counts <- dplyr::bind_rows(denom_counts, denom_counts1)
    }

    denom_counts <- tibble::as_tibble(denom_counts)

    ## There are two formats in use for the value columns:
    ## 1. E.g., MW_1549_1979 (i.e., marrital group, age group, year)
    ## 2. E.g., 1549_1979 (i.e., age group, year)

    id_cols <- which(colnames(denom_counts) %in% c("ISO.code", "Country"))

    col_fmt1_regexp <- "^(M|U)W_[0-9]{4}_(19|20)[0-9]{2}$"
    col_fmt2_regexp <- "^[^0-9]{4}(19|20)[0-9]{2}$"

    check_fmt1 <- grep(col_fmt1_regexp, colnames(denom_counts), value = TRUE)
    check_fmt2 <- grep(col_fmt2_regexp, colnames(denom_counts), value = TRUE)

    is_fmt1 <- length(check_fmt1)
    is_fmt2 <- length(check_fmt2)

    if (is_fmt1) value_cols <- check_fmt1
    else if (is_fmt2) value_cols <- check_fmt2
    else stop("Column format of '", fpath, " ' cannot be determined.")

    denom_counts <-
        tidyr::gather(denom_counts, tidyselect::all_of(value_cols),
                      key = "key", value = "count") %>%
        dplyr::mutate(year = substr(key, start = nchar(key) - 3, stop = nchar(key)))

    if (is_fmt1) {
        denom_counts <-
            dplyr::mutate(denom_counts,
                          age_group = sapply(strsplit(key, "_", fixed = TRUE), "[[", 2),
                          age_group = paste(substr(age_group, 1, 2), "-", substr(age_group, 3, 4), sep = ""))
    } else denom_counts$age_group <- age_group

    denom_counts <- dplyr::select(denom_counts, -key) %>%
        dplyr::mutate(year = as.numeric(year))
    if (any(is.na(denom_counts$year))) stop("'year' is 'NA' for some rows.")

    if("all women" %in% marital_group) {
        denom_counts_aw <-
            denom_counts %>% group_by(ISO.code, Country, year, age_group) %>%
            dplyr::summarize_at("count", sum, na.rm = TRUE) %>%
            dplyr::mutate(marital_group = "all women")
        denom_counts <- dplyr::bind_rows(denom_counts, denom_counts_aw)
    }
    denom_counts <-
        denom_counts[denom_counts$marital_group %in% marital_group,]

    return(denom_counts)

}




