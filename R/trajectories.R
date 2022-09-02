###-----------------------------------------------------------------------------
### * Work with Trajectories


##' Get model parameter trajectory array
##'
##' This function \code{\link{load}}s and returns MCMC trajectories
##' stored in the file \file{mcmc.array.rda} in the output directory
##' \code{output_dir}. This contains MCMC trajectories for the model
##' parameters. If you want country trajectories, use
##' \code{\link{get_country_traj_muw}} or
##' \code{\link{get_country_traj_aw}}.
##'
##' The dimensions are not named by \pkg{FPEMglobal}. If
##' \code{name_dims} is \code{TRUE} they are given the names
##' \dQuote{iteration}, \dQuote{chain}, \dQuote{parameter}.
##'
##' @family trajectory functions
##'
##' @inheritParams get_csv_res
##' @inheritParams get_output_dir
##' @param name_dims Logical; should the dimensions be given informative names? See \dQuote{Details}.
##' @return The loaded object.
##'
##' @author Mark Wheldon
##' @export
get_model_traj <- function(run_name = NULL, output_dir = NULL, root_dir = NULL, name_dims = TRUE,
                           verbose = FALSE) {

    res_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)

    tmp_env <- new.env()
    if (verbose) message("Reading '", file.path(res_dir, "mcmc.array.rda"), "'.")
    out <- get(load(file = file.path(res_dir, "mcmc.array.rda"), envir = tmp_env),
               envir = tmp_env)

    if (name_dims) names(dimnames(out)) <- c("iteration", "chain", "parameter")

    return(out)
}


##' Lists unique parameter names in mcmc array
##'
##' Parameters in the mcmc.array object are named with country and
##' region indices, e.g., \code{theta.ci[98,7]}. This function
##' strips the indices in the square brackets, and the square brackets
##' themselves, and returns the unique strings in what's left. This is
##' helpful for listing the \emph{names} of the parameters.
##'
##' @param mcmc_array An mcmc array from a completed model run, e.g.,
##'     of the kind retrieved by \code{\link{get_model_traj}}.
##' @return Character vector of unique names.
##' @author Mark Wheldon
##' @export
get_model_param_names <- function(mcmc_array) {
    unique(gsub("\\[[0-9,]+\\]", "", dimnames(mcmc_array)[[3]]))
    }


##' Load and return country-level trajectories for married and unmarried women
##'
##' This function \code{\link{load}}s the MCMC sample from the
##' posterior sample for married or unmarried for a single country and returns it as an R
##' object. Trajectories are loaded from \file{.rda} files found in
##' the subdirectory \file{countrytrajectories} of the results
##' directory (see below). The filename for the given country is
##' determined by reference to an index which must be in the file
##' \code{file.path(output_dir, "iso.Ptp3s.key.csv")}.
##'
##' Country trajectories are 3D matrices:
##' \preformatted{str(...)
##' num [1:61, 1:3, 1:13344] 0.0622 0.0725 0.0879 0.0633 0.1078 ...
##' - attr(*, "dimnames")=List of 3
##'  ..$ : chr [1:61] "1970.5" "1971.5" "1972.5" "1973.5" ...
##'  ..$ : chr [1:3] "Traditional" "Modern" "Unmet"
##'  ..$ : NULL}
##'
##' If \code{round_down_years} is \code{TRUE}, the year labels are rounded
##' down to the nearest integer. E.g., \code{"1970.5"} becomes
##' \code{"1970"}.This affects the \code{dimnames} of the first
##' dimension of the result matrix.
##'
##' If \code{clean_indicator_names} is \code{TRUE}, the names of the
##' family planning indicators are made all lower case. This affects
##' the \code{dimnames} of the second dimension of the result matrix.
##'
##' \subsection{Married/unmarried vs all women trajectories}{
##' The country trajectories for all women are stored differently. Do
##' not use this function for all women country trajectories. Use
##' \code{\link{get_country_traj_aw}} instead.
##'
##' The country trajectories for all women are \emph{counts}. Those
##' for married and unmarried are \emph{proportions}.}
##'
##' @family trajectory functions
##'
##' @section Specifying results directory:
##' See the section in \code{\link{get_csv_res}}.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##' @param iso_code Numeric ISO code of country to get trajectories
##'     for.
##' @param clean_indicator_names Logical; should indicator names
##'     within the result's dimnames be \dQuote{cleaned} up? See
##'     \dQuote{Details}.
##' @param round_down_years Should year names within the result's dimnames
##'     be rounded down to integer values (e.g., 1970.5 becomes 1970)?
##'     See \dQuote{Details}.
##' @return The loaded country trajectory object.
##' @author Mark Wheldon
##' @export
get_country_traj_muw <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                 iso_code,
                                 clean_indicator_names = TRUE,
                                 round_down_years = FALSE,
                                 verbose = FALSE) {

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = TRUE, countrytrajectories = TRUE)

    if (is_all_women_run(output_dir = output_dir))
        stop("'", output_dir, "' is an all women output directory. Use 'get_country_traj_aw' instead. NOTE that all women trajectories are in a different format from married and unmarried trajectories.")

    stopifnot(identical(length(iso_code), 1L))
    iso_code <- as.character(iso_code)

    traj_index <-
        get_country_index(run_name = run_name, output_dir = output_dir,
                          root_dir = root_dir, verbose = verbose)
    if (!(iso_code %in% traj_index$iso.c)) stop("'iso_code' not found in trajectory index (see '?get_country_index').")
    traj_fname <-
        traj_index$filename[traj_index$iso.c == iso_code]

    traj_full_path <- file.path(output_dir, "countrytrajectories", traj_fname)
    tmp_env <- new.env()
    if (verbose) message("Reading '", traj_full_path, "'.")
    obj <- get(load(file = traj_full_path, envir = tmp_env), envir = tmp_env)

    if (round_down_years) dimnames(obj)[[1]] <- round_down_years(dimnames(obj)[[1]])
    if (clean_indicator_names) dimnames(obj)[[2]] <- clean_indic_names(dimnames(obj)[[2]])

    return(obj)
}


##' Load and return country-level trajectories for all women
##'
##' This function \code{\link{load}}s the MCMC sample from the
##' posterior sample for all women for a single country and returns it
##' as an R object. Trajectories are loaded from \file{.rda} files
##' found in the subdirectory \file{countrytrajectories} of the
##' results directory (see below). The filename for the given country
##' is \code{paste0("aw_ISO_", iso_code, "_counts.rda")}.
##'
##' Country trajectories are 3D matrices:
##' \preformatted{str(...)
##' num [1:61, 1:6, 1:15000] 37.2 37.3 39.2 43.6 51.9 ...
##' - attr(*, "dimnames")=List of 3
##'  ..$ : chr [1:61] "1970.5" "1971.5" "1972.5" "1973.5" ...
##'  ..$ : chr [1:6] "Total" "Modern" "Traditional" "Unmet" ...
##'  ..$ : NULL}
##'
##' See \dQuote{Details} section under
##' \code{\link{get_country_traj_aw}} for more information about
##' arguments \code{clean_indicator_names} and
##' \code{round_down_years}.
##'
##' \subsection{Married/unmarried vs all women trajectories}{
##' The country trajectories for married and unmarried women are
##' stored differently. Do not use this function for married and
##' unmarried women country trajectories. Use
##' \code{\link{get_country_traj_muw}} instead.
##'
##' The country trajectories for all women are \emph{counts}. Those
##' for married and unmarried are \emph{proportions}.}
##'
##' @family trajectory functions
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##' @inheritParams get_country_traj_muw
##' @return The loaded country trajectory object.
##' @author Mark Wheldon
##' @export
get_country_traj_aw <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                iso_code,
                                 clean_indicator_names = TRUE,
                                 round_down_years = FALSE,
                                verbose = FALSE) {
    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = TRUE)

    if (!is_all_women_run(output_dir = output_dir))
        stop("'", output_dir, "' is not an all women output directory. Use 'get_country_traj_muw' instead. NOTE that married/unmarried trajectories are in a different format from all women trajectories.")

    stopifnot(identical(length(iso_code), 1L))

    traj_fname <- paste0("aw_ISO_", iso_code, "_counts.rda")
    traj_full_path <- file.path(output_dir, "countrytrajectories", traj_fname)

    tmp_env <- new.env()
    if (verbose) message("Reading '", traj_full_path, "'.")
    obj <- get(load(file = traj_full_path, envir = tmp_env), envir = tmp_env)

    if (round_down_years) dimnames(obj)[[1]] <- round_down_years(dimnames(obj)[[1]])
    if (clean_indicator_names) dimnames(obj)[[2]] <- clean_indic_names(dimnames(obj)[[2]])

    return(obj)
}


##' Load and return aggregate trajectories
##'
##' @description
##' This function \code{\link{load}}s the MCMC sample from the
##' posterior sample for married, unmarried or all women for a single
##' aggregate and returns it as an R object. Trajectories are loaded
##' from \file{.rda} files found in subdirectories of the the
##' subdirectory \file{aggregatetrajectories} of the results directory
##' (see below). The directory and file names for the given aggregate
##' are determined from \code{agg_family_name} and
##' \code{agg_name}.
##'
##' \emph{Note:} Aggregate trajectories are only
##' stored in an all women output directory; calling this function on
##' a married or unmarried output directory will result in an error.
##'
##' @details
##' Aggregate trajectories are 3D matrices:
##' \preformatted{str(...)
##' num [1:61, 1:3, 1:13344] 0.0622 0.0725 0.0879 0.0633 0.1078 ...
##' - attr(*, "dimnames")=List of 3
##'  ..$ : chr [1:61] "1970.5" "1971.5" "1972.5" "1973.5" ...
##'  ..$ : chr [1:3] "Traditional" "Modern" "Unmet"
##'  ..$ : NULL}
##'
##' See \dQuote{Details} section under
##' \code{\link{get_country_traj_aw}} for more information about
##' arguments \code{clean_indicator_names} and
##' \code{round_down_years}.
##'
##' \subsection{Aggregate names}{
##' It is recommended to check the actual output files to get the
##' aggregate family names and aggregate names correct. The aggregate
##' names in particular may not be guessable because many will have
##' been abbreviated to create valid, short(ish) filenames.}
##'
##' \subsection{Married/unmarried vs all women trajectories}{
##' The aggregate trajectories for all marital groups are saved only
##' when all women results are created by running
##' \code{link[FPEMglobal]{combine_runs}}. They are saved in only in
##' the all women output directory, hence this function must be called
##' on such a directory.
##'
##' The trajectories are on the \emph{count}
##' scale for all marital groups. This is \emph{not} the same as for
##' country trajectories.}
##'
##' @param agg_family_name Name of the aggregate family for which to
##'     retrieve trajectories. This must match the name of the
##'     subdirectory of \code{file.path(output_dir,
##'     "aggregatetrajectories")}.
##' @param agg_name Name of the aggregate for which to retrieve
##'     trajectories. This must match the filename \emph{exactly} (see
##'     \dQuote{Description}).
##' @inheritParams get_country_traj_muw
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##' @return The loaded aggregate trajectory object.
##'
##' @family trajectory functions
##'
##' @author Mark Wheldon
##' @export
get_agg_traj <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                         agg_name, agg_family_name = "UNPDaggregates",
                         marital_group = c("married", "unmarried", "all_women"),
                                 clean_indicator_names = TRUE,
                                 round_down_years = FALSE,
                         verbose = FALSE) {

    if (missing(agg_name)) stop("Must supply 'agg_name'.")

    marital_group <- match.arg(marital_group)
    ## Marital group must match the file naming convention:
    marital_group[marital_group == "married"] <- "mwra"
    marital_group[marital_group == "unmarried"] <- "uwra"
    marital_group[marital_group == "all_women"] <- "awra"

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = TRUE, countrytrajectories = TRUE)

    if (!is_all_women_run(output_dir = output_dir))
        stop("Aggregate trajectories are stored in the 'all_women' output directory. Call this function on an 'all_women' output directory.")

    traj_fname <- paste0(marital_group, "_CP_counts_agg_li_", agg_name, ".RData")
    traj_full_path <- file.path(output_dir, "aggregatetrajectories", agg_family_name, traj_fname)
    if (!file.exists(traj_full_path)) stop("File '", traj_full_path, "' does not exist.")

    tmp_env <- new.env()
    if (verbose) message("Reading '", traj_full_path, "'.")
    obj <- get(load(file = traj_full_path, envir = tmp_env), envir = tmp_env)[["CP"]]

    if (round_down_years) dimnames(obj)[[1]] <- round_down_years(dimnames(obj)[[1]])
    if (clean_indicator_names) dimnames(obj)[[2]] <- clean_indic_names(dimnames(obj)[[2]])

    return(obj)
}



