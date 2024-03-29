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
get_model_traj <- function(run_name = NULL, output_dir = NULL, root_dir = NULL, name_dims = TRUE) {

    verbose <- getOption("FPEMglobal.aux.verbose")

    res_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir)

    tmp_env <- new.env()
    if (verbose) message("Reading '", file.path(res_dir, "mcmc.array.rda"), "'.")
    out <- get(load(file = file.path(res_dir, "mcmc.array.rda"), envir = tmp_env),
               envir = tmp_env)

    if (name_dims) names(dimnames(out)) <- c("iteration", "chain", "parameter")

    return(out)
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
##' \code{\link{load}} is called is such a way that ensures nothing is
##' added to the global environment; the only copy of the loaded
##' object is that returned by the function.
##'
##' Country trajectories are 3D arrays:
##' \preformatted{str(...)
##' num [1:61, 1:3, 1:13344] 0.0622 0.0725 0.0879 0.0633 0.1078 ...
##' - attr(*, "dimnames")=List of 3
##'  ..$ : chr [1:61] "1970.5" "1971.5" "1972.5" "1973.5" ...
##'  ..$ : chr [1:3] "Traditional" "Modern" "Unmet"
##'  ..$ : NULL}
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
##' @return The loaded country trajectory object.
##' @author Mark Wheldon
##' @export
get_country_traj_muw <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                 iso_code) {

    verbose <- getOption("FPEMglobal.aux.verbose")

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir,
                           post_processed = TRUE, countrytrajectories = TRUE)

    if (is_all_women_run(output_dir = output_dir))
        stop("'", output_dir, "' is an all women output directory. Use 'get_country_traj_aw' instead. NOTE that all women trajectories are in a different format from married and unmarried trajectories.")

    stopifnot(identical(length(iso_code), 1L))
    iso_code <- as.character(iso_code)

    traj_index <-
        get_country_index(run_name = run_name, output_dir = output_dir,
                          root_dir = root_dir)
    if (!(iso_code %in% traj_index$iso.c)) stop("'iso_code' not found in trajectory index (see '?get_country_index').")
    traj_fname <-
        traj_index$filename[traj_index$iso.c == iso_code]

    traj_full_path <- file.path(output_dir, "countrytrajectories", traj_fname)
    tmp_env <- new.env()
    if (verbose) message("Reading '", traj_full_path, "'.")
    obj <- get(load(file = traj_full_path, envir = tmp_env), envir = tmp_env)

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
##' \code{\link{load}} is called is such a way that ensures nothing is
##' added to the global environment; the only copy of the loaded
##' object is that returned by the function.
##'
##' Country trajectories are 3D arrays:
##' \preformatted{str(...)
##' num [1:61, 1:6, 1:15000] 37.2 37.3 39.2 43.6 51.9 ...
##' - attr(*, "dimnames")=List of 3
##'  ..$ : chr [1:61] "1970.5" "1971.5" "1972.5" "1973.5" ...
##'  ..$ : chr [1:6] "Total" "Modern" "Traditional" "Unmet" ...
##'  ..$ : NULL}
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
                                iso_code) {
    verbose <- getOption("FPEMglobal.aux.verbose")
    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir,
                           post_processed = TRUE)

    if (!is_all_women_run(output_dir = output_dir))
        stop("'", output_dir, "' is not an all women output directory. Use 'get_country_traj_muw' instead. NOTE that married/unmarried trajectories are in a different format from all women trajectories.")

    stopifnot(identical(length(iso_code), 1L))

    traj_fname <- paste0("aw_ISO_", iso_code, "_counts.rda")
    traj_full_path <- file.path(output_dir, "countrytrajectories", traj_fname)

    tmp_env <- new.env()
    if (verbose) message("Reading '", traj_full_path, "'.")
    obj <- get(load(file = traj_full_path, envir = tmp_env), envir = tmp_env)

    return(obj)
}




## get_country_traj <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
##                              iso,
##                              stat = c("prop", "count", "ratio"),
##                              indicators) {
## verbose <- getOption("FPEMglobal.aux.verbose")
##     output_dir <-
##         output_dir_wrapper(run_name = run_name, output_dir = output_dir,
##                            root_dir = root_dir,
##                            post_processed = TRUE)

##     stopifnot(identical(length(iso), 1L))

##     tmp_env <- new.env()

##     ## -------* Load Trajectores

##     ## ! Early return if 'stat' and 'indicators' satisfied

##     if (is_all_women_run(output_dir = output_dir)) {

##         ## ALL WOMEN

##         traj_fname <- paste0("aw_ISO_", iso_code, "_counts.rda")
##         traj_full_path <- file.path(output_dir, "countrytrajectories", traj_fname)

##         if (verbose) message("Reading '", traj_full_path, "'.")
##         traj <- get(load(file = traj_full_path, envir = tmp_env), envir = tmp_env)

##         if (identical(stat, "count") &&
##             all(indicators %in% dimnames(traj)[[2]])) {
##             traj <- traj[, indicators, , drop = FALSE]
##             return(traj)
##         }

##     } else {

##         ## MARR / UN-MARR

##         iso_code <- as.character(iso_code)
##         traj_index <-
##             get_country_index(run_name = run_name, output_dir = output_dir,
##                               root_dir = root_dir)
##         if (!(iso_code %in% traj_index$iso.c)) stop("'iso_code' not found in trajectory index (see '?get_country_index').")
##         traj_fname <-
##             traj_index$filename[traj_index$iso.c == iso_code]
##         traj_full_path <- file.path(output_dir, "countrytrajectories", traj_fname)

##         if (verbose) message("Reading '", traj_full_path, "'.")
##         traj <- get(load(file = traj_full_path, envir = tmp_env), envir = tmp_env)

##         if (identical(stat, "prop") &&
##             all(indicators %in% dimnames(traj)[[2]])) {
##             traj <- traj[, indicators, , drop = FALSE]
##             return(traj)
##         }
##     }

##     ## -------* Conversions If Needed

##     if (is_all_women_run(output_dir = output_dir)) {

##         ## ALL WOMEN

##         if (identical(stat, "prop")) {
##             traj <- convert_country_traj_to_props(traj,
##                                                   denominator_counts_df = get_used_denominators(output_dir = output_dir,
##                                                                                                marital_group = "all women"),
##                                                   iso = iso, safe = FALSE)

################# HERE HERE HERE ##########################

            #### COME BACK TO THIS *AFTER* FINISHING THE .age STUFF. NOT ESSENTIAL. IF YOU USE THIS IN THE CALIBARTION FUNCTIONS, THE TRAJECTORY FILES WILL HAVE TO BE LOADED MULTIPLE TIMES

##################################################




##' Load and return aggregate trajectories
##'
##' @description
##' This function \code{\link{load}}s the MCMC sample from the
##' posterior sample for married, unmarried or all women for a single
##' aggregate and returns it as an R object. Trajectories are loaded
##' from \file{.rda} files found in subdirectories of the the
##' subdirectory \file{aggregatetrajectories} of the results directory
##' (see below). The directory and file names for the given aggregate
##' are determined from \code{marital_group} and \code{agg_family_name}.
##'
##' \emph{Note:} Aggregate trajectories are only
##' stored in an all women output directory; calling this function on
##' a married or unmarried output directory will result in an error.
##'
##' @details
##' Aggregate trajectories are 3D arrays, for example:
##' \preformatted{str(...)
##' num [1:61, 1:3, 1:13344] 0.0622 0.0725 0.0879 0.0633 0.1078 ...
##' - attr(*, "dimnames")=List of 3
##'  ..$ : chr [1:61] "1970.5" "1971.5" "1972.5" "1973.5" ...
##'  ..$ : chr [1:3] "Traditional" "Modern" "Unmet"
##'  ..$ : NULL}
##'
##' \subsection{Aggregate names}{
##' \code{agg_name} must be a valid name. Aggregates are stored in named lists with one element per aggregate. An error will be thrown if the aggregate is not valid.}
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
##'     trajectories. This must match the element name in the aggregate list.
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
                         marital_group = c("married", "unmarried", "all women")) {

    verbose <- getOption("FPEMglobal.aux.verbose")

    if (missing(agg_name)) stop("Must supply 'agg_name'.")

    marital_group <- match.arg(marital_group)
    ## Marital group must match the file naming convention:
    marital_group[marital_group == "married"] <- "mwra"
    marital_group[marital_group == "unmarried"] <- "uwra"
    marital_group[marital_group == "all women"] <- "awra"

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir,
                           post_processed = TRUE, countrytrajectories = TRUE)

    if (!is_all_women_run(output_dir = output_dir))
        stop("Aggregate trajectories are stored in the 'all women' output directory. Call this function on an 'all women' output directory.")

    traj_fname <- paste0(marital_group, "_CP_counts_agg_li.RData")
    traj_full_path <- file.path(output_dir, "aggregatetrajectories", agg_family_name, traj_fname)
    if (!file.exists(traj_full_path)) stop("File '", traj_full_path, "' does not exist.")

    tmp_env <- new.env()
    if (verbose) message("Reading '", traj_full_path, "'.")
    agg_li <- get(load(file = traj_full_path, envir = tmp_env), envir = tmp_env)

    if (!agg_name %in% names(agg_li))
        stop("'agg_name' is not a valid aggregate name. Available aggregates for family '",
             agg_family_name,
             "' are: '", toString(names(agg_li)), "'.")

    return(agg_li[[agg_name]][["CP"]])
}



