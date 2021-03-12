###-----------------------------------------------------------------------------
### * Work with Trajectories


##' Get model parameter trajectory array
##'
##' This function \code{\link{load}}s \file{mcmc.array.rda} from the output directory. This
##' contains MCMC trajectories for the model parameters. If you want
##' country trajectories, use \code{\link{get_country_traj}}.
##'
##' The dimensions are not named by the esimtation software. If
##' \code{name_dims} is \code{TRUE} they are given the names
##' \dQuote{iteration}, \dQuote{chain}, \dQuote{parameter}.
##'
##' @family trajectory functions
##'
##' @section Specifying results directory:
##' See the section in \code{\link{get_cuaw_csv_res}}.
##'
##' @param output_dir Output directory, e.g., like that returned by
##'     \code{\link{get_output_dir}}. Note that \code{root_dir} is
##'     ignored if \code{output_dir} is supplied.
##' @param verbose Logical; report the path, filename, and object name
##'     in a message?
##' @param name_dims Logical; should the dimensions be given informative names? See \dQuote{Details}.
##' @return The loaded object.
##' @inheritParams get_output_dir
##' @author Mark Wheldon
##' @export
get_model_traj <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
                           verbose = FALSE, name_dims = FALSE) {

    res_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)

    ob_name <- load(file = file.path(res_dir, "mcmc.array.rda"))

    if(verbose) {
        msg_loaded(file.path(res_dir, "mcmc.array.rda"), ob_name)
    }

    out <- get(ob_name[1])

    if(name_dims) {
        names(dimnames(out)) <- c("iteration", "chain", "parameter")
        }

    return(out)
}


##' Lists unique parameter names in mcmc array
##'
##' Parameters in the mcmc.array object are named with country and
##' region indices, e.g., \dQuote{theta.ci[98,7]}. This function
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
##' @section Married/unmarried vs all women trajectories:
##'
##' The country trajectories for all women are stored differently. Do
##' not use this function for all women country trajectories. Use
##' \code{\link{get_country_traj_aw}} instead.
##'
##' The country trajectories for all women are \emph{counts}. Those
##' for married and unmarried are \emph{proportions}.
##'
##' @family trajectory functions
##'
##' @section Specifying results directory:
##' See the section in \code{\link{get_cuaw_csv_res}}.
##'
##' @param country_name Name of country to get trajectories for; only one of
##'     \code{iso_code} and \code{country_name} should be given.
##' @param iso_code Alternative way of selecting country to get trajectories
##'     for; only one of \code{iso_code} and \code{country_name} should be given.
##' @param verbose Logical; report the path, filename, and object name in a
##'     message?
##' @inheritParams get_output_dir
##' @inheritParams get_model_traj
##' @return The loaded country trajectory object.
##' @author Mark Wheldon
##' @export
get_country_traj_muw <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
                                 country_name, iso_code, verbose = FALSE) {

    if((missing(country_name) && missing(iso_code)) || (
        !missing(country_name) && !missing(iso_code))
       ) stop("Supply one, and only one, of 'country_name', 'iso_code'.")

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)

    traj_index <-
        get_country_index(run_name = run_name, output_dir = output_dir,
                          root_dir = root_dir, verbose = FALSE)

    if(!(iso_code %in% traj_index$iso.c)) stop("'iso_code' not valid.")
    if(missing(country_name)) {
        traj_fname <-
            traj_index$filename[traj_index$iso.c == as.character(iso_code)]
    } else {
        traj_fname <-
            traj_index$filename[traj_index$name.c == as.character(country_name)]
    }

    traj_full_path <- file.path(output_dir, "countrytrajectories", traj_fname)
    ob_name <- load(file = traj_full_path, verbose = verbose)

    if(verbose) msg_loaded(traj_full_path, ob_name)

    return(get(ob_name[1]))
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
##' @section Married/unmarried vs all women trajectories:
##'
##' The country trajectories for married and unmarried women are
##' stored differently. Do not use this function for married and
##' unmarried women country trajectories. Use
##' \code{\link{get_country_traj_muw}} instead.
##'
##' The country trajectories for all women are \emph{counts}. Those
##' for married and unmarried are \emph{proportions}.
##'
##' @family trajectory functions
##'
##' @inheritParams get_output_dir
##' @inheritParams get_model_traj
##' @inheritParams get_country_traj_muw
##' @return The loaded country trajectory object.
##' @author Mark Wheldon
##' @export
get_country_traj_aw <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
                                 iso_code, verbose = FALSE) {
    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)

    traj_fname <- paste0("aw_ISO_", iso_code, "_counts.rda")
    traj_full_path <- file.path(output_dir, "countrytrajectories", traj_fname)

    ob_name <- load(file = traj_full_path, verbose = verbose)

    if(verbose) msg_loaded(traj_full_path, ob_name)

    return(get(ob_name[1]))
}


##' Load and return quantiles of country-specific model parameters.
##'
##' This function \code{\link{load}}s and returns the object in the
##' file \file{par.ciq.rda} found in
##' \code{file.path{output_dir}}. These are the quantiles of the
##' country-specific model parameters, such as the asymptotes, rate
##' paratmeters, timing parameters, etc.
##'
##' The specific parameters loaded are, \dQuote{omega.c}, \dQuote{T.c},
##' \dQuote{pmax.c}, \dQuote{Romega.c}, \dQuote{RT.c}, \dQuote{Rmax.c},
##' \dQuote{unmet.intercept.c}.
##'
##' @section Specifying results directory:
##' See the section in \code{\link{get_cuaw_csv_res}}.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_country_traj
##' @return The loaded object.
##' @inheritParams get_output_dir
##' @inheritParams get_model_traj
##' @author Mark Wheldon
##' @export
get_countries_model_params_q <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
                            verbose = FALSE) {
    res_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)
    par_ciq_path <- file.path(res_dir, "par.ciq.rda")
    ob_name <- load(file = par_ciq_path)
    if(verbose) msg_loaded(par_ciq_path, ob_name)

    get(ob_name[1])
    }



##' Load and return aggregate trajectories
##'
##' This function \code{\link{load}}s the MCMC sample from the
##' posterior sample for married or unmarried for a single aggregate and returns it as an R
##' object. Trajectories are loaded from \file{.rda} files found in subdirectories of the
##' the subdirectory \file{aggregatetrajectories} of the results
##' directory (see below). The directory and file names for the given aggregate are
##' determined from \code{agg_family_name} and \code{agg_name}.
##'
##' Aggregate trajectories are 3D matrices:
##' \preformatted{str(...)
##' num [1:61, 1:3, 1:13344] 0.0622 0.0725 0.0879 0.0633 0.1078 ...
##' - attr(*, "dimnames")=List of 3
##'  ..$ : chr [1:61] "1970.5" "1971.5" "1972.5" "1973.5" ...
##'  ..$ : chr [1:3] "Traditional" "Modern" "Unmet"
##'  ..$ : NULL}
##'
##' @section Aggregate names:
##' It is recommended to check the actual
##'     output files to get the aggregate family names and aggregate
##'     names correct. The aggregate names in particular may not be
##'     guessable because many will have been abbreviated to create
##'     valid, short(ish) filenames.
##'
##' @section Married/unmarried vs all women trajectories:
##' The aggregate trajectories for all marital groups are saved only
##'     when all women results are created, e.g., by
##'     \code{link{combine_runs}}. The trajectories are on the
##'     \emph{count} scale for all marital groups. This is \emph{not}
##'     the same as for country trajectories.
##'
##' @section Specifying results directory:
##' See the section in \code{\link{get_cuaw_csv_res}}.
##'
##' @family trajectory functions
##' @param run_name
##' @param output_dir
##' @param root_dir
##' @param agg_family_name Name of the aggregate family for which to
##'     retrieve trajectories. This must match the name of the
##'     subdirectory of \code{file.path(output_dir,
##'     "aggregatetrajectories")}.
##' @param agg_name Name of the aggregate for which to retrieve
##'     trajectories. This must match the filename \emph{exactly} (see
##'     \dQuote{Description}).
##' @param verbose Logical; report the path, filename, and object name
##'     in a message?
##' @inheritParams get_output_dir
##' @inheritParams get_model_traj
##' @return The loaded aggregate trajectory object.
##' @author Mark Wheldon
##' @export
get_agg_traj <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
                         agg_family_name, agg_name, marital_group = c("married", "unmarried", "all women"),
                         verbose = FALSE) {

    if(missing(agg_name)) stop("Must supply 'agg_name'.")
    if(missing(agg_family_name)) stop("Must supply 'agg_family_name'.")

    marital_group <- match.arg(marital_group)
    ## Marital group must match the file naming convention:
    marital_group[marital_group == "married"] <- "mwra"
    marital_group[marital_group == "unmarried"] <- "uwra"
    marital_group[marital_group == "all women"] <- "awra"

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)

    traj_fname <- paste0(marital_group, "_CP_counts_agg_li_", agg_name, ".RData")
    traj_full_path <- file.path(output_dir, "aggregatetrajectories", agg_family_name, traj_fname)
    if(!file.exists(traj_full_path)) stop("File '", traj_full_path, "' does not exist.")

    ob_name <- load(file = traj_full_path, verbose = verbose)

    if(verbose) msg_loaded(traj_full_path, ob_name)

    return(get(ob_name[1]))
}
