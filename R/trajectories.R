###-----------------------------------------------------------------------------
### * Work with Trajectories


##' Get MCMC trajectories of FPEMglobal model parameters
##'
##' This function \code{\link{load}}s and returns MCMC trajectories
##' stored in the file \file{mcmc.array.rda} in the output directory
##' \code{output_dir}. This contains MCMC trajectories for the model
##' parameters. This function simply returns the loaded object; if you
##' want quantiles of model parameters, see
##' \code{\link{get_model_param_quantiles}}. If you want country
##' trajectories of CP indicators use
##' \code{\link{get_country_traj_muw}} or
##' \code{\link{get_country_traj_aw}}.
##'
##' The dimensions are not named by \pkg{FPEMglobal}. If
##' \code{name_dims} is \code{TRUE} they are given the names
##' \dQuote{iteration}, \dQuote{chain}, \dQuote{parameter}.
##'
##' The array of trajectories has the following structure (dimnames
##' are named only if \code{name_dims = TRUE}):
##' \preformatted{
##'  num [1:832, 1:20, 1:12234] 1952 1939 1957 1940 1924 ...
##'  - attr(*, "dimnames")=List of 3
##'   ..$ iteration: NULL
##'   ..$ chain    : NULL
##'   ..$ parameter: chr [1:12234] "RT.c[1]" "RT.c[2]" "RT.c[3]" "RT.c[4]" ...
##' }
##'
##' \subsection{Contraceptive prevalence timing parameter}{
##'
##' The timing parameter of the increase in contraceptive prevalence
##' (CP timing parameter) was a model parameter in the original
##' formulation of Alkema et al.\sspace{}(2013), where it was denoted
##' \eqn{\Omega_c}{Omega_c}. This was replaced with the "set-level"
##' parameter (\eqn{P_{c,t^*}}{P_c,t*}) in the update by Cahill et
##' al.\sspace{}(2017) and maintained in Kantorová et
##' al.\sspace{}(2020). If the output directory is from a run of a
##' version of FPEMglobal that uses the Cahill et al.\sspace{}(2017)
##' modification, the array returned by
##' \code{get_model_param_quantiles} will not contain results for the
##' CP timing parameter.
##'
##' To include the CP timing parameter, set logical argument
##' \code{add_cp_timing_param} to \code{TRUE}. It will be calculated
##' from the other model parameters via the following
##' expression (the calculation is applied to each trajectory):
##'
##' \deqn{\Omega_c = 1990 + \frac{1}{\omega_c} \log \left(\frac{\tilde{P}_c}{\exp(S_c^*)} - 1 + \tilde{P}_c \right)}{Omega_c = 1990 + 1 / omega_c * log[tilde{P}_c / exp(S_c^*) - 1 + tilde{P}_c]}
##' where
##' \deqn{S_c^* = S_c - \epsilon_{c,1990}}{S_c^* = S_c - epsilon_c,1990}
##'
##' This expression is due to N. Cahill (pers. comm.); see also Dasgupta et al.\sspace{}(2022), Appendix A, Sect. 1.4.1.}
##'
##' @family Trajectory functions
##'
##' @inheritParams get_csv_res
##' @inheritParams get_output_dir
##' @param add_cp_timing_param Logical; add the CP timing parameter? See \dQuote{Details}.
##' @param name_dims Logical; should the dimensions be given informative names? See \dQuote{Details}.
##' @return An array of trajectories.
##'
##' @author Mark Wheldon
##' @export
get_model_traj <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                           add_cp_timing_param = FALSE,
                           name_dims = TRUE) {

    ## -------* Functions

    name_dimnames <- function(x, name_dims) {
        if (name_dims) names(dimnames(x)) <- c("iteration", "chain", "parameter")
        return(x)
    }

    ## -------** CP Timing Parameter

    ## Note that the timing parameter is only available for countries
    ## with observations, and even then, only for that subset that is
    ## not at the asymptote level already.

    ## Checks that parameter names generated from country and year
    ## indices are actually in the trajectory array.
    validate_par_names <- function(par_names, traj_array, return_value = c("indices", "names")) {
        return_value <- match.arg(return_value)
        par_names_not_in <- par_names[!par_names %in% dimnames(traj_array)[[3]]]
        if (length(par_names_not_in)) {
            stop(paste0("The following parameters were not found in the trajectory array: '",
                        toString(par_names_not_in), "'."))
        } else {
            if (identical(return_value, "indices"))
                return(which(dimnames(traj_array)[[3]] %in% par_names))
            else
                return(par_names)
        }
    }

    ## Next two functions create the parameter names for which timing
    ## parameter can be calculated.
    get_c_param_dim_idx <- function(parname_prefix, #< including the '.c' at the end
                                    traj_array, meta_info, return_value = c("indices", "names")) {
        return_value <- match.arg(return_value)
        cseq <- 1:meta_info$winbugs.data$C
        par_names <- paste0(parname_prefix, "[", cseq, "]")
        par_names_not_in <- par_names[!par_names %in% dimnames(traj_array)[[3]]]
        return(validate_par_names(par_names, traj_array, return_value = return_value))
    }
    get_eps_1990_idx <- function(traj_array, meta_info, return_value = c("indices", "names")) {
        return_value <- match.arg(return_value)
        cseq <- 1:meta_info$winbugs.data$C
        sl_year_seq <- meta_info$winbugs.data$year.set.index
        stopifnot(identical(length(cseq), length(sl_year_seq)))
        par_names <- paste0("eps.ci[", cseq, ",", sl_year_seq, "]")
        return(validate_par_names(par_names, traj_array, return_value = return_value))
    }

    ## 'cp_timing' computs the timing parameter. 'Sstar_c' generates input for 'cp_timing'.
    Sstar_c <- function(eps, setlev) {
        stopifnot(is.array(eps) && is.array(setlev))
        stopifnot(identical(unname(dim(eps)), unname(dim(setlev))))
        out <- setlev - eps
        if(is.array(out)) {
            dimnames(out)[[3]] <-
                gsub("^setlevel\\.c\\[", "setlevelStar.[", dimnames(out)[[3]])
        } else {
            names(out)[[3]] <-
                gsub("^setlevel\\.c\\[", "setlevelStar.[", names(out)[[3]])
        }
        return(out)
    }

    cp_timing <- function(omega, pmax, Sstar) {
        stopifnot(is.array(omega), is.array(pmax), is.array(Sstar))
        stopifnot(identical(unname(dim(omega)), unname(dim(pmax))),
                  identical(unname(dim(pmax)), unname(dim(Sstar))))
        out <- array(NA, dim = dim(omega))
        dimnames(out) <- dimnames(omega)
        valid <- invlogit(Sstar) < pmax
        out[valid] <- 1990 +
            1/omega[valid] *
            log( pmax[valid] / exp(Sstar[valid]) - 1 + pmax[valid] )
        if(is.array(out)) {
            dimnames(out)[[3]] <-
                gsub("^omega\\.c\\[", "T.c[", dimnames(out)[[3]])
        } else {
            names(out)[[3]] <-
                gsub("^omega\\.c\\[", "T.c[", names(out)[[3]])
        }

        return(out)
    }

    ## Expand cp_timing
    expand_cp_timing <- function(cp_timing, traj_array) {
        stopifnot(is.array(cp_timing), is.array(traj_array))
        stopifnot(identical(dim(cp_timing)[1:2], dim(traj_array)[1:2]))

        ## The final length of cp_timing should match omega_c
        length_c_param <- length(grep("^omega\\.c\\[[0-9]+]$", dimnames(traj_array)[[3]]))

        ## New array
        abind::abind(cp_timing,
                     array(NA,
                           dim = c(dim(cp_timing)[1], dim(cp_timing)[2],
                                   length_c_param - dim(cp_timing)[3]),
                           dimnames = c(dimnames(cp_timing)[1:2],
                                        list(paste0("T.c[", (dim(cp_timing)[3] + 1):length_c_param, "]")))))
    }

    ## -------* Checks and House-keeping

    stopifnot(is.logical(add_cp_timing_param))
    stopifnot(is.logical(name_dims))

    verbose <- getOption("FPEMglobal.aux.verbose")

    ## -------* Get trajectories

    res_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir)
    tmp_env <- new.env()
    if (verbose) message("Reading '", file.path(res_dir, "mcmc.array.rda"), "'.")
    out <- get(load(file = file.path(res_dir, "mcmc.array.rda"), envir = tmp_env),
               envir = tmp_env)

    ## -------* CP timing?

    if (add_cp_timing_param) {

        meta_info <- get_model_meta_info(output_dir = output_dir)

        ## (Reverse-Polish
        out <-
            abind::abind(out,
                         expand_cp_timing(
                             cp_timing(omega = out[, ,
                                                   get_c_param_dim_idx("omega.c", out, meta_info = meta_info),
                                                   drop = FALSE],
                                       pmax = out[, ,
                                                  get_c_param_dim_idx("pmax.c", out, meta_info = meta_info),
                                                  drop = FALSE],
                                       Sstar = Sstar_c(eps = out[, ,
                                                                 get_eps_1990_idx(out, meta_info = meta_info),
                                                                 drop = FALSE],
                                                       setlev = out[, ,
                                                                    get_c_param_dim_idx("setlevel.c", out,
                                                                                        meta_info = meta_info),
                                                                    drop = FALSE])),
                             traj_array = out),
                         along = 3)
    }

    ## -------* Finish

    return(name_dimnames(out, name_dims = name_dims))
}


##' Get MCMC trajectories of contraceptive prevalence indicators for married and unmarried women
##'
##' This function \code{\link{load}}s and returns the MCMC
##' trajectories of contraceptive prevalence indicators for married or
##' unmarried women for a single country. Trajectories are loaded from \file{.rda} files found in
##' the subdirectory \file{countrytrajectories} of the results
##' directory (see below). The filename for the given country is
##' determined by reference to an index which must be in the file
##' \code{file.path(output_dir, "iso.Ptp3s.key.csv")}.
##'
##' \code{\link{load}} is called is such a way that ensures nothing is
##' added to the global environment. Instead, the loaded object is
##' returned by the function (so it's a good idea to assign to result
##' to an object).
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
##' @family Trajectory functions
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


##' Get MCMC trajectories of contraceptive prevalence indicators for all women
##'
##' This function \code{\link{load}}s and returns the MCMC
##' trajectories of contraceptive prevalence indicators for for all women. Trajectories are loaded from \file{.rda} files
##' found in the subdirectory \file{countrytrajectories} of the
##' results directory (see below). The filename for the given country
##' is \code{paste0("aw_ISO_", iso_code, "_counts.rda")}.
##'
##' \code{\link{load}} is called is such a way that ensures nothing is
##' added to the global environment. Instead, the loaded object is
##' returned by the function (so it's a good idea to assign to result
##' to an object).
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
##' @family Trajectory functions
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
##' @family Trajectory functions
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



