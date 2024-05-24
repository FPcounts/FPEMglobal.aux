###-----------------------------------------------------------------------------
### * Work With Main Model Results

##' Get quantiles of country-specific model parameters.
##'
##' Posterior distributions of a subset of the country-specific model
##' parameters are stored in an array and saved to the file
##' \file{par.ciq.rda} in the main output directory
##' (\code{output_dir}). This function loads that file and returns the
##' array. Only the 2.5, 50, and 97.5 percentiles are saved by \pkg{FPEMglobal}. If others
##' are requested via the \code{percentiles} argument they will be
##' calculated from the full set of MCMC trajectories (loaded by
##' \code{\link{get_model_traj}}) and included in the array returned.
##'
##' The parameters returned are the the country-specific model parameters, such as the
##' asymptotes, rate paratmeters, timing parameters, etc (see Kantorová et al., 2020, S1 Appendix).
##'
##' The array has three dimensions with structure (dimnames
##' are named only if \code{name_dims = TRUE}):
##' \preformatted{
##' num [1:248, 1:7, 1:3] 0.0259 0.0112 0.0163 0.015 0.0332 ...
##' - attr(*, "dimnames")=List of 3
##' ..$ : chr [1:248] "Afghanistan" "Albania" "Algeria" "Angola" ...
##' ..$ : chr [1:7] "omega.c" "pmax.c" "setlevel.c" "Romega.c" ...
##' ..$ : chr [1:3] "2.5%" "50%" "97.5%"
##' }
##' The default behaviour is to load and return this array from \file{par.ciq.rda}.
##'
##' Only the parameters listed in the table below are stored in
##' \file{par.ciq.rda}. The parameter names in the mathematical
##' notation used by Kantorová et al.\sspace{}(2020) are also included
##' for reference. An additional parameter, the midpoint of the
##' increase in contraceptive prevalence (CP timing) can be added; see
##' Section \dQuote{CP Timing Parameter} below.
##'
##' \tabular{rll}{
##' \emph{Dimname}      \tab \emph{Notation} \tab \emph{Description} \cr
##' \code{omega.c} \tab \eqn{\omega_c}{omega_c} \tab Pace parameter for increase \cr
##'                \tab                         \tab of contraceptive prevalence \cr
##'                \tab                         \tab  (any method) \cr
##' \code{pmax.c} \tab \eqn{\tilde{P}_c}{tilde{P}_c} \tab Asymptote of contraceptive \cr
##'                \tab                              \tab prevalence (any method) \cr
##' \code{setlevel.c} \tab \eqn{P_{c,t^*}}{P_c,t*} \tab Set-level of prevalence, \cr
##'                   \tab                         \tab \eqn{t^*=1990}{t* = 1990} \cr
##' \code{Romega.c} \tab \eqn{\psi_c}{psi_c} \tab Pace parameter for increase \cr
##'                  \tab                     \tab of the ratio of modern to \cr
##'                  \tab                     \tab traditional prevalence \cr
##' \code{RT.c} \tab \eqn{\Psi_c}{Psi_c} \tab Midpoint for increase \cr
##'             \tab                      \tab  in the ratio of modern to \cr
##'            \tab                       \tab  traditional prevalence \cr
##' \code{Rmax.c} \tab \eqn{\tilde{R}_c}{tilde{R}_c} \tab Asymptote of increase of \cr
##'               \tab                               \tab the ratio of modern to \cr
##'               \tab                               \tab traditional prevalence \cr
##' \code{unmet.intercept.c} \tab \eqn{z_c}{z_c} \tab Intercept of the parametric \cr
##'                          \tab                 \tab model for the ratio of unmet \cr
##'                          \tab                 \tab model to no contraceptive use \cr
##' \code{T.c} \tab \eqn{\Omega_c}{Omega_c} \tab Midpoint for increase \cr
##'             \tab                        \tab of contraceptive prevalence \cr
##'             \tab                        \tab (any method) \cr
##'             \tab                        \tab \emph{only if} \cr
##'             \tab                        \tab \code{add_cp_timing_param = TRUE}
##' }
##'
##' \subsection{Contraceptive prevalence timing parameter}{ If
##' \code{add_cp_timing_param = TRUE}, quantiles of the contraceptive
##' prevalence timing parameter will be added. For details, see this
##' same subsection under \dQuote{Details} in the documentation for
##' \code{\link{get_model_traj}}.}
##'
##'
##' @references
##'
##'Alkema, L., Kantorová, V., Menozzi, C., & Biddlecom, A. (2013). National, Regional, and Global Rates and Trends in Contraceptive Prevalence and Unmet Need for Family Planning Between 1990 and 2015: A Systematic and Comprehensive Analysis. The Lancet, 381(9878), 1642–1652. \doi{10.1016/S0140-6736(12)62204-1}.
##'
##' Cahill, N., Sonneveldt, E., Stover, J., Weinberger, M., Williamson, J., Wei, C., Brown, W., & Alkema, L. (2017). Modern Contraceptive Use, Unmet Need, and Demand Satisfied Among Women of Reproductive Age Who Are Married or in a Union in the Focus Countries of the Family Planning 2020 Initiative: A Systematic Analysis Using the Family Planning Estimation Tool. \emph{The Lancet}, 391(10123), 870–882. \doi{10.1016/S0140-6736(17)33104-5}.
##'
##' Dasgupta, A. N. Z., Wheldon, M., Kantorová, V., & Ueffing, P. (2022). Contraceptive Use and Fertility Transitions: The Distinctive Experience of Sub-Saharan Africa. Demographic Research, 46(4), 97–130. \doi{10.4054/DemRes.2022.46.4}.
##'
##' Kantorová, V., Wheldon, M. C., Ueffing, P., and Dasgupta, A. N. Z. (2020), Estimating progress towards meeting women’s contraceptive needs in 185 countries: A Bayesian hierarchical modelling study, \emph{PLOS Medicine}, 17, e1003026. \doi{10/ggk3cf}.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##' @inheritParams get_model_traj
##' @param percentiles Percentiles of parameter distributions to be included in the output.
##' @return An array of quantiles.
##' @author Mark Wheldon
##'
##' @family Get results from rda files
##'
##' @export
get_model_param_quantiles <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                      percentiles = c(2.5, 50, 97.5), add_cp_timing_param = FALSE,
                                      name_dims = TRUE) {

    ## -------* Sub-functions

    name_dimnames <- function(x, name_dims) {
        if (name_dims) names(dimnames(x)) <- c("name", "parameter", "percentile")
        return(x)
    }

    add_cells <- function(param_names, percentiles, par_ciq, mcmc_array, na_rm = FALSE) {
        percentiles_nm <- paste0(percentiles, "%")
        extra_par_ciq <- array(NA,
                               dim = c(dim(par_ciq)[1], #countries
                                       length(param_names), #parameters
                                       length(percentiles)), #percentiles
                               dimnames = list(dimnames(par_ciq)[[1]],
                                               param_names,
                                               percentiles_nm))
        for (c_idx in seq_len(dim(par_ciq)[1])) { #countries
            for (par_nm in param_names) { #parameters
                extra_par_ciq[c_idx, par_nm, percentiles_nm] <-
                    quantile(mcmc_array[,, paste0(par_nm, "[", c_idx, "]")],
                             probs = percentiles / 100, na.rm = na_rm)
            }
        }
        return(extra_par_ciq)
    }

    ## -------* House-keep and Argument Checks

    checkmate::assert_numeric(percentiles)
    stopifnot("You must request at least one percentile" = length(percentiles) > 0)

    verbose <- getOption("FPEMglobal.aux.verbose")
    if (verbose) on.exit(message("Loaded '", file.path(output_dir, "par.ciq.rda"), "'."),
                         add = TRUE, after = FALSE)

    new_percentiles <- percentiles[!percentiles %in% c(2.5, 50, 97.5)]
    if (length(new_percentiles)) add_percentiles <- TRUE
    else add_percentiles <- FALSE

    retained_percentiles <- c(2.5, 50, 97.5)[c(2.5, 50, 97.5) %in% percentiles]
    if (length(retained_percentiles) < 3) remove_percentiles <- TRUE
    else remove_percentiles <- FALSE

    ## -------* Inputs

    ## -------** Load par.ciq.rda

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir)
    tmp_env <- new.env()
    par_ciq <- get(load(file.path(output_dir, "par.ciq.rda"), envir = tmp_env)[1], envir = tmp_env)

    ## -------** Load MCMC Trajectories

    ## Only do it if necessary
    if (add_cp_timing_param || add_percentiles)
        mcmc_array <- get_model_traj(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                                     add_cp_timing_param = add_cp_timing_param, name_dims = FALSE)

    ## -------* Create Percentiles

    ## -------** Discarded Percentiles

    if (remove_percentiles) par_ciq <- par_ciq[, , paste0(retained_percentiles, "%"), drop = FALSE]

    ## -------** Extra Percentiles

    if (add_percentiles) {
        ## Add new percentiles and re-order
        par_ciq <-
            abind::abind(par_ciq,
                         add_cells(param_names = dimnames(par_ciq)[[2]],
                                   percentiles = new_percentiles,
                                   par_ciq = par_ciq, mcmc_array = mcmc_array),
                         along = 3)[, , order(percentiles), drop = FALSE]
    }

    ## -------** Timing Parameter

    if (add_cp_timing_param) {
        ## Add T.c and order it consistently
        par_ciq <-
            abind::abind(par_ciq,
                         add_cells(param_names = "T.c",
                                   percentiles = percentiles,
                                   par_ciq = par_ciq, mcmc_array = mcmc_array,
                                   na_rm = TRUE), # < have to use this because some params have 'NA' timing
                         along = 2)[, c("omega.c", "pmax.c", "T.c", dimnames(par_ciq)[[2]][-(1:2)]), , drop = FALSE]
    }

    ## -------* Rename and return

    if (name_dims) names(dimnames(par_ciq)) <- c("name", "parameter", "percentile")
    return(par_ciq)
}


##' DEPRECATED
##'
##' Use \code{\link{get_model_quantiles}} instead.
##'
##' @author Mark Wheldon
##' @export
get_countries_model_params_q <- function(...) {
    warning("'get_countries_model_params_q' is DEPRACATED and will soon be removed! Please use 'get_model_param_quantiles' instead.")
    get_model_param_quantiles(...)
}


##' Load and return country-specific posterior indicator quantiles
##'
##' This function \code{\link{load}}s and returns the object in the
##' file \file{res.country.rda} or \file{res.country.all.women.rda}
##' found in \code{file.path{output_dir}}. These are the quantiles of
##' the family planning indicators by country.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##' @param aggregate Return country or aggregate results. Only the
##'     \dQuote{UNPDaggregate}s are available in this format.
##' @param stat Which statistics should be loaded? Allowable values
##'     are \code{"std"} for \dQuote{standard} statistics, and
##'     \code{"age_ratio"} to return the age ratios.
##' @param adjusted Loads original results (\dQuote{orig}), adjusted
##'     medians only (\dQuote{adj}).
##' @return A list with posterior quantiles of indicators, by country.
##' @author Mark Wheldon
##'
##' @family Get results from rda files
##'
##' @export
get_indicator_summary_results <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                          aggregate = c("country", "aggregate"),
                                          stat = c("std", "age_ratio"),
                                          adjusted = c("orig", "adj")) {

    verbose <- getOption("FPEMglobal.aux.verbose")

    ## Checks

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir,
                           post_processed = TRUE, countrytrajectories = FALSE,
                           made_results = FALSE)
    aggregate <- match.arg(aggregate)
    stat <- match.arg(stat)
    adjusted <- match.arg(adjusted)

    if (identical(stat, "age_ratio") && identical(adjusted, "adj"))
        stop("'stat' is 'age_ratio' and 'adjusted' = 'adj'. There are no adjusted results for age ratios.")

    ## Get Files

    if (is_all_women_run(output_dir = output_dir))
        fname <- paste0("res.", aggregate, ".all.women")
    else fname <- paste0("res.", aggregate)

    if (identical(adjusted, "adj")) {
        if (identical(aggregate, "aggregate")) {
            fname <- gsub("aggregate", "UNPDaggregate", fname, fixed = TRUE)
        }
        fname <- paste0(fname, ".adj-mod_tot_unmet")
    } else {
        if (identical(stat, "age_ratio")) {
            fname <- paste0(fname, ".age.ratio")
        }
    }

    full_fpath <- file.path(output_dir, paste0(fname, ".rda"))

    tmp_env <- new.env()
    if (verbose) on.exit(message("Loaded '", full_fpath, "'."), add = TRUE, after = FALSE)
    return(get(load(full_fpath, envir = tmp_env)[1], envir = tmp_env))
}
