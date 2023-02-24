###-----------------------------------------------------------------------------
### * Work With Output Meta Data

###-----------------------------------------------------------------------------
### ** Load/Return Meta Data

## RULES:
## - 'load_...' functions load the objects into the environment
##   'envir' (.GlobalEnv by default) and invisibly return the object
##   name.
## - 'get_...' functions load the objects and return it. The loaded
##   object is not stored in any persistent environment, i.e., the
##   global workspace is not modified.


##' Get the meta data for an FPEMglobal model run
##'
##' \code{\link{load}}s and returns the \file{mcmc.meta.rda} file from
##' the output directory. This contains all sorts of information about
##' the model run. This will cause the printing of a large amount of
##' output; it is usually much better to assign the result to a
##' variable name.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##'
##' @return Either the name of the loaded object, invisibly
##'     (\code{load_model_meta}), or the loaded object itself
##'     (\code{get_model_meta_info}).
##'
##' @author Mark Wheldon
##'
##' @family model_run_meta_info
##' @export
get_model_meta_info <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                           verbose = FALSE) {
    res_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = FALSE)
    tmp_env <- new.env()
    if (verbose) on.exit(message("Loaded '", file.path(res_dir, "mcmc.meta.rda"), "'."),
                         add = TRUE, after = FALSE)
    return(get(load(file = file.path(res_dir, "mcmc.meta.rda"), envir = tmp_env), envir = tmp_env))
}


##' Get the country-index map file
##'
##' The model outputs are not labelled with country names or ISO
##' codes. Instead there is an internal index for the countries. This
##' function returns a data frame with columns \dQuote{iso.c},
##' \dQuote{name.c}, and \dQuote{filename} containing the ISO codes,
##' country names, and filename of the country-level trajectories. The
##' internal country index corresponds to the country's row number in
##' this data frame. This is only available for married and unmarried
##' women runs. Calling this function on an output directory from an
##' all women run will result in an error.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##' @return A \code{\link[tibble]{tibble}} with country ISO codes, names, and trajectory
##'     file names, where row number corresponds to the internal index
##'     number.
##'
##' @author Mark Wheldon
##'
##' @family model_run_meta_info
##'
##' @export
get_country_index <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                              verbose = FALSE) {

    if (!verbose) { op <- options(readr.show_progress = verbose, readr.show_col_types = verbose)
        on.exit(options(op), add = TRUE, after = FALSE) }

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose, post_processed = TRUE)
    if (is_all_women_run(output_dir = output_dir))
        stop("This is an all women run; country index files are only available for married and unmarried women runs.")

    traj_index <- try(readr::read_csv(file.path(output_dir, "iso.Ptp3s.key.csv"), col_types = "c"))

    if (identical(class(traj_index), "try-error")) {
        stop("Error reading 'iso.Ptp3s.key.csv'. Did you supply a run name or output directory for an all women run?")
    } else return(tibble::as_tibble(traj_index))
}


##' Get the global arguments used to generate an FPEMglobal model run
##'
##' \code{\link{load}}s and returns the \file{global_mcmc_args.RData}
##' file from the output directory. This contains the values of all
##' arguments passed to \pkg{FPEMglobal} functions (e.g.,
##' \code{\link[FPEMglobal]{do_global_mcmc}}). This function will throw
##' an error if \code{output_dir} is an all women output
##' directory.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##'
##' @return A named list.
##'
##' @author Mark Wheldon
##'
##' @family model_run_meta_info
##' @export
get_global_mcmc_args <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                 verbose = FALSE) {
    res_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = FALSE)
    if (is_all_women_run(output_dir = res_dir)) stop("This is an all women run; 'get_global_mcmc_args' not available.")
    tmp_env <- new.env()
    if (verbose) on.exit(message("Loaded '", file.path(res_dir, "global_mcmc_args.RData"), "'."),
                         add = TRUE, after = FALSE)
    return(get(load(file = file.path(res_dir, "global_mcmc_args.RData"), envir = tmp_env), envir = tmp_env))
}


##' Get the arguments used to combine married and unmarried runs
##'
##' \code{\link{load}}s and returns the \file{combine_runs_args.RData}
##' file from the output directory. This contains the values of all
##' arguments passed to the \pkg{FPEMglobal} function
##' \code{\link[FPEMglobal]{combine_runs}}. This function will throw
##' an error if \code{output_dir} is not an all women output
##' directory.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##'
##' @return A named list.
##'
##' @author Mark Wheldon
##'
##' @family model_run_meta_info
##' @export
get_combine_runs_args <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                 verbose = FALSE) {
    res_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = TRUE)
    if (!is_all_women_run(output_dir = res_dir)) stop("This is not an all women run; 'combine_runs_args' not available.")
    tmp_env <- new.env()
    if (verbose) on.exit(message("Loaded '", file.path(res_dir, "combine_runs_args.RData"), "'."),
                         add = TRUE, after = FALSE)
    return(get(load(file = file.path(res_dir, "combine_runs_args.RData"), envir = tmp_env), envir = tmp_env))
}


##' Get arguments used to produce a global run
##'
##' This function is a convenience wrapper that calls
##' \code{\link{get_global_mcmc_args}} for married and unmarried runs,
##' and \code{\link{get_combine_runs_args}} for all women runs.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##'
##' @return A named list.
##'
##' @author Mark Wheldon
##'
##' @family model_run_meta_info
##' @export
get_global_run_args <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                verbose = FALSE) {
    res_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = TRUE)
    if (!is_all_women_run(output_dir = res_dir))
        return(get_global_mcmc_args(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                                    verbose = verbose))
    else
        return(get_combine_runs_args(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                                    verbose = verbose))
}


##' Get the global arguments used to process an FPEMglobal model run
##'
##' \code{\link{load}}s and returns the \file{post_process_args.RData}
##' file from the output directory. This contains the values of all
##' arguments passed to
##' \code{\link[FPEMglobal]{post_process_mcmc}}. This will cause the
##' printing of a large amount of output; it is usually much better to
##' assign the result to a variable name.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##'
##' @return A named list.
##'
##' @author Mark Wheldon
##'
##' @family model_run_meta_info
##' @export
get_global_post_process_args <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                 verbose = FALSE) {
    res_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose,
                           post_processed = TRUE)
    if (is_all_women_run(output_dir = res_dir)) stop("This is an all women run; 'post_process_args' not available.")
    tmp_env <- new.env()
    if (verbose) on.exit(message("Loaded '", file.path(res_dir, "post_process_args.RData"), "'."),
                         add = TRUE, after = FALSE)
    return(get(load(file = file.path(res_dir, "post_process_args.RData"), envir = tmp_env), envir = tmp_env))
}


##' Attempt to get the run name of an FPEM run from output files
##'
##' Search the meta info and output files of an FPEM run and attempt
##' to determine the run name. If the run name is saved in
##' \file{\code{output_dir}/global_mcmc_args.RData} it will be
##' returned. Otherwise, the names of certain output files will be
##' used.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##'
##' @return The run name as a character string.
##' @author Mark Wheldon
##'
##' @family model_run_meta_info
##' @export
get_run_name <- function(output_dir = NULL, verbose = FALSE) {

    res_dir <- output_dir_wrapper(output_dir = output_dir, verbose = verbose,
                                  post_processed = FALSE)

    ## Look in meta data files
    args <- get_global_run_args(output_dir = res_dir, verbose = verbose)
    if ("run_name" %in% names(args) && !is.null(args$run_name))
        return(args$run_name)
    else if ("run_name_override" %in% names(args) && !is.null(args$run_name_override))
        return(args$run_name_override)

    ## Try to extract from plots and tables
    file_name <- grep("CIs\\.pdf$", dir(file.path(res_dir, "fig", "CI")), value = TRUE)
    if (length(file_name) && nchar(file_name))
        return(gsub("CIs\\.pdf$", "", file_name))
    else {
        file_name <- grep("CIs_nopar\\.pdf$", dir(file.path(res_dir, "fig", "CI")), value = TRUE)
        if (length(file_name) && nchar(file_name))
            return(gsub("CIs_nopar\\.pdf$", "", file_name))
        else {
            file_name <- grep("datainfo_total\\.pdf$", dir(file.path(res_dir, "fig", "data_info")), value = TRUE)
            if (length(file_name) && nchar(file_name))
                return(gsub("datainfo_total\\.pdf$", "", file_name))
            else {
                warning("Cannot determine run name.")
                return(NULL)
            }
        }
    }
}


##' Determine the marital group or age group of an FPEM run
##'
##' These functions get and check the marital group and age group of
##' an FPEMglobal model run. \code{is_marital_group_run} is intended
##' for checking if runs are either \code{"married"} or
##' \code{"unmarried"} only. Use \code{is_all_women_run} to check if a
##' run is an \code{"all women"} run.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_csv_res
##' @param marital_group Character string; the marital group to check against.
##' @return A character string (\code{get_...}) or logical value (\code{is_...}).
##' @author Mark Wheldon
##'
##' @family model_run_meta_info
##' @export
get_marital_group <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                              verbose = FALSE) {
    mg <- get_model_meta_info(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                              verbose = verbose)$general
    if (isTRUE(mg$all.women.run.copy)) return("all women")
    else return(convert_marital_group_names(mg$marital.group))
}

##' @rdname get_marital_group
##' @export
is_all_women_run <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                 verbose = FALSE) {
    isTRUE(get_model_meta_info(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                              verbose = verbose)$general$all.women.run.copy)
}

##' @rdname get_marital_group
##' @export
is_marital_group_run <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                 marital_group = c("married", "unmarried"),
                                 verbose = FALSE) {
    marital_group <- match.arg(marital_group)
    if (is_all_women_run(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                         verbose = verbose)) {
        return(FALSE)
    } else {
        mgp <- get_model_meta_info(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                                   verbose = verbose)$general$marital.group
        return(identical(convert_marital_group_names(mgp), marital_group))
    }
}

##' @rdname get_marital_group
##' @export
is_married_women_run <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                 verbose = FALSE) {
    is_marital_group_run(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                         marital_group = "married",
                         verbose = verbose)
}

##' @rdname get_marital_group
##' @export
is_unmarried_women_run <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                 verbose = FALSE) {
    is_marital_group_run(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                         marital_group = "unmarried",
                         verbose = verbose)
}

##' @rdname get_marital_group
##' @export
get_age_group <- function(run_name = NULL, output_dir = NULL, root_dir = NULL,
                                 verbose = FALSE) {
    return(get_model_meta_info(run_name = run_name, output_dir = output_dir, root_dir = root_dir,
                              verbose = verbose)$general$age.group)
}

