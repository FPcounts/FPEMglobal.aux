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


##' Load and return model meta data
##'
##' \code{load_model_meta} \code{\link{load}}s \file{mcmc.meta.rda}
##' from the output directory. This contains all sorts of information
##' about the model run. \code{get_model_meta} calls
##' \code{load_model_meta} and, additionally, returns the loaded
##' object. This will cause the printing of a large amount of output;
##' it is usually much better to assign the result to a variable name.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_FPEMglobal_csv_res
##'
##' @return Either the name of the loaded object, invisibly
##'     (\code{load_model_meta}), or the loaded object itself
##'     (\code{get_model_meta}).
##'
##' @author Mark Wheldon
##' @export
load_model_meta <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
             verbose = FALSE, envir = parent.frame()) {

    res_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)

    ob_name <- load(file = file.path(res_dir, "mcmc.meta.rda"), envir = envir)

    if(verbose) {
        message("Loaded '", file.path(res_dir, "mcmc.meta.rda"), "'. \n.. Object is '",
                paste(ob_name, collapse = " "), "'. \n.. Returning '", ob_name[1],
                "'.")
    }

    return(invisible(ob_name))
    }

##' @rdname load_model_meta
##' @export
get_model_meta <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
                           verbose = FALSE) {
    tmp_env <- new.env()
    if (verbose) on.exit(message("Loaded '", file.path(res_dir, "mcmc.meta.rda"), "'."),
                         add = TRUE, after = FALSE)
    return(get(load_model_meta(run_name = run_name, output_dir = output_dir,
                               root_dir = root_dir, verbose = FALSE, envir = tmp_env), envir = tmp_env))
}


##' Load and return global arguments used to generate the run
##'
##' \code{load_global_mcmc_args} \code{\link{load}}s \file{mcmc.meta.rda}
##' from the output directory. This contains all sorts of information
##' about the model run. \code{get_global_mcmc_args} calls
##' \code{load_global_mcmc_args} and, additionally, returns the loaded
##' object. This will cause the printing of a large amount of output;
##' it is usually much better to assign the result to a variable name.
##'
##' @inheritParams get_output_dir
##' @inheritParams get_FPEMglobal_csv_res
##'
##' @return Either the name of the loaded object, invisibly
##'     (\code{load_global_mcmc_args}), or the loaded object itself
##'     (\code{get_global_mcmc_args}).
##'
##' @author Mark Wheldon
##' @export
load_global_mcmc_args <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
             verbose = FALSE, envir = parent.frame()) {

    res_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)

    ob_name <- load(file = file.path(res_dir, "global_mcmc_args.RData"), envir = envir)

    if(verbose) {
        message("Loaded '", file.path(res_dir, "global_mcmc_args.RData"), "'. \n.. Object is '",
                paste(ob_name, collapse = " "), "'. \n.. Returning '", ob_name[1],
                "'.")
    }

    return(invisible(ob_name))
}


##' @rdname load_global_mcmc_args
##' @export
get_global_mcmc_args <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
                                 verbose = FALSE) {
    res_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)
    tmp_env <- new.env()
    if (verbose) on.exit(message("Loaded '", file.path(res_dir, "global_mcmc_args.rda"), "'."),
                         add = TRUE, after = FALSE)
    return(get(load_global_mcmc_args(run_name = run_name, output_dir = output_dir,
                               root_dir = root_dir, verbose = FALSE, envir = tmp_env), envir = tmp_env))
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
##' @inheritParams get_FPEMglobal_csv_res
##'
##' @return The run name as a character string.
##' @author Mark Wheldon
##' @export
get_run_name <- function(output_dir = NULL, verbose = FALSE) {

    args <- get_global_mcmc_args(output_dir = output_dir, verbose = verbose)
    if ("run_name" %in% names(args))
        return(args$run_name)

    else {
        res_dir <-
            output_dir_wrapper(output_dir = output_dir, verbose = verbose)

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

                else stop("Cannot determine run name.")

            }
        }
    }
}



