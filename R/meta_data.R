###-----------------------------------------------------------------------------
### * Work With Output Meta Data

##' Get model meta data
##'
##' This function \code{\link{load}}s \file{mcmc.meta.rda} from the output directory. This
##' contains all sorts of information about the model run.
##'
##' @section Specifying results directory:
##' See the section in \code{\link{get_FPEMglobal_csv_res}}.
##'
##' @param output_dir Output directory, e.g., like that returned by
##'     \code{\link{get_output_dir}}.
##' @param verbose Logical; report the path, filename, and object name in a
##'     message?
##' @return The loaded object.
##' @inheritParams get_output_dir
##' @author Mark Wheldon
##' @export
get_model_meta <-
    function(run_name = NULL, output_dir = NULL, root_dir = ".",
             verbose = FALSE) {

    res_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)

    ob_name <- load(file = file.path(res_dir, "mcmc.meta.rda"))

    if(verbose) {
        message("Loaded '", file.path(res_dir, "mcmc.meta.rda"), "'. \n.. Object is '",
                paste(ob_name, collapse = " "), "'. \n.. Returning '", ob_name[1],
                "'.")
    }

    return(get(ob_name[1]))
}
