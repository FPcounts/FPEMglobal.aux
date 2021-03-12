###-----------------------------------------------------------------------------
### * Work With Main Model Results

##' Get model parameter quantiles
##'
##' \code{\link{load}}s the file \dQuote{par.ciq.rda} which contains
##' posterior quantiles of model parameters.
##'
##' @param run_name
##' @param output_dir
##' @param root_dir
##' @param verbose
##' @inheritParams get_output_dir
##' @return The \code{\link{load}}ed object.
##' @author Mark Wheldon
##' @export
get_model_quantiles <- function(run_name = NULL, output_dir = NULL, root_dir = ".",
                         verbose = FALSE) {

    output_dir <-
        output_dir_wrapper(run_name = run_name, output_dir = output_dir,
                           root_dir = root_dir, verbose = verbose)
    return(get(load(file.path(output_dir, "par.ciq.rda"))[1]))
}
