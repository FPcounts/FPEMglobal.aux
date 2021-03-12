##' Construct path to output directory
##'
##' Constructs the path to an output directory for a given model run
##' name and a root directory.
##'
##' @param run_name The run name.
##' @param root_dir A root directory if needed, i.e., the location of
##'     the directory called \dQuote{output}.
##' @return \code{file.path(root_dir, "output", run_name)}
##' @author Mark Wheldon
##' @export
get_output_dir <- function(run_name, root_dir = ".") {
    file.path(root_dir, "output", run_name)
}

## Used in functions to check if user supplied the necessary arguments.
output_dir_wrapper <- function(run_name = NULL, output_dir = NULL,
                               root_dir = ".", verbose = FALSE) {
    if(is.null(output_dir)) {
        if(is.null(run_name)) stop("Must supply 'run_name' or 'output_dir'")
        else get_output_dir(run_name, root_dir)
    } else {
        if(!is.null(root_dir)) {
            if(verbose) message("'output_dir' has been supplied so 'root_dir' is ignored.")
            output_dir
        }
    }
}

## Select the 'orig' or 'adj' subdirectory of the table directory.
table_orig_adj_dir <- function(tbl_dir, adj, verbose = FALSE) {
    if(adj) {
        tbl_dir <- file.path(tbl_dir, "adj")
    } else {
        orig_dir <- file.path(tbl_dir, "orig")
        if(dir.exists(orig_dir)) {
            if(verbose) message("Reading from '", orig_dir)
            orig_dir
        } else {
            if(verbose) message("Reading from '", tbl_dir)
            tbl_dir
            }
    }
}

