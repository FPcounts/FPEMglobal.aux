##' Construct path to output directory
##'
##' Constructs the path to an output directory for a given model run
##' name and a root directory. This output directory structure is now
##' obsolete, but the function is retained for backward
##' compatibility. The section \dQuote{Specifying the results
##' directory from other functions} below constains further
##' information, in particular for users who arrived here from the
##' help file of another function.
##'
##' @section Specifying the results directory from other functions:
##'
##' Several functions, e.g., \code{\link{get_csv_res}},
##' require the output directory to be specified via arguments
##' \code{run_name}, \code{root_dir}, and \code{output_dir}. There are
##' two ways to do this:
##'
##' \enumerate{
##'     \item Provide only \code{output_dir}
##'     \item Provide \code{root_dir} and \code{run_name}
##' }
##'
##' The first method is recommended. Using this approach,
##' \code{output_dir} specifies the location of the main MCMC results
##' \file{.rda} file, and the \file{fig} and \file{table} directories
##' that hold the plots and \file{.csv} tables. \code{run_name} is
##' still required for finding the output files themselves, but it
##' only specifies the tag at the start of the plot and table
##' filenames. It does not specify the name of any directory.
##'
##' Using the first method, if \code{output_dir} = \dQuote{myOutputs}
##' and \code{run_name} = \dQuote{myRun}, the \file{.pdf} file holding
##' the main output plots has file path:
##' \file{myOutputs/fig/myRunCIs.pdf}.
##'
##' The second method is now obsolete. It only works when the
##' outputs are all contained in the directory
##' \file{\code{root_dir}/output/\code{run_name}}, \emph{and} the tag at the start
##' of the plot and table filenames is also \code{run_name}.
##'
##' Under the second method, if \code{root_dir} = \dQuote{myRootDir}
##' and \code{run_name} = \dQuote{myRun}, the \file{.pdf} file holding the main
##' output plots has file path: \file{myRootDir/output/myRun/fig/myRunCIs.pdf}.
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
                               root_dir = NULL, verbose = FALSE) {
    if (is.null(output_dir)) {
        if (is.null(run_name)) stop("Must supply 'run_name' or 'output_dir'")
        out <- get_output_dir(run_name, root_dir)
    } else {
        if (!is.null(root_dir)) {
            if (verbose) {
                message("'output_dir' has been supplied so 'root_dir' is ignored.")
                }
        }
        out <- output_dir
    }
    return(assert_valid_output_dir(out))
}

## Select the 'orig' or 'adj' subdirectory of the table directory.
table_orig_adj_dir <- function(tbl_dir, adj, verbose = FALSE) {
    if (adj) {
        tbl_dir <- file.path(tbl_dir, "adj")
    } else {
        orig_dir <- file.path(tbl_dir, "orig")
        if (dir.exists(orig_dir)) {
            if (verbose) message("Reading from '", orig_dir)
            return(orig_dir)
        } else {
            if (verbose) message("Reading from '", tbl_dir)
            return(tbl_dir)
            }
    }
}

