##' Construct path to output directory
##'
##' Constructs the path to an output directory for a given model run
##' name and a root directory. This output directory structure is now
##' obsolete, but the function is retained for backward
##' compatibility. The section \dQuote{Specifying the results
##' directory from other functions} below contains further
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
##' that hold the plots and \file{.csv} tables for a single marital
##' group. \code{run_name} only specifies the tag at the start of the
##' plot and table filenames (unless the run has been renamed by
##' \code{\link[FPEMglobal]{rename_global_run}}). It does not specify
##' the name of any directory. This will be determined automatically
##' from \code{output_dir} by \code{\link{get_run_name}}; the user
##' does not need to provide it when using this method.
##'
##' The default file naming scheme works as follows. For results
##' stored in output directory \dQuote{myOutputs}, with run name
##' \dQuote{myRun}, the \file{.pdf} file holding the main output plots
##' has file path: \file{myOutputs/fig/myRunCIs.pdf}.
##'
##' The second method is now obsolete. It only works when the
##' outputs are all contained in the directory
##' \file{\code{root_dir}/output/\code{run_name}}, \emph{and} the tag at the start
##' of all plot and table filenames is also \code{run_name}.
##'
##' Under the second method, if the results are stored in directory
##' \dQuote{myOutputs}, and the run has run name \dQuote{myRun}, the
##' \file{.pdf} file holding the main output plots has file path:
##' \file{myRootDir/output/myRun/fig/myRunCIs.pdf}. Note the extra
##' directory \file{output}.
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
                               root_dir = NULL,
                               assert_valid = FALSE,
                               post_processed = FALSE, countrytrajectories = FALSE,
                               made_results = FALSE,
                               adjusted_medians = FALSE,
                               age_ratios = FALSE) {

    ## Verbose ........................................
    ##
    op <- options(FPEMglobal.verbose = getOption("FPEMglobal.aux.verbose"))
    on.exit(options(op), add = TRUE, after = FALSE)
    ## ................................................

    verbose <- getOption("FPEMglobal.aux.verbose")

    if (is.null(output_dir)) {
        if (is.null(run_name)) stop("Must supply 'run_name' or 'output_dir'; both are 'NULL'.")
        else if (grepl("/", run_name)) stop("'run_name' contains '/'; did you supply 'output_dir' to the 'run_name' argument?")
        else out <- get_output_dir(run_name, root_dir)
    } else {
        if (!is.null(root_dir)) {
            if (verbose) {
                message("'output_dir' has been supplied so 'root_dir' is ignored.")
                }
        }
        if (!is.character(output_dir) || !identical(length(output_dir), 1L))
            stop("'output_dir' must be character, length 1.")
        out <- output_dir
    }

    if (assert_valid)
        out <- FPEMglobal::assert_valid_output_dir(out,
                                   post_processed = post_processed,
                                   countrytrajectories = countrytrajectories,
                                   made_results = made_results,
                                   adjusted_medians = adjusted_medians,
                                   age_ratios = age_ratios)

    return(out)
}

## Select the 'orig' or 'adj' subdirectory of the table directory.
table_orig_adj_dir <- function(tbl_dir, adj) {
    verbose <- getOption("FPEMglobal.aux.verbose")
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

