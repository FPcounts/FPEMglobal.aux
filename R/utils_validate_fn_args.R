
## Make sure output directory is valid
assert_valid_output_dir <- function(dirpath, post_processed = FALSE, countrytrajectories = post_processed) {

    ## Argument check
    checkmate::qassert(dirpath, "S+")

    ## ----------
    ## RECURSE
    if (length(dirpath) > 1) return(sapply(setNames(dirpath, dirpath), "assert_valid_output_dir",
                                           post_processed = post_processed,
                                           countrytrajectories = countrytrajectories))
    ## ----------

    ## Existence
    checkmate::assert_directory_exists(dirpath)

    ## Content checks

    ## Emptiness
    empty_dirs <- isTRUE(identical(length(dir(dirpath)), 0L))
    if (any(empty_dirs)) {
        stop("Directory \n\t", dirpath, "\nis empty.")
    }

    ## Has necessary files
    checkmate::assert_file_exists(file.path(dirpath, c("global_mcmc_args.RData", "mcmc.array.rda", "model.txt", "par.ciq.rda")))
    checkmate::assert_directory_exists(file.path(dirpath, "data"))
    if (post_processed) {
        checkmate::assert_file_exists(file.path(dirpath, "data.global.rda", "post_process_args.RData",
                                                "res.country.rda", "res.aggregate.rda"))
        checkmate::assert_directory_exists(file.path(dirpath, c("fig", "table")))
    }
    if (countrytrajectories) {
        checkmate::assert_directory_exists(file.path(dirpath, "countrytrajectories"))
    }

    ## RETURN
    return(invisible(dirpath))
}
