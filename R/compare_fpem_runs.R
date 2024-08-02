
##' Check if two FPEMglobal runs are identical
##'
##' \pkg{FPEMglobal} model runs may be renamed, making it difficult to
##' determine if two runs are the same. A run may have been renamed if
##' filenames ended up being too long for runs in deeply nested
##' folders.
##'
##' @param output_dir_1,output_dir_2 See \code{output_dir} in help
##'     file for \code{\link{get_csv_res}}.
##' @param assert_valid_dirs Logical; should the run directories be
##'     checked to make sure they are valid \pkg{FPEMglobal} output
##'     directories?
##' @param report Logical; print messages reporting outcome of each check?
##' @return A list with the results of various comparisons, returned
##'     invisibly.
##' @author Mark Wheldon
##' @export
identical_fpem_runs <- function(output_dir_1 = NULL,
                                output_dir_2 = NULL,
                                assert_valid_dirs = TRUE, report = TRUE) {

    verbose <- getOption("FPEMglobal.aux.verbose")

    ## -------* Functions

    do_test <- function(name, test_fun = c("identical", "all.equal"), extract_fun) {
        test_fun <- match.arg(test_fun)
        if (report) message("\n* Checking '", name, "' via '", test_fun, "' ... ")
        out <- do.call(test_fun,
                              args = list(do.call(extract_fun,
                                                  args = list(output_dir = output_dir_1)),
                                          do.call(extract_fun,
                                                  args = list(output_dir = output_dir_2))))
        if (report) {
            if (isTRUE(out)) message("  ... '", name, "' match.")
            else {
                if (identical(test_fun, "identical")) {
                    message(toString(out))
                    out <- do_test(name = name, test_fun = "all.equal", extract_fun = extract_fun)
                } else {
                    message(toString(out))
                }
            }
        }
        return(setNames(list(out), nm = name))
    }

    ## -------* Check Inputs

    stopifnot(is.logical(report))

    mg_1 <- is_married_women_run(output_dir = output_dir_1) ||
        is_unmarried_women_run(output_dir = output_dir_1)

    mg_2 <- is_married_women_run(output_dir = output_dir_2) ||
        is_unmarried_women_run(output_dir = output_dir_2)

    if (!identical(mg_1, mg_2)) stop("Runs must both be of the same marital group.")

    run_1_dir <-
        output_dir_wrapper(output_dir = output_dir_1,
                           assert_valid = assert_valid_dirs)

    run_2_dir <-
        output_dir_wrapper(output_dir = output_dir_2,
                           assert_valid = assert_valid_dirs)

    ## -------* Tests

    out <- list()

    ## -------** All runs

    if (report) message("\n* Checking 'log.txt' via 'all.equal' ... ")
    out <- c(out,
             list(log.txt = all.equal(readLines(file.path(run_1_dir, "log.txt")),
                                      readLines(file.path(run_2_dir, "log.txt")))))
    if (report) {
        if (isTRUE(out$log.txt)) message("  ... 'log.txt' match.")
        else message(toString(out$log.txt))
    }

    out <- c(out,
             do_test("par.ciq.rda", "identical", "get_model_param_quantiles"),
             do_test("mcmc.meta.rda", "identical", "get_model_meta_info"))

    ## -------** Only married/unmarried

    if (mg_1 && mg_2) {
        out <- c(out,
                 do_test("model.txt", "identical", "get_model_JAGS_txt"),
                 do_test("res.country.rda", "identical", "get_indicator_summary_results"),
                 do_test("mcmc.array.rda", "identical", "get_model_traj"),
                 do_test("global_mcmc_args.RData", "identical", "get_global_run_args"))

        pp_1 <- try(output_dir_wrapper(output_dir = output_dir_1,
                                 post_processed = TRUE))
        pp_2 <- try(output_dir_wrapper(output_dir = output_dir_2,
                                       post_processed = TRUE))
        if (!inherits(pp_1, "try-error") && !inherits(pp_2, "try-error")) {
            out <- c(out,
                     do_test("post_process_args.RData", "identical", "get_global_post_process_args"))
        }
    }

    ## -------** Only All Women

    if (!mg_1 && !mg_2) {
        out <- c(out,
                 do_test("res.country.all.women.rda", "identical", "get_indicator_summary_results"),
                 do_test("combine_runs_args.RData", "identical", "get_combine_runs_args"))
    }

    ## -------** Check File Tree

    if (report) message("\n* Checking file names, recursively, via 'all.equal' ... ")
    out <- c(out,
             list(file.tree = all.equal(dir(run_1_dir, recursive = TRUE),
                                        dir(run_2_dir, recursive = TRUE))))
    if (report) {
        if (isTRUE(out$file.tree)) message("  ... file names match.")
        else message(toString(out$file.tree))
    }

    return(invisible(out))
}
