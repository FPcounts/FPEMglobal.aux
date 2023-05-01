
##' Check if two FPEMglobal runs are identical
##'
##' \pkg{FPEMglobal} model runs may be renamed, making it difficult to
##' determine if two runs are the same. A run may have been renamed if
##' filenames ended up being too long for runs in deeply nested
##' folders.
##'
##' @param run_name_1,run_name_2 See \code{run_name} in help file for
##'     \code{\link{get_output_dir}}.
##' @param output_dir_1,output_dir_2 See \code{output_dir} in help
##'     file for \code{\link{get_csv_res}}.
##' @param root_dir_1,root_dir_2 See \code{root_dir} in help file for
##'     \code{\link{get_output_dir}}.
##' @param assert_valid_dirs Logical; should the run directories be
##'     checked to make sure they are valid \pkg{FPEMglobal} output
##'     directories?
##' @param report Logical; print messages reporting outcome of each check?
##' @inheritParams get_csv_res
##' @return A list with the results of various comparisons, returned
##'     invisibly.
##' @author Mark Wheldon
##' @export
identical_fpem_runs <- function(run_name_1 = NULL, output_dir_1 = NULL, root_dir_1 = NULL,
                                run_name_2 = NULL, output_dir_2 = NULL, root_dir_2 = NULL,
                                assert_valid_dirs = TRUE, report = TRUE,
                                verbose = FALSE) {

    stopifnot(is.logical(report))

    run_1_dir <- output_dir_wrapper(run_name = run_name_1,
                                    output_dir = output_dir_1,
                                    root_dir = root_dir_1,
                                    verbose = verbose,
                                    assert_valid = assert_valid_dirs)

    run_2_dir <- output_dir_wrapper(run_name = run_name_2,
                                    output_dir = output_dir_2,
                                    root_dir = root_dir_2,
                                    verbose = verbose,
                                    assert_valid = assert_valid_dirs)

    out <- list()

    if (report) message("Checking 'model.txt' via 'identical()'... ")
    out <- c(out,
             list(model_txt =
                      identical(get_model_JAGS_txt(run_name = run_name_1,
                                                           output_dir = output_dir_1,
                                                           root_dir = root_dir_1,
                                                          verbose = verbose),
                                get_model_JAGS_txt(run_name = run_name_2,
                                                           output_dir = output_dir_2,
                                                           root_dir = root_dir_2,
                                                           verbose = verbose))))
    if (report) {
        if (isTRUE(out$model_txt)) message(" ... 'model.txt' identical.")
        else message(" ", out$model_txt)
    }

    if (report) message("Checking 'par.ciq.rda' via 'identical()'... ")
    out <- c(out,
             list(par_ciq_rda =
                      identical(get_model_param_quantiles(run_name = run_name_1,
                                                           output_dir = output_dir_1,
                                                           root_dir = root_dir_1,
                                                          verbose = verbose),
                                get_model_param_quantiles(run_name = run_name_2,
                                                           output_dir = output_dir_2,
                                                           root_dir = root_dir_2,
                                                           verbose = verbose))))
    if (report) {
        if (isTRUE(out$par_ciq_rda)) message(" ... 'par.ciq.rda' identical.")
        else message(" ", out$par_ciq_rda)
    }

    if (report) message("Checking 'mcmc.array.rda' via 'identical()' ...")
    out <- c(out,
             list(mcmc_array_rda =
                      identical(get_model_traj(run_name = run_name_1,
                                               output_dir = output_dir_1,
                                               root_dir = root_dir_1,
                                               verbose = verbose),
                                get_model_traj(run_name = run_name_2,
                                               output_dir = output_dir_2,
                                               root_dir = root_dir_2,
                                               verbose = verbose))))
    if (report) {
        if (isTRUE(out$mcmc_array_rda)) message(" ... 'mcmc.array.rda' identical.")
        else if (report) message(" ", out$mcmc_array_rda)
    }

    if (report) message("Checking 'mcmc.meta.rda' via 'all.equal()' ...")
    out <- c(out,
             mcmc_meta_rda = all.equal(get_model_meta_info(run_name = run_name_1,
                                                           output_dir = output_dir_1,
                                                           root_dir = root_dir_1,
                                                           verbose = verbose),
                                       get_model_meta_info(run_name = run_name_2,
                                                           output_dir = output_dir_2,
                                                           root_dir = root_dir_2,
                                                           verbose = verbose)))
    if (report) {
        if (isTRUE(out$mcmc_meta_rda)) message(" ... 'mcmc.meta.rda' all equal.")
        else if (report) message(" ", out$mcmc_meta_rda)
    }

    if (report) message("Checking file names (recursively) via 'all.equal()' ...")
    out <- c(out,
             list(file_names = all.equal(dir(run_1_dir, recursive = TRUE),
                                         dir(run_2_dir, recursive = TRUE))))
    if (report) {
        if (isTRUE(out$file_names)) message(" ... file names all equal.")
        else if (report) message(" ", out$file_names)
    }

    return(invisible(out))
    }
