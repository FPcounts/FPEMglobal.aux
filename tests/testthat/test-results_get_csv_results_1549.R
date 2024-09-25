###-----------------------------------------------------------------------------
### * get_csv_results

###-----------------------------------------------------------------------------
### ** 15-49 ("Age Total") Directories

###-----------------------------------------------------------------------------
### *** Married

test_that("get_csv_results works with default argument values on a 15-49, married directory", {
    ## Output directory
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## Defaults
    expect_s3_class(get_csv_res(output_dir = test_output_dir),
                    "data.frame")
})

test_that("get_csv_results produces data frame without duplicates", {

    ## NOTE: This uses the _installed_ version of the package, so if
    ## you have updated relevant code you will either need to use the
    ## non-parallel version by uncommenting (see below) or reinstall
    ## the package.

    ## Output directory
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    args_list <- lapply(as.list(args(get_csv_res)), "eval")

    argvals_grid <- expand.grid(STAT = args_list$stat,
                                ADJ = args_list$adjusted,
                                INF = args_list$indicator_name_format,
                                ACC = c(TRUE, FALSE),
                                stringsAsFactors = FALSE)

    if (requireNamespace("parallel", quietly = TRUE)) {

        if (requireNamespace("parallelly", quietly = TRUE)) {
            nodes <- parallelly::availableCores(omit = 2) #one for system, one for testthat parallel runner
        } else nodes <- 1
        cl <- parallel::makeCluster(nodes)

        parallel::clusterExport(cl = cl, varlist = c("argvals_grid", "test_output_dir"),
                                envir = environment())

        dump <- parallel::parLapply(cl = cl, X = 1:nrow(argvals_grid), fun = function(i) {
            library(testthat)
            library(FPEMglobal.aux)

            ## ## UNCOMMENT for debugging: -->

            ## for (i in 1:nrow(argvals_grid)) {
            ## for (STAT in args_list$stat) {
            ##     for (ADJ in args_list$adjusted) {
            ##         for (INF in args_list$indicator_name_format) {
            ##             for (ACC in c(TRUE, FALSE)) {

            ##                 cat("\nstat = ", STAT)
            ##                 cat("\nadjusted = ", ADJ)
            ##                 cat("\ninf = ", INF)
            ##                 cat("\nacc = ", ACC)

            ## ## |<---

            ## NO Aggregates:

            ## Long
            res_df <- get_csv_res(output_dir = test_output_dir,
                                  aggregate = "country",
                                  adjusted = argvals_grid$ADJ[i],
                                  table_format = "long",
                                  indicator_name_format = argvals_grid$INF[i],
                                  add_country_classifications = argvals_grid$ACC[i])
            expect_s3_class(res_df, "data.frame")
            expect_identical(nrow(
                res_df[res_df$iso == unique(res_df$iso)[1] &
                       res_df$year == unique(res_df$year)[1] &
                       res_df$indicator == unique(res_df$indicator)[1] &
                       res_df$quantile == unique(res_df$quantile)[1], , drop = FALSE]),
                1L)

            ## Wide
            res_df <- get_csv_res(output_dir = test_output_dir,
                                  aggregate = "country",
                                  adjusted = argvals_grid$ADJ[i],
                                  table_format = "wide",
                                  indicator_name_format = argvals_grid$INF[i],
                                  add_country_classifications = argvals_grid$ACC[i])
            expect_s3_class(res_df, "data.frame")
            expect_identical(nrow(
                res_df[res_df$iso == unique(res_df$iso)[1] &
                       res_df$year == unique(res_df$year)[1] &
                       res_df$quantile == unique(res_df$quantile)[1], , drop = FALSE]),
                1L)

            ## ## UNCOMMENT FOR DEBUGGING: -->
            ##             }
            ##             }
            ##         }
            ##     }
            ## }
            ## ## |<---

        })
        parallel::stopCluster(cl)

    } else warning("Package 'parallel' not installed - test not done.")

})

###-----------------------------------------------------------------------------
### *** All women

test_that("get_csv_results works with default argument values on a 15-49, all_women directory", {
    ## Output directory
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## Defaults
    expect_s3_class(get_csv_res(output_dir = test_output_dir),
                    "data.frame")
})

###-----------------------------------------------------------------------------
### * convert_csv_res_2_fpemdata

test_that("convert_csv_res_2_fpemdata works", {

    ## Married
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(convert_csv_res_2_fpemdata(output_dir = test_output_dir), "tbl_df")

    ## All women
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(convert_csv_res_2_fpemdata(output_dir = test_output_dir), "tbl_df")
})

