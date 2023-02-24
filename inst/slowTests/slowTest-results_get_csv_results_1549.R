###-----------------------------------------------------------------------------
### * get_csv_results

###-----------------------------------------------------------------------------
### ** 15-49 ("Age Total") Directories

###-----------------------------------------------------------------------------
### *** Married

test_that("get_csv_results works with specified argument values on a 15-49, married directory", {
    ## Output directory
    test_output_dir <-
        system.file("data-test/test_output_15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## All options
    args_grid <-
        expand.grid(output_dir = test_output_dir,
                    aggregate = c("country", "UNPDaggregate"),
                    add_aggregate_column = c(TRUE, FALSE),
                    stat = c("prop", "count", "ratio"),
                    add_stat_column = c(TRUE, FALSE),
                    adjusted = c("orig", "adj", "sub_adj"),
                    add_adjusted_column = c(TRUE, FALSE),
                    clean_col_names = c(TRUE, FALSE),
                    years_as_midyear = c(TRUE, FALSE),
                    add_country_classifications = c(TRUE, FALSE),
                    table_format = c("long", "wide", "raw"),
                    sort = c(TRUE, FALSE),
                    verbose = c(TRUE, FALSE),
                    stringsAsFactors = FALSE)

    cl <- parallel::makeCluster(parallelly::availableCores(omit = 1))
    parallel::clusterExport(cl, varlist = "args_grid", envir = environment())
    dump <- parallel::parLapply(cl = cl, X = seq_len(nrow(args_grid)),
                                fun = function(z) {
                          library(testthat)
                          library(FPEMglobal.aux)
                          out <- try(expect_s3_class(do.call("get_csv_res",
                                                  args = do.call("list", args_grid[z, ])),
                                                 "tbl_df"))
                          if (!inherits(out, "try-error")) return(NULL)
                          else return(out)
                      })
    parallel::stopCluster(cl)
})

###-----------------------------------------------------------------------------
### *** All women

test_that("get_csv_results works with specified argument values on a 15-49, all_women directory", {
    ## Output directory
    test_output_dir <-
        system.file("data-test/test_output_15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## All options
    args_grid <-
        expand.grid(output_dir = test_output_dir,
                    aggregate = c("country", "UNPDaggregate"),
                    add_aggregate_column = c(TRUE, FALSE),
                    stat = c("prop", "count", "ratio"),
                    add_stat_column = c(TRUE, FALSE),
                    adjusted = c("orig", "adj", "sub_adj"),
                    add_adjusted_column = c(TRUE, FALSE),
                    clean_col_names = c(TRUE, FALSE),
                    years_as_midyear = c(TRUE, FALSE),
                    add_country_classifications = c(TRUE, FALSE),
                    table_format = c("long", "wide", "raw"),
                    sort = c(TRUE, FALSE),
                    verbose = c(TRUE, FALSE),
                    stringsAsFactors = FALSE)

    cl <- parallel::makeCluster(parallelly::availableCores(omit = 1))
    parallel::clusterExport(cl, varlist = "args_grid", envir = environment())
    dump <- parallel::parLapply(cl = cl, X = seq_len(nrow(args_grid)),
                                fun = function(z) {
                          library(testthat)
                          library(FPEMglobal.aux)
                          out <- try(expect_s3_class(do.call("get_csv_res",
                                                  args = do.call("list", args_grid[z, ])),
                                                 "tbl_df"))
                          if (!inherits(out, "try-error")) return(NULL)
                          else return(out)
                      })
    parallel::stopCluster(cl)
})

