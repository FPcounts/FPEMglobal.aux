###-----------------------------------------------------------------------------
### * get_csv_results

###-----------------------------------------------------------------------------
### ** 15-49 ("Age Total") Directories

###-----------------------------------------------------------------------------
### *** Married

test_that("get_csv_results works with default argument values on a 15-49, married directory", {
    ## Output directory
    test_output_dir <-
        system.file("data-test/test_output_15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## Defaults
    expect_s3_class(get_csv_res(output_dir = test_output_dir, verbose = TRUE),
                    "data.frame")
    expect_s3_class(get_csv_res(output_dir = test_output_dir, verbose = FALSE),
                    "data.frame")
})

###-----------------------------------------------------------------------------
### *** All women

test_that("get_csv_results works with default argument values on a 15-49, all_women directory", {
    ## Output directory
    test_output_dir <-
        system.file("data-test/test_output_15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## Defaults
    expect_s3_class(get_csv_res(output_dir = test_output_dir, verbose = TRUE),
                    "data.frame")
    expect_s3_class(get_csv_res(output_dir = test_output_dir, verbose = FALSE),
                    "data.frame")
})