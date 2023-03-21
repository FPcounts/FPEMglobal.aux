### Note that other scripts test `get_used_input_data` and the denominator importers.

test_that("'input_data_2_fpemdata' works", {
    ## 15-19, Married
    test_output_dir <-
        system.file("data-test/15-19_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(input_data_2_fpemdata(output_dir = test_output_dir), "tbl_df")

    ## 15-49, Married
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(input_data_2_fpemdata(output_dir = test_output_dir), "tbl_df")

    ## 15-49, All women
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_error(input_data_2_fpemdata(output_dir = test_output_dir),
                 "data not stored")
})


test_that("'denominators_2_fpemdata' works", {
    ## 15-19, Married
    test_output_dir <-
        system.file("data-test/15-19_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(denominators_2_fpemdata(output_dir = test_output_dir), "tbl_df")

    ## 15-19, All women
    test_output_dir <-
        system.file("data-test/15-19_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(denominators_2_fpemdata(output_dir = test_output_dir), "tbl_df")

    ## 15-49, Married
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(denominators_2_fpemdata(output_dir = test_output_dir), "tbl_df")

    ## 15-49, All women
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(denominators_2_fpemdata(output_dir = test_output_dir), "tbl_df")
})

