###-----------------------------------------------------------------------------
### * Test 'fpemdata_interface' functions


test_that("get_hierarchical_medians works", {

    ## Married
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_type(get_hierarchical_medians(test_output_dir), "list")

    ## All women
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_error(get_hierarchical_medians(test_output_dir))
})


test_that("convert_country_classifications_2_fpemdata(..., version = \"used\") works", {

    ## Married
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(convert_country_classifications_2_fpemdata(test_output_dir, version = "used"), "tbl_df")

    ## All women
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(convert_country_classifications_2_fpemdata(test_output_dir, version = "used"), "tbl_df")

})


test_that("convert_country_classifications_2_fpemdata(..., version = \"installed\") works", {

    ## Married
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(convert_country_classifications_2_fpemdata(test_output_dir, version = "installed"), "tbl_df")

    ## All women
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(convert_country_classifications_2_fpemdata(test_output_dir, version = "installed"), "tbl_df")

})


test_that("convert_input_data_2_fpemdata works", {

    ## Married
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(convert_input_data_2_fpemdata(test_output_dir), "tbl_df")

    ## All women
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_error(convert_input_data_2_fpemdata(test_output_dir))

})


test_that("convert_denominators_2_fpemdata works", {

    ## Married
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(convert_denominators_2_fpemdata(test_output_dir), "tbl_df")

    ## All women
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(convert_denominators_2_fpemdata(test_output_dir), "tbl_df")

})


test_that("convert_csv_res_2_fpemdata works", {

    ## Married
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(convert_csv_res_2_fpemdata(output_dir = test_output_dir), "tbl_df")

    ## All women
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_s3_class(convert_csv_res_2_fpemdata(output_dir = test_output_dir), "tbl_df")
})
