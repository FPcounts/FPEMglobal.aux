###-----------------------------------------------------------------------------
### * get_csv_denominators

###-----------------------------------------------------------------------------
### ** 15-19 Directories

###-----------------------------------------------------------------------------
### *** Married

test_that("get_csv_denominators works with default argument values on a 15-19, married directory", {
    ## Output directory
    test_output_dir <-
        system.file("data-test/15-19_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## Defaults
    expect_s3_class(get_csv_denominators(output_dir = test_output_dir, verbose = TRUE),
                    "data.frame")
    expect_s3_class(get_csv_denominators(output_dir = test_output_dir, verbose = FALSE),
                    "data.frame")
})


test_that("get_csv_denominators works with specified arguments on a 15-19, married directory", {
    ## Output directory
    test_output_dir <-
        system.file("data-test/15-19_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## All options
    for (mg in c("default", "married", "unmarried", "all women")) {
        for (ccn in c(TRUE, FALSE)) {
            for (un in c("units", "thousands")) {
                for (yrs_as_mid in c(TRUE, FALSE)) {
                    for (tbl_fmt in c("long", "raw")) {
                        expect_s3_class(get_csv_denominators(output_dir = test_output_dir,
                                                             marital_group = mg,
                                                             clean_col_names = ccn,
                                                             units = un,
                                                             years_as_midyear = yrs_as_mid,
                                                             table_format = tbl_fmt,
                                                             verbose = TRUE),
                                        "data.frame")
                    }
                }
            }
        }
    }
})
