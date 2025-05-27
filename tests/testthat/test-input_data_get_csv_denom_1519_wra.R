###-----------------------------------------------------------------------------
### * get_used_csv_denominators

###-----------------------------------------------------------------------------
### ** 15-19 Directories

###-----------------------------------------------------------------------------
### *** All Women

test_that("'get_used_csv_denominators_filepath' works with default argument values on a 15-19, all women directory", {
    ## Output directory
    test_output_dir <-
        system.file("data-test/15-19_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_true(file.exists(get_used_csv_denominators_filepath(output_dir = test_output_dir)))
    })

test_that("get_denominators(..., used = \"used_csv\") works with default argument values on a 15-19, all women directory", {
    ## Output directory
    test_output_dir <-
        system.file("data-test/15-19_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## Defaults
    expect_s3_class(get_denominators(output_dir = test_output_dir, used = "used_csv"),
                    "data.frame")
})


test_that("get_denominators(..., used = \"used_csv\") works with specified arguments on a 15-19, all women directory", {
    ## Output directory
    test_output_dir <-
        system.file("data-test/15-19_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## All options
    for (mg in c("default", "married", "unmarried", "all_women")) {
        for (ccn in c(TRUE, FALSE)) {
            for (un in c("units", "thousands")) {
                for (yrs_as_mid in c(TRUE, FALSE)) {
                    for (tbl_fmt in c("long", "raw")) {
                        expect_s3_class(get_denominators(output_dir = test_output_dir,
                                                             marital_group = mg,
                                                             clean_col_names = ccn,
                                                             units = un,
                                                             years_as_midyear = yrs_as_mid,
                                                         table_format = tbl_fmt, used = "used_csv"),
                                        "data.frame")
                    }
                }
            }
        }
    }
})
