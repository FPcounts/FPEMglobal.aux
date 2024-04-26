###-----------------------------------------------------------------------------
### * countries_aggregates.R

test_that("get_195_countries works", {
    expect_s3_class(get_195_countries(), "tbl_df")
})

test_that("get_185_countries works", {
    expect_s3_class(get_185_countries(), "tbl_df")
})

test_that("get_country_classifications works", {
    ## Defaults
    expect_s3_class(get_country_classifications(), "tbl_df")

    ## Specified args
    for (unl in c(TRUE, FALSE)) {
        for (ccn in c(TRUE, FALSE)) {
                expect_s3_class(get_country_classifications(M49_region_names = unl,
                                                            clean_col_names = ccn), "tbl_df")
        }
    }
})

test_that("list_world_bank_aggregates_names works", {
    expect_error(list_world_bank_aggregates_names(), NA)
})

test_that("list_special_aggregates_csv_filenames", {
    expect_error(list_special_aggregates_csv_filenames(), NA)
})

test_that("list_special_aggregates_names works", {
    expect_error(list_special_aggregates_names(), NA)
})

test_that("convert_country_classifications_2_fpemdata works", {
    expect_s3_class(convert_country_classifications_2_fpemdata(), "tbl_df")
    })


###-----------------------------------------------------------------------------
### ** 15-49 "Total" Age Group

###-----------------------------------------------------------------------------
### *** Married Women

test_that("get_used_unpd_regions works for 15-49, married women run", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## Defaults
    expect_s3_class(get_used_unpd_regions(output_dir = test_output_dir), "tbl_df")

    ## Specified args
    for (unl in c(TRUE, FALSE)) {
        for (ccn in c(TRUE, FALSE)) {
                expect_s3_class(get_used_unpd_regions(output_dir = test_output_dir,
                                                      M49_region_names = unl,
                                                      clean_col_names = ccn), "tbl_df")
        }
    }
})

test_that("get_used_special_aggregates works for 15-49, married women run", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## Default
    get_used_special_aggregates(output_dir = test_output_dir)

    ## Specified args
    for (ccn in c(TRUE, FALSE)) {
            expect_s3_class(get_used_special_aggregates(output_dir = test_output_dir,
                                                        clean_col_names = ccn), "tbl_df")
    }
})
