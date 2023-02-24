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
            for (vrb in c(TRUE, FALSE)) {
                expect_s3_class(get_country_classifications(UNlocations_names = unl,
                                                            clean_col_names = ccn,
                                                            verbose = vrb), "tbl_df")
            }
        }
    }
})

test_that("list_world_bank_aggregates_names works", {
    list_world_bank_aggregates_names()
})

test_that("list_special_aggregates_csv_filenames", {
    list_special_aggregates_csv_filenames()
})

test_that("list_special_aggregates_names workds", {
    list_special_aggregates_names()
})


###-----------------------------------------------------------------------------
### ** 15-49 "Total" Age Group

###-----------------------------------------------------------------------------
### *** Married Women

test_that("get_used_unpd_regions works for 15-49, married women run", {
    test_output_dir <-
        system.file("data-test/test_output_15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## Defaults
    expect_s3_class(get_used_unpd_regions(output_dir = test_output_dir), "tbl_df")

    ## Specified args
    for (unl in c(TRUE, FALSE)) {
        for (ccn in c(TRUE, FALSE)) {
            for (vrb in c(TRUE, FALSE)) {
                expect_s3_class(get_used_unpd_regions(output_dir = test_output_dir,
                                                      UNlocations_names = unl,
                                                      clean_col_names = ccn,
                                                      verbose = vrb), "tbl_df")
            }
        }
    }
})

test_that("get_used_special_aggregates works for 15-49, married women run", {
    test_output_dir <-
        system.file("data-test/test_output_15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## Default
    get_used_special_aggregates(output_dir = test_output_dir)

    ## Specified args
    for (ccn in c(TRUE, FALSE)) {
        for (vrb in c(TRUE, FALSE)) {
            expect_s3_class(get_used_special_aggregates(output_dir = test_output_dir,
                                                        clean_col_names = ccn,
                                                        verbose = vrb), "tbl_df")
        }
    }
})
