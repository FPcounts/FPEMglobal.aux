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

## test_that("list_world_bank_aggregates_names works", {
##     expect_error(list_world_bank_aggregates_names(), NA)
## })

## test_that("list_special_aggregates_csv_filenames", {
##     expect_error(list_special_aggregates_csv_filenames(), NA)
## })

## test_that("list_special_aggregates_names works", {
##     expect_error(list_special_aggregates_names(), NA)
## })

test_that("country_classifications_2_fpemdata works on a married women run", {
    expect_s3_class(country_classifications_2_fpemdata(system.file("data-test/15-49_married", package = "FPEMglobal.aux")),
                    "tbl_df")
})

test_that("country_classifications_2_fpemdata works on an all women run", {
    expect_s3_class(country_classifications_2_fpemdata(system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")),
                    "tbl_df")
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

## test_that("get_used_special_aggregates works for 15-49, married women run", {
##     test_output_dir <-
##         system.file("data-test/15-49_married", package = "FPEMglobal.aux")
##     expect_true(dir.exists(test_output_dir))

##     ## Default
##     agg <- get_used_special_aggregates(output_dir = test_output_dir)
##     expect_s3_class(agg, "tbl_df")
##     expect_true(nrow(agg) > 0)

##     ## Specified args
##     for (ccn in c(TRUE, FALSE)) {
##             expect_s3_class(get_used_special_aggregates(output_dir = test_output_dir,
##                                                         clean_col_names = ccn), "tbl_df")
##     }
## })
