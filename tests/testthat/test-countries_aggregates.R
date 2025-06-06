###-----------------------------------------------------------------------------
### * countries_aggregates.R

test_that("get_195_countries(..., version = \"installed\") works", {
    expect_s3_class(get_195_countries(version = "installed"), "tbl_df")
})

test_that("get_185_countries(..., version = \"installed\") works", {
    expect_s3_class(get_185_countries(version = "installed"), "tbl_df")
})

test_that("get_country_classifications(..., version = \"installed\") works", {
    ## Defaults
    expect_s3_class(get_country_classifications(version = "installed"), "tbl_df")

    ## Specified args
    for (unl in c(TRUE, FALSE)) {
        for (ccn in c(TRUE, FALSE)) {
                expect_s3_class(get_country_classifications(M49_region_names = unl,
                                                            clean_col_names = ccn,
                                                            version = "installed"), "tbl_df")
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


###-----------------------------------------------------------------------------
### ** 15-49 "Total" Age Group

###-----------------------------------------------------------------------------
### *** Married Women

test_that("get_country_classifications(..., version = \"used\") works for 15-49, married women run", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## Defaults
    expect_s3_class(get_country_classifications(output_dir = test_output_dir, version = "used"), "tbl_df")

    ## Specified args
    for (unl in c(TRUE, FALSE)) {
        for (ccn in c(TRUE, FALSE)) {
                expect_s3_class(get_country_classifications(output_dir = test_output_dir,
                                                      M49_region_names = unl,
                                                      clean_col_names = ccn, version = "used"), "tbl_df")
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
