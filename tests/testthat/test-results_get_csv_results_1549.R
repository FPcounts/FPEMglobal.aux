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

test_that("get_csv_results works with specified argument values on a 15-49, married directory", {
    ## Output directory
    test_output_dir <-
        system.file("data-test/test_output_15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## All options
    for (ag in c("country", "UNPDaggregate")) {
        for (aagc in c(TRUE, FALSE)) {
            for (st in c("prop", "count", "ratio")) {
                for (asc in c(TRUE, FALSE)) {
                    for (ad in c("orig", "adj", "sub_adj")) {
                        for (aadc in c(TRUE, FALSE)) {
                            for (ccn in c(TRUE, FALSE)) {
                                for (yamy in c(TRUE, FALSE)) {
                                    for (acc in c(TRUE, FALSE)) {
                                        for (tf in c("long", "wide", "raw")) {
                                            for (srt in c(TRUE, FALSE)) {
                                                for (vrb in c(TRUE, FALSE)) {
                                                    expect_s3_class(get_csv_res(output_dir = test_output_dir,
                                                                                aggregate = ag,
                                                                                add_aggregate_column = aagc,
                                                                                stat = st,
                                                                                add_stat_column = asc,
                                                                                adjusted = ad,
                                                                                add_adjusted_column = aadc,
                                                                                clean_col_names = ccn,
                                                                                years_as_midyear = yamy,
                                                                                add_country_classifications = acc,
                                                                                table_format = tf,
                                                                                sort = srt,
                                                                                verbose = vrb),
                                                                    "data.frame")
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
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

test_that("get_csv_results works with specified argument values on a 15-49, all_women directory", {
    ## Output directory
    test_output_dir <-
        system.file("data-test/test_output_15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    ## All options
    for (ag in c("country", "UNPDaggregate")) {
        for (aagc in c(TRUE, FALSE)) {
            for (st in c("prop", "count", "ratio")) {
                for (asc in c(TRUE, FALSE)) {
                    for (ad in c("orig", "adj", "sub_adj")) {
                        for (aadc in c(TRUE, FALSE)) {
                            for (ccn in c(TRUE, FALSE)) {
                                for (yamy in c(TRUE, FALSE)) {
                                    for (acc in c(TRUE, FALSE)) {
                                        for (tf in c("long", "wide", "raw")) {
                                            for (srt in c(TRUE, FALSE)) {
                                                for (vrb in c(TRUE, FALSE)) {
                                                    expect_s3_class(get_csv_res(output_dir = test_output_dir,
                                                                                aggregate = ag,
                                                                                add_aggregate_column = aagc,
                                                                                stat = st,
                                                                                add_stat_column = asc,
                                                                                adjusted = ad,
                                                                                add_adjusted_column = aadc,
                                                                                clean_col_names = ccn,
                                                                                years_as_midyear = yamy,
                                                                                add_country_classifications = acc,
                                                                                table_format = tf,
                                                                                sort = srt,
                                                                                verbose = vrb),
                                                                    "data.frame")
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
})
