test_that("FPEMglobal ext data file names are returned properly", {
        out <- get_FPEMglobal_extdata_filenames(file_ext = TRUE)
        expect_type(out, "list")
        expect_true(!is.null(names(out)) && length(names(out)) > 0)
        out <- unlist(out)
        expect_true(all(grepl(pattern = "\\.csv", x = out)))

        out <- get_FPEMglobal_extdata_filenames(file_ext = FALSE)
        expect_type(out, "list")
        expect_true(!is.null(names(out)) && length(names(out)) > 0)
        out <- unlist(out)
        expect_true(all(!grepl(pattern = "\\.csv", x = out)))
})


test_that("Standard indicator names are properly returned", {
    args_list <- lapply(head(as.list(args(get_std_indicator_names)), -1), "eval")
    expect_type(args_list, "list")
    for (STAT in args_list$stat) {
        for (MG in args_list$marital_group) {
            for (ADJ in args_list$adjusted) {
                for (AW in args_list$aw_set) {
                    for (V in args_list$indicator_name_format) {
                        if (identical(STAT, "ratio") && identical(MG, "all_women") &&
                            identical(ADJ, "adj") && identical(AW, "only extra")) {
                            expect_error(get_std_indicator_names(stat = STAT, marital_group = MG,
                                                                 adjusted = ADJ,
                                                                 aw_set = AW, indicator_name_format = V),
                                         "'only extra' is not a valid option for 'aw_set'")
                        } else {
                        expect_type(
                            get_std_indicator_names(stat = STAT, marital_group = MG,
                                                    aw_set = AW, indicator_name_format = V),
                            "character")
                        }
                    }
                }
            }
        }
    }
})

