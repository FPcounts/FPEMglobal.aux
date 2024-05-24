test_that("Standard indicator names are properly returned", {
    args_list <- lapply(as.list(args(get_std_indicator_names)), "eval")
    expect_type(args_list, "list")
    for (STAT in args_list$stat) {
        for (MG in args_list$marital_group) {
            for (AW in args_list$aw_set) {
                for (V in args_list$indicator_name_format) {
                    expect_type(
                        get_std_indicator_names(stat = STAT, marital_group = MG,
                                                aw_set = AW, indicator_name_format = V),
                        "character")
                }
            }
        }
    }
})
