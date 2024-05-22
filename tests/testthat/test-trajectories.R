### Test import of trajectories

test_mcmc_array <- function(dir, name_dims = FALSE, add_cp_timing_param = FALSE) {
        expect_true(dir.exists(dir))
        expect_error(x <- get_model_traj(output_dir = dir, name_dims = name_dims,
                                         add_cp_timing_param = add_cp_timing_param),
                     NA)
        expect_type(x, "double")
        expect_identical(class(x), "array")
        expect_identical(length(dim(x)), 3L)
        if (name_dims)
            expect_identical(names(dimnames(x)), c("iteration", "chain", "parameter"))
        if (add_cp_timing_param)
            expect_true(length(grep("^T\\.c\\[[0-9]+]$", dimnames(x)[[3]])) > 0)
}


test_that("'get_model_traj' works", {
    ## 15-19, Married
    test_mcmc_array(system.file("data-test/15-19_married", package = "FPEMglobal.aux"),
                    name_dims = FALSE)
    test_mcmc_array(system.file("data-test/15-19_married", package = "FPEMglobal.aux"),
                    name_dims = TRUE)

    ## 15-49, Married
    test_mcmc_array(system.file("data-test/15-49_married", package = "FPEMglobal.aux"),
                    name_dims = FALSE)
    test_mcmc_array(system.file("data-test/15-49_married", package = "FPEMglobal.aux"),
                    name_dims = TRUE)
})


test_that("CP timing parameter can be created using 'get_model_traj'", {

    ## 15-19, Married
    test_mcmc_array(system.file("data-test/15-19_married", package = "FPEMglobal.aux"),
                    name_dims = FALSE, add_cp_timing_param = TRUE)
    test_mcmc_array(system.file("data-test/15-19_married", package = "FPEMglobal.aux"),
                    name_dims = TRUE, add_cp_timing_param = TRUE)

    ## 15-49, Married
    test_mcmc_array(system.file("data-test/15-49_married", package = "FPEMglobal.aux"),
                    name_dims = FALSE, add_cp_timing_param = TRUE)
    test_mcmc_array(system.file("data-test/15-49_married", package = "FPEMglobal.aux"),
                    name_dims = TRUE, add_cp_timing_param = TRUE)
})
