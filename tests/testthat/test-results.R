### Get quantiles of model parameters, etc.

test_par_ciq <- function(dir, percentiles = c(2.5, 50, 97.5),
                         name_dims = FALSE, add_cp_timing_param = FALSE) {
    expect_true(dir.exists(dir))
    expect_error(x <- get_model_param_quantiles(output_dir = dir, percentiles = percentiles), NA)
    expect_type(x, "double")
    expect_identical(class(x), "array")
    expect_identical(length(dim(x)), 3L)
    expect_identical(dimnames(x)[[3]], paste0(percentiles, "%"))
    if (name_dims)
        expect_identical(names(dimnames(x)), c("name", "parameter", "percentile"))
    if (add_cp_timing_param)
        expect_true("T.c" %in% dimnames(x)[[2]])
}

############ UP TO HERE !!!!!!!!!!!!!!!!
stop("PICK UP FROM HERE:")
param_grid <- expand.grid(name_dims = c(TRUE, FALSE), cp_timing = c(TRUE, FALSE))

## Need to use each combination in tests below. Alternatively, could put the iteration in the 'test_par_ciq' function.


test_that("'get_model_param_quantiles' works with default arguments", {
    ## 15-19, Married
    test_par_ciq(system.file("data-test/15-19_married", package = "FPEMglobal.aux"))

    ## 15-49, Married
    test_par_ciq(system.file("data-test/15-49_married", package = "FPEMglobal.aux"))
})


test_that("'get_model_param_quantiles' works when a subset of percentiles are requested", {
    ## 15-19, Married
    test_par_ciq(system.file("data-test/15-19_married", package = "FPEMglobal.aux"), percentiles = c(50))

    ## 15-49, Married
    test_par_ciq(system.file("data-test/15-49_married", package = "FPEMglobal.aux"), percentiles = c(50))
})


test_that("'get_model_param_quantiles' works when additional percentiles are requested", {
    ## 15-19, Married
    output_dir <-system.file("data-test/15-19_married", package = "FPEMglobal.aux")
    test_par_ciq(output_dir, percentiles = c(50, 60))
    ## '50%' percentile should be the same:
    expect_identical(get_model_param_quantiles(output_dir = output_dir,
                                               percentiles = c(50, 60))[,,"50%"],
                     get_model_param_quantiles(output_dir = output_dir,
                                               percentiles = c(50))[,,"50%"])

    ## 15-49, Married
    test_par_ciq(system.file("data-test/15-49_married", package = "FPEMglobal.aux"))
})
