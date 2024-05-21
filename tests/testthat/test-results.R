### Get quantiles of model parameters, etc.

test_that("'get_model_param_quantiles' works with default arguments", {

    test_par_ciq <- function(dir) {
        expect_true(dir.exists(dir))
        expect_error(x <- get_model_param_quantiles(output_dir = dir), NA)
        expect_type(x, "double")
        expect_identical(class(x), "array")
        expect_identical(length(dim(x)), 3L)
        expect_identical(dimnames(x)[[3]], c("2.5%", "50%", "97.5%"))
    }

    ## 15-19, Married
    test_par_ciq(system.file("data-test/15-19_married", package = "FPEMglobal.aux"))

    ## 15-49, Married
    test_par_ciq(system.file("data-test/15-49_married", package = "FPEMglobal.aux"))
})


test_that("'get_model_param_quantiles' works when a subset of percentiles are requested", {

    test_par_ciq <- function(dir) {
        expect_true(dir.exists(dir))
        expect_error(x <- get_model_param_quantiles(output_dir = dir, percentiles = c(50)), NA)
        expect_type(x, "double")
        expect_identical(class(x), "array")
        expect_identical(length(dim(x)), 3L)
        expect_identical(dimnames(x)[[3]], c("50%"))
    }

    ## 15-19, Married
    test_par_ciq(system.file("data-test/15-19_married", package = "FPEMglobal.aux"))

    ## 15-49, Married
    test_par_ciq(system.file("data-test/15-49_married", package = "FPEMglobal.aux"))
})


test_that("'get_model_param_quantiles' works when additional percentiles are requested", {

    test_par_ciq <- function(dir) {
        expect_true(dir.exists(dir))
        expect_error(x <- get_model_param_quantiles(output_dir = dir,
                                                    percentiles = c(50, 60)), NA)
        expect_type(x, "double")
        expect_identical(class(x), "array")
        expect_identical(length(dim(x)), 3L)
        expect_identical(dimnames(x)[[3]], c("50%", "60%"))
    }

    ## 15-19, Married
    output_dir <-system.file("data-test/15-19_married", package = "FPEMglobal.aux")
    test_par_ciq(output_dir)
    ## '50%' percentile should be the same:
    expect_identical(get_model_param_quantiles(output_dir = output_dir,
                                               percentiles = c(50, 60))[,,"50%"],
                     get_model_param_quantiles(output_dir = output_dir,
                                               percentiles = c(50))[,,"50%"])

    ## 15-49, Married
    test_par_ciq(system.file("data-test/15-49_married", package = "FPEMglobal.aux"))
})
