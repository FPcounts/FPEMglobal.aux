###-----------------------------------------------------------------------------
### * meta_data.csv

###-----------------------------------------------------------------------------
### ** 15-49 ("Age Total") Directories

###-----------------------------------------------------------------------------
### *** Married

test_that("get_JAGS_model_param_names works with default argument values on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    x <- get_JAGS_model_param_names(output_dir = test_output_dir)
    expect_true(is.character(x))
    expect_true(length(x) <= dim(get(load(file.path(test_output_dir, "mcmc.array.rda"))))[3])
})
test_that("get_JAGS_model_param_names works with 'abbreviate = FALSE' on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    x <- get_JAGS_model_param_names(output_dir = test_output_dir, abbreviate = FALSE)
    expect_true(is.character(x))
    expect_identical(length(x),
                     dim(get(load(file.path(test_output_dir, "mcmc.array.rda"))))[3])
    })

test_that("get_model_JAGS_txt works with default argument values on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(get_model_JAGS_txt(output_dir = test_output_dir),
                     readLines(system.file("data-test/15-49_married/model.txt",
                                          package = "FPEMglobal.aux")))
})

test_that("get_model_meta_info works with default argument values on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(get_model_meta_info(output_dir = test_output_dir),
                     get(load(system.file("data-test/15-49_married/mcmc.meta.rda",
                                          package = "FPEMglobal.aux"))))
})

test_that("get_country_index works with default argument values on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_s3_class(get_country_index(output_dir = test_output_dir),
                    "tbl_df")
})

test_that("get_global_mcmc_args works with default argument values on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(get_global_mcmc_args(output_dir = test_output_dir),
                    get(load(system.file("data-test/15-49_married/global_mcmc_args.RData",
                                          package = "FPEMglobal.aux"))))
})

test_that("get_combine_runs_args works with default argument values on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_error(get_combine_runs_args(output_dir = test_output_dir),
                 "not an all women run")
})

test_that("get_global_run_args works with default argument values on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(get_global_run_args(output_dir = test_output_dir),
                    get(load(system.file("data-test/15-49_married/global_mcmc_args.RData",
                                          package = "FPEMglobal.aux"))))
})

test_that("get_global_post_process_args works with default argument values on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(get_global_post_process_args(output_dir = test_output_dir),
                    get(load(system.file("data-test/15-49_married/post_process_args.RData",
                                          package = "FPEMglobal.aux"))))
})

test_that("get_run_name works with default argument values on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(get_run_name(output_dir = test_output_dir), "220425_181439_15-49_married")
})

test_that("get_marital_group works with default argument values on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(get_marital_group(output_dir = test_output_dir), "married")
})

test_that("is_all_women_run works with default argument values on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(is_all_women_run(output_dir = test_output_dir), FALSE)
})

test_that("is_married_women_run works with default argument values on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(is_married_women_run(output_dir = test_output_dir), TRUE)
})

test_that("is_unmarried_women_run works with default argument values on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_married", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(is_unmarried_women_run(output_dir = test_output_dir), FALSE)
})

###-----------------------------------------------------------------------------
### *** All Women

test_that("get_JAGS_model_param_names works with default argument values on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_error(get_JAGS_model_param_names(output_dir = test_output_dir))
})
test_that("get_JAGS_model_param_names works with 'abbreviate = FALSE' on a 15-49 married directory", {
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))
    expect_error(get_JAGS_model_param_names(output_dir = test_output_dir, abbreviate = FALSE))
    })

test_that("get_model_meta_info works with default argument values on a 15-49 all women directory", {
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(get_model_meta_info(output_dir = test_output_dir),
                     get(load(system.file("data-test/15-49_all_women/mcmc.meta.rda",
                                          package = "FPEMglobal.aux"))))
})

test_that("get_country_index works with default argument values on a 15-49 all women directory", {
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_error(get_country_index(output_dir = test_output_dir),
                 "all women run")
})

test_that("get_global_mcmc_args works with default argument values on a 15-49 all_women directory", {
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_error(get_global_mcmc_args(output_dir = test_output_dir),
                 "all women run")

})

test_that("get_combine_runs_args works with default argument values on a 15-49 all_women directory", {
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(get_combine_runs_args(output_dir = test_output_dir),
                    get(load(system.file("data-test/15-49_all_women/combine_runs_args.RData",
                                          package = "FPEMglobal.aux"))))
})

test_that("get_global_run_args works with default argument values on a 15-49 all_women directory", {
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(get_global_run_args(output_dir = test_output_dir),
                    get(load(system.file("data-test/15-49_all_women/combine_runs_args.RData",
                                          package = "FPEMglobal.aux"))))
})

test_that("get_global_post_process_args works with default argument values on a 15-49 all_women directory", {
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_error(get_global_post_process_args(output_dir = test_output_dir),
                 "all women run")
})

test_that("get_run_name works with default argument values on a 15-49 all_women directory", {
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(get_run_name(output_dir = test_output_dir), "220425_181439_15-49_all_women")
})

test_that("get_marital_group works with default argument values on a 15-49 all_women directory", {
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(get_marital_group(output_dir = test_output_dir), "all_women")
})

test_that("is_all_women_run works with default argument values on a 15-49 all_women directory", {
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(is_all_women_run(output_dir = test_output_dir), TRUE)
})

test_that("is_married_women_run works with default argument values on a 15-49 all_women directory", {
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(is_married_women_run(output_dir = test_output_dir), FALSE)
})

test_that("is_unmarried_women_run works with default argument values on a 15-49 all_women directory", {
    test_output_dir <-
        system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
    expect_true(dir.exists(test_output_dir))

    expect_identical(is_unmarried_women_run(output_dir = test_output_dir), FALSE)
})
