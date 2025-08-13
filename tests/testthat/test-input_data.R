### Test `get_input_data()`

test_get_input_data <- function(od) {
    expect_true(dir.exists(od))
    param_grid <- expand.grid(variant = c("raw", "preprocessed", "to_model"),
                              version = c("used", "installed"),
                              stringsAsFactors = FALSE)

    for (i in 1:nrow(param_grid)) {
        test_that(paste0("get_input_data( variant = ", param_grid[i, "variant"],
                         ", version = ", param_grid[i, "version"], ") works on ",
                         basename(od)), {
                             x <- get_input_data(output_dir = od,
                                                 variant = param_grid[i, "variant"],
                                                 version = param_grid[i, "version"])
                             expect_s3_class(x, "tbl_df")
                         })
    }
}


for (age in c("15-49", "15-19")) {
    for (marr in c("married")) {
        test_get_input_data(
            od = system.file(
                file.path("data-test", paste0(age, "_", marr)), package = "FPEMglobal.aux"))
    }
}
