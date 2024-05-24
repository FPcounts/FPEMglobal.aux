test_that("Conversion of SDG region names works", {
    for (FROM in c("FPEM", "SDG_Data")) {

        ## DATA FRAMES ---

        for (SAO in c(TRUE, FALSE)) {

            ## Names that don't exist
            expect_warning(convert_SDG_region_names(
                x = data.frame(name = c("World", "Nowhereville"), z = 1:2),
                convert_from = FROM, subset_and_order = SAO,
                region_column_name = "name"),
                regexp = "The following elements of 'x' are not members")

            ## Names exist
            expect_identical(
                convert_SDG_region_names(
                    x = data.frame(name = c("World", "Small island developing States (SIDS)"),
                                   z = 1:2),
                    convert_from = "FPEM"),
                data.frame(name = c("World", "Small island developing States"), z = 1:2))

            ## One row
            expect_identical(convert_SDG_region_names(x = data.frame(name = "World", z = 1:2),
                                                                 convert_from = FROM,
                                                                 subset_and_order = SAO,
                                                                 region_column_name = "name"),
                             data.frame(name = "World", z = 1:2))

            ## One column
            expect_identical(convert_SDG_region_names(x = data.frame(name = "World"),
                                                                 convert_from = FROM,
                                                                 subset_and_order = SAO,
                                                                 region_column_name = "name"),
                             data.frame(name = "World"))
        }

        ## VECTORS ---

            ## Names that don't exist
            expect_warning(convert_SDG_region_names(
                x = c("World", "Nowhereville"),
                convert_from = FROM, subset_and_order = SAO),
                regexp = "The following elements of 'x' are not members")

            ## Names exist
            expect_identical(
                convert_SDG_region_names(
                    x = c("World", "Small island developing States (SIDS)"),
                    convert_from = "FPEM"),
                c("World", "Small island developing States"))
    }
})
