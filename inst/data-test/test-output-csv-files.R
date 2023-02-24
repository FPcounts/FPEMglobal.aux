################################################################################
###
### Code used to create test versions of output csv files from a complete run.
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

### !!! Set working directory to the directory of this file.
setwd(here::here("data-raw"))

###-----------------------------------------------------------------------------
### * Paths

mwra_1519_src_path <- file.path(Sys.getenv("SHAREPOINT_PDU_FPEM_RESULTS"), "Developing", "20220627-1", "220627_122800_15-19_married")
mwra_1519_src_run_name <- "220914_1519_mw"
mwra_1519_new_path <- file.path("..", "inst", "data-test", "test_output_15-19_married")
mwra_1519_new_run_name <- "220627_122800_15-19_married"

wra_1519_src_path <- file.path(Sys.getenv("SHAREPOINT_PDU_FPEM_RESULTS"), "Developing", "20220627-1", "220627_122800_15-19_all_women")
wra_1519_src_run_name <- "220914_1519all women"
wra_1519_new_path <- file.path("..", "inst", "data-test", "test_output_15-19_all_women")
wra_1519_new_run_name <- "220627_122800_15-19_all_women"

mwra_1549_src_path <- file.path(Sys.getenv("SHAREPOINT_PDU_FPEM_RESULTS"), "Released", "2022", "15-49_mwra")
mwra_1549_src_run_name <- "2022_15-49_mwra"
mwra_1549_new_path <- file.path("..", "inst", "data-test", "test_output_15-49_married")
mwra_1549_new_run_name <- "220425_181439_15-49_married"

wra_1549_src_path <- file.path(Sys.getenv("SHAREPOINT_PDU_FPEM_RESULTS"), "Released", "2022", "15-49_wra")
wra_1549_src_run_name <- "2022_15-49_wra"
wra_1549_new_path <- file.path("..", "inst", "data-test", "test_output_15-49_all_women")
wra_1549_new_run_name <- "220425_181439_15-49_all_women"

src_paths <- c(mwra_1519_src_path, wra_1519_src_path, mwra_1549_src_path, wra_1549_src_path)
src_run_names <- c(mwra_1519_src_run_name, wra_1519_src_run_name, mwra_1549_src_run_name, wra_1549_src_run_name)
new_paths <- c(mwra_1519_new_path, wra_1519_new_path, mwra_1549_new_path, wra_1549_new_path)
new_run_names <- c(mwra_1519_new_run_name, wra_1519_new_run_name, mwra_1549_new_run_name, wra_1549_new_run_name)

###-----------------------------------------------------------------------------
### * Copy

keep_names <- c("Azerbaijan", "Benin", "Kiribati", "Russian Federation", "Martinique",
                "Western Asia", "Eastern Europe", "Southern Asia",
                "Latin America and the Caribbean", "Western Europe")

for (k in seq_along(src_paths)) {
    ## Paths for this iteration
    src_path <- src_paths[k]
    src_run_name <- src_run_names[k]
    new_path <- new_paths[k]
    new_run_name <- new_run_names[k]

    ## 'orig' tables
    src_tbl_orig_dir <- file.path(src_path, "table", "orig")
    new_tbl_orig_dir <- file.path(new_path, "table", "orig")

    ## 'adj' tables
    src_tbl_adj_dir <- file.path(src_path, "table", "adj")
    new_tbl_adj_dir <- file.path(new_path, "table", "adj")

    src_tbl_dirs <- c(src_tbl_orig_dir, src_tbl_adj_dir)
    new_tbl_dirs <- c(new_tbl_orig_dir, new_tbl_adj_dir)

    for (m in seq_along(src_tbl_dirs)) {
        src_tbl_dir <- src_tbl_dirs[m]
        new_tbl_dir <- new_tbl_dirs[m]
        dir.create(new_tbl_dir, recursive = TRUE)

        for (n in grep("_Country_|_UNPDaggregate", dir(src_tbl_dir), value = TRUE)) {
            x <- read.csv(file = file.path(src_tbl_dir, n), check.names = FALSE)
            ## Keep only 5 countries
            x <- x[x$Name %in% keep_names, ]
            new_fname <- gsub(src_run_name, new_run_name, n)
            write.csv(x, file = file.path(new_tbl_dir, new_fname), row.names = FALSE)
        }
    }
}


