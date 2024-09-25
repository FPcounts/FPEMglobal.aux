################################################################################
###
### Code used to create test versions of output csv files from a complete run.
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

### !!! Set working directory to the directory of this file.
setwd(here::here("data-raw"))

## Need to know where results are kept on SharePoint. Use system environment variable.
if (identical(Sys.getenv("USERPROFILE"), "")) stop("Env. var. 'USERPROFILE' is undefined.")

## Check paths exist.
valid_path <- function(...) {
    path <- file.path(...)
    path_passed <- deparse(substitute(path))
    if (dir.exists(path)) return(path)
    else stop("Path '", path, "' [", path_passed, "] does not exist.")
}

## Results taken from here:
FPEM_res_path_1519 <-
    valid_path(Sys.getenv("USERPROFILE"), "United Nations", "DESA-POP - PDU",
               "FPEM", "Results", "Developing", "2022", "20220627-1 (RES ONLY)")
FPEM_res_path_1549 <-
    valid_path(Sys.getenv("USERPROFILE"), "United Nations", "DESA-POP - PDU",
               "FPEM", "Results", "Released", "2022")

###-----------------------------------------------------------------------------
### * Paths

mwra_1519_src_path <- valid_path(FPEM_res_path_1519, "220627_122800_15-19_married")
mwra_1519_src_run_name <- "220914_1519_mw"
mwra_1519_new_path <- file.path("..", "inst", "data-test", "15-19_married")
mwra_1519_new_run_name <- "220627_122800_15-19_married"

wra_1519_src_path <- valid_path(FPEM_res_path_1519, "220627_122800_15-19_all_women")
wra_1519_src_run_name <- "220914_1519all women"
wra_1519_new_path <- file.path("..", "inst", "data-test", "15-19_all_women")
wra_1519_new_run_name <- "220627_122800_15-19_all_women"

mwra_1549_src_path <- valid_path(FPEM_res_path_1549, "15-49_mwra")
mwra_1549_src_run_name <- "2022_15-49_mwra"
mwra_1549_new_path <- file.path("..", "inst", "data-test", "15-49_married")
mwra_1549_new_run_name <- "2022_15-49_mwra"

wra_1549_src_path <- valid_path(FPEM_res_path_1549, "15-49_wra")
wra_1549_src_run_name <- "2022_15-49_wra"
wra_1549_new_path <- file.path("..", "inst", "data-test", "15-49_all_women")
wra_1549_new_run_name <- "2022_15-49_wra"

src_paths <- c(mwra_1519_src_path, wra_1519_src_path, mwra_1549_src_path, wra_1549_src_path)
src_run_names <- c(mwra_1519_src_run_name, wra_1519_src_run_name, mwra_1549_src_run_name, wra_1549_src_run_name)
new_paths <- c(mwra_1519_new_path, wra_1519_new_path, mwra_1549_new_path, wra_1549_new_path)
new_run_names <- c(mwra_1519_new_run_name, wra_1519_new_run_name, mwra_1549_new_run_name, wra_1549_new_run_name)

###-----------------------------------------------------------------------------
### * Copy

keep_names <- c("Azerbaijan", "Benin", "Kiribati", "Russian Federation", "Martinique",
                "Western Asia", "Eastern Europe", "Southern Asia",
                "Latin America and the Caribbean", "Western Europe")

keep_iso_c <- c(31, 204, 296, 643, 474)

for (k in seq_along(src_paths)) {
    ## Paths for this iteration
    src_path <- src_paths[k]
    src_run_name <- src_run_names[k]
    new_path <- new_paths[k]
    new_run_name <- new_run_names[k]

    ## input data
    for (suff in c("raw", "preprocessed", "to_model")) {
        fname <- paste0("dataCPmodel_input_", suff, ".csv")
        fpath <- file.path(src_path, fname)
        iso_col <- switch(suff,
                          raw = "ISO.code",
                          preprocessed = "ISO.code",
                          to_model = "iso.j",
                          stop("error in 'suff'."))
        if (file.exists(fpath)) {
            x <- readr::read_csv(file = fpath,
                                 name_repair = "minimal", show_col_types = FALSE)
            x <- x[x[[iso_col]] %in% keep_iso_c, ]
            write.csv(x, file = file.path(new_path, fname), row.names = FALSE)
        } else {
            warning("File '", fpath, "' not found.")
        }
    }

    ## NOTE: files in the "data" subdir are copied manually.

    ## mcmc array
    mcmc_file_path <- file.path(src_path, "mcmc.array.rda")
    if (file.exists(mcmc_file_path)) {
    mcmc.array <- get(load(mcmc_file_path))
    dims <- dim(mcmc.array)
    ## Only keep 5 trajectories and 2 chains. Don't subset the
    ## countries because that would result in inconsistencies with all
    ## the meta data and don't want to have to modify that.
    mcmc.array <- mcmc.array[1:min(5, dims[1]), 1:min(2, dims[2]), , drop = FALSE]
    save(mcmc.array, file = file.path(new_path, "mcmc.array.rda"))
    }

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


