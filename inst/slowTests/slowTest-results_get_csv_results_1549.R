################################################################################
###
### DATE: 2023-02-27
###
### DESC: Test 'get_csv_res` all combinations of arguments.
###
################################################################################

library(FPEMglobal.aux)
library(parallel)
library(parallelly)
library(testthat)

###-----------------------------------------------------------------------------
### * get_csv_results

## All options
args_grid <-
    expand.grid(aggregate = list("country", "UNPDaggregate",
                                 c("country", "UNPDaggregate")),
                stat = list("prop", "count", "ratio",
                            c("prop", "count"),
                            c("prop", "ratio"),
                            c("count", "ratio"),
                            c("prop", "count", "ratio")),
                adjusted = c("orig", "adj", "sub_adj"),
                clean_col_names = c(TRUE, FALSE),
                indicator_name_format = c("clean", "traj_array", "csv_results_file_names"),
                years_as_midyear = c(TRUE, FALSE),
                add_country_classifications = c(TRUE, FALSE),
                table_format = c("long", "wide", "raw"),
                sort = c(TRUE, FALSE),
                stringsAsFactors = FALSE)

## Remove dis-allowed combinations
stat_mult_idx <- sapply(args_grid$stat, function(z) length(z)) > 1
args_grid <-
    args_grid[!with(args_grid,
                    (add_country_classifications & aggregate != "country") |
                    (stat_mult_idx & table_format != "long")), ]

rownames(args_grid) <- NULL

## Inner function
run_res <- function(z, args_grid, row_1_names) {
    library(testthat)
    library(FPEMglobal.aux)
    out <- try(do.call("get_csv_res",
                       args = lapply(do.call("list", args_grid[z, ]), "unlist")))
    if (!inherits(out, "try-error")) return(NULL)
    else return(list(args = args_grid[z, ],
                     error = out))
}

###-----------------------------------------------------------------------------
### ** 15-49 ("Age Total") Directories

message("15-49 ('Age Total') Directories")

###-----------------------------------------------------------------------------
### *** Married

message("  Married")

## Output directory
test_output_dir <-
    system.file("data-test/15-49_married", package = "FPEMglobal.aux")
expect_true(dir.exists(test_output_dir))

args_grid_mwra <- cbind(output_dir = test_output_dir, args_grid)

cl <- parallel::makeCluster(parallelly::availableCores(omit = 2))

message("    Testing ", nrow(args_grid_mwra), " argument combinations with ",
        length(cl), " nodes")

parallel::clusterExport(cl, varlist = c("args_grid_mwra", "run_res"), envir = environment())
mwra_res <- parallel::parLapply(cl = cl, X = seq_len(nrow(args_grid_mwra)),
                                fun = "run_res", args_grid = args_grid_mwra)
parallel::stopCluster(cl)

mwra_res <- Filter(Negate(is.null), mwra_res)

### CHECK
message("      Error checks (if no output, no errors):")
if (length(mwra_res)) mwra_res

###-----------------------------------------------------------------------------
### *** All women

message("  All women")

## Output directory
test_output_dir <-
    system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
expect_true(dir.exists(test_output_dir))

args_grid_wra <- cbind(output_dir = test_output_dir, args_grid)

cl <- parallel::makeCluster(parallelly::availableCores(omit = 2))

message("    Testing ", nrow(args_grid_wra), " argument combinations with ",
        length(cl), " nodes")

parallel::clusterExport(cl, varlist = c("args_grid_wra", "run_res"), envir = environment())
wra_res <- parallel::parLapply(cl = cl, X = seq_len(nrow(args_grid_wra)),
                                fun = "run_res", args_grid = args_grid_wra)
parallel::stopCluster(cl)

wra_res <- Filter(Negate(is.null), wra_res)

### CHECK
message("      Error checks (if no output, no errors):")
if (length(wra_res)) wra_res

###-----------------------------------------------------------------------------
### *** Married and All women

message("  Married + All women")

## Output directory
test_output_dir <-
    list(c(married = system.file("data-test/15-49_married", package = "FPEMglobal.aux"),
           all_women = system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")))
expect_true(all(dir.exists(unlist(test_output_dir))))

## Remove dis-allowed combinations
args_grid_both_mar <- args_grid[args_grid$table_format == "long", ]
rownames(args_grid_both_mar) <- NULL

## Use 'tibble' to make the column for 'output_dir', to make sure it's a list column.
args_grid_both_mar_both_mar <-
    tibble::tibble(output_dir = test_output_dir, args_grid_both_mar)

## Now turn it into a regular data frame so that the extraction in
## 'run_res()' works correctly -- i.e., the names of the elements come
## out correctly.
args_grid_both_mar_both_mar <- as.data.frame(args_grid_both_mar_both_mar)

## CHECK
stopifnot(is.list(args_grid_both_mar_both_mar[1,1]))
stopifnot(identical(length(args_grid_both_mar_both_mar[1,1][[1]]), 2L))

cl <- parallel::makeCluster(parallelly::availableCores(omit = 2))

message("    Testing ", nrow(args_grid_both_mar_both_mar), " argument combinations with ",
        length(cl), " nodes")

parallel::clusterExport(cl, varlist = c("args_grid_both_mar_both_mar", "run_res"),
                        envir = environment())
both_mar_res <-
    parallel::parLapply(cl = cl, X = seq_len(nrow(args_grid_both_mar_both_mar)),
                        fun = "run_res", args_grid = args_grid_both_mar_both_mar)
parallel::stopCluster(cl)

both_mar_res <- Filter(Negate(is.null), both_mar_res)

### CHECK
message("      Error checks (if no output, no errors):")
if (length(both_mar_res)) both_mar_res

###-----------------------------------------------------------------------------
### *** CHECK

###-----------------------------------------------------------------------------
### **** Error

if (length(mwra_res) || length(wra_res) || length(both_mar_res))
    stop("  Some errors found: see above.")


