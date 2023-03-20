################################################################################
###
### DATE: 2023-02-27
###
### DESC: Test 'get_csv_res` all combinations of arguments.
###
################################################################################

library(testthat)
library(FPEMglobal.aux)

###-----------------------------------------------------------------------------
### * get_csv_results

## All options
args_grid <-
    expand.grid(aggregate = c("country", "UNPDaggregate"),
                stat = c("prop", "count", "ratio"),
                adjusted = c("orig", "adj", "sub_adj"),
                clean_col_names = c(TRUE, FALSE),
                years_as_midyear = c(TRUE, FALSE),
                add_country_classifications = c(TRUE, FALSE),
                table_format = c("long", "wide", "raw"),
                sort = c(TRUE, FALSE),
                verbose = c(TRUE, FALSE),
                stringsAsFactors = FALSE)

## Remove dis-allowed combinations
args_grid <-
    args_grid[!with(args_grid, add_country_classifications & aggregate != "country"), ]

## Inner function
run_res <- function(z, args_grid) {
    library(testthat)
    library(FPEMglobal.aux)
    out <- try(do.call("get_csv_res",
                       args = do.call("list", args_grid[z, ])),
               "tbl_df")
    if (!inherits(out, "try-error")) return(NULL)
    else return(list(args = args_grid[z, ],
                     error = out))
}

###-----------------------------------------------------------------------------
### ** 15-49 ("Age Total") Directories

###-----------------------------------------------------------------------------
### *** Married

## Output directory
test_output_dir <-
    system.file("data-test/15-49_married", package = "FPEMglobal.aux")
expect_true(dir.exists(test_output_dir))

args_grid_mwra <- cbind(output_dir = test_output_dir, args_grid)

cl <- parallel::makeCluster(parallelly::availableCores(omit = 1))
parallel::clusterExport(cl, varlist = c("args_grid_mwra", "run_res"), envir = environment())
mwra_res <- parallel::parLapply(cl = cl, X = seq_len(nrow(args_grid_mwra)),
                                fun = "run_res", args_grid = args_grid_mwra)
parallel::stopCluster(cl)

mwra_res <- Filter(Negate(is.null), mwra_res)

###-----------------------------------------------------------------------------
### *** All women

## Output directory
test_output_dir <-
    system.file("data-test/15-49_all_women", package = "FPEMglobal.aux")
expect_true(dir.exists(test_output_dir))

args_grid_wra <- cbind(output_dir = test_output_dir, args_grid)

cl <- parallel::makeCluster(parallelly::availableCores(omit = 1))
parallel::clusterExport(cl, varlist = c("args_grid_wra", "run_res"), envir = environment())
wra_res <- parallel::parLapply(cl = cl, X = seq_len(nrow(args_grid_wra)),
                                fun = "run_res", args_grid = args_grid_wra)
parallel::stopCluster(cl)

wra_res <- Filter(Negate(is.null), wra_res)

###-----------------------------------------------------------------------------
### ** CHECK

###-----------------------------------------------------------------------------
### *** Married Women

if (length(mwra_res)) mwra_res

###-----------------------------------------------------------------------------
### *** All women

if (length(wra_res)) wra_res

###-----------------------------------------------------------------------------
### *** Error

if (length(mwra_res) || length(wra_res)) stop("Some errors found: see above.")


