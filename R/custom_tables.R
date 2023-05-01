###-----------------------------------------------------------------------------
### * Custom Tables

##' Format table columns containing confidence intervals
##'
##' Replace columns with lower, median, and upper limits of CIs with
##' single columns containing formatted cells like "med (l, u)".
##'
##' @param tbl A matrix or data frame.
##' @param col_triples A list of triples giving column locations that
##'     should be combined.
##' @param ... Passed to \code{\link{format}}. You could pass
##'     \code{digits} or \code{trim} here, for example.
##' @return A data frame.
##' @author Mark Wheldon
##' @export
format_CIs_table <- function(tbl, col_triples, round_digits = 1, ...) {

    if(!is.recursive(col_triples)) {
        if(identical(0, length(col_triples) %% 3)) {
            z <- list()
            for(x in seq(from = 1, to = length(col_triples), by = 3)) {
                z <- c(z, list(col_triples[x:(x + 2)]))
            }
            col_triples <- z
        } else {
            col_triples <- as.list(col_triples)
        }
    }

    cols_to_keep <- !(1:ncol(tbl) %in% unlist(col_triples))

    ncols_to_keep <- sum(cols_to_keep)
    ncols_new_tbl <- ncols_to_keep + length(col_triples)

    new_tbl <- data.frame(matrix(NA, ncol = ncol(tbl), nrow = nrow(tbl)))
    colnames(new_tbl) <- colnames(tbl)

    if(ncols_to_keep > 0) {
        new_tbl[,cols_to_keep] <- tbl[,cols_to_keep]
    }

    for(i in seq_along(col_triples)) {

        tbl_cols <-
            round(tbl[, col_triples[[i]], drop = FALSE],
                  digits = round_digits)

        new_tbl[, col_triples[[i]][1]] <-
            paste0(format(tbl_cols[, 2, drop = TRUE], ...),
                   " (", format(tbl_cols[,1, drop = TRUE], ...),
                   ", ", format(tbl_cols[,3, drop = TRUE], ...), ")"
                   )

    }

    new_tbl <-
        new_tbl[,sort(c(which(cols_to_keep),
                        sapply(col_triples, "[[", 1)
                        ))]

    return(new_tbl)

    }
