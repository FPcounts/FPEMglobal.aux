###-----------------------------------------------------------------------------
### * Constants

###-----------------------------------------------------------------------------
### ** Sting Constants

## Marital group names and abbreviations
## Used in conversion from short/acronym marital group names to long names.
get_std_marr_group_names <- function(named = TRUE) {
    out <- c("mwra" = "married", "uwra" = "unmarried", "wra" = "all_women")
    if (named) return(out)
    else return(unname(out))
}


