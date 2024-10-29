###-----------------------------------------------------------------------------
### * Text Conversion

###-----------------------------------------------------------------------------
### ** Marital Groups

## Get 'long' marital group name from the acronym
expand_marital_group_acronyms <- function(x, return_case = c("lower", "title", "sentence", "upper")) {
    x <- tolower(x)
    x <- match.arg(x, choices = c("mwra", "uwra", "wra", "awra"))
    if (identical(x, "awra")) x <- "wra"
    return_case <- match.arg(return_case)
    basis <- get_std_marital_group_names(return_case = return_case, named = TRUE)
    return(as.character(basis[x])) # Need 'as.character' to remove names.
}

###-----------------------------------------------------------------------------
### ** Locations

##' Standardize country names for manuscripts
##'
##' Standardizes names of countries for use in tables and plots. Uses
##' LaTeX sytax for non-ASCII characters.
##'
##' @param x Vector of country names to be standardized.
##' @return Vector of standardized country names.
##' @author Mark Wheldon
##'
##' @family String utility functions
##'
##' @export
convert_country_names_2_latex <- function(x) {
x[grep("^Bolivia \\(Plurinational State of\\)", x)] <- "Bolivia, Plurinational State of"
x[grep("^C.+e d'Ivoire$", x)] <- "C{\\^o}te d'Ivoire"
x[grep("^China, Hong Kong \\(SAR\\)", x)] <- "China, Hong Kong SAR"
x[grep("^China, Hong Kong Special Administrative Region", x)] <- "China, Hong Kong SAR"
x[grep("^China, Macao \\(SAR\\)", x)] <- "China, Macao SAR"
x[grep("China, Macao Special Administrative Region", x)] <- "China, Macao SAR"
x[grep("^Cura.+ao$ ", x)] <- "Cura{\\,c}ao"
x[grep("^Democratic People's Republic of Korea", x)] <- "Democratic People's Rep. of Korea"
x[grep("^Democratic Republic of the Congo", x)] <- "Democratic Rep. of the Congo"
x[grep("^Democratic Republic of Timor-Leste", x)] <- "Democratic Rep. of Timor-Leste"
x[grep("^Timor-Leste", x)] <- "Democratic Rep.\\ of Timor-Leste"
x[grep("^Iran \\(Islamic Republic of\\)", x)] <- "Iran, Islamic Republic of"
x[grep("^Lao People.+s Democratic Republic", x)] <- "Lao People's Dem. Republic"
x[grep("^R.+nion$", x)] <- "R{\\'e}union"
x[grep("^Saint Lucia", x)] <- "St. Lucia"
x[grep("^Saint Vincent and the Grenadines", x)] <- "St. Vincent and the Grenadines"
x[grep("^The former Yugoslav Republic of Macedonia", x)] <- "TFYR Macedonia"
x[grep("^United Kingdom of Great Britain and Northern Ireland", x)] <- "United Kingdom"
x[grep("^United Republic of Tanzania", x)] <- "United Rep. of Tanzania"
x[grep("^Venezuela \\(Bolivarian Republic of\\)", x)] <- "Venezuela, Bolivarian Republic of"
    return(x)
}


##' Match UN locations country names
##'
##' Converts between the country names in
##' \code{\link[wpp2019]{M49_region_names}} and those in the \pkg{\link{FPEMglobal}} outputs.
##'
##' @param x Vector of names, factor or character.
##' @param convert_to If \dQuote{M49_region_names}, converts \emph{from}
##'     \pkg{\link{FPEMglobal}} names to \code{\link[wpp2019]{M49_region_names}}
##'     names. Converts the other way if \dQuote{FPEMglobal}. Note that this
##'     means \code{x} should be in the opposite format.
##' @return Vector of converted country names, character or factor
##'     following the class of \code{x}.
##' @author Mark Wheldon
##'
##' @family String utility functions
##'
##' @export
convert_M49_region_names <- function(x, convert_to = c("M49_region_names", "FPEMglobal")) {

    x_fac <- is.factor(x)
    if (x_fac) x <- as.character(x)

    convert_to <- match.arg(convert_to)

    names_df <-
        data.frame(M49_region_names = c("Reunion",
                                   "Eswatini",
                                   "Cote d'Ivoire",
                                   "China, Hong Kong SAR",
                                   "China, Macao SAR",
                                   "China, Taiwan Province of China",
                                   "Dem. People's Republic of Korea",
                                   "Curacao",
                                   "Saint-Barthelemy",
                                   "Saint-Martin (French part)",
                                   "Micronesia (Fed. States of)",
                                   "United Kingdom",
                                   "North Macedonia",
                                   NA
                                   ),
                   FPEMglobal = c("R.+union",
                            "Swaziland",
                            "C.+te d.+Ivoire",
                            "China, Hong Kong Special Administrative Region",
                            "China, Macao Special Administrative Region",
                            NA,
                            "Democratic People's Republic of Korea",
                            "Cura.+ao",
                            NA,
                            NA,
                            "Micronesia (Federated States of)",
                            "United Kingdom of Great Britain and Northern Ireland",
                            "The former Yugoslav Republic of Macedonia",
                            "Other non-specified areas"),
                   stringsAsFactors = FALSE)

    if (identical(convert_to, "M49_region_names")) {
        for (i in 1:nrow(names_df)) {
            if (!is.na(names_df[i, "FPEMglobal"])) {
                x[grep(paste0("^", names_df[i, "FPEMglobal"], "$"), x)] <- names_df[i, "M49_region_names"]
            }
        }
    } else if (identical(convert_to, "FPEMglobal")) {
        for (i in 1:nrow(names_df)) {
            if (!is.na(names_df[i, "M49_region_names"])) {
                x[grep(paste0("^", names_df[i, "M49_region_names"], "$"), x)] <- names_df[i, "FPEMglobal"]
            }
        }
    }

    if (x_fac) x <- factor(x)
    return(x)
}


##' Shorten region names for manuscripts
##'
##' Shortens names of regions and subregions for use in tables and plots.
##'
##' @param x Vector of (sub)region names to shorten.
##' @return Vector of shortened (sub)region names.
##' @author Mark Wheldon
##'
##' @family String utility functions
##'
##' @export
shorten_reg_names_manus <- function(x) {
    x[grep("^Federated States of Micronesia", x)] <- "Micronesia, Fed.\\ States of"
    x[grep("^Latin America and the Caribbean"    , x)] <- "LAC"
    x[grep("^Northern America"                   , x)] <- "N. America"
    return(x)
}


##' Convert between SDG region name schemes
##'
##' The SDG Data and Metadata Submission Form (UN Statistics Division) has different versions
##' of SDG region names. This function will convert between them.
##'
##' If \code{subset_and_order = TRUE}, the output is filtered and
##' ordered such that only the region names in the 2024 version of the
##' SDG Annex Data and Metadata submission form are retained, and in
##' the correct order. This will only operate if \code{convert_from =
##' "SDG_Data"}.
##'
##' @param x Vector of region names to convert
##' @param convert_from Direction of conversion; convert from SDG Annex
##'     format or convert to FPEM format?
##' @param subset_and_order Logical; if \code{convert_from =
##'     "SDG_Data"}, filter and order the result according to the
##'     format in the SDG Annex Data and Metadata submission form?
##'     See \dQuote{Details} for more information.
##' @param region_column_name For the data frame method, the name of
##'     the column in \code{x} containing the region names to convert.
##' @return \code{x} converted.
##' @author Mark Wheldon
##' @export
convert_SDG_region_names <- function(x, ...) {
    UseMethod("convert_SDG_region_names")
}

get_SDG_region_data_submission_convert_df <- function() {
    data.frame(
        FPEM = c(
            ## in 'UNPDaggregate' files:
            "World",
            ## in 'SDG_regions_level_1' files:
            "Central Asia and Southern Asia", "Eastern Asia and South-eastern Asia",
            "Latin America and the Caribbean", "Northern America and Europe",
            "Oceania", "Sub-Saharan Africa", "Western Asia and Northern Africa",
            ## in 'SDG_regions_level_2' files:
            ## NB: 'Sub-Saharan Africa' is also in the level 2 files
            "Australia and New Zealand", "Caribbean", "Central America",
            "Central Asia", "Eastern Asia", "Europe", "Northern Africa",
            "Northern America", "Oceania excluding Australia and New Zealand",
            "South-eastern Asia", "South America", "Southern Asia", "Western Asia",
            ## in 'SDG_regions_LLDCs_SIDS' files:
            "Landlocked developing countries (LLDCs)",
            "Small island developing States (SIDS)",
            ## in 'UNPDaggregate' files:
            "Least developed countries"
        ),
        SDG_Data = c("World", "Central and Southern Asia", "Eastern and South-Eastern Asia",
                      "Latin America and the Caribbean", "Europe and Northern America",
                      "Oceania", "Sub-Saharan Africa", "Northern Africa and Western Asia",
                      "Australia and New Zealand", "Caribbean", "Central America",
                      "Central Asia", "Eastern Asia", "Europe", "Northern Africa",
                      "Northern America", "Oceania (exc. Australia and New Zealand)",
                      "South-Eastern Asia", "South America", "Southern Asia", "Western Asia",
                      "Landlocked developing countries", "Small island developing States",
                     "Least developed countries"),
        rank_order = c(1, 6, 9, 12, 16, 13, 2, 3, 14, NA, NA, 7, 10, 17, 4, 18, 15, 11,
                       NA, 8, 5, 19, 21, 20))
}

##' @rdname convert_SDG_region_names
##' @export
convert_SDG_region_names.data.frame <- function(x, convert_from = c("FPEM", "SDG_Data"),
                                                   subset_and_order = FALSE, region_column_name = "name") {

    stopifnot(is.data.frame(x))
    convert_from <- match.arg(convert_from)
    convert_to <- c("FPEM", "SDG_Data")[c("FPEM", "SDG_Data") != convert_from]
    stopifnot(is.logical(subset_and_order))
    stopifnot(region_column_name %in% colnames(x))

    ## KEY to convert region names
    key <- get_SDG_region_data_submission_convert_df()

    ## Check input region names
    x_not_in <- x[!x[[region_column_name]] %in% key[[convert_from]], region_column_name]
    if (length(x_not_in))
        warning("The following elements of 'x' are not members of the '", convert_from,
                "' naming scheme: '", toString(x_not_in), "'.")

    ## Convert the names
    x_idx <- match(x[[region_column_name]], key[[convert_from]])
    x[[region_column_name]] <- key[x_idx, convert_to]

    ## Subset and re-order
    if (subset_and_order) {
        x_rank_order <- key[x_idx, "rank_order"]
        x <- x[order(x_rank_order), , drop = FALSE][!is.na(x_rank_order)[order(x_rank_order)], , drop = FALSE]
    }

    return(x)
}

##' @rdname convert_SDG_region_names
##' @export
convert_SDG_region_names.character <- function(x, convert_from = c("FPEM", "SDG_Data"),
                                                          subset_and_order = FALSE) {
    convert_SDG_region_names(data.frame(name = x),
                                        convert_from = convert_from,
                                        subset_and_order = subset_and_order,
                                        region_column_name = "name")$name
}


##' Convert country classifications to fpemdata format
##'
##' Retrieves country codes and region classifications from
##' \pkg{FPEMglobal} and returns them in \pkg{fpemdata} format.
##'
##' @family fpemdata converters
##' @seealso get_country_classifications
##'
##' @return A \code{\link[tibble]{tibble}} with the requested results.
##' @author Mark Wheldon
##'
##' @export
convert_country_classifications_2_fpemdata <-
    function() {
        verbose <- getOption("FPEMglobal.aux.verbose")
        get_country_classifications(clean_col_names = TRUE) |>
            dplyr::rename(division_numeric_code = iso,
                          name_country = name,
                          name_sub_region = region,
                          name_region = major_area)
        }

###-----------------------------------------------------------------------------
### ** Parameter Names

##' Convert parameter names in code to plain English
##'
##' Converts model parameter names from those used in the R code to
##' human readable, plain English names.
##'
##' @param x Character, R code parameter name.
##' @return Character, plain English name.
##' @author Mark Wheldon
##'
##' @family String utility functions
##'
##' @export
convert_param_name <- function(x) {
    code <- c("omega.c", "T.c", "pmax.c", "Romega.c", "RT.c", "Rmax.c",
               "unmet.intercept.c")
    plain <- c("Pace (CP Any)", "Timing (CP Any)",
               "Aysmptote (CP Any)",
               "Pace (Mod/Trad)", "Timing (Mod/Trad)",
               "Asymptote (Mod/Trad)",
               "Intercept (Z)")
    plain[x == code]
}
