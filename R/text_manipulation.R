###-----------------------------------------------------------------------------
### * Text Manipulation

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
make_c_names_manus <- function(x) {
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
##' \code{\link[wpp2019]{UNlocations}} and those in the \pkg{\link{FPEMglobal}} outputs.
##'
##' @param x Vector of names, factor or character.
##' @param return_names If \dQuote{UNlocations}, converts \emph{from}
##'     \pkg{\link{FPEMglobal}} names to \code{\link[wpp2019]{UNlocations}}
##'     names. Converts the other way if \dQuote{FPEMglobal}. Note that this
##'     means \code{x} should be in the opposite format.
##' @return Vector of converted country names, character or factor
##'     following the class of \code{x}.
##' @author Mark Wheldon
##'
##' @family String utility functions
##'
##' @export
match_UNlocations <- function(x, return_names = c("UNlocations", "FPEMglobal")) {

    x_fac <- is.factor(x)
    if (x_fac) x <- as.character(x)

    return_names <- match.arg(return_names)
    from_names <- c("UNlocations", "FPEMglobal")

    names_df <-
        data.frame(UNlocations = c("Reunion",
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

    if (identical(return_names, "UNlocations")) {
        for (i in 1:nrow(names_df)) {
            if (!is.na(names_df[i, "FPEMglobal"])) {
                x[grep(paste0("^", names_df[i, "FPEMglobal"], "$"), x)] <- names_df[i, "UNlocations"]
            }
        }
    } else if (identical(return_names, "FPEMglobal")) {
        for (i in 1:nrow(names_df)) {
            if (!is.na(names_df[i, "UNlocations"])) {
                x[grep(paste0("^", names_df[i, "UNlocations"], "$"), x)] <- names_df[i, "FPEMglobal"]
            }
        }
    }

    if (x_fac) x <- factor(x)
    return(x)
}


##' Standardize (sub)region names for manuscripts
##'
##' Standardizes names of regions and subregions for use in tables and plots.
##'
##' @param x Vector of country names to be standardized.
##' @return Vector of standardized country names.
##' @author Mark Wheldon
##'
##' @family String utility functions
##'
##' @export
make_reg_names_manus <- function(x) {
    x[grep("^Federated States of Micronesia", x)] <- "Micronesia, Fed.\\ States of"
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
    x[grep("^Latin America and the Caribbean"    , x)] <- "LAC"
    x[grep("^Northern America"                   , x)] <- "N. America"
    return(x)
    }


##' Return names of members of aggregate families
##'
##' Returns a vector with the names of the members of various
##' aggregate families, e.g., UNPD aggregates, World Bank,
##' etc.
##'
##' \emph{Note:} The names returned are the aggregate names,
##' e.g., \dQuote{Eastern Africa}, not the names of the countries in
##' the aggregates.
##'
##' @param family Code naming the aggregate family.
##' @return Character vector of family members.
##' @author Mark Wheldon
##'
##' @family String utility functions
##'
##' @export
get_aggregate_names <- function(family = c("geog_maj", "geog_subr", "development_v1", "development_v2",
                                           "sdg1", "sdg2", "sdg_geog", "wb", "fp2020")) {

    family <- match.arg(family)

    if (identical(family, "geog_maj")) {
        c("Africa", "Asia", "Europe", "Latin America and the Caribbean", "Northern America", "Oceania")

    } else if (identical(family, "geog_subr")) {
        c("Northern Africa", "Eastern Africa", "Middle Africa", "Western Africa",
          "Southern Africa",
          "Central Asia", "Eastern Asia", "South-eastern Asia", "Western Asia", "Southern Asia",
          "Northern Europe", "Eastern Europe", "Western Europe", "Southern Europe",
          "Caribbean", "Central America", "South America",
          "Northern America",
          "Mela-Micro-Polynesia", "Australia and New Zealand")

    } else if (identical(family, "sdg1")) {
        c("Sub-Saharan Africa",
          "Western Asia and Northern Africa",
          "Central Asia and Southern Asia",
          "Eastern Asia and South-eastern Asia",
          "Latin America and the Caribbean",
          "Oceania",
          "Northern America and Europe")

    } else if (identical(family, "sdg2")) {
        ## NB: 'Sub-Saharan Africa' is also in the level 2 files
       c("Australia and New Zealand", "Caribbean", "Central America",
        "Central Asia", "Eastern Asia", "Europe", "Northern Africa",
        "Northern America", "Oceania excluding Australia and New Zealand",
        "South-eastern Asia", "South America", "Southern Asia", "Western Asia")

    } else if (identical(family, "sdg_geog")) {
        c("Landlocked developing countries (LLDCs)",
          "Small island developing States (SIDS)")

    } else if (identical(family, "wb")) {
        c("High-income countries", "Upper-middle-income countries", "Middle-income countries",
          "Lower-middle-income countries", "Low-income countries", "No income group")

    } else if (identical(family, "development_v1")) {
        c("Developed countries", "Developing countries", "Developing (excl. China)")

    } else if (identical(family, "development_v2")) {
        c("More developed countries", "Less developed countries", "Least developed countries",
          "Other developing countries", "Less developed countries, excluding China")

    } else if (identical(family, "fp2020")) {
        "FP2020 69 Countries"
    }
}


##' Convert between SDG region name schemes
##'
##' The SDG Data and Metadata Submission Form (UN Statistics Division) has different versions
##' of SDG region names. This function will convert between them.
##'
##' If \code{subset_and_order = TRUE}, the output is filtered and
##' ordered such that only the region names in the 2024 version of the
##' SDG Annex Data and Metadata submission form are retained, and in
##' the correct order. This will only operate if \code{convert_to =
##' "SDG_Data"}.
##'
##' @param x Vector of region names to convert
##' @param convert_to Direction of conversion; convert to SDG Annex
##'     format or convert to FPEM format?
##' @param subset_and_order Logical; if \code{convert_to =
##'     "SDG_Data"}, filter and order the result according to the
##'     format in the SDG Annex Data and Metadata submission form?
##'     See \dQuote{Details} for more information.
##' @param region_column_name For the data frame method, the name of
##'     the column in \code{x} containing the region names to convert.
##' @return \code{x} converted.
##' @author Mark Wheldon
##' @export
convert_SDG_region_names_2_SDG_Data <- function(x, ...) {
    UseMethod("convert_SDG_region_names_2_SDG_Data")
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

##' @rdname convert_SDG_region_names_2_SDG_Data
##' @export
convert_SDG_region_names_2_SDG_Data.data.frame <- function(x, convert_to = c("SDG_Data", "FPEM"),
                                                   subset_and_order = FALSE, region_column_name = "name") {

    stopifnot(is.data.frame(x))
    convert_to <- match.arg(convert_to)
    convert_from <- c("SDG_Data", "FPEM")[c("SDG_Data", "FPEM") != convert_to]
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

##' @rdname convert_SDG_region_names_2_SDG_Data
##' @export
convert_SDG_region_names_2_SDG_Data.character <- function(x, convert_to = c("SDG_Data", "FPEM"),
                                                          subset_and_order = FALSE) {
    convert_SDG_region_names_2_SDG_Data(data.frame(name = x),
                                        convert_to = convert_to,
                                        subset_and_order = subset_and_order)$name
}


##' Convert country classifications to fpemdata format
##'
##' Retrieves country codes and region classifications from
##' \pkg{FPEMglobal} and returns them in \pkg{fpemdata} format.
##'
##' @family fpemdata converters
##' @seealso get_country_classifications
##'
##' @inheritParams get_csv_res
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


###-----------------------------------------------------------------------------
### ** Marital Groups

## Get 'long' marital group name from the acronym
convert_marital_group_names <- function(x, return_case = c("lower", "title", "sentence", "upper")) {
    x <- tolower(x)
    x <- match.arg(x, choices = c("mwra", "uwra", "wra", "awra"))
    if (identical(x, "awra")) x <- "wra"
    return_case <- match.arg(return_case)
    basis <- get_std_marital_group_names(return_case = return_case, named = TRUE)
    return(as.character(basis[x])) # Need 'as.character' to remove names.
}


###-----------------------------------------------------------------------------
### ** Indicators and Columns

##' Clean indicator names
##'
##' This function takes out spaces and makes the whole string lower
##' case. This is not quite the same as lower snake case as spaces are
##' not replaced with underscores, rather they are just removed
##' resulting in a single unbroken string.
##'
##' @param indicator Character string to \dQuote{clean} up.
##' @return Character string.
##' @author Mark Wheldon
##'
##' @family String utility functions
##'
##' @examples
##'
##' ## Known indicators are:
##' get_traj_array_indicator_names()
##'
##' @export
clean_indic_names <- function(indicator) {
    clean <- function(x) {
        tolower(gsub("[ ]+", "_", x))
    }
    ## regexp <- paste(get_traj_array_indicator_names(), collapse = "|")
    ## match_list <- gregexpr(pattern = regexp, text = indicator)
    ## mapply(function(as.list(ind), mat) {

    ## },
    ## indicator, match_list,
    ## SIMPLIFY = TRUE, USE.NAMES = FALSE)
    clean(indicator)
}


## Round down years to nearest integer?
round_down_years <- function(x) {
    out <- floor(as.numeric(x))
    if (is.character(x)) return(as.character(out))
    else return(out)
}

## Put years in mid-year format
put_years_in_mid_year_fmt <- function(x) {
    out <- as.numeric(round_down_years(x)) + 0.5
    if (is.character(x)) return(as.character(out))
    else return(out)
}



## Turn strings into lower_snake_case.  Spaces and full stops are
## replaced with underscores and the whole thing is put in lower case.
lower_snake_casify <- function(x, use_make.names = TRUE) {
    if (use_make.names) x <- base::make.names(x)
    x <- gsub("[ .\n\r]+", "_", x)
    x <- gsub("_+", "_", x)
    tolower(x)
}


##' Clean names of columns in data frames
##'
##' Reformats names to lower snake case by replacing spaces and other
##' non-letter characters (e.g., \dQuote{.}) with underscores. If
##' \code{use_make.names = TRUE}, \code{\link[base]{make.names}} is
##' run \emph{first}. In addition, the following substitutions are
##' made \emph{after} all re-formats:
##' \describe{
##' \item{\code{iso_code}}{replaced with \code{iso}}
##' \item{\code{country}, \code{country_or_area}}{replaced with \code{name}}
##' }
##'
##' If \code{clean_indicator_names} is \code{FALSE}, any strings
##' matching the known standard indicator names are \emph{not}
##' cleaned. The known standard indicator names are defined by
##' \code{\link{get_traj_array_indicator_names}}.
##'
##' @param x Object for which a method is defined (e.g., character
##'     string or data frame).
##' @param use_make.names Logical; apply
##'     \code{\link[base]{make.names}} to \code{x} \emph{before} doing
##'     anything else?
##' @param clean_indicator_names Logical; should the procedure be
##'     applied to standard indicator names as well? See
##'     \dQuote{Details} for more information.
##' @return Formatted version of \code{x}.
##' @author Mark Wheldon
##'
##' @family String utility functions
##'
##' @export
clean_col_names <- function(x, ...) {
    UseMethod("clean_col_names")
}

##' @rdname clean_col_names
##' @export
clean_col_names.character <- function(x, use_make.names = TRUE, clean_indicator_names = FALSE) {
    if (!clean_indicator_names) {
        idx <- !(x %in% get_traj_array_indicator_names())
    } else { idx <- rep.int(TRUE, length(x)) }
    if (sum(idx)) x[idx] <- lower_snake_casify(x[idx], use_make.names = use_make.names)
    x[x == "iso_code"] <- "iso"
    x[x == "iso_country"] <- "iso"
    x[x %in% c("country", "country_or_area")] <- "name"
    return(x)
}

##' @rdname clean_col_names
##' @export
clean_col_names.data.frame <- function(x, use_make.names = TRUE, clean_indicator_names = FALSE) {
    colnames(x) <- clean_col_names(colnames(x), use_make.names = use_make.names,
                                   clean_indicator_names = clean_indicator_names)
    return(x)
}

##' @rdname clean_col_names
##' @export
clean_col_names.list <- function(x, use_make.names = TRUE, clean_indicator_names = FALSE) {
    avail_methods <-
        gsub("clean_col_names\\.", "",
             row.names(attr(methods("clean_col_names"), "info")))
    lapply(x, function(z, umn = use_make.names, cin = clean_indicator_names, am = avail_methods) {
        if (class(z) %in% am)
            clean_col_names(z, use_make.names = umn, clean_indicator_names = cin)
    })
}
