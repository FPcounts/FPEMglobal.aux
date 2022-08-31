###-----------------------------------------------------------------------------
### * Work with Country Names

##' Standardize country names for manuscripts
##'
##' Standardizes names of countries for use in tables and plots. Uses
##' LaTeX sytax for non-ASCII characters.
##'
##' @param x Vector of country names to be standardized.
##' @return Vector of standardized country names.
##' @author Mark Wheldon
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
get_aggregate_names <- function(family = c("geog_maj", "geog_subr", "sdg1", "wb")) {

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

    } else if (identical(family, "wb")) {
        c("High-income countries", "Upper-middle-income countries", "Middle-income countries",
          "Lower-middle-income countries", "Low-income countries")
    }
}


###-----------------------------------------------------------------------------
### * Parameter Names

##' Convert parameter names in code to plain English
##'
##' Converts model parameter names from those used in the R code to
##' human readable, plain English names.
##'
##' @param x Character, R code parameter name.
##' @return Character, plain English name.
##' @author Mark Wheldon
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
### * Misc Text String Manipulations

## Clean up names of indicators.
## This function takes out spaces and makes the whole thing lower case.
clean_indic_name <- function(indicator) {
        ind_no_spaces <- gsub("[ ]+", "", indicator)
        tolower(ind_no_spaces)
}


## Round down years to nearest integer?
round_down_years <- function(x) {
    out <- floor(as.numeric(x))
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
##' @param x Object for which a method is defined (e.g., character
##'     string or data frame).
##' @param use_make.names Logical; apply \code{\link[base]{make.names}} to
##'     \code{x} \emph{before} doing anything else?
##' @return Formatted version of \code{x}.
##' @author Mark Wheldon
##' @export
clean_col_names <- function(x, ...) {
    UseMethod("clean_col_names")
}

##' @rdname clean_col_names
##' @export
clean_col_names.character <- function(x, use_make.names = TRUE) {
    x <- lower_snake_casify(x, use_make.names = use_make.names)
    x[x == "iso_code"] <- "iso"
    x[x == "iso_country"] <- "iso"
    x[x %in% c("country", "country_or_area")] <- "name"
    return(x)
}

##' @rdname clean_col_names
##' @export
clean_col_names.data.frame <- function(x, use_make.names = TRUE) {
    colnames(x) <- clean_col_names(colnames(x), use_make.names = use_make.names)
    return(x)
}

##' @rdname clean_col_names
##' @export
clean_col_names.list <- function(x, use_make.names = TRUE) {
    avail_methods <-
        gsub("clean_col_names\\.", "",
             row.names(attr(methods("clean_col_names"), "info")))
    lapply(x, function(z, umn = use_make.names, am = avail_methods) {
        if (class(z) %in% am)
            clean_col_names(z, use_make.names = umn)
    })
}
