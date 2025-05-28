###-----------------------------------------------------------------------------
### * Constants

###-----------------------------------------------------------------------------
### ** String Constants

###-----------------------------------------------------------------------------
### ** File Names in FPEMglobal

get_FPEMglobal_extdata_filenames <- function(file_ext = TRUE) {

    ## Verbose ........................................
    ##
    op <- options(FPEMglobal.verbose = getOption("FPEMglobal.aux.verbose"))
    on.exit(options(op), add = TRUE, after = FALSE)
    ## ................................................

    FPEMglobal_files <- FPEMglobal::pkg_files_included(result = "filename")

    out <-  list(countries_mwra_195 = FPEMglobal_files$post_process_inputs$countries_for_aggregates,
              countries_unpd_185 = FPEMglobal_files$post_process_inputs$countries_for_output_unpd_185,
              country_classifications = FPEMglobal_files$model_run_inputs$region_information,
              input_data_1549 = FPEMglobal_files$model_run_inputs$input_data_1549,
              number_of_women_15_49 = FPEMglobal_files$post_process_inputs$denominator_counts_1549,
              special_aggregates = FPEMglobal_files$special_aggregates)

    if (!file_ext) out <- lapply(out, tools::file_path_sans_ext)

    return(out)
}

###-----------------------------------------------------------------------------
### *** Marital Groups

##' Standard marital group names
##'
##' Returns a \emph{named} character vector with standard marital
##' group names. The names are the abbreviations in lower case.
##'
##' @param return_case Case of the return value.
##' @param snake_case Logical; use \dQuote{_} instead of spaces?
##' @return Character string.
##' @author Mark Wheldon
##'
##' @family String constants
##'
##' @export
get_std_marital_group_names <- function(return_case = c("lower", "sentence", "title", "upper"),
                                        snake_case = TRUE,
                                        named = FALSE) {
    return_case <- match.arg(return_case)
    stopifnot(is.logical(snake_case))
    basis <- c("mwra" = "Married", "uwra" = "Unmarried", "wra" = "All Women")
    basis2 <- c("mwra" = "Married", "uwra" = "Unmarried", "wra" = "All women")
    if (identical(return_case, "lower")) out <- tolower(basis)
    else if (identical(return_case, "upper")) out <- toupper(basis)
    else if (identical(return_case, "sentence")) out <- basis2
    else if (identical(return_case, "title")) out <- basis
    else stop("Nothing selected")
    if (snake_case) out <- gsub(" ", "_", out)
    if (!named) out <- unname(out)
    return(out)
}

###-----------------------------------------------------------------------------
### *** Indicator Names

##' Report family planning indicator names
##'
##' These functions return character vectors containing indicator
##' names as used in either the \code{dimnames} attribute of the
##' country and aggregate trajectory arrays (e.g., see
##' \code{\link{get_country_traj_muw}}), or in the file names of the
##' csv results files. \code{list_all_std_indicator_names} returns
##' all valid names, \code{get_std_indicator_names}
##' provides functionality to subet by statistic and marital group.
##'
##' The indicator names stored in the country and aggregate trajectory
##' arrays differ by statistic (\code{stat} argument) and marital
##' group (\code{marital_group}) argument.
##'
##' In addition, more ratio indicators are available in the all women
##' results if \code{adjusted = "orig"}. The \code{aw_set} argument
##' provides control over whether all all-women ratio names are
##' returned (\code{"all"}), only those in common with
##' married/unmarried results (\code{"only_common"q}), or only those
##' that are unique to all women results (\code{"only_extra"}).
##'
##' The extra all women indicators are not available if \code{adjusted
##' = "adj"}. In this case, \code{aw_set = "all"} and \code{aw_set =
##' "only_common"} return the same thing, and \code{aw_set =
##' "only_extra"} is an error.
##'
##' @inheritParams get_csv_res
##' @param marital_group Character; marital group for which names are
##'     required.
##' @param adjusted Character; returns names for original
##'     (\dQuote{orig}) or adjusted medians results.
##' @param aw_set Character; the set of all women names required; see
##'     \dQuote{Details}.
##' @param indicator_name_format Character; return the indicator names as used in
##'     trajectory array dimnames or as used in the file names of the
##'     csv results files?
##' @return Character vector of indicator names.
##' @author Mark Wheldon
##'
##' @family String constants
##'
##' @name get_std_indicator_names
##' @export
get_std_indicator_names <- function(stat = c("prop", "count", "ratio", "age_ratio"),
                                    marital_group = get_std_marital_group_names(),
                                    adjusted = c("orig", "adj"),
                                    aw_set = c("all", "only_common", "only_extra"),
                                    indicator_name_format = c("traj_array", "csv_results_file_names", "clean")) {

    ## Verbose ........................................
    ##
    op <- options(FPEMglobal.verbose = getOption("FPEMglobal.aux.verbose"))
    on.exit(options(op), add = TRUE, after = FALSE)
    ## ................................................

    stat <- match.arg(stat)
    marital_group <- match.arg(marital_group)
    adjusted <- match.arg(adjusted)
    aw_set <- match.arg(aw_set)
    indicator_name_format <- match.arg(indicator_name_format)

    ## Name Strings
    prop_count <-
        c(total = "Total", traditional = "Traditional", modern = "Modern",
          unmet = "Unmet", total_plus_unmet = "TotalPlusUnmet", trad_plus_unmet = "TradPlusUnmet")
    ratio_common <-
        c(modern_over_total = "Modern/Total", met_demand = "Met Demand",
          met_demand_with_modern_methods = "Met Demand with Modern Methods",
          z = "Z")
    if (identical(adjusted, "orig")) {
        ratio_aw_extra <-
            c(modern_married_over_all = "Modern Married Over All",
              trad_married_over_all = "Trad Married Over All", unmet_married_over_all = "Unmet Married Over All",
              modern_unmarried_over_all = "Modern Unmarried Over All",
              trad_unmarried_over_all = "Trad Unmarried Over All",
              unmet_unmarried_over_all = "Unmet Unmarried Over All")
    }

    if (identical(indicator_name_format, "csv_results_file_names")) {
        prop_count <- FPEMglobal:::makeShortIndicatorFileName(prop_count)
        ratio_common <-
            FPEMglobal:::makeShortIndicatorFileName(gsub("Modern/Total", "ModernOverTotal", x = ratio_common))
        if (identical(adjusted, "orig"))
            ratio_aw_extra <- FPEMglobal:::makeShortIndicatorFileName(ratio_aw_extra)
    } else if (identical(indicator_name_format, "clean")) {
        prop_count[] <- names(prop_count)
        ratio_common[] <- names(ratio_common)
        if (identical(adjusted, "orig"))
            ratio_aw_extra[] <- names(ratio_aw_extra)
    }

    ## Output
    if (stat %in% c("prop", "count", "age_ratio")) return(prop_count)
    else { # stat must == "ratio"
        if (!identical(marital_group, "all_women")) return(ratio_common)
        else { # marital_group must == "all_women"
            if (identical(adjusted, "orig")) {
                if (identical(aw_set, "all")) return(c(ratio_common, ratio_aw_extra))
                else if (identical(aw_set, "only_common")) return(ratio_common)
                else return(ratio_aw_extra)
            } else { # adjusted must == "adj"
                if (aw_set %in% c("all", "only_common")) return(ratio_common)
                else { # aw_set must == "only_extra"
                    stop("'only extra' is not a valid option for 'aw_set' when 'adjusted = \"adj\"'.")
                }
            }
        }
    }
}

##' @rdname get_std_indicator_names
##' @export
list_all_std_indicator_names <- function() {
    out <- character()
    for (i in c("prop", "count", "ratio", "age_ratio")) {
        for (j in get_std_marital_group_names()) {
            out <- c(out, get_std_indicator_names(stat = i, marital_group = j, aw_set = "all"))
        }
    }
    return(unique(out))
}

###-----------------------------------------------------------------------------
### *** Locations

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
##' @family String constants
##' @family Country and region/aggregate names
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


