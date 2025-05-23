###-----------------------------------------------------------------------------
### * Load fpemdata / fpemlocal files

##' Get hierarchical parameter medians for one-country runs
##'
##' Loads the posterior medians of the hierarchical parameters from a global
##' run, stored in the file \file{[output_dir]/data.global.rda}. These are
##' needed in one-country runs.
##'
##' @inheritParams get_csv_res
##' @return A list, loaded from the \dQuote{data global} file.
##' @author Mark C Wheldon
##' @export
get_hierarchical_medians <- function(output_dir = NULL) {

    verbose <- getOption("FPEMglobal.aux.verbose")

    if (is_all_women_run(output_dir))
        stop("Hierarchical medians are not available for all women runs.")

    temp_env <- new.env()
    return(get(load(file.path(output_dir, "data.global.rda"), envir = temp_env),
               envir = temp_env, inherits = FALSE))
}


###-----------------------------------------------------------------------------
### * Convert FPEMglobal files to fpemdata / fpemlocal format

###-----------------------------------------------------------------------------
### ** Inputs

##' Convert country classifications to fpemdata format
##'
##' Takes the country classifications file used in an \pkg{FPEMglobal} run and returns
##' it in \pkg{fpemdata} format. \code{\link{get_country_classifications}} is
##' called to get the classifications file.
##'
##' @inheritParams get_csv_res
##' @return A \code{\link[tibble]{tibble}}.
##' @author Mark Wheldon
##'
##' @family FPEM data converters
##' @seealso \code{\link{get_country_classifications}}
##'
##' @importFrom gdata rename.vars
##'
##' @export
convert_country_classifications_2_fpemdata <- function(output_dir = NULL) {

    verbose <- getOption("FPEMglobal.aux.verbose")

    if (is_all_women_run(output_dir))
        stop("Hierarchical medians are not available for all women runs.")

    temp_env <- new.env()
    return(get(load(file.path(output_dir, "data.global.rda"), envir = temp_env),
               envir = temp_env, inherits = FALSE))

    ## -------* Get input file

    input_df <-
        get_used_unpd_regions(output_dir = output_dir,
                              M49_region_names = TRUE, clean_col_names = TRUE)

    ## -------* Rename Columns

    names_new_old <-
        data.frame(
            rbind(
                c("division_numeric_code",                      "iso"),
                c("name_country",                               "name"),
                c("name_region",                                "major_area"),
                c("name_sub_region",                            "region"),
                c("region_numeric_code",                        "major_area_code"),
                c("sub_region_numeric_code",                    "region_code"),
                c("is_developed_region",                        "developed_region"),
                c("is_less_developed_region",                   "less_developed_regions"),
                c("is_least_developed_country",                 "least_developed_country"),
                c("is_in_sub_saharan_africa",                   "sub_saharanafrica"),
                c("is_unmarried_sexual_activity",               "sexual_activity_among_unmarried"),
                c("is_low_population",                          "wpp_small_country"),
                c("is_fp2020",                                  "fp2020_country")),
            stringsAsFactors = FALSE)
    names(names_new_old) <- c("new", "old")

    names_old_idx <- !is.na(names_new_old$old) & names_new_old$old %in% colnames(input_df)

    input_df <-
        gdata::rename.vars(input_df,
                           from = names_new_old$old[names_old_idx],
                           to = names_new_old$new[names_old_idx],
                           info = FALSE)

    ## -------* Recode Variables

    nm_Y_N <- c("is_developed_region", "is_less_developed_region", "is_least_developed_country",
                 "is_in_sub_saharan_africa", "is_unmarried_sexual_activity", "is_low_population",
                "is_fp2020")

    idx <- input_df[, nm_Y_N] == "Yes"
    input_df[, nm_Y_N][idx] <- "Y"

    idx <- input_df[, nm_Y_N] == "No"
    input_df[, nm_Y_N][idx] <- "N"

    ## -------* Select and Order Columns

    input_df <- input_df[, unique(names_new_old$new)]

    ## -------* END

    return(input_df)
}

country_classifications_2_fpemdata <- function(output_dir = NULL) {

    lifecycle::deprecate_soft(
                   when = "1.3.0",
                   what = "country_classifications_2_fpemdata()",
                   with = "convert_country_classifications_2_fpemdata()")

    return(convert_country_classifications_2_fpemdata(output_dir = output_dir))
}


##' Convert input file to fpemdata format
##'
##' Takes the raw input file from an \pkg{FPEMglobal} run and returns
##' it in \pkg{fpemdata} format. \code{\link{get_used_input_data}} is
##' called to get the input file.
##'
##' @inheritParams get_csv_res
##' @return A \code{\link[tibble]{tibble}}.
##' @author Mark Wheldon
##'
##' @family FPEM data converters
##' @seealso \code{\link{get_used_input_data}}
##'
##' @importFrom gdata rename.vars
##'
##' @export
convert_input_data_2_fpemdata <- function(output_dir = NULL) {

    verbose <- getOption("FPEMglobal.aux.verbose")

    if (is_all_women_run(output_dir = output_dir))
        stop("Input data not stored in output of an all women run. Use 'output_dir' from a married or unmarried women run.")

    ## -------* Get input file

    input_df <- get_used_input_data(output_dir = output_dir)

    ## -------* Rename Columns

    names_new_old <-
        data.frame(
            rbind(
                c("division_numeric_code",                      "ISO.code"),
                c("start_date",                                 "Start.year"),
                c("end_date",                                   "End.year"),
                c("is_in_union",                                "In.union"),#recode
                c("is_in_union",                                "In union"),#recode
                c("age_range",                                  "Age..range"),
                c("data_series_type",                           "Data.series.type"),
                c("group_type_relative_to_baseline",            "Population.type"),
                c("contraceptive_use_modern",                   "Contraceptive.use.MODERN"),
                c("contraceptive_use_traditional",              "Contraceptive.use.TRADITIONAL"),
                c("contraceptive_use_any",                      "Contraceptive.use.ANY"),
                c("unmet_need_modern",                          NA),
                c("unmet_need_any",                             "Unmet"),
                c("is_pertaining_to_methods_used_since_last_pregnancy",         NA),#all are 'N'
                c("pertaining_to_methods_used_since_last_pregnancy_reason",     NA),#all are blank
                c("has_geographical_region_bias",               NA),#based on ..._reason column
                c("geographical_region_bias_reason",            "Note.on.country"),
                c("has_non_pregnant_and_other_positive_biases", "Non.pregnant.and.other.positive.biases"),#based on Note.on.population and note.on.data
                c("non_pregnant_and_other_positive_biases_reason",              NA),
                c("age_group_bias",                             "age.cat.bias"),#Needs recoding
                c("modern_method_bias",                         "Modern.method.bias"),#recode
                c("has_traditional_method_bias",                NA),#from 'note.on.methods'
                c("traditional_method_bias_reason",             NA),
                c("has_absence_of_probing_questions_bias",      "Absence.of.probing.questions.bias...1"),
                c("se_modern",                                  "SE.modern"),
                c("se_traditional",                             "SE.trad"),
                c("se_unmet_need",                              "SE.unmet"),
                c("se_log_r_modern_no_use",                     "SE.logR.modern.nouse"),
                c("se_log_r_traditional_no_use",                "SE.logR.trad.nouse"),
                c("se_log_r_unmet_no_need",                     "SE.logR.unmet.noneed"),
                c("source_id",                                  "Catalog.ID"),
                c("record_id",                                  NA)),
            stringsAsFactors = FALSE)
    names(names_new_old) <- c("new", "old")

    names_old_idx <- !is.na(names_new_old$old) & names_new_old$old %in% colnames(input_df)

    input_df <-
        gdata::rename.vars(input_df,
                           from = names_new_old$old[names_old_idx],
                           to = names_new_old$new[names_old_idx],
                           info = FALSE)

    ## -------* Recode Variables

    input_df$is_in_union <- c("N", "Y")[input_df$is_in_union + 1]

    input_df$age_range <- "15-49"

    input_df$data_series_type[input_df$data_series_type %in%
                              c("RHS", "CPS", "WFS", "GGS", "LSMS", "GFHS",
                                "PAPCHILD", "PAPFAM", "CCPS", "FFS")] <- "Other"

    input_df$contraceptive_use_modern <- input_df$contraceptive_use_modern / 100
    input_df$contraceptive_use_traditional <- input_df$contraceptive_use_traditional / 100
    input_df$contraceptive_use_any <- input_df$contraceptive_use_any / 100
    input_df$unmet_need_any <- input_df$unmet_need_any / 100
    input_df$unmet_need_modern <- NA_real_

    input_df$is_pertaining_to_methods_used_since_last_pregnancy <- "N"
    input_df$pertaining_to_methods_used_since_last_pregnancy_reason <- ""

    input_df$age_group_bias[input_df$age_group_bias == "0"] <- "None"

    input_df$modern_method_bias[is.na(input_df$modern_method_bias)] <- "None"

    input_df$has_absence_of_probing_questions_bias <-
        c("N", "Y")[input_df$has_absence_of_probing_questions_bias + 1]

    input_df$record_id <- paste(input_df$is_in_union, 1:nrow(input_df), sep = "_")

    ## -------* Biases

    ## -------** Code Simple Biases

    ## Code geographical reason bias binary indicator
    input_df$has_geographical_region_bias <-
        c("N", "Y")[sapply(input_df$geographical_region_bias_reason != "",
                           "isTRUE") + 1]

    ## Traditional method bias
    input_df$has_traditional_method_bias <-
        c("N", "Y")[sapply(input_df$Note.on.methods ==
                           "Traditional methods include folk methods.",
                           "isTRUE") + 1]
    input_df$traditional_method_bias_reason <- ""
    input_df$traditional_method_bias_reason[input_df$has_traditional_method_bias == "Y"] <-
        "Traditional methods include folk methods."

    ## Non pregnant and other positive biases
    input_df$has_non_pregnant_and_other_positive_biases[input_df$has_non_pregnant_and_other_positive_biases == "+"] <- "Y"
    input_df$has_non_pregnant_and_other_positive_biases[is.na(input_df$has_non_pregnant_and_other_positive_biases)] <- "N"

    input_df$non_pregnant_and_other_positive_biases_reason <- ""
    idx <- input_df$has_non_pregnant_and_other_positive_biases == "Y"
    input_df$non_pregnant_and_other_positive_biases_reason[idx] <-
        input_df$Note.on.population[idx]
    input_df$non_pregnant_and_other_positive_biases_reason[idx] <-
        input_df$Note.on.data[idx]    #so will replace 'note.on.population'

    ## -------** Non Pregnant and Other Positive Biases

    positive_list <-
        c("Data pertain to non-pregnant women.", #1
          "Data pertain to women exposed to the risk of pregnancy.", #2
          "Data pertain to sexually active women of reproductive age.", #3 #for married change base population, see below
          "Data pertain to women exposed to the risk of pregnancy (non-pregnant, who has ever had sex).", #4 #for married change base population, see below
          "Data pertain to sexually active women.", #5 #for married change base population, see below
          "Data pertain to women who were sexually active during the month prior to the interview.", #6 #for married change base population, see below
          "Data pertain to women who were sexually active during the three months prior to the interview.", #7 #for married change base population, see below
          "Data pertain to sexually active, non-pregnant women.", #8 #for married change base population, see below
          "Data pertain to fecund women.", #9
          "Including single women who have born a child.", #10
          "Including women in cohabiting unions.", #11
          "Contraceptive use question refers to contraceptive use in the 12 months prior to the survey.", #12
          "Data pertain to fecund women.", #13
          "Data pertain to women who ever had sex", #14 #for married change base population, see below
          "Data are on contraceptive method used in the last year.") #15 -- added MCW 2020-02-11

    posl_only_marr <- positive_list[10:11]
    posl_only_unmarr <- positive_list[c(3:8,14)]
    posl_all <- positive_list[!(positive_list %in% posl_only_marr |
                                positive_list %in% posl_only_unmarr)]

    ## Initialize with "N"s
    input_df$has_non_pregnant_and_other_positive_biases <- "N"

    ## Note.on.population
    idx <-
        (input_df$is_in_union == "Y" & input_df$Note.on.population %in% posl_only_marr) |
        (input_df$is_in_union == "N" & input_df$Note.on.population %in% posl_only_unmarr) |
        input_df$Note.on.population %in% posl_all
    input_df$has_non_pregnant_and_other_positive_biases[idx] <- "Y"
    input_df$non_pregnant_and_other_positive_biases_reason[idx] <-
        input_df$Note.on.population[idx]

    ## Note on data
    idx <-
        (input_df$is_in_union == "Y" & input_df$Note.on.data %in% posl_only_marr) |
        (input_df$is_in_union == "N" & input_df$Note.on.data %in% posl_only_unmarr) |
        input_df$Note.on.data %in% posl_all
    input_df$has_non_pregnant_and_other_positive_biases[idx] <- "Y"
    input_df$non_pregnant_and_other_positive_biases_reason[idx] <-
        input_df$Note.on.data[idx]

    ## Re-order columns
    input_df <- input_df[, unique(names_new_old$new)]

    ## -------** NA's

    for (j in seq_len(ncol(input_df))) {
        if (is.character(input_df[[j]]))
            input_df[is.na(input_df[[j]]), j] <- ""
    }

    ## -------* END

    return(input_df)
}

input_data_2_fpemdata <- function(output_dir = NULL) {

    lifecycle::deprecate_soft(
                   when = "1.3.0",
                   what = "input_data_2_fpemdata()",
                   with = "convert_input_data_2_fpemdata()")

    return(convert_input_data_2_fpemdata(output_dir = output_dir))
}


##' Convert denominators to fpemdata format
##'
##' Takes the raw input file from an \pkg{FPEMglobal} run and returns
##' it in \pkg{fpemdata} format. \code{\link{get_used_input_data}} is
##' called to get the input file.
##'
##' @inheritParams get_csv_res
##' @return A \code{\link[tibble]{tibble}}.
##' @author Mark Wheldon
##'
##' @family FPEM data converters
##' @seealso \code{\link{get_used_denominators}}, \code{\link{get_used_csv_denominators}}
##'
##' @export
convert_denominators_2_fpemdata <- function(output_dir = NULL) {

    verbose <- getOption("FPEMglobal.aux.verbose")

    ## -------* Get denominators

    denom_csv <- get_used_csv_denominators(output_dir = output_dir,
                                      clean_col_names = TRUE, table_format = "long",
                                      marital_group = c("married", "unmarried"),
                                      years_as_midyear = FALSE,
                                      processed = FALSE)

    ## -------* Reformat

    denom_csv <- denom_csv |>
        dplyr::mutate(is_in_union = dplyr::case_when(marital_group == "married" ~ "Y",
                                                     marital_group == "unmarried" ~ "N",
                                                     TRUE ~ NA_character_),
                      iso = as.numeric(iso)) |>
        dplyr::rename(division_numeric_code = iso,
                      population_count = count,
                      age_range = age_group,
                      mid_year = year) |>
        dplyr::select(-marital_group, -name)

    ## Remove rows with missing counts
    denom_csv <- denom_csv[complete.cases(denom_csv),]

    ## -------* END

    return(denom_csv)
}

denominators_2_fpemdata <- function(output_dir = NULL) {

    lifecycle::deprecate_soft(
                   when = "1.3.0",
                   what = "denominators_2_fpemdata()",
                   with = "convert_denominators_2_fpemdata()")

    return(convert_denominators_2_fpemdata(output_dir = output_dir))
}


###-----------------------------------------------------------------------------
### ** Outputs

##' Convert csv results to \pkg{fpemdata} format.
##'
##' This function provides for interaction with the \pkg{fpemdata}
##' package. The csv results are loaded \emph{via}
##' \code{\link{get_csv_res}} and then modified accordingly.
##'
##' \pkg{fpemdata} expects input data in a data frame with the
##' following columns:
##' \describe{
##' \item{Name}{Country name}
##' \item{Iso}{Numeric ISO code}
##' \item{Percentile}{\emph{Quantile}, e.g., 0.025, 0.1, etc.}
##' \item{par}{Indicator in lower case; can take values "modern", "traditional", "unmet"}
##' \item{1970.5, etc.}{Mid-year; multiple columns from 1970.5 up to 2030.5}}
##'
##' @family fpemdata converters
##' @seealso \code{\link{get_csv_res}}
##'
##' @param output_dir Path(s) to director(y/ies) containing
##'     outputs. See \code{\link{get_csv_res}} for details.
##' @param stat Which statistics should be loaded? See
##'     \code{\link{get_csv_res}} for details, but note that, unlike
##'     with \code{\link{get_csv_res}}, you can only specifiy one
##'     \dQuote{\code{stat}} at a time with this function.
##' @inheritParams get_csv_res
##' @return A \code{\link[tibble]{tibble}}.
##' @author Mark C Wheldon
##' @export
convert_csv_res_2_fpemdata <- function(output_dir = NULL,
                                       stat = c("prop", "count", "ratio"),
                               adjusted = c("orig", "adj", "sub_adj")) {

    verbose <- getOption("FPEMglobal.aux.verbose")

    stat <- match.arg(stat, several.ok = FALSE)
    adjusted <- match.arg(adjusted, several.ok = FALSE)

    output_dir <-
        output_dir_wrapper(output_dir = output_dir,
                           post_processed = TRUE, made_results = TRUE,
                           age_ratios = FALSE,
                           adjusted_medians = any(c("adj", "sub_adj") %in% adjusted))

    out <- get_csv_res(output_dir = output_dir,
                       stat = stat,
                       adjusted = adjusted,
                       table_format = "raw", clean_col_names = FALSE)

    out <- out %>%
        dplyr::mutate(indicator = tolower(indicator)) %>%
        dplyr::rename(par = indicator) %>%
        dplyr::arrange(par, Iso, Percentile)
    return(tibble::as_tibble(out))
}


