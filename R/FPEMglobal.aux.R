
#' FPEMglobal.aux: Auxillary functions for FPEMglobal
#'
#' @docType package
#' @name FPEMglobal.aux

#' @importFrom magrittr %>%
NULL

#' @import dplyr
NULL

#' @import readr
NULL

#' @import tibble
NULL

#' @import tidyr
NULL



.onAttach <- function(libname, pkgname) {
    ## START-UP MESSAGE
    desc_fields <-
        c("Package", "Title", "Version",
          "Depends", "License", "Encoding", "Maintainer", "Built")
    pkg_desc <- packageDescription(pkgname)
    if("GitHubSHA1LastCommit" %in% names(pkg_desc))
        desc_fields <- c(desc_fields, "GitHubSHA1LastCommit")
    pkg_desc <- pkg_desc[desc_fields]
    pkg_desc <- paste(names(pkg_desc), pkg_desc, sep = ": ", collapse = "\n")
  packageStartupMessage(pkg_desc)
}
