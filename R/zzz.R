.onAttach <- function(libname, pkgname) {
    desc <- utils::packageDescription(pkgname, fields = c("Package", "Version", "Packaged", "Built"))
    packageStartupMessage(format(paste(paste(names(desc), desc, sep = ": " ), collapse = "\n")))
    return(invisible())
}


## Form taken from 'R Packages' by Hadley Wickham, 'https://r-pkgs.org/r.html'
.onLoad <- function(libname, pkgname) {

    ## -------* OPTIONS

    op <- options()
    op.FPEMglobal.aux <- list(
        FPEMglobal.aux.verbose = FALSE,
        readr.show_col_types = FALSE
    )
    toset <- !(names(op.FPEMglobal.aux) %in% names(op))
    if(any(toset)) options(op.FPEMglobal.aux[toset])

    return(invisible())
}
