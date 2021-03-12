################################################################################
###
###  DATE CREATED: 2019-10-10
###
###  AUTHOR: Mark Wheldon
###
###  PROJECT: Making Family Planning Count
###
###  DESCRIPTION:
###
###     Build and install 'cuawaux' package --- mainly for interactive
###     testing and development.
###
################################################################################

###-----------------------------------------------------------------------------
### * Setup

library(git2r)
library(devtools)

###-----------------------------------------------------------------------------
### * Functions

divider <- function(x) {
    cat("\n\n\n********************************************************************************\n** ", x,
        "\n********************************************************************************\n\n")
}

## From 'example("source", package = "base")
sourceDir <- function(path, trace = TRUE, ...) {
    for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
        if(trace) cat(nm,":")
        source(file.path(path, nm), ...)
        if(trace) cat("\n")
    }
}

###-----------------------------------------------------------------------------
### ** Github stuff

write_sha1_DESC <- function(pkg_dir, git_dir) {
    ## Create new line with SHA1
    new_sha1_line <- paste0("GitHubSHA1LastCommit: ", git2r::sha(git2r::last_commit(git_dir)))

    ## Read DESC file as lines
    DESC_con <- file(file.path(pkg_dir, "DESCRIPTION"))
    DESC_lines <- readLines(DESC_con)

    ## Is there already a 'GitHubSHA1LastCommit:' line? If so, replace it. If
    ## not, add one.
    sha1_line_num <- grep("GitHubSHA1LastCommit:", DESC_lines)
    if(identical(length(sha1_line_num), 1L)) DESC_lines[sha1_line_num] <- new_sha1_line
    else DESC_lines <- rbind(DESC_lines, new_sha1_line)

    ## Write DES
    writeLines(DESC_lines, con = DESC_con)

    close(DESC_con)
}

clean_sha1_DESC <- function(pkg_dir) {
    ## Read DESC file as lines
    DESC_con <- file(file.path(pkg_dir, "DESCRIPTION"))
    DESC_lines <- readLines(DESC_con)

    ## Is there already a 'GitHubSHA1LastCommit:' line? If so, replace it with 'GitHubSHA1LastCommit: -'.
    sha1_line_num <- grep("GitHubSHA1LastCommit:", DESC_lines)
    if(identical(length(sha1_line_num), 1L)) DESC_lines[sha1_line_num] <- "GitHubSHA1LastCommit: -"

    ## Write DES
    writeLines(DESC_lines, con = DESC_con)

    close(DESC_con)
}

###-----------------------------------------------------------------------------
### * Test and Install

### Make sysdata.rda
divider("MAKING SYSDATA")
sourceDir("data-raw")

### Document
divider("MAKING DOCUMENTATION")
devtools::document()

### Do all tests
divider("RUNNING TESTS")
if(length(dir("tests/testthat")) > 0) {
    devtools::test(reporter = c("summary", "fail"))
}

### Install
divider("INSTALLING")
## write_sha1_DESC(pkg_dir = ".", git_dir = ".")
devtools::install(upgrade = "never")# devtools::install(build_vignettes = TRUE)
## clean_sha1_DESC(pkg_dir = ".")
