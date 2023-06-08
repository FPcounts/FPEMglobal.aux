###-----------------------------------------------------------------------------
### * Documenation


##' Control messaging in FPEMglobal.aux package
##'
##' @section Package messaging:
##'
##' The global option \code{"FPEMglobal.aux.verbose"} controls
##' whether progress and information messages are printed to the
##' console. The default setting is \code{FALSE}. Set it to
##' \code{FALSE} to suppress messages specific to this package.
##'
##' Messages are issued using the standard messaging system (i.e., via
##' \code{message}). This means that, for example,
##' \code{suppressMessages} will also stop package-specific messages
##' from being printed (but also all other messages as well).
##'
##' @section Reading csv files:
##'
##' Almost everywhere, \code{\link[readr]{read_csv}} is used to read
##' csv files. This can result in lots of messages if you set argument
##' \code{verbose} to \code{TRUE}. To turn these off, either set
##' \code{verbose} to \code{FALSE}, or set the options
##' \code{readr.show_progress} and \code{readr.show_col_types} to
##' \code{FALSE}. The former will also suppress function-specific
##' messages, whereas the latter will only affect \pkg{readr} messages.
##'
##' @name messaging
##' @rdname messaging
NULL


###-----------------------------------------------------------------------------
### * Helpers for Commonly Used Messages

## Used when 'verbose' is TRUE and loading some .rda files.
msg_loaded <- function(full_path, ob_name) {
    message("Loaded '", full_path, "'. \n.. Object is '",
                    paste(ob_name, collapse = " "), "'. \n.. Keeping '", ob_name[1],
            "'.")
    }
