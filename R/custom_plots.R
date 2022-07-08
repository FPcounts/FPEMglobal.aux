###-----------------------------------------------------------------------------
### * Custom Plots

##' Plot CP total versus Z
##'
##' This is 'Figure 4' in the Appendix to Alkema at al. (2013).
##' @param CP_data Data frame like that returned by
##'     \code{\link{get_used_input_data}}.
##' @return
##' @author Mark Wheldon
##' @export
plot_CP_vs_Z <- function(CP_data, main = NULL, sub = NULL, add = FALSE,
                         ppar = list(), loesspar = list(), lpars = NULL,
                         res_dir, res_country, country_names = NULL, col = NULL,
                         legend = FALSE,
                         ...) {

    if(all(c("props.tot.j", "props.unmet.j") %in% colnames(CP_data))) {
        CP <- CP_data$props.tot.j
        Z <- with(CP_data, props.unmet.j / (1 - props.tot.j))
    } else if(all(c("Contraceptive.use.ANY", "Unmet") %in% colnames(CP_data))) {
        CP <- CP_data$Contraceptive.use.ANY
        Z <- with(CP_data, Unmet / (1 - Contraceptive.use.ANY))
    } else {
        stop("Input data does not have the required columns.")
    }
    if(!is.null(country_names)) {
        if((missing(res_dir) && missing(res_country)) ||
           (!missing(res_dir) && !missing(res_country))
       ) stop("Supply exactly one of 'res_dir', 'res_country'.")
    }

    if(add) {

        op <- par()
        par(ppar)
        points(CP, Z, type = "p")

        suppressWarnings(par(op))
        if(identical(length(loesspar), 0L)) {
            loesspar <-  list(lty = 1, lwd = 2, col = "blue")
            }
        par(loesspar)
        lines(loess.smooth(CP, Z, span = 2/3, degree = 1,
                     family = c("symmetric", "gaussian"), evaluation = 50))

        suppressWarnings(par(op))

    } else {

        scatter.smooth(CP, Z,
                       xlim = c(0, 1),
                       ylab = "Unmet need/(1 - Total contraceptive prevalence)",
                       xlab = "Total contraceptvie prevalence",
                       main = if(!is.null(main)) { main } else {"CP vs 'Z'"},
                       sub = if(!is.null(sub)) { sub } else { deparse(substitute(CP_data)) },
                       ...,
                       lpars = if(!is.null(lpars)) {
                                   lpars } else { list(col = "black", lty = 1,
                                                       lwd = 2) }
                       )
    }

    if(!is.null(country_names)) {
        nc <- length(country_names)
        if(!missing(res_dir)) {
            fitt_Z <- get_fitted_Z_country_quants(res_dir = res_dir)
        } else {
            fitt_Z <- get_fitted_Z_country_quants(res_country = res_country)
        }
        if(!is.null(col)) {
            col <- rep(col, length.out = nc)
        } else {
            col <- rainbow(nc)
        }
        for(x in seq_along(country_names)) {
            with(dplyr::filter(fitt_Z, name.j == country_names[x] & quant == 0.5),
                 lines(Total, Z, lty = 2, col = col[x], lwd = 2))
        }
    }

    if(legend) {
        if(add) {
            if(identical(length(loesspar), 0L)) {
                leg.lty <- 1
                leg.lwd <- 2
                leg.col <- "blue"
            } else {
                if(!is.null(loesspar$lty)) {
                    leg.lty <- loesspar$lty
                } else {
                    leg.lty <- 1
                }
                if(!is.null(loesspar$lwd)) {
                    leg.lwd <- loesspar$lwd
                } else {
                    leg.lwd <- 2
                }
                if(!is.null(loesspar$col)) {
                    leg.col <- loesspar$col
                } else {
                    leg.col <- "blue"
                }
            }
        } else {
            if(is.null(lpars)) {
                leg.lty <- 1
                leg.lwd <- 2
                leg.col <- "black"
            } else {
                if(!is.null(lpars$lty)) {
                    leg.lty <- lpars$lty
                } else {
                    leg.lty <- 1
                    }
                if(!is.null(lpars$lwd)) {
                    leg.lwd <- lpars$lwd
                } else {
                    leg.lwd <- 2
                }
                if(!is.null(lpars$col)) {
                    leg.col <- lpars$col
                } else {
                    leg.col <- "black"
                }
            }
        }
        if(is.null(country_names)) {
            legend("topright", lty = leg.lty, lwd = leg.lwd, col = leg.col,
                   legend = "loess fit")
        } else {
            leg.col.c <- rep(NA, nc)
            for(x in seq_along(country_names)) {
                leg.col.c[x] <- col[x]
            }
            leg.col <- c(leg.col, leg.col.c)
            leg.leg <- c("loess fit", country_names)
            legend("topright", lty = c(leg.lty, rep(2, nc)),
                   lwd = c(leg.lwd, rep(1, nc)),
                   col = c(leg.col, leg.col.c),
                   legend = leg.leg)
        }
    }

}
