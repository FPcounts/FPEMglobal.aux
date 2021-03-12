###-----------------------------------------------------------------------------
### * Functions to do with the Model

###-----------------------------------------------------------------------------
### ** P* model

##' The logistic function (e.g., for \eqn{P^*}).
##'
##' This returns the value of the logistic curve which is used to model
##' \eqn{P^*} and \eqn{R^*}.
##'
##' @param t Year.
##' @param Ptilde Asymptote.
##' @param omega Rate parameter.
##' @param OMEGA Timing parameter.
##' @return The value of the logistic function for the given parameters.
##' @author Mark Wheldon
##' @export
cp_logistic <- function(t, Ptilde, omega, OMEGA)
{
    Ptilde / ( 1 + exp(-omega * ( t - OMEGA ) ) )
}
