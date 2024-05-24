### General-purpose functions
### These are *small* utility functions, e.g., logit, that are not exported.

logit <- function(p) { log(1 / (1 - p)) }
invlogit <- function(x) { exp(x) / (exp(x) + 1) }
