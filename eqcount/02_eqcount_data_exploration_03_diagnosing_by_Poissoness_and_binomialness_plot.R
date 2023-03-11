setwd("//media//kswada//MyFiles//R//eqcount")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqcount
# ------------------------------------------------------------------------------

data(EQcount, package = "astsa")

str(EQcount)

EQcount



# ------------------------------------------------------------------------------
# Distplot
#
# Hoaglin and Tukey (1985) developed similar plots of a count metameter against k, which appear as a straight line
# when a data distribution follows the general power series family of discrete distribution
#
# For the Binomial distribution, a "binomialness" plot is constructed by plotting log(n(k) / N(n,k)) against k.
# If the points in this plot approximate a stright line, the slope is interpreted as log(p / (1-p)),
# so the binomial parameter p may be estimated as p = exp(b) / ( 1 + exp(b) )
# ------------------------------------------------------------------------------

# binomialness plot shows that heavier tails than in a binomial distribution
vcd::distplot(table(EQcount), type = "poisson", xlab = "Annual count of major earthquakes")

vcd::distplot(table(EQcount), type = "nbinomial", xlab = "Annual count of major earthquakes")


# -->
# Poisson model provides an acceptable fit ???