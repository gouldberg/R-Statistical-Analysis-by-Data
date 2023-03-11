setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ----------
# convert to table
( tab <- table(PhdPubs$articles) )




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

distplot(tab, type = "poisson", xlab = "Number of Articles")


# -->
# If Poisson distribution fitts to the data, slope = log(lombda) and intercept = - lambda.
# --> not holds



distplot(tab, type = "nbinomial", xlab = "Number of Articles")


# -->
# Number of articles 8 or more deviate from negative binomial distribution



# ----------
# for reference:  sample from Poisson
tmp <- rpois(lambda = 10, 100)

distplot(table(tmp), type = "poisson", xlab = "Number of Articles")

