setwd("//media//kswada//MyFiles//R//federalist")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Federalist
# ------------------------------------------------------------------------------

data("Federalist", package = "vcd")

data <- Federalist

data

sum(data)




# ------------------------------------------------------------------------------
# Ord plot for Federalist Papers data
#
# Ord plot may be used to diagnose the form of the discrete distribution.
# Ord showed that a linear relationship:  the ratios k * n(k) / n(k-1) plotted against k should appear as a straight line,
# whose slope and intercept determin the particular distribution
#
#  slope = 0,  Intercept = + : Posisson
#  slope = -,  Intercept = + : Binomial
#  slope = +,  Intercept = + : Negative binomial
#  slope = +,  Intercept = - : Log. series
#
# black line shows the usual ordinary least squares (OLS) regression fit of frequency, n(k), on number of occurrences, k
# red line shows the weighted least sqaures fit, using weights of sqrt(n(k) - 1)
# ------------------------------------------------------------------------------

library(vcd)

Ord_plot(data, main = "instances of 'may' in Federalist Papers", gp = gpar(cex = 1), pch = 16)



# -->
# The slope is positive, so either the negative binomial or log series are possible.
# The intercept is essentially zero, which is ambiguous.
# However, the logarithmic series requires b = -a, so the negative binomial is a better choice.

# Note that there is one apparent outlier, at k = 6, whose effect on the OLS line is to increase the slope and decrease the intercept

