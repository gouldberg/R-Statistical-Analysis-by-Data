setwd("//media//kswada//MyFiles//R//federalist_hamilton")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Federalist Papers  Hamilton
# ------------------------------------------------------------------------------
data <- c(129, 83, 20, 9, 5, 1)
names(data) <- 0:5

data <- as.table(data)



# ------------------------------------------------------------------------------
# Ord plot for Hamilton data
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
Ord_plot(data, main = "instances of 'upon' in each text block", gp = gpar(cex = 1), pch = 16)


# -->
# The slope and intercept are positive, so the negative binomial is possible.

# Note that there is one apparent outlier, at k = 5, whose effect on the OLS line is to decrease the slope and increase the intercept

