setwd("//media//kswada//MyFiles//R//depends")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Depends
# ------------------------------------------------------------------------------
data("Depends", package = "vcdExtra")

data <- Depends

data



# ------------------------------------------------------------------------------
# Ord plot for Depends data
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
Ord_plot(data, main = "Numbers of dependencies", gp = gpar(cex = 1), pch = 16)


# -->
# Positive slope and negative intercept diagnose this distribution as log series ??.
# But the Ord plot shows as "type: none"

# Number of dependencies at k = 12 is quite large.

# Worth trying to fit negative binomial distribution
