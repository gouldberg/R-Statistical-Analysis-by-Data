setwd("//media//kswada//MyFiles//R//sp500w")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sp500w
#   - weekly S&P 500 returns from to Jan. 2003 to Sept. 2012
# ------------------------------------------------------------------------------

data(sp500w, package = "astsa")


str(sp500w)


sp500w



# ------------------------------------------------------------------------------
# data exploration:  data distribution
# ------------------------------------------------------------------------------

psych::describe(sp500w)



# -->
# a little bit skew in right (skew is negative)
# kurtosis = 8.52, very large ... long tail



hist(sp500w, breaks = seq(-0.3, 0.2, 0.01))


