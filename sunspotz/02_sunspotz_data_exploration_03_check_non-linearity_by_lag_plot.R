setwd("//media//kswada//MyFiles//R//sunspotz")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sunspotz
# ------------------------------------------------------------------------------

data(sunspotz, package = "astsa")

str(sunspotz)

head(sunspotz)



# ------------------------------------------------------------------------------
# data exploration:  Check the non-linearity by lag1.plot
# ------------------------------------------------------------------------------

graphics.off()


# scatterplot with lowess fit
lag1.plot(dsun, max.lag = 1, corr = FALSE)



# -->
# Suggesting the possibility of two linear regimes based on whether or not x(t-1) exceeds 5 ??

