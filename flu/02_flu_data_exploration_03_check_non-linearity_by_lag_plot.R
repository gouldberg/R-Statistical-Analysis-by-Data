setwd("//media//kswada//MyFiles//R//flu")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  flu
# ------------------------------------------------------------------------------

data(flu, package = "astsa")

str(flu)

flu



# ------------------------------------------------------------------------------
# data exploration:  Check the non-linearity by lag1.plot
# ------------------------------------------------------------------------------

graphics.off()

# scatterplot with lowess fit
lag1.plot(dflu, max.lag = 1, corr = FALSE)


# -->
# Suggesting the possibility of two linear regimes based on whether or not x(t-1) exceeds 0.05



# ----------
dflu <- diff(flu)

par(mfrow=c(1,1))

plot(dflu, ylim = c(-0.5, 0.5), type = "l")

abline(h = 0.05, col = "gray", lty = 2)


