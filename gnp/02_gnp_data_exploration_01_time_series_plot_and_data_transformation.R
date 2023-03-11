setwd("//media//kswada//MyFiles//R//gnp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  GNP data
# ------------------------------------------------------------------------------

data(gnp, package = "astsa")

str(gnp)

head(gnp)



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow=c(2,1))

plot(gnp, type = "l")


# growth rate
plot(diff(log(gnp)), type = "l")



# -->
# Because strong trend tends to obscure other effects, it is difficult to say any other variability in data except for periodic large dips in the economy.
# When reports of GNP and similar economic indicators are given, it is often in growth rate (percent change) rather than in actual (or adjusted)
# values that is of interest.

# The growth rate appears to be a stable process.



# ----------
mean(diff(log(gnp)))



# -->
# average GNP growth rate is 0.83%


# ----------
forecast::ndiffs(log(gnp))

