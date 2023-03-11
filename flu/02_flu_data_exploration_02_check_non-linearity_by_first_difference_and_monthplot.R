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
# data exploration:  Check the non-linearity by first difference and monthplot
# ------------------------------------------------------------------------------

# Remove trend by first difference
dflu <- diff(flu)



# ----------
par(mfrow=c(1,2))

plot(dflu, ylim = c(-0.5, 0.5), type = "l")

monthplot(flu)



# -->
# The nonlinearity of the data is more pronounced in the plot of the first differences.
# Clearly x(t) slowly rises for some months and then sometime in the winter, has a possibility of jumping to a large number once x(t)
# exceeds aboud 0.05.
# If the process does make a large jump, then a subsequent significant decrease occurs in x(t).
