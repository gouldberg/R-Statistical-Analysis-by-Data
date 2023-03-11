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
# data exploration:  Check the non-linearity by first difference
# ------------------------------------------------------------------------------

# Remove trend by first difference
dsun <- diff(sunspotz)



# ----------
par(mfrow=c(1,1))

plot(dsun, type = "l")


# -->
# The nonlinearity of the data is more pronounced in the plot of the first differences.
