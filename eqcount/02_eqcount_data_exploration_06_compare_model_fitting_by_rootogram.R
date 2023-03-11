setwd("//media//kswada//MyFiles//R//eqcount")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqcount
# ------------------------------------------------------------------------------

data(EQcount, package = "astsa")

str(EQcount)

EQcount



# ------------------------------------------------------------------------------
# Fit the poisson and negative binomial distribution
# ------------------------------------------------------------------------------

data_fit0 <- vcd::goodfit(table(EQcount), type = "poisson")

data_fit1 <- vcd::goodfit(table(EQcount), type = "nbinomial")

data_fit2 <- vcd::goodfit(table(EQcount), type = "nbinomial", par=list(size=1))




# ------------------------------------------------------------------------------
# Plot for data and fitting Poisson model
#
# plot.goodfit() method for "goodfit" objects
# ------------------------------------------------------------------------------
# Observed frequencies are shown by bars and fitted frequencies are shown by points, connected by a smooth (spline) curve
plot(data_fit0, scale = "raw", type = "standing")



# ------------------------------------------------------------------------------
# Rootogram
#
# plot.goodfit() method for "goodfit" objects
# ------------------------------------------------------------------------------
# --> dominated by the largest frequenceis, making it hard to assess the deviations among the smaller frequencies.
# To make the smaller frequenceis more visible, Tukey suggests plotting the frequenceies on a square-root scale with he calls a rootogram

# rootogram
plot(data_fit0, type = "standing")



# ----------
# We can more easily judge the pattern of departures against the horizontal reference line at 0, than against the curve.
# Color reflects the sign and magnitude of the contributions to lack of fit
plot(data_fit0, type = "hanging", shade = TRUE)

plot(data_fit0, type = "deviation", shade = TRUE)



# ------------------------------------------------------------------------------
# Rootogram
#
# Comparison between fitting by Poisson, negative binomial, and geometric distribution
# ------------------------------------------------------------------------------
plot(data_fit0, main = "Poisson", shade = TRUE, legend = FALSE)

plot(data_fit1, main = "Negative binomial", shade = TRUE, legend = FALSE)

plot(data_fit2, main = "Geometric", shade = TRUE, legend = FALSE)


# -->
# Comparing the two plots, we can see that the Poisson model underestimates and overestimates ...
# The deviations for the negative binomial are small and unsystematic.



