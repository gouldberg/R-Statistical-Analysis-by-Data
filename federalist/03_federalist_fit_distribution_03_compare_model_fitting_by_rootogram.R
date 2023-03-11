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
# Fit the poisson and negative binomial distribution
# ------------------------------------------------------------------------------

data_fit0 <- vcd::goodfit(data, type = "poisson")

data_fit1 <- vcd::goodfit(data, type = "nbinomial")

data_fit2 <- vcd::goodfit(data, type = "nbinomial", par=list(size=1))




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
# Comparing the two plots, we can see that the Poisson model overestimates the frequency of counts k = 1 and
# underestimates the larger counts for k = 4 - 6 occurrences.
# The surprising feature here is that the greatest contribution to lack of fit for the Poisson model is the frequency for k = 6.
# The deviations for the negative binomial are small and unsystematic.



