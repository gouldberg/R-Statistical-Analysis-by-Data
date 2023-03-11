setwd("//media//kswada//MyFiles//R//saxony")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Saxnony
#  - The nmber of male children in families of size 12 in Saxony
# ------------------------------------------------------------------------------
data("Saxony", package = "vcd")

data <- Saxony



# ------------------------------------------------------------------------------
# Fit the binomial distribution to Saxony data
# ------------------------------------------------------------------------------
data_fit <- vcd::goodfit(data, type = "binomial", par=list(size=12))


# specifying probability paramter = 1/2 in advance
data_fit1 <- vcd::goodfit(data, type = "binomial", par=list(size = 12, prob = 1/2))


# specifying probability paramter as the value estimated by ML
data_fit2 <- vcd::goodfit(data, type = "binomial", par=list(size = 12, prob = unlist(data_fit$par["prob"])))



# ------------------------------------------------------------------------------
# Rootogram
#
# plot.goodfit() method for "goodfit" objects
# ------------------------------------------------------------------------------
# To make the smaller frequenceis more visible, Tukey suggests plotting the frequenceies on a square-root scale with he calls a rootogram
# We can more easily judge the pattern of departures against the horizontal reference line at 0, than against the curve.
# Color reflects the sign and magnitude of the contributions to lack of fit
plot(data_fit, type = "hanging", shade = TRUE, xlab = "Number of males")

plot(data_fit, type = "deviation", shade = TRUE, xlab = "Number of males")


# -->
# deviation rootogram shows that heavier tails than in a binomial distributino



# ----------
# type = "deviation" is better to compare with other models
plot(data_fit, type = "deviation", shade = TRUE, xlab = "Number of males")
plot(data_fit1, type = "deviation", shade = TRUE, xlab = "Number of males")
plot(data_fit2, type = "deviation", shade = TRUE, xlab = "Number of males")


# -->
# the binomial model with probability = 1/2 has worse fit at 8 or more number of males.
# the data has fat tail at 8 or more number of males.
# It seems that assumption of independent Bernoulli trial is not satisfied at this fat tail area.




