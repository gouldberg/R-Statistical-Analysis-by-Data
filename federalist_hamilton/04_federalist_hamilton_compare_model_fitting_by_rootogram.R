setwd("//media//kswada//MyFiles//R//federalist_hamilton")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Federalist Papers  Hamilton
# ------------------------------------------------------------------------------
data <- c(129, 83, 20, 9, 5, 1)
names(data) <- 0:5

data <- as.table(data)



# ------------------------------------------------------------------------------
# Fit the poisson and negative binomial distribution
# ------------------------------------------------------------------------------
data_fit0 <- vcd::goodfit(data, type = "poisson")
data_fit1 <- vcd::goodfit(data, type = "nbinomial")



# ------------------------------------------------------------------------------
# Rootogram
#
# Comparison between fitting by Poisson and negative binomial
# ------------------------------------------------------------------------------
plot(data_fit0, main = "Poisson", type = "hanging", shade = TRUE)
plot(data_fit0, main = "Poisson", type = "deviation", shade = TRUE)


plot(data_fit1, main = "Negative Binomial", type = "hanging", shade = TRUE)
plot(data_fit1, main = "Negative Binomial", type = "deviation", shade = TRUE)


# -->
# Comparing the two plots, we can see that the Poisson model overestimates the frequency of counts k = 4



