setwd("//media//kswada//MyFiles//R//depends")

packages <- c("dplyr", "vcd", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Depends
# ------------------------------------------------------------------------------
data("Depends", package = "vcdExtra")

data <- Depends

data



# ------------------------------------------------------------------------------
# Use goodfit() to fit negative binomial distribution
# ------------------------------------------------------------------------------
data_fit <- vcd::goodfit(data, type = "nbinomial")



# ----------
# estimated paramters by maximum likelihood estimation
# estimated mean and variance value
unlist(data_fit$par)



# ----------
print(data_fit, digits=0)



# ----------
# does not fit well
summary(data_fit)



# ------------------------------------------------------------------------------
# Check by rootogram
# ------------------------------------------------------------------------------
plot(data_fit, type="hanging", shade=TRUE)

plot(data_fit, type="deviation", shade=TRUE)
