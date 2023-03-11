setwd("//media//kswada//MyFiles//R//vietnam")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Vietnam
# ------------------------------------------------------------------------------

data("Vietnam", package = "vcdExtra")

dim(Vietnam)

str(Vietnam)


car::some(Vietnam)



# ----------
# lrm needs case form and ordered factor
Vietnam2 <- expand.dft(Vietnam)

Vietnam2$response <- ordered(Vietnam2$response)



# ------------------------------------------------------------------------------
# proportional odds model by lrm
# ------------------------------------------------------------------------------

library(rms)


viet.po2 <- lrm(response ~ sex + year, data = Vietnam2)


viet.po2



# ------------------------------------------------------------------------------
# plot of conditional X means
# ------------------------------------------------------------------------------

op <- par(mfrow = c(1,2))

plot.xmean.ordinaly(response ~ sex + year, data = Vietnam2, lwd = 2, pch = 16)

par(op)



# -->
# Solid lines connect the stratified means of X given Y.
# Dashed lines show the estimated expected value of X given Y = j if the proportional odds model hods for X


# There is some evidence that the effect of sex is non-monotonic and the means differ from their model-implied values under the PO assumtion
# The effect of year looks good by tis method.


