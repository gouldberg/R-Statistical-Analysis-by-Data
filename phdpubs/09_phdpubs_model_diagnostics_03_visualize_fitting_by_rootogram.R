setwd("//media//kswada//MyFiles//R//phdpubs")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PhdPubs data
# ------------------------------------------------------------------------------

data("PhdPubs", package = "vcdExtra")


dim(PhdPubs)

str(PhdPubs)


car::some(PhdPubs)



# ----------
PhdPubs <- within(PhdPubs, {
  female <- factor(female)
  married <- factor(married)
})



# ------------------------------------------------------------------------------
# Visualize fitting by rootogram
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(3,3))

countreg::rootogram(modp, max = 25, main = "Poisson")

countreg::rootogram(mod.nbin, max = 25, main = "Negative Binomial")

countreg::rootogram(mod.nbin2, max = 25, main = "Negative Binomial 2")

countreg::rootogram(mod.zpois, max = 25, main = "Zero-inflated Poisson")

countreg::rootogram(mod.znbin, max = 25, main = "Zero-inflated Negative Binomial")

countreg::rootogram(mod.hpois, max = 25, main = "Hurdle Poisson")

countreg::rootogram(mod.hnbin, max = 25, main = "Hurdle Negative Binomial")

