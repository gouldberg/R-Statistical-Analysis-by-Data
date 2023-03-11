setwd("//media//kswada//MyFiles//R//codparasites")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Cod Parasites
# ------------------------------------------------------------------------------

data("CodParasites", package = "countreg")


dim(CodParasites)

str(CodParasites)


car::some(CodParasites)



# ----------
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, "no", "yes")



# ------------------------------------------------------------------------------
# Visualize fitting by rootogram
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(3,2))

countreg::rootogram(modp, max = 25, main = "Poisson")

countreg::rootogram(mod.nbin, max = 25, main = "Negative Binomial")

countreg::rootogram(mod.zpois, max = 25, main = "Zero-inflated Poisson")

countreg::rootogram(mod.znbin, max = 25, main = "Zero-inflated Negative Binomial")

countreg::rootogram(mod.hpois, max = 25, main = "Hurdle Poisson")

countreg::rootogram(mod.hnbin, max = 25, main = "Hurdle Negative Binomial")

