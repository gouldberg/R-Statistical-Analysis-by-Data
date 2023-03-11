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



# ----------
modp <- glm(intensity ~ length + area * year, data = CodParasites, family = poisson(link = "log"))



# ------------------------------------------------------------------------------
# The quasi-poisson model
# ------------------------------------------------------------------------------

mod.qpois <- glm(intensity ~ length + area * year, data = CodParasites, family = quasipoisson)

summary(mod.qpois)


plot(mod.qpois)

plot_resid(mod=mod.qpois, y = "intensity")



# ------------------------------------------------------------------------------
# negative-binomiaol model
# ------------------------------------------------------------------------------

library(MASS)


# ----------
# estimate theta first
( nbin <- glm.nb(intensity ~ length + area * year, data = CodParasites) )

( theta <- nbin$theta )



# ----------
mod.nbin <- glm(intensity ~ length + area * year, CodParasites, family = negative.binomial(theta))

summary(mod.nbin)



# ----------
plot(allEffects(mod.nbin), band.colors = "blue", lwd=3, ylab = "Intensity", main="", ylim=c(-2, log(30)))

plot_resid(mod = mod.nbin, y = "intensity")


