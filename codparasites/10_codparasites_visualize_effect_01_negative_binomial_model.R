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
# Main effect plot for NB model
# ------------------------------------------------------------------------------

library(effects)


# effects is applicable to glm.nb
eff.nb <- effects::allEffects(mod.nbin)



# ----------
# year effect
plot(eff.nb[1], type = "response", ylim = c(0, 30), main = "NB model: length effect")



# ----------
# area * year effect
# Multiline type is better !!!
plot(eff.nb[2], type = "response", ylim = c(0, 30),
     multiline = TRUE, ci.style = "bars",
     key.args = list(x = .05, y = .95, columns = 1),
     colors = c("black", "red", "blue"), lty = 1:3,
     symbols = 15:17, cex = 2,
     main = "NB model: area * year effect")

