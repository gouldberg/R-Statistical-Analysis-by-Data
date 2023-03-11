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


# convert ot numeric
CodParasites$prevalence <- ifelse(CodParasites$intensity == 0, 0, 1)



# ----------
cp_zero <- glm(prevalence ~ length + area * year, data = CodParasites, family = binomial)

cp_nzero <- glm.nb(intensity ~ length + area * year, data = CodParasites, subset = intensity > 0)



# ----------
eff.zero <- effects::allEffects(cp_zero)




# ----------
# year effect
plot(eff.zero[1], ylim = c(-2.5, 2.5), main = "Hurdle zero model: length effect")



# ----------
# area * year effect
# Multiline type is better !!!
plot(eff.zero[2], ylim = c(-2.5, 2.5),
     multiline = TRUE, ci.style = "bars",
     key.args = list(x = .05, y = .95, columns = 1),
     colors = c("black", "red", "blue"), lty = 1:3,
     symbols = 15:17, cex = 2,
     main = "Hurdle zero model: area * year effect")



# -->
# The effect of length on prevalence is slightly increasing, but we saw earlier that this is not significant.

# For the area-year interaction, the 3 curves have similar shapes, except for the aberrant value for soroya in 2001.
# and the closeness of the values at Mageroya in all years.
# Overall, prevalence was highest in 2000, and also in the Varangerfjord samples.


