setwd("//media//kswada//MyFiles//R//arrests")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra", "carData")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arrests
# ------------------------------------------------------------------------------
data("Arrests", package = "carData")

dim(Arrests)
str(Arrests)


car::some(Arrests)


data <- Arrests



# To allow for possible nonlinear effects of year, this variable was treated as a factor rather than as a (linear) numeric variable
data$year <- as.factor(data$year)



# ------------------------------------------------------------------------------
# main effect of colour
# ------------------------------------------------------------------------------
# The primary question for the newspaper concerned the overall difference between the treatment of blacks and whites -- main effect of colour
# --> This supports the claim by the Star because the 95% confidence limits for blacks and whites do not overlap, and all other relevant predictors
# that could account for this effect have been controlled or adjusted for.
graphics.off()

plot(effects::Effect("colour", arrests.mod),
     lwd = 3, ci.style = "bands", xlab = "Skin color of arrestee", cex = 1.25, ylab = "Probability(released)")



# ------------------------------------------------------------------------------
# effects of all higher-order terms
# ------------------------------------------------------------------------------
# allEffects() calculates the effects for all high-order terms in a given model.
# Relatively compact and understandable summary of the model
#  - (a) People were more likely to be released if they were employed and citizens
#  - (b) each additional police check decreased the likelihood of release with a summons
#  - (c) the effect of skin color varied with age and year of arrest,
# in ways that tell a far more nuanced story than reported in the newspaper.
arrests.effects <- effects::allEffects(arrests.mod, xlevels = list(age = seq(15, 45, 5)))

plot(arrests.effects, ylab = "Probability(released)", ci.style = "bands", ask = FALSE)

