setwd("//media//kswada//MyFiles//R//bartlett")

packages <- c("dplyr", "datasets", "vcd", "vcdExtra", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Bartlett data on plum-root cuttings
# ------------------------------------------------------------------------------
data("Bartlett", package = "vcdExtra")

data <- Bartlett

data



# ------------------------------------------------------------------------------
# Fit the model of no 3-way association:  [Alive * Time][Alive * Length][Time * Length]
# ------------------------------------------------------------------------------
mod1 <- MASS::loglm(~ Alive * Time + Alive * Length + Time * Length, data = data)



# The output includes both the chi-square statistic and the deviance test statistics, 
mod1
residuals(mod1, type = "pearson")


mosaic(data, expected = ~ Alive * Time + Alive * Length + Time * Length, gp = shading_Friendly2)



# ------------------------------------------------------------------------------
# Fit simpler models that omit one or more of the 2-way terms
#  - joint independence:  [Alive * Time][Length]
#  - joint independence:  [Alive * Length][Time]
#  - joint independence:  [Time * Length][Alive]
#  - conditional independence:  [Alive * Length][Time * Length]
#  - conditional independence:  [Alive * Time][Length * Time]
#  - conditional independence:  [Length * Alive][Time * Alive]
# ------------------------------------------------------------------------------
mod2 <- MASS::loglm(~ Alive * Time + Length, data = data)
mod3 <- MASS::loglm(~ Alive * Length + Time, data = data)
mod4 <- MASS::loglm(~ Time * Length + Alive, data = data)
mod5 <- MASS::loglm(~ Alive * Length + Time * Length, data = data)

mod6 <- MASS::loglm(~ Alive * Time + Time * Length, data = data)
mod7 <- MASS::loglm(~ Alive * Length + Time * Alive, data = data)

mod8 <- MASS::loglm(~ Alive * Length, data = data)
mod9 <- MASS::loglm(~ Alive * Time, data = data)
mod10 <- MASS::loglm(~ Time * Length, data = data)


mod1
mod2
mod3
mod4
mod5
mod6
mod7
mod8
mod9
mod10


# ----------
# The best fit is the model with no 3-way interaction
mosaic(data, expected = ~ Alive * Time + Alive * Length + Time * Length, gp = shading_Friendly2)

mosaic(data, expected = ~ Alive * Length + Time, gp = shading_Friendly2)



