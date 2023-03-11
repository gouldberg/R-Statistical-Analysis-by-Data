setwd("//media//kswada//MyFiles//R//lawn")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lawn
# ------------------------------------------------------------------------------

data("lawn", package = "faraway")

str(lawn)


car::some(lawn)



# ------------------------------------------------------------------------------
# Nested effect model
# ------------------------------------------------------------------------------


cmod_nest <- lmer(time ~ 1 + speed + (1 | manufact) + (1 | manufact : machine), data = lawn)



summary(cmod_nest)



VarCorr(cmod_nest)




# -->
# variability between machines and variability between manufactures are almost same




# ------------------------------------------------------------------------------
# Confidence intervals of SD
# ------------------------------------------------------------------------------

confint(cmod_nest, method = "boot")



# -->
# but the confidence intervals are wider for manufactures variability
# Variablity can be ascribed solely to manufacturers ...



