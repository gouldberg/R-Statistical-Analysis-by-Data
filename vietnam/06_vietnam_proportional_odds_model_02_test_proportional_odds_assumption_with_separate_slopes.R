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
# vglm needs case form and ordered factor  ("weights" have different meaning)
Vietnam2 <- expand.dft(Vietnam)

Vietnam2$response <- ordered(Vietnam2$response)



# ------------------------------------------------------------------------------
# proportional odds model by VGAM
# fitting separate slopes for males and females but parallel lines for the other predictor
# ------------------------------------------------------------------------------

library(VGAM)


viet.ppo <- vglm(response ~ sex + year, data = Vietnam2, family = cumulative(parallel = FALSE ~ sex))

viet.ppo

summary(viet.ppo)



# ----------
# VGAM package defines a coef() method that can print the coefficients in a more readable matrix form giving the category cutpoints
coef(viet.ppo, matrix = TRUE)




# ----------
VGAM::lrtest(viet.npo, viet.po, viet.ppo)

