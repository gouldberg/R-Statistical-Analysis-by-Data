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
# nested effects model
# ------------------------------------------------------------------------------

library(lme4)


cmod <- lmer(time ~ 1 + manufact * speed + (1 | machine), data = lawn)


summary(cmod)



# -->
# Note that sigma(machine) is almost close to sigma(error)




# ----------
# overall intercept has been changed 

coef(lmod)

fixef(cmod)