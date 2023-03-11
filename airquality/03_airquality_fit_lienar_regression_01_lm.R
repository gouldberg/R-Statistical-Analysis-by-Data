setwd("//media//kswada//MyFiles//R//airquality")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  airquality
# ------------------------------------------------------------------------------

data("airquality")


str(airquality)

car::some(airquality)




# ------------------------------------------------------------------------------
# Fit the linear regression
# ------------------------------------------------------------------------------

da <- na.omit(airquality)

l1 <- lm(Ozone ~ Temp + Wind + Solar.R, data = airquality)



# ----------
summary(l1)



# -->
# All terms are significant
# Temp and Solar.R has positive coef and Wind has negative coef


