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
# Data Exploration:  data transformation for "Ozone"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ Ozone, data = airquality)



# transforming for symmetry
car::symbox(~ Ozone, data = airquality)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,

p1 <- car::powerTransform(Ozone ~ 1, data = airquality, family = "bcnPower")

summary(p1)



# should use lambda = 0.2032
car::testTransform(p1, lambda = 0.2032)



# ----------
tmp <- airquality %>% mutate(Ozone2 = Ozone ^ (0.2))

car::densityPlot( ~ Ozone2, data = tmp)


