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
# Fit the linear regression by gamlss
# ------------------------------------------------------------------------------

# gamlss() function does not work with NA's, so before fitting the model the cases with missing values have to be removed.
da <- na.omit(airquality)

r1 <- gamlss(Ozone ~ Temp + Wind + Solar.R, data = da)



# ----------
summary(r1)


summary(l1)



# -->
# The coefficient estimates of the two fits are identical.




# ----------
# Residual Standard Errors:

# The MLE of sigma  = 20.80 = exp(3.035)
fitted(r1, "sigma")[1]


summary(r1)


summary(l1)



# -->
# slight difference from l1 model (21.18)



