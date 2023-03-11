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
# Fit inverse Gaussian distributions and Box Cox Cole Green (BCCGo)
# ------------------------------------------------------------------------------

r22 <- gamlss(Ozone ~ Temp + Wind + Solar.R, data = da, family = IG)

r23 <- gamlss(Ozone ~ Temp + Wind + Solar.R, data = da, family = BCCGo)


plot(r22)

plot(r23)



# ------------------------------------------------------------------------------
# Compare models
# ------------------------------------------------------------------------------

# AIC
GAIC(r1, r2, r22, r23, k = 2)

AIC(r1, r2, r22, r23)



# ----------
# The best model is Gamma distribution model
plot(r2)


wp(r2, ylim.all = 2)



# -->
# The residuals at this stage look a lot better than the residuals in liner regression in that 
# at least some of the hetrogeneity in the residuals has disappeared.
# Also the curvature in the normal Q-Q plot has been substantially reduced.



# -->
# next we try GAM model



