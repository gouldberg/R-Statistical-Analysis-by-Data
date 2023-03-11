setwd("//media//kswada//MyFiles//R//favric")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fabric
# ------------------------------------------------------------------------------
data("fabric", package = "gamlss.data")


str(fabric)

car::some(fabric)



# ------------------------------------------------------------------------------
# Flexible regression and smoothing
# ------------------------------------------------------------------------------

# Fit the Poisson distribution model

mPO1 <- gamlss(y ~ x, data = fabric, family = PO)

mPO2 <- gamlss(y ~ pb(x), data = fabric, family = PO)


mPO1_l <- gamlss(y ~ leng, data = fabric, family = PO)

mPO2_l <- gamlss(y ~ pb(leng), data = fabric, family = PO)


summary(mPO1)

summary(mPO2)



# ----------
GAIC(mPO1, mPO2, mPO1_l, mPO2_l)


# -->
# slightly better for original "leng" ??

