setwd("//media//kswada//MyFiles//R//fatalities")

packages <- c("dplyr", "AER")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Fatalities
# ------------------------------------------------------------------------------

data("Fatalities", packages = "AER")


str(Fatalities)

dim(Fatalities)



# ----------
Fatalities$frate <- with(Fatalities, fatal / pop * 10000)




# ------------------------------------------------------------------------------
# Estimate unobserved effect for each individual:
# Within estimator (Fixed Effects Model)
# 
#  - The fixed efects model, requiring only minimal assumptions on the nature of heterogeneity is one of the simplest and most robust specifications
#    in panel data econometrics and often the benchmark against which more sophisticated, and possible efficient,
#    ones are compared and judged in applied practice.
#  - LSDV estimator is adding a potentially large number of covariates to the basic specification of interest and can be very inefficient.
#    Within estimator's approach is to transform the data by subtracting the average over time (individual) to every variable, "time-demeaning"
# ------------------------------------------------------------------------------

fm <- frate ~ beertax


# model = "within" is default
femod <- plm(fm, Fatalities, method = "within")



# ----------
lmtest::coeftest(femod)



# ----------
fixef(femod)

coef(lsdvmod)



# -->
# same estimation as Least Squares Dummy Variable Model



# ------------------------------------------------------------------------------
# Cluster-robust standard errors
# ------------------------------------------------------------------------------

# standard SE
coeftest(femod)


# cluster-robust standard errors
coeftest(femod, vcov = vcovHC(femod), type = "HCO", cluster = "state")


# -->
# Note that the standard errors are larger



# ------------------------------------------------------------------------------
# Individual effects
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(fixef(femod, tyoe = "level"), type = "h", xlab = "state", ylab = "", main = "individual intercepts")
plot(fixef(femod, type = "dfirst"), type = "h", xlab = "state", ylab = "", main = "individual effects in deviations from the 1st individual" )
plot(fixef(femod, type = "dmean"), type = "h", xlab = "state", ylab = "", main = "individual effects in deviations from mean")

