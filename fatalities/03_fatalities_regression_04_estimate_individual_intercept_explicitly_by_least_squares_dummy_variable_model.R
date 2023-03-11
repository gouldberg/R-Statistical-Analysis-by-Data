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
# Estimate individual intercepts explicitly  (Least Squares Dummy Variable Model)
# ------------------------------------------------------------------------------

fm <- frate ~ beertax

lsdv.fm <- update(fm, . ~ . + state - 1)



# ----------
lsdvmod <- lm(lsdv.fm, Fatalities)



# ----------
lmtest::coeftest(lsdvmod)



# -->
# The estimate is numerically different but supports the samle qualitative conclusions.
