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
# Estimate unobserved effect for each state by differencing
# ------------------------------------------------------------------------------

library(plm)


# There we take differences between the 1st and last years in the sample to get rid of the individual intercepts

dmod <- plm(diff(frate, 5) ~ diff(beertax, 5), Fatalities, model = "pooling")



# ----------
lmtest::coeftest(dmod)


# -->
# Estimation on five-year differences finally yields a sensible result: after controlling for state heterogeneity,
# higher taxation on beer is associated with a lower number of fatalities.


# ----------
# index of the model
index(dmod)