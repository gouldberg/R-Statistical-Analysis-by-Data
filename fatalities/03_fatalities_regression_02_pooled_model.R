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
# pooled model by lm()
# ------------------------------------------------------------------------------


fm <- frate ~ beertax


# pooling all across sections together without considering any form of individual effect, can be done using the regular lm funtion
poolmod0 <- lm(fm, Fatalities)


lmtest::coeftest(poolmod0)



# ------------------------------------------------------------------------------
# pooled model by plm()
# ------------------------------------------------------------------------------

library(plm)


# pooling all across sections together without considering any form of individual effect, can be done using the regular lm funtion,
# or, equivalently, plm

poolmod <- plm(fm, Fatalities, model = "pooling")



# ----------
stargazer::stargazer(poolmod0, poolmod, intercept.bottom = FALSE, type = "text")




# ----------
lmtest::coeftest(poolmod0)


lmtest::coeftest(poolmod)



# -->
# the results of lm() and plm() are equivalent.
# Here again, surprisingly, the coefficient is significant and positive !!

# Taxing beer would seem to increase the number of deaths from road accidents so that, extendng this line of reasoning far beyond what the given evidence supports,
# i.e., for outside the given sample,
# one could even argue that free beer might lead to safer driving !!
# Similar results, contradicting the most basic intuition, appear for any single year in the sample.

# We suspect the presence of unobserved heterogeneity.
# If omitted from the specification, the individual intercepts will end up in the error term;
# if they are not independent of the regressor (here, if unobserved state-level characteristics are related to how the local beer tax is set)
# the OLS estimate will be biased and inconsistent.



