setwd("//media//kswada//MyFiles//R//hsb")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  hsb
# ------------------------------------------------------------------------------

data("hsb", package = "faraway")


str(hsb)


head(hsb)



# ------------------------------------------------------------------------------
# Proportional Hazards Model
#   - Hazards of category j is the probability of falling in category j given that your category is greater than j.
#   - The link function is complementary log-log and the corresponding latent variable distribution is the extreme value
# ------------------------------------------------------------------------------

# The extreme value distribution is not symmetric like the logistic and normal and so there seems little justification for
# applying it to this data, but try.

library(MASS)


phmodi <- polr(prog ~ ses + schtyp + math + science + socst, method = "cloglog", hsb)


phmodi



# ----------
pchisq(deviance(phmodi) - deviance(mmodi), mmodi$edf - phmodi$edf, lower = FALSE)



# -->
# Proportional Hazards is marginally better ..



# ----------
summary(mmodi)


summary(phmodi)


# -->
# But the coefficients appear to be different


