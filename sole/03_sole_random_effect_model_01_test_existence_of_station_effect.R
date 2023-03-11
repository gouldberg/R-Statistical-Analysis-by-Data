setwd("//media//kswada//MyFiles//R//sole")

packages <- c("dplyr", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sole
# ------------------------------------------------------------------------------

data("sole", package = "gamair")

str(sole)

head(sole)



# ------------------------------------------------------------------------------
# GLRT test for existence of station effect
#   - It is very likely that there is a "sampling station" component to the variance of the data.
# ------------------------------------------------------------------------------


# Extract deviance residuals
rf <- residuals(b4, type = "d")


# create an identifier for each sampling station
solr$station <- factor(with(solr, paste(-la, -lo, -t, sep = "")))



# ----------
# Is there evidence of a station effect in the residuals ?
solr$rf <- rf

rm0 <- lm(rf ~ 1, solr)


rm <- lme(rf ~ 1, solr, random = ~ 1 | station)



# ----------
summary(rm)

anova(rm, rm0)


# -->
# The GLRT test, performed by the anova function, clearly rejectes the i.i.d model
# AIC also suggests that there is a real sampling station effect.



