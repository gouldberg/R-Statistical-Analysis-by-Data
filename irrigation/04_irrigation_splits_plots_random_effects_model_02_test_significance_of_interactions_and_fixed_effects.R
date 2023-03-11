setwd("//media//kswada//MyFiles//R//irrigation")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  irrigation
# ------------------------------------------------------------------------------

data("irrigation", package = "faraway")

str(irrigation)

car::some(irrigation)




# ------------------------------------------------------------------------------
# Model term selection by KRmodcomp()
# ------------------------------------------------------------------------------

# The relatively large standard errors compared to the fixed effect estimates suggest that
# there may be no significant fixed effects.
# We can check this sequentially using F-tests with adjusted degreees of freedom.

library(pbkrtest)


cmod <- lmer(yield ~ irrigation * variety + (1 | field), data = irrigation)

cmoda <- lmer(yield ~ irrigation + variety + (1 | field), data = irrigation)


KRmodcomp(cmod, cmoda)



# -->
# We find there is no significant interaction term.



# ----------
# Dropping variety from the model
cmodi <- lmer(yield ~ irrigation + (1 | field), data = irrigation)


KRmodcomp(cmoda, cmodi)



# -->
# Dropping variety from the model seems reasonable since the p-value of 0.25 is large.



# ----------
# Dropping irrigation from the model
cmodv <- lmer(yield ~ variety + (1 | field), data = irrigation)


KRmodcomp(cmoda, cmodv)



# -->
# Irrigation also fails to be significant.



# ------------------------------------------------------------------------------
# Check the diagnostic plots
# ------------------------------------------------------------------------------

par(mfrow = c(1,2))

plot(fitted(cmod), residuals(cmod), xlab = "Fitted", ylab = "Residuals")

qqnorm(residuals(cmod), main = "")




# -->
# We can see that there is no problem with the nonconstant variance, but that the residuals indicate a bimodal distribution
# caused by the pairs of observations in each field.
# But this type of divergence from normality is unlikely to cause any major problems with the estimation and inference.


