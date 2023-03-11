setwd("//media//kswada//MyFiles//R//troutegg")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  troutegg
# ------------------------------------------------------------------------------

data("troutegg", package = "faraway")

str(troutegg)

head(troutegg)


troutegg$ratio <- troutegg$survive / troutegg$total



# ------------------------------------------------------------------------------
# Logistic regression for binomial (proportion) responses
# ------------------------------------------------------------------------------

bmod <- glm(cbind(survive, total - survive) ~ location + period, family = binomial, troutegg)

summary(bmod)

faraway::sumary(bmod)



# ------------------------------------------------------------------------------
# raw, deviance and pearson residuals vs. linear predictors and predicted probabilities (ratio)
# ------------------------------------------------------------------------------

head(predict(bmod))
head(predict(bmod, type = "response"))


linpred <- predict(bmod)
predprob <- predict(bmod, type = "response")


# raw residuals
( rawres <- residuals(bmod, type = "response") )
( rawres <- troutegg$ratio - predprob )


# standardized deviance residuals
devres <- rstandard(bmod, type = "deviance")


# standardized pearson residuals
pearres <- rstandard(bmod, type = "pearson")



# ----------
par(mfrow=c(2,3))
plot(rawres ~ linpred, xlab = "linpred", ylab = "raw resid")
plot(devres ~ linpred, xlab = "linpred", ylab = "st. deviance resid")
plot(pearres ~ linpred, xlab = "linpred", ylab = "st. pearson resid")


plot(rawres ~ predprob, xlab = "predprob", ylab = "raw resid")
plot(devres ~ predprob, xlab = "predprob", ylab = "st. deviance resid")
plot(pearres ~ predprob, xlab = "predprob", ylab = "st. pearson resid")



# ----------
# This is standardized deviance residuals
par(mfrow=c(1,2))
plot(devres ~ linpred, xlab = "linpred", ylab = "st. deviance resid")
plot(bmod, 1)



# ------------------------------------------------------------------------------
# levearage in halfnorm plot
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))
halfnorm(hatvalues(bmod))



# -->
# No single outlier is apparent, but perhaps one can discern a larger number of residuals which seem to follow a more dispersed distribution
# than the rest.



# ------------------------------------------------------------------------------
# Standardized Pearson Residuals vs. Leverage and Cook's Distance
# ------------------------------------------------------------------------------

par(mfrow=c(1,2))
plot(bmod, 4)
plot(bmod, 5)


# -->
# 14, 19, 20 



# ------------------------------------------------------------------------------
# Influence measures
# ------------------------------------------------------------------------------

influence.measures(bmod)



# ------------------------------------------------------------------------------
# Mean of empirical logits by period and location (interaction.plot)
# ------------------------------------------------------------------------------

# Calculate empirical logits: 0.5 is added to prevent infinite values for groups consisting of all successes or failures
elogits <- with(troutegg, log((survive + 0.5) / (total - survive + 0.5)))


with(troutegg, interaction.plot(period, location, elogits))



# -->
# There is no obvious sign of large interactions.
# So there is no evidence that the linear model is inadequate.



# ------------------------------------------------------------------------------
# Dispersion parameter
# ------------------------------------------------------------------------------

( sigma2 <- sum(residuals(bmod, type = "pearson")^2/bmod$df.residual) )


# -->
# We see that this is substantially larger than one as it would be in the standard binomial GLM.



# ----------
# We can now make F-tests on the predictors
drop1(bmod, scale = sigma2, test = "F")



# -->
# We see that both terms are clearly significant.


