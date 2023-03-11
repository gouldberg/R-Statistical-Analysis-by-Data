setwd("//media//kswada//MyFiles//R//wcgs")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wcgs
# ------------------------------------------------------------------------------

data("wcgs", package = "faraway")

str(wcgs)



# ------------------------------------------------------------------------------
# Logistic regression for binary response
# ------------------------------------------------------------------------------

# By default, the first lvel alphabetically will be asoociated with y = 0
lmod <- glm(chd ~ height + cigs, family = binomial, wcgs)


# Note that this summary shows not test by deviance,
# but test by z-value (= beta / se(beta)), which is approximately normally distributed.
summary(lmod)


# shortned summary
faraway::sumary(lmod)



# ------------------------------------------------------------------------------
# Compute the probability of heart disease given the values of the predictors
# and Plot the ilogit curve
# ------------------------------------------------------------------------------

( beta <- coef(lmod) )


# ----------
plot(jitter(y, 0.1) ~ jitter(height), wcgs, xlab = "Height", ylab = "Heart Disease", pch = ".")
curve(ilogit(beta[1] + beta[2] * x + beta[3] * 0), add = TRUE)
curve(ilogit(beta[1] + beta[2] * x + beta[3] * 20), add = TRUE, lty = 2)



# ----------
plot(jitter(y, 0.1) ~ jitter(cigs), wcgs, xlab = "Cigarette Use", ylab = "Heart Disease", pch = ".")
curve(ilogit(beta[1] + beta[2] * 60 + beta[3] * x), add = TRUE)
curve(ilogit(beta[1] + beta[2] * 78 + beta[3] * x), add = TRUE, lty = 2)



# ------------------------------------------------------------------------------
# Interpretation of the coefficient
# ------------------------------------------------------------------------------

# beta1 is a unit increase in x1 with x2 held fixed increases the log-odds of success by beta1
# or increase the odds of success by a factor of exp(beta1)
beta
exp(beta)  # nearly equal to 1 + beta


# -->
# We can say that the odds of heart disease increase
# by 2.5% with each additional inch in height
# and by 2.3% with each additional cigarette smoked per day.

# Note that exp(x) nearly equal to 1 + x for small values of x
# We observe that beta2 = 0.023 so the 2.3% increase in odds due to smoking a cigarette a day could have been quickly estimated from the original model.



# ------------------------------------------------------------------------------
# relative risk = p1 / p2  nearly equal to odds ratio
# ------------------------------------------------------------------------------
# the relative risk = p1 / p2
c(ilogit(sum(beta * c(1, 68, 20))), ilogit(sum(beta * c(1, 68, 0))))

ilogit(sum(beta * c(1, 68, 20))) / ilogit(sum(beta * c(1, 68, 0)))


# odds ratio
exp(beta[3] * 20)


# -->
# The relative risk of 1.54 is not far different from the odds ratio of 1.59
# For low probability outcomes, the relative risk and the odds ratio will be very similar, but for larger probabilitieis, there may be substantial differences.



# ------------------------------------------------------------------------------
# Test the individual predictors:  Deviance based test is preferred !!!
# ------------------------------------------------------------------------------

# We test the significance of height in the model
# by computing the difference in the deviance
lmodc <- glm(chd ~ cigs, family = binomial, wcgs)

anova(lmodc, lmod, test = "Chi")

drop1(lmod, test = "Chi")



# ----------
# An alternative to this test is the z-value, which is beta / se(beta), which is approximately normally distributed.
summary(lmod)



# -->
# In this case, the outcomes are similar to previous tests but not identical.
# In some cases, espeically with saprse data, the standard errors can be overestimated and so the z-value is too small
# and the significance of an effect could be missed.



# ------------------------------------------------------------------------------
# Confidence intervals for the regression parameters
# ------------------------------------------------------------------------------

# profile likelihood-based confidence interval
confint(lmod)








