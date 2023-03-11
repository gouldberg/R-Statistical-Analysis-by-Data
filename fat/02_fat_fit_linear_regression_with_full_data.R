setwd("//media//kswada//MyFiles//R//fat")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fat
# ------------------------------------------------------------------------------

data(fat, package="faraway")

str(fat)



# ------------------------------------------------------------------------------
# Fit linear regression with all variables
# ------------------------------------------------------------------------------

lmod <- lm(brozek ~ age + weight + height + neck + chest + abdom + hip + thigh + knee + ankle + biceps + forearm + wrist, data = fat)


summary(lmod)



# ------------------------------------------------------------------------------
# Check the median characteristics
# ------------------------------------------------------------------------------
# let's consider the typical man, exemplified by the median value of all the predictors

x <- model.matrix(lmod)

head(x)

( x0 <- apply(x, 2, median) )

( y0 <- sum(x0 * coef(lmod)))



# ----------
# 95% prediction interval and confidence interval

predict(lmod, newd = data.frame(t(x0)), interval = "prediction")

predict(lmod, new = data.frame(t(x0)), interval = "confidence")


# -->
# The prediction iterval ranges from 9.6% body fat up to 25.4%, this is a wide interval.
# Also R2 = 0.75 but perhaps not sufficient for practical use.

# confidence interval for the mean response is much narrower, indicating we can be quire sure about the average body fat of the man with the median characteristics.



# ------------------------------------------------------------------------------
# Let's see the body fat value at characteristics at further from the original data
# ------------------------------------------------------------------------------

( x1 <- apply(x, 2, function(x) quantile(x, 0.95)) )


predict(lmod, newd = data.frame(t(x1)), interval = "prediction")

predict(lmod, new = data.frame(t(x1)), interval = "confidence")


# -->
# For confidene interval, uncertainty is increased from 1% to 4% compared to median characteristics
# But prediction interval is only slightly wider because this interval is dominated by the new error rather than the uncertainty in the estimation of beta.




