setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)

dim(orings)


car::some(orings)



# ------------------------------------------------------------------------------
# Logistic regression for binomial (proportion) responses
# ------------------------------------------------------------------------------

lmod <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial, orings)


# Note that this summary shows not test by deviance,
# but test by z-value (= beta / se(beta)), which is approximately normally distributed.
summary(lmod)


# shortned summary
faraway::sumary(lmod)



# ------------------------------------------------------------------------------
# Fitted curve
# ------------------------------------------------------------------------------

# faraway for ilogit
library(faraway)


plot(damage/6 ~ temp, orings, xlim = c(25, 85), ylim = c(0, 1), xlab = "Temperature", ylab = "Prob of damage")


beta <- coef(lmod)
x <- seq(25, 85, 1)
curve(ilogit(beta[1] + beta[2] * x), add = TRUE)



# ----------
# We see very high probability of damage
ilogit(beta[1] + beta[2] * 31)




# ----------
library(broom)
tmp <- tidy(lmod)
plot(damage/6~temp, data = orings, xlim=c(25,85), ylim=c(0,1), xlab="Temperature", ylab="Prob of Damage")
x <- seq(25, 85, 1)
lines(x, exp(tmp$estimate[1] + tmp$estimate[2] * x) / (1 + exp(tmp$estimate[1] + tmp$estimate[2] * x)), lty=2)
lines(x, exp(tmp$estimate[1] + 0 * x) / (1 + exp(tmp$estimate[1] + 0 * x)), lty=3)




# ------------------------------------------------------------------------------
# Interpretation of the coefficient
# ------------------------------------------------------------------------------

# beta1 is a unit increase in x1 with x2 held fixed increases the log-odds of success by beta1
# or increase the odds of success by a factor of exp(beta1)
beta

round(exp(beta), 3)   # nearly equal to 1 + beta


# -->
# We can say that the odds of damage ratio decrease
# by 21.6% with each additional temperature ...

# Note that exp(x) nearly equal to 1 + x for small values of x


