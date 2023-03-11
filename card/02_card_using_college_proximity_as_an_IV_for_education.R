# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//card")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  card
# ------------------------------------------------------------------------------

card <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/card.dta")


str(card)



# ------------------------------------------------------------------------------
# Check for relevance as an IV for education:  using college proximity as an IV for education
#   - nearc4:  indicates whether the individual grew up close to a college.
# ------------------------------------------------------------------------------

# Education is allowed to be endogenous and instrumented with the dummy variable nearc4 which indicates whether the individual grew up close to a college.
# In addition, we control for experience, race, and regional information.
# These variables are assumed to be exogeneous and act as their own instruments.

# We first check for relevance by regressing the endogeneous independent variable educ on all exogeneous variables including the instrument nearc4.

redf <- lm(educ ~ nearc4 + exper + I(exper^2) + black + smsa + south + smsa66 + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669, data = card)

summary(redf)



# -->
# The coefficient of nearc4 is 0.32 and statistically highly significant.



# ------------------------------------------------------------------------------
# Estimate the log wage equation with OLS
# ------------------------------------------------------------------------------

ols <- lm(log(wage) ~ educ + exper + I(exper^2) + black + smsa + south + smsa66 + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669, data = card)


summary(ols)



# -->
# coefficients of educ is 0.075 and highly significant.

plot(ols)



# ------------------------------------------------------------------------------
# IV estimation
# ------------------------------------------------------------------------------

iv <- ivreg(log(wage) ~ educ + exper + I(exper^2) + black + smsa + south + smsa66 + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669 |
              nearc4 + exper + I(exper^2) + black + smsa + south + smsa66 + reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669, data = card)


# coeftest(iv, vcov = vcovHC, type = "HC1")
summary(iv, vcov = vcovHC, type = "HC1")


stargazer(redf, ols, iv, type = "text", keep = c("ed", "near", "exp", "bl"), keep.stat = c("n", "rsq"))



# -->
# coeffs of educ is 0.132 and larger than that of OLS (0.075)


