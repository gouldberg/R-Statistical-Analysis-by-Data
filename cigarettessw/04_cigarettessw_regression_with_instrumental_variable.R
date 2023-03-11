setwd("//media//kswada//MyFiles//R//cigarettessw")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  CigarettesSW
# ------------------------------------------------------------------------------


data("CigarettesSW", package = "AER")



dim(CigarettesSW)


str(CigarettesSW)



car::some(CigarettesSW)




# ----------
CigarettesSW$rprice <- with(CigarettesSW, price / cpi)

CigarettesSW$salestax <- with(CigarettesSW, (taxs - tax) / cpi)

CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)

CigarettesSW$cigtax <- with(CigarettesSW, tax / cpi)



# ----------
# generate a subset for the year 1995

c1995 <- subset(CigarettesSW, year == "1995")




# ------------------------------------------------------------------------------
# Instrumental Variable estimation with 2SLS:  1st stage
# ------------------------------------------------------------------------------

# here, not take into account rincome



# perform the first stage regression
cig_s1 <- lm(log(rprice) ~ salestax, data = c1995)




# ----------
# it is important to compute heteroskedasticity-robust standard errors
coeftest(cig_s1, vcov = vcovHC, type = "HC1")



# ----------
# inspect the R^2 of the first stage regression
summary(cig_s1)$r.squared



# -->
# 47% of the variation in after tax prices is explained by the variation of the sales tax across states.




# ------------------------------------------------------------------------------
# Instrumental Variable estimation with 2SLS:  2nd stage
# ------------------------------------------------------------------------------

# 2nd stage regression

lcigp_pred <- cig_s1$fitted.values


cig_s2 <- lm(log(c1995$packs) ~ lcigp_pred)




# ----------
# compare the coefficients with previous ordinaly regression model

coeftest(cig_s2, vcov = vcovHC)

coeftest(lmod_1995, vcov = vcovHC)




# ----------

library(effects)


plot(predictorEffects(cig_s2))





# ------------------------------------------------------------------------------
# Instrumental Variable estimation with 2SLS by ivreg() automatically
# ------------------------------------------------------------------------------

cig_ivreg <- ivreg(log(packs) ~ log(rprice) | salestax, data = c1995)


coeftest(cig_ivreg, vcov = vcovHC, type = "HC1")



# -->
# the TSLS estimate for beta1 suggests that an increase in cigarette prices by one percent reduces cigarette consumption by roughly 1.08 percentage points,
# which is fairly elastic. However, we should keep in mind that this estimate might not be trustworthy
# even though we used IV estimation:
# there still might be a bias due to omitted variables. Thus a multiple IV regression approach is needed.
