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
# Ordinaly Linear Regression separately by year
# ------------------------------------------------------------------------------


library(lme4)


lmod_list <- lmList(log(packs) ~ log(rprice) + log(rincome) + salestax | year, data = CigarettesSW)


summary(lmod_list)



# -->
# again, the coefficient of salestax is positive and NOT significant
# rincome is NOT significant




# ------------------------------------------------------------------------------
# Compare coefficient with confidential interval
# ------------------------------------------------------------------------------


coef(lmod_list)


plot(confint(lmod_list))




# ------------------------------------------------------------------------------
# Ordinaly Linear Regression separately for only 1995
# ------------------------------------------------------------------------------


# here not take into account rincome

lmod_1995 <- lm(log(packs) ~ log(rprice) + salestax, data = c1995)


summary(lmod_1995)



# -->
# salestax:  positive value coefficient
# the coefficient for log(rprice) = -1.328
# --> roughly the 1% of cigarette price decrease will increase 1.3% of cigarrete consumption




# ----------

library(effects)


plot(predictorEffects(lmod_1995))


