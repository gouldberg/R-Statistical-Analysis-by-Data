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
# Ordinaly Linear Regression separately for only 1995:  model comparison
# ------------------------------------------------------------------------------


lmod_1995_0 <- lm(log(packs) ~ log(rprice) + log(rincome), data = c1995)


lmod_1995_1 <- lm(log(packs) ~ log(rprice) + salestax, data = c1995)


lmod_1995_2 <- lm(log(packs) ~ log(rprice) + salestax + log(rincome), data = c1995)


lmod_1995_3 <- lm(log(packs) ~ log(rprice) + salestax + log(rincome) + cigtax, data = c1995)




# ----------
stargazer(lmod_1995_0, lmod_1995_1, lmod_1995_2, lmod_1995_3,
          type = "text",
          digits = 3, 
          column.labels = c("rincome", "salestax", "rincome + salestax", "also cigtax"))




# -->
# Note that the coefficients for log(rprice) are very different for each model ...

# for model lmod_1995_3:  cigtax and saletax has prositive coefficients
#  -->  coefficient of log(rprice) to adjust those effects resulted in large negative coefficient ...




# ----------
car::residualPlots(lmod_1995_0)

car::residualPlots(lmod_1995_1)

car::residualPlots(lmod_1995_2)

car::residualPlots(lmod_1995_3)




# ----------
# AIC is better for the third model ...

AIC(lmod_1995_1, lmod_1995_2, lmod_1995_3)




# ----------
# and note that standard errors of log(rprice) is increasing
# model 0  -->  model 1  -->  model 2  -->  model 3

# the starndard errors of log(rprice)for is largest for model 3 (still, coef is significant)

coeftest(lmod_1995_0)

coeftest(lmod_1995_1)

coeftest(lmod_1995_2)

coeftest(lmod_1995_3)

