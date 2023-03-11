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
# Assess correlation among variables again
# ------------------------------------------------------------------------------


library(psych)


var_obj <- c("packs", "rprice", "rincome", "salestax", "cigtax")


# here we apply method = "spearman" due to terrible skewness

pairs.panels(c1995[, var_obj], ci = TRUE, smoother = TRUE, stars = TRUE, method = "spearman")




# -->
# saletax and cigtax are correlated strongly with rprice and rincome
# (but negative to packs)




# ------------------------------------------------------------------------------
# multiple regression with IV
# ------------------------------------------------------------------------------


cig_ivreg2 <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + salestax, data = c1995)


summary(cig_ivreg2)





# ----------
# overidentification model for comparison

cig_ivreg3 <- ivreg(log(packs) ~ log(rprice) + log(rincome) |  log(rincome) + salestax + cigtax, data = c1995)




# ----------
coeftest(cig_ivreg2, vcov = vcovHC, type = "HC1")

coeftest(cig_ivreg3, vcov = vcovHC, type = "HC1")


coeftest(lmod_1995_0)




# -->
# coefficients of log(rprice) is -1.14 and -1.28, respectively

# Note that in the latter model, the standard errors are small (2/3 of former model)




# -->
# Using the two instruments salestaxi and cigtaxi we have m=2 and k=1
# so the coefficient on the endogenous regressor log(P) is overidentified.

# The estimates obtained using both instruments are more precise
# since in the latter all standard errors reported are smaller than in the former.

# In fact, the standard error for the estimate of the demand elasticity is only two thirds of the standard error
# when the sales tax is the only instrument used.
# This is due to more information being used in estimation in the latter.
# If the instruments are valid, the latter can be considered more reliable.
# However, without insights regarding the validity of the instruments it is not sensible to make such a statement. 
# This stresses why checking instrument validity is essential. 

