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
# Ordinaly Linear Regression simultaneously with all data
# ------------------------------------------------------------------------------


lmod <- lm(log(packs) ~ log(rprice) + log(rincome) + salestax, data = CigarettesSW)


summary(lmod)



# -->
# the coefficient of salestax is positive and NOT significant



# ---------
# It seems not so bad fit by seeing residuals .. but fitted values are really separated 
car::residualPlot(lmod)


car::residualPlot(lmod, groups = CigarettesSW$year)


