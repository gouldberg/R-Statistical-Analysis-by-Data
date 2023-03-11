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




# ------------------------------------------------------------------------------
# data exploration: scatterplot by year
# ------------------------------------------------------------------------------


# packs ~ price

car::scatterplot(packs ~ price | year, data = CigarettesSW, col = c(gray(0.7), "black"))

car::scatterplot(packs ~ rprice | year, data = CigarettesSW, col = c(gray(0.7), "black"))




# ----------
# packs ~ taxs

car::scatterplot(packs ~ taxs | year, data = CigarettesSW, col = c(gray(0.7), "black"))


# packs ~ salestax

car::scatterplot(packs ~ salestax | year, data = CigarettesSW, col = c(gray(0.7), "black"))


# packs ~ cigtax

car::scatterplot(packs ~ cigtax  | year, data = CigarettesSW, col = c(gray(0.7), "black"))




# ----------
# price ~ salestax

car::scatterplot(rprice ~ salestax | year, data = CigarettesSW, col = c(gray(0.7), "black"))


# price ~ cigtax

car::scatterplot(rprice ~ cigtax | year, data = CigarettesSW, col = "black")





# ------------------------------------------------------------------------------
# data exploration: scatterplot matrix by year
# ------------------------------------------------------------------------------


formula <- ~ packs + rprice + salestax + cigtax + rincome

scatterplotMatrix(formula, data = CigarettesSW, groups = CigarettesSW$year,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c(gray(0.6), "black"), pch = c(1, 20))



# -->
# Note that the relationship rprice and packs (including both of 1985 and 1995)




# ------------------------------------------------------------------------------
# Correlation between sales tax and price
# ------------------------------------------------------------------------------


# check the correlation between sales tax and price

cor(CigarettesSW$salestax, CigarettesSW$rprice)

cor(CigarettesSW$cigtax, CigarettesSW$rprice)



#-->
# the sample correlation is a consistent estimator of the population correlation.
# The estimate of approximately 0.70 indicates:  higher sales taxes lead to higher prices.
# However, a correlation analysis like this is not sufficient for checking
# whether the instrument is relevant.





# ------------------------------------------------------------------------------
# Comparison between salestax and cigtax
# ------------------------------------------------------------------------------

library(psych)


var_obj <- c("packs", "rprice", "salestax", "cigtax", "rincome")


# here we apply method = "spearman" due to terrible skewness

pairs.panels(CigarettesSW[, var_obj], ci = TRUE, smoother = TRUE, stars = TRUE, method = "spearman")


# only 1995
pairs.panels(c1995[, var_obj], ci = TRUE, smoother = TRUE, stars = TRUE, method = "spearman")




# -->
# note that cigtax is more correlated with packs and rpices thatn salestax

