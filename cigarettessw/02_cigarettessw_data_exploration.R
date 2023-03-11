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





# ------------------------------------------------------------------------------
# data exploration: data distribution
# ------------------------------------------------------------------------------


summary(CigarettesSW)



# taxs:  sales tax + cig tax
# tax:  cig tax
summary(CigarettesSW[,c("price", "taxs", "tax")])




# ----------
# compute real per capita prices
CigarettesSW$rprice <- with(CigarettesSW, price / cpi)



# compute the sales tax
CigarettesSW$salestax <- with(CigarettesSW, (taxs - tax) / cpi)



# real imcome
CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)



# cigtax
CigarettesSW$cigtax <- with(CigarettesSW, tax / cpi)



summary(CigarettesSW[,c("rprice", "salestax", "cigtax")])

