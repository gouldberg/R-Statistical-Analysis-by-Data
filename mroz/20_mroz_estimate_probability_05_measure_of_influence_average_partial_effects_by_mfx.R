setwd("//media//kswada//MyFiles//R//mroz")

packages <- c("dplyr", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mroz
# ------------------------------------------------------------------------------

library(foreign)


# This data has "inlf" variable
mroz <- read.dta("http://fmwww.bc.edu//ec-p//data//wooldridge//mroz.dta")

str(mroz)

names(mroz)


# oursample <- subset(mroz, !is.na(wage))



# ------------------------------------------------------------------------------
# Automatic average partial effect calculations with package mfx
# ------------------------------------------------------------------------------

# automatic APE calculations with package mfx
library(mfx)

logitmfx(inlf ~ nwifeinc + educ + exper + I(exper^2) + age + kidslt6 + kidsge6, data = mroz, atmean = FALSE)



# -->
# The APEs are calcualted by multiplying the coefficient vector obtained with coef with the corresponding factor.
# Note that for the linear probability model, the partial effects are constant and simply equal to the coefficients.

# The results for the constant do not have a direct meaningful interpretation.
# The APEs for the other variables do not differ too much between the models.
# As a general observation, as long as we are interested in APEs only and not in individual predictions or prtial effects and 
# as long as not too many probabilities are close to 0 or 1, the linear probability model often works well enough.



# ----------
# The twos are differenct
cbind(APE.lin, APE.log, APE.prob)

coeftest(probitres)




