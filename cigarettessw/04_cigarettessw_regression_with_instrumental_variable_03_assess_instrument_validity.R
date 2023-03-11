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
# generate a subset for the year 1995 and 1985

c1995 <- subset(CigarettesSW, year == "1995")

c1985 <- subset(CigarettesSW, year == "1985")




# ------------------------------------------------------------------------------
# Are the general sales tax and the cigarette-specific tax valid instruments?
# If not, TSLS is not helpful to estimate the demand elasticity for cigarettes

# Both variables are likely to be relevant but whether they are exogenous is a different question.
# Cigarette-specific taxes could be endogenous because there might be state specific historical factors like economic
# importance of the tobacco farming and cigarette production industry that lobby for low cigarette specific taxes.

# Since it is plausible that tobacco growing states have higher rates of smoking than others,
# this would lead to endogeneity of cigarette specific taxes.
# If we had data on the size on the tobacco and cigarette industry,
# we could solve this potential issue by including the information in the regression.
# Unfortunately, this is not the case.

# However, since the role of the tobacco and cigarette industry is a factor that can be assumed to differ across states
# but not over time we may exploit the panel structure of CigarettesSW instead: 

# regression using data on changes between two time periods eliminates such state specific and time invariant effects.
# we consider changes in variables between 1985 and 1995. That is, we are interested in estimating the long-run elasticity of the demand for cigarettes.



# ------------------------------------------------------------------------------
# differences from 1985 to 1995 for the dependent variable, the regressors and both instruments
# ------------------------------------------------------------------------------


packsdiff <- log(c1995$packs) - log(c1985$packs)


pricediff <- log(c1995$price/c1995$cpi) - log(c1985$price/c1985$cpi)


incomediff <- log(c1995$income/c1995$population/c1995$cpi) - log(c1985$income/c1985$population/c1985$cpi)


salestaxdiff <- (c1995$taxs - c1995$tax)/c1995$cpi - (c1985$taxs - c1985$tax)/c1985$cpi


cigtaxdiff <- c1995$tax/c1995$cpi - c1985$tax/c1985$cpi




# ----------
tmp <- data.frame(packsdiff, pricediff, incomediff, salestaxdiff, cigtaxdiff)



# ----------
par(mar = c(2,2,2,2))

formula <- ~ packsdiff + pricediff + incomediff + salestaxdiff + cigtaxdiff

scatterplotMatrix(formula, data = tmp,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c("black"), pch = c(1, 20))





# ------------------------------------------------------------------------------
# Checking instrument validity:  compare coefficients of IV models
# ------------------------------------------------------------------------------

# estimate the three models

cig_ivreg_diff1 <- ivreg(packsdiff ~ pricediff + incomediff | incomediff + salestaxdiff)

cig_ivreg_diff2 <- ivreg(packsdiff ~ pricediff + incomediff | incomediff + cigtaxdiff)

cig_ivreg_diff3 <- ivreg(packsdiff ~ pricediff + incomediff | incomediff + salestaxdiff + cigtaxdiff)




# ----------
# robust coefficient summary

coeftest(cig_ivreg_diff1, vcov = vcovHC, type = "HC1")

coeftest(cig_ivreg_diff2, vcov = vcovHC, type = "HC1")

coeftest(cig_ivreg_diff3, vcov = vcovHC, type = "HC1")




# ----------
( rob_se <- list(sqrt(diag(vcovHC(cig_ivreg_diff1, type = "HC1"))),
               sqrt(diag(vcovHC(cig_ivreg_diff2, type = "HC1"))),
               sqrt(diag(vcovHC(cig_ivreg_diff3, type = "HC1")))) )


stargazer(cig_ivreg_diff1, cig_ivreg_diff2, cig_ivreg_diff3,
          header = FALSE, 
          type = "text",
          omit.table.layout = "n",
          digits = 3, 
          column.labels = c("IV: salestax", "IV: cigtax", "IVs: salestax, cigtax"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Dependent Variable: 1985-1995 Difference in Log per Pack Price",
          se = rob_se)


# -->
# reports negative estimates of the coefficient on pricediff that are quite different in magnitude.
# Which one should we trust?
# This hinges on the validity of the instruments used.




# ------------------------------------------------------------------------------
# Checking instrument validity:  heteroskedasticity-robust F-statistic for 1st-stage regression
# 
#   - F-statistics for the first-stage regressions of all three models to check instrument relevance.
# ------------------------------------------------------------------------------


# 1st-stage regresssion

mod_relevance1 <- lm(pricediff ~ salestaxdiff + incomediff)

mod_relevance2 <- lm(pricediff ~ cigtaxdiff + incomediff)

mod_relevance3 <- lm(pricediff ~ incomediff + salestaxdiff + cigtaxdiff)



summary(mod_relevance1)

summary(mod_relevance2)

summary(mod_relevance3)




# ----------
# compute the heteroskedasticity-robust F-statistic by means of linearHypothesis()
# check instrument relevance

linearHypothesis(mod_relevance1,  "salestaxdiff = 0",  vcov = vcovHC, type = "HC1")

linearHypothesis(mod_relevance2,  "cigtaxdiff = 0",  vcov = vcovHC, type = "HC1")

linearHypothesis(mod_relevance3,  c("salestaxdiff = 0", "cigtaxdiff = 0"), vcov = vcovHC, type = "HC1")

