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
# Test overidentifying restrictions
#    - We also conduct the overidentifying restrictions test for model three
#      which is the only model where the coefficient on the difference in log prices is overidentified (m=2, k=1)
#      such that the J-statistic can be computed. 
# ------------------------------------------------------------------------------

# compute the J-statistic

cig_iv_OR <- lm(residuals(cig_ivreg_diff3) ~ incomediff + salestaxdiff + cigtaxdiff)


cig_OR_test <- linearHypothesis(cig_iv_OR, 
                                c("salestaxdiff = 0", "cigtaxdiff = 0"), 
                                test = "Chisq")

cig_OR_test



# -->
# In this case the p-Value reported by linearHypothesis() is wrong
# because the degrees of freedom are set to 2.
# This differs from the degree of overidentification so the J-statistic is not reliable,
# as assumed defaultly by linearHypothesis(). We may compute the correct p-Value using pchisq().




# ----------
# compute correct p-value for J-statistic

pchisq(cig_OR_test[2, 5], df = 1, lower.tail = FALSE)




# -->
# Since this value is smaller than 0.05
# we reject the hypothesis that both instruments are exogenous at the level of 5%
# This means one of the following:
#  - The sales tax is an invalid instrument for the per-pack price.
#  - The cigarettes-specific sales tax is an invalid instrument for the per-pack price.
#  - Both instruments are invalid.

# The assumption of instrument exogeneity is more likely to hold for the general sales tax 
# such that the IV estimate of the long-run elasticity of demand for cigarettes we consider
# the most trustworthy is 0.94, 
# the TSLS estimate obtained using the general sales tax as the only instrument.

# The interpretation of this estimate is that over a 10-year period, 
# an increase in the average price per package by one percent is
# expected to decrease consumption by about 0.94 percentage points.
# This suggests that, in the long run, price increases can reduce cigarette consumption considerably.




stargazer(cig_ivreg_diff1, cig_ivreg_diff2, cig_ivreg_diff3,
          header = FALSE, 
          type = "text",
          omit.table.layout = "n",
          digits = 3, 
          column.labels = c("IV: salestax", "IV: cigtax", "IVs: salestax, cigtax"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Dependent Variable: 1985-1995 Difference in Log per Pack Price",
          se = rob_se)



# ----------
# formula <- ~ log(packs) + rprice + rincome + salestax
formula <- ~ packs + rprice + rincome + salestax

scatterplotMatrix(formula, data = CigarettesSW, groups = CigarettesSW$year,
                  smooth = FALSE, ellipse = list(levels = 0.5), 
                  id = list(n = 3), col = c(gray(0.6), "black"), pch = c(1, 20))

