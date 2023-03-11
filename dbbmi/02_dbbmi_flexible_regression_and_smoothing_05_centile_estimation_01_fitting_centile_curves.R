setwd("//media//kswada//MyFiles//R//dbbmi")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  dbbmi
# ------------------------------------------------------------------------------
data("dbbmi", package = "gamlss.data")


str(dbbmi)

car::some(dbbmi)



# ------------------------------------------------------------------------------
# sample data
# ------------------------------------------------------------------------------


# Sample 1000 observations
IND <- sample.int(7040, 1000, replace = FALSE)

dbbmi1 <- dbbmi[IND,]



# ----------
graphics.off()
par(mfrow=c(1,2))

plot(bmi ~ age, data = dbbmi, pch = 15, cex = 0.5, col = gray(0.5))
title("(a)")

plot(bmi ~ age, data = dbbmi1, pch = 15, cex = 0.5, col = gray(0.5))
title("(b)")



# ------------------------------------------------------------------------------
# Fit centile curves by LMS method
#   - The LMS method was developed by Cole and Green to create centile curves for a response variable Y agaist a single explanatory variable x(e.g. age)
#     Because the LMS method asuumes that the Y variable has a specific distribution, centile (quantile) curves for all p can be obtained simultaneously
#     and do not cross.
#   - The LMS method can be fitted within gamlss by assuming that the response variable has a Box-Cox Cole and Green (BCCG) distritbuion,
#     which is suitable for possibly or negatively skewed data with Y > 0.
#   - Rigby and Stasinopoulos extended the LMS method by introducing the Box-Cox power exponential (BCPE) and Box-Cox t (BCT) distribution for Y
#     and called the resulting centile estimation methods LMSP and LMST, respectively.
#   - The poswer transformation for x, i.e. x ^ eta, is usually needed when the response variable has an early or late spell of fast growth.
#     In those cases the transformation of x can stretch the x scale, improving the fit of the smooth curve.
# ------------------------------------------------------------------------------

# A fast growth in BMI for children during the first year of life, indicating that a power transformation for age could be appropriate for these data.
# Therefore the arugment trans.x = TRUE is used:  whether to transform x to x^eta, with default trans.x = FALSE
# --> more intervals for small values

par(mfrow=c(1,1))

m0 <- lms(bmi, age, data = dbbmi1, trans.x = TRUE, k = 2)
m0_f <- lms(bmi, age, data = dbbmi1, trans.x = FALSE, k = 2)

m0


# -->
# when trans.x = TRUE:  BCPEo is selected
# when trans.x = FALSE:  BCCGo is selected



# ----------
# The chosen power transformation for age
m0$power


# Best distribution according to GAIC (k = 2)
m0$family



# ----------
# Effective degrees of freedom used for the smooth functions in model m0
# (including 2 for the constant and linear terms)
edfAll(m0)

edfAll(m0_f)


plot(m0)



# ------------------------------------------------------------------------------
# Fit the chosen model directly by gamlss
# ------------------------------------------------------------------------------

dbbmi1$Tage <- (dbbmi1$age) ^ (m0$power)

hist(dbbmi1$age)

hist(dbbmi1$Tage)


m0A <- gamlss(bmi ~ pb(Tage), sigma.fo = ~pb(Tage), nu.fo = ~pb(Tage), family = BCCGo, trace = FALSE, data = dbbmi1)

# alternatively
m0B <- gamlss(bmi ~ pb(age ^ m0$power), sigma.fo = ~pb(age ^ m0$power), nu.fo = ~pb(age ^ m0$power), family = BCCGo, trace = FALSE, data = dbbmi1)



# ------------------------------------------------------------------------------
# Use a local GAIC criterion with penalty k (k = 4 here)
# ------------------------------------------------------------------------------

m0C <- gamlss(bmi ~ pb(Tage, method = "GAIC", k = 4), sigma.fo = ~pb(Tage, method = "GAIC", k = 4), 
              nu.fo = ~pb(Tage, method = "GAIC", k = 4), family = BCCGo, trace = FALSE, data = dbbmi1)


edfAll(m0C)



# ----------
GAIC(m0, m0_f, m0A, m0B, m0C)
