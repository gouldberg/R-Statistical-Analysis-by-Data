setwd("//media//kswada//MyFiles//R//grip")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  grip
# ------------------------------------------------------------------------------

data("grip", package = "gamlss.data")


str(grip)

car::some(grip)



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

par(mfrow=c(1,1))


# Here we fit BCCGo distribution
m0 <- lms(grip, age, data = grip, families = c("BCCGo"), trans.x = FALSE, k = 2)

m0



# ----------
# Effective degrees of freedom used for the smooth functions in model m0
# (including 2 for the constant and linear terms)
edfAll(m0)


plot(m0)



