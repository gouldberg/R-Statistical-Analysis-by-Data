setwd("//media//kswada//MyFiles//R//db")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  db
# ------------------------------------------------------------------------------
data("db", package = "gamlss.data")


str(db)

car::some(db)



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

# A fast growth in head for children during the first year of life, indicating that a power transformation for age could be appropriate for these data.
# Therefore the arugment trans.x = TRUE is used:  whether to transform x to x^eta, with default trans.x = FALSE
# --> more intervals for small values

par(mfrow=c(1,1))

m0 <- lms(head, age, data = db, trans.x = TRUE, k = 2)
m0_c <- lms(head, age, families = c("BCCGo", "BCPEo", "BCTo"), calibration = F, data = db, trans.x = TRUE, k = 4)
m0_f <- lms(head, age, data = db, trans.x = FALSE, k = 2)

m0
m0_c
m0_f


# -->
# when trans.x = TRUE:  BCTo is selected
# when trans.x = TRUE and k = 4:  BCTo is selected
# when trans.x = FALSE:  BCTo is selected


GAIC(m0, m0_c, m0_f, k = 4)



# ----------
# The chosen power transformation for age
m0_c$power


# Best distribution according to GAIC (k = 2)
m0_c$family



# ----------
# Effective degrees of freedom used for the smooth functions in model m0
# (including 2 for the constant and linear terms)
edfAll(m0_c)

edfAll(m0_f)


plot(m0_c)



# ------------------------------------------------------------------------------
# Fit the chosen model directly by gamlss
# ------------------------------------------------------------------------------
db$Tage <- (db$age) ^ (m0_c$power)

hist(db$age)

hist(db$Tage)


m01 <- gamlss(head ~ pb(Tage), sigma.fo = ~pb(Tage), nu.fo = ~pb(Tage), tau.fo = ~pb(Tage), family = BCCGo, trace = FALSE, data = db)
m02 <- gamlss(head ~ pb(Tage), sigma.fo = ~pb(Tage), nu.fo = ~pb(Tage), tau.fo = ~pb(Tage), family = BCPEo, trace = FALSE, data = db)
m03 <- gamlss(head ~ pb(Tage), sigma.fo = ~pb(Tage), nu.fo = ~pb(Tage), tau.fo = ~pb(Tage), family = BCTo, trace = FALSE, data = db)


# alternatively
# but NOT recommended for large data since the calculation age ^ m0_c$power is performed at each iteration
m0B <- gamlss(head ~ pb(age ^ m0_c$power), sigma.fo = ~pb(age ^ m0_c$power), nu.fo = ~pb(age ^ m0_c$power), tau.fo = ~pb(age ^ m0_c$power),
              family = BCTo, trace = FALSE, data = db)



# ----------
GAIC(m0_c, m0_f, m01, m02, m03, k = log(nrow(db)))


# -->
# BCTo distribution) is the best

edfAll(m03)

