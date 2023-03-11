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
# Fit a quantile sheets model
# ------------------------------------------------------------------------------

# We first use findPower(9 to find a suitable power transformation for age)
ppp <- findPower(dbbmi1$bmi, dbbmi1$age)

ppp


# The smoothing parameter values x.lambda = 1 and p.lambda = 10 are chosen arbitrarily
# x.lambda:  smoothing parameter in the direction of the explanatory variable
# p.lambda:  smoothing parameter in the direction of the response variable  (most likely to affect the shape of the distribution)
qs1 <- quantSheets(bmi, age, data = dbbmi1, cent = c(0.4, 2, 10, 25, 50, 75, 90, 98, 99.6), x.lambda = 1, p.lambda = 10, logit = TRUE, power = ppp)


qs1



# -->
# Note that there are considerable differences between the sample percentage of cases below a centile curve (at end of each line).
# and the nominal model centile (after below on each line)



# ------------------------------------------------------------------------------
# residuals diagnostics by multiple worm plots and Q.statistics
# ------------------------------------------------------------------------------

res1 <- resid(qs1)


wp(resid = res1, xvar = dbbmi1$age, n.inter = 9)



# ----------
round(Q.stats(resid = res1, xvar = dbbmi1$age), 3)



# -->
# showing evidence that the skewness of the response distribution is not modelled properly.
# The worm plots show quadratic shapes, while the skewness column of the Z statistics, Z3, has five values larger than 2.


# Unfortunately since quantile sheets estimation does not easily provide an overall measurement of fit, e.g. GAIC


# ------------------------------------------------------------------------------
# Decrease the value of p.lambda
# ------------------------------------------------------------------------------

qs2 <- quantSheets(bmi, age, data = dbbmi1, cent = c(0.4, 2, 10, 25, 50, 75, 90, 98, 99.6), x.lambda = 1, p.lambda = 0.05, logit = TRUE, power = ppp)

qs2



# ----------
res2 <- resid(qs2)

wp(resid = res2, xvar = dbbmi1$age, n.inter = 9)

round(Q.stats(resid = res2, xvar = dbbmi1$age), 3)



# -->
# It seems that decreasing p.lambda to 0.05 provides a model with good residual diagnostics



