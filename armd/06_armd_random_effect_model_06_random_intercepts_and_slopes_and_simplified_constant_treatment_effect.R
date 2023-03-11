
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\armd")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  ARMD (Age-Related Macular Degeneration Trial)
# ------------------------------------------------------------------------------


armd.wide <- read.csv("armd.wide.txt", header = T, sep = "\t")


armd0 <- read.csv("armd0.txt", header = T, sep = "\t")



str(armd.wide)


str(armd0)



# ----------
data("armd", package = "nlmeU")


str(armd)




# ------------------------------------------------------------------------------
# Random intercepts and slopes with varPower residual variance function
# model with diagonal matrix D and constant treatment effect
# ------------------------------------------------------------------------------


# we apply diagonal matrix D in fm16.4
# removing interaction term in the fixed-effects part

lm3.form <- formula(visual ~ visual0 + time + treat.f)


fm16.5 <- update(fm16.4, lm3.form, data = armd)



# ----------
# fixed effects table
# Note that t-stats and p-values for the fixed-effects coefficients are for the marginal-approach tests.

printCoefmat(summary(fm16.5)$tTable, has.Pvalue = TRUE, P.value = TRUE)



# -->
# treat.f factor is statistically significant at the 5% significance level.
# suggesting a time-independent, negative average effect of the active treatment




# ----------
# Correlation coefficient for the random effects intercept and slopes
intervals(fm16.5, which = "var-cov")
intervals(fm16.4, which = "var-cov")




# -->
# very close to fm16.4



# ----------
VarCorr(fm16.5)



# ----------
getVarCov(fm16.5, type = "conditional", individual = "2")



# ----------
( fm16.5cov <- getVarCov(fm16.5, type = "marginal", individual = "2") )


cov2cor(fm16.5cov[[1]])




# -->
# the estiamted marginal variance-covariance matrix indicates an increasing trend of variances
# of visual acuity measurements over time,
# while the corresponding correlation matrix suggests a decreasing correlation between the measurements
# obtained at more distant timepoints.

# Note that a direct comparison of the estimated marginal matrices to their counterparts
# obtained for model fm12.3 is not appropriate,
# because the matrices for fm16.4 are much more structured than those of model fm12.3



