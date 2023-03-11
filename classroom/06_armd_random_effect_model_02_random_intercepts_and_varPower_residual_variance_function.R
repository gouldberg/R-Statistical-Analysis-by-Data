
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
# Random intercepts and varPower residual variance function
# ------------------------------------------------------------------------------

fm16.2 <- update(fm16.1, weights = varPower(form = ~ time), data = armd)




# ----------
# fixed effects table
# Note that t-stats and p-values for the fixed-effects coefficients are for the marginal-approach tests.

printCoefmat(summary(fm16.2)$tTable, has.Pvalue = TRUE, P.value = TRUE)




# ----------
summary(fm16.2)



# -->
# standard deviation of random interceps = 7.71
# sigma = 3.60  -->  residual variance = 3.60^2 * (time)^(2 * 0.3144) = 31.103 (if time = 4)



# ------------------------------------------------------------------------------
# estimated variance-covariance matrices for random effects and residual errors
# ------------------------------------------------------------------------------


# estimated variance-covariance matrix of the residual errors for id = 2

getVarCov(fm16.2, individual = "2", type = "conditional")




# ----------
# estimated variance of random intercepts = 59.376
# (smaller than 80.608 by fm16.1)
# This is expected by allowing for heteroscedastic residual random errors,
# a larger part of the total variability is explained by the residual variances.
# = 3.60^2 * (time)^2(2*0.3144)

VarCorr(fm16.2)




# ------------------------------------------------------------------------------
# estimated marginal variance-covariance matrices for random effects and residual errors
# ------------------------------------------------------------------------------

( fm16.2cov <- getVarCov(fm16.2, individual = "2", type = "marginal") )


( cov2cor(fm16.2cov[[1]]) )



# -->
# decreasing correlation between visual acuity measurements made at more distant timepoints



