
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
# Random intercepts and homogeneous residual variance
# ------------------------------------------------------------------------------

# subject-specific intercepts

lm2.form <- formula(visual ~ visual0 + time + treat.f + treat.f : time)



library(nlme)

fm16.1 <- lme(lm2.form, random = ~ 1 | subject, data = armd)




# ----------
# fixed effects table
# Note that t-stats and p-values for the fixed-effects coefficients are for the marginal-approach tests.
printCoefmat(summary(fm16.1)$tTable, has.Pvalue = TRUE, P.value = TRUE)




# ----------
summary(fm16.1)



# -->
# standard deviation of random interceps = 8.98
# residual standard deviation, sigma, is 8.63



# ------------------------------------------------------------------------------
# estimated variance-covariance matrices for random effects and residual errors
# ------------------------------------------------------------------------------


# default is type = "random.effect"
# estimated variance and standard deviation of the subject-specific random intercepts

# individual = "2" but here, the same for all individuals

getVarCov(fm16.1, individual = "2")

getVarCov(fm16.1, individual = "2", type = "random.effect")



# ----------
# variance-covariance matrix for residual errors

VarCorr(fm16.1)





# ------------------------------------------------------------------------------
# estimated marginal variance-covariance matrices for random effects and residual errors
# ------------------------------------------------------------------------------

( fm16.1cov <- getVarCov(fm16.1, individual = "2", type = "marginal") )


( cov2cor(fm16.1cov[[1]]) )

