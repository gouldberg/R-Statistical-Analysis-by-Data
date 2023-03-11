
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
# model with general matrix D
# ------------------------------------------------------------------------------

fm16.3 <- update(fm16.2, random = ~ 1 + time | subject, data = armd)




# ----------
# fixed effects table
# Note that t-stats and p-values for the fixed-effects coefficients are for the marginal-approach tests.

printCoefmat(summary(fm16.3)$tTable, has.Pvalue = TRUE, P.value = TRUE)




# ----------
summary(fm16.3)



# ----------
# random effects variance-covariance matrix for individual = 2


getVarCov(fm16.3, individual = "2")




# ----------
# Correlation coefficient for the random effects intercept and slopes

# low estimated value of the correlation coefficient for the random effects  = 0.138

intervals(fm16.3, which = "var-cov")




# -->
# two random coefficients can be uncorrelated



# ------------------------------------------------------------------------------
# Random intercepts and slopes with varPower residual variance function
# model with diagonal matrix D
# ------------------------------------------------------------------------------


fm16.4 <- update(fm16.3, random = list(subject = pdDiag(~time)), data = armd)


intervals(fm16.4)



# -->
# suggesting mean structure could be simplified by removing the time : treat.f interaction.



# ----------
# REML-based LR test
# both models have the same mean structure so that the use of the REML-based RL test is justified.
anova(fm16.4, fm16.3)



# -->
# we do not worsen the fit of the model



