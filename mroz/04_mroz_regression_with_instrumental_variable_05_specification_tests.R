setwd("//media//kswada//MyFiles//R//mroz")

packages <- c("dplyr", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mroz
# ------------------------------------------------------------------------------

data(mroz, package = "POE5Rdata")


dim(mroz)

str(mroz)


# ----------
# restrict to wome in the labour force
mroz_lfp1 <- subset(mroz, lfp == 1)



# ------------------------------------------------------------------------------
# Specification Test
#  - Weak instruments test:  H0 is that all instrument are weak, rejecting H0 indicates that at least one instrument is strong
#  - Hausman test for endogeneity:  H0 is Cov(x, e) = 0, rejecting H0 indicates the existence of endogeneity and the need for instrumental variables
#  - Sargan test (overidentifying restrictions):  H0 is Cov(z, e) = 0, rejecting H0 indicates that at least one of the extra instruments is not valid
# ------------------------------------------------------------------------------

mroz1.iv1 <- AER::ivreg(log(wage) ~ educ + exper + I(exper^2) | exper + I(exper^2) + mothereduc + fathereduc, data = mroz_lfp1)


summary(mroz1.iv1, diagnostics = TRUE)


# -->
# Weak instrument test:  rejects H0, meaning that at least one instument is strong
# (Wu)-Hausman test for endogeneity:  barely rejects the H0 that the variable of concern is uncorrelated with the error term, indicating that educ is marginally endogeneous
# Sargan overidentigying restrictions:  does not reject the H0, meatning that the extra instruments are valid (are uncorrelated with the error term)



# ------------------------------------------------------------------------------
# Testing overidentifing restrictions
#   - If we have more instruments than endogenous variables, we can use eigher all or only some of them.
#     If are all valid, using all improves the accuracy of the 2SLS estimator and reduces its standard errors.
#     If the exogeneity of some is dubious, including them might cause inconsistency.
#     It is therefore useful to test for exogeneity of a set of dubious instruments if we have another (large enough) set that is undoubtedly exogenous.
#   - The procedure is described by Wooldridge:
#       1. Estimate the model by 2SLS and obtain residuals u-hat
#       2. Regress u-hat on all exogenous variables and calculate R^2
#       3. The test statistic n * R^2 is asymptotically distributed as Chisq^2(p) where p is the number of overidentifying restrictions,
#          i.e. number of instruments minus number of endogenous regressors.
# ------------------------------------------------------------------------------

res.aux <- lm(resid(mroz1.iv1) ~ exper + I(exper^2) + mothereduc + fathereduc, data = mroz_lfp1)



# ----------
( r2 <- summary(res.aux)$r.squared )


( n <- nobs(res.aux) )


( teststat <- n * r2 )


( pval <- 1 - pchisq(teststat, 1))


# -->
# We can not reject exogeneity of the instruments using this test.
# But be aware of the fact that the underlying assumption that at least one instrument is valid might be violated here.

