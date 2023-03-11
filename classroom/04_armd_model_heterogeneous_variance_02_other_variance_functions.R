
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
# test other variance model
# ------------------------------------------------------------------------------

library(nlme)



fm9.2 <- update(fm9.1, weights = varPower(form = ~ time))


fm9.3 <- update(fm9.1, weights = varPower(form = ~ time | treat.f))


fm9.4 <- update(fm9.1, weights = varPower())


fm9.5 <- update(fm9.1, weights = varPower(fixed = 1))




# ----------
# NOT significant  -->  common power variance function of the TIME covariate can be used for both treatment groups
# fm9.2 seems to offer an adequate description of variance structure of the data.
anova(fm9.2, fm9.3)




# ----------
# power of time vs. timepoint-specific variances
# Null hypothesis:  sigma1^2 = (4^delta) * (sigma^2)   sigma2^2 = (12^delta) * (sigma^2) ...
# NOT significant  -->  fm9.2, which specifies that the variance is a power function of the time (in weeks), offers an
# adequate description of the variance structure of the data
anova(fm9.2, fm9.1)




# ----------
# power of the mean value equal to 1  --> REJECTED
# the inference on the implied variance structure may need to be treated with caution.
# fm9.4 is better than fm9.5
anova(fm9.5, fm9.4)




# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------

# fm9.2 offers a better fit to the data than model fm9.4
AIC(fm9.1, fm9.2, fm9.3, fm9.4, fm9.5)





# ----------
summary(fm9.2)



# -->
# Residual standard error = 5.97
# variance function power parameter estimates = 0.25

# --> Var(VISUAL) = (5.97 * TIME^0.25)^2



# ----------
printCoefmat(coef(summary(fm9.2)))


car::compareCoefs(fm9.1, fm9.2, fm9.3, fm9.4, fm9.5)



# -->
# The estimates of the timepoint-specific treatment effects (fm9.1) are
# remarkably consistent among the 5 models, though.
# All the models suggest an increasing negative effect of the active treatment compared to placebo.

# The estimated standard errors of the fixed-effects coefficients vary more noticeably between all the models.
# This is related to the differences in the assumed residual-variance structure.




# ------------------------------------------------------------------------------
# Extracting infromation about the variance function for models fm9.2 and fm9.3
# ------------------------------------------------------------------------------

# fm9.2:  model structure, variance function

mSt2 <- fm9.2$modelStruct

vF2 <- mSt2$varStruct

summary(vF2)

summary(fm9.2)$sigma




# ----------
# fm9.3:  model structure and variance function

mSt3 <- fm9.3$modelStruct

vF3 <- mSt3$varStruct

summary(vF3)

coef(vF3)

formula(vF3)


# weights for two subjects
varWeights(vF3)[3:10]





