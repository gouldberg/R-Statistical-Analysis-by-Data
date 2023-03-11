
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\armd")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

# https://rdrr.io/cran/nlmeU/src/inst/scriptsR2.15.0/Ch12.R



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
# Sequential test for terms
# ------------------------------------------------------------------------------

# refit model using ML estimation

anova(update(fm12.3, method = "ML"))



# -->
# by default anova() provides results of the sequential test.
# The nonsignificant, at the 5% significance level, result of the test for the time.f : treat.f
# interaction suggests that a simpler, constant treatment effect might be plausible.




# ------------------------------------------------------------------------------
# Models with general correlation structure and Power Variance Function
# based onfm12.3
# ------------------------------------------------------------------------------

# update by method = "ML"


lm1a.form <- formula (visual ~ visual0 + time.f + treat.f + time.f:treat.f)   
fm12.3a <- update(fm12.3, lm1a.form, method = "ML", data = armd)



# linear effect of continuous TIME variable

lm2.form <- formula(visual ~ visual0 + time + treat.f + treat.f:time)
fm12.4 <- update(fm12.3, lm2.form, method = "ML", data = armd)



# removing interactions

lm3.form <-  update(lm2.form, . ~ . - treat.f:time)
fm12.5 <- update(fm12.3, lm3.form, method = "ML", data = armd)




# ------------------------------------------------------------------------------
# Likelihood ratio test for mean linear time trend and interaction term
# ------------------------------------------------------------------------------

anova(fm12.3a, fm12.4, fm12.5)


# -->
# fm12.3a (= fm12.3) can be simplified to fm12.5
# by assuming a mean linear trend of visual acuity measurements in time
# and a constant treatment effect.




# ------------------------------------------------------------------------------
# sequential test for fm12.5
# ------------------------------------------------------------------------------

anova(fm12.5) 



# -->
# The result of the test is statistically significant at the 5% significance level (p = 0.015)



# ----------
summary(fm12.5)



# -->
# negative treatment effect.
# post-randomization visual acuity measurements are higher by 0.89 for each unit increase
# in the baseline measurement.
# they decrease linearly by 0.23 with each week.



# ------------------------------------------------------------------------------
# estimated variance-covariance and correlation matrices
# ------------------------------------------------------------------------------

fm12.5vcov <- getVarCov(fm12.5, individual = "2")


dimnames(fm12.5vcov) <- dnms


fm12.5vcov 



# -->
# residual variance increases with time (see diagnal elements)



fm12.5cor <- cov2cor(fm12.5vcov)


print(fm12.5cor, corr=TRUE, stdevs=FALSE)



# -->
# the correlation between the measurements decreases wwhen the observations are made at more
# distant timepoints (in terms of the order, not time distance)



