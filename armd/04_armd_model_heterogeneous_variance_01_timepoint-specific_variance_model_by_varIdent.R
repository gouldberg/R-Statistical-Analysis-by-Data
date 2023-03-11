
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
# timepoint-specific variance model
# ------------------------------------------------------------------------------

library(nlme)


lm1.form <- formula(visual ~ -1 + visual0 + time.f + treat.f : time.f)


fm9.1 <- gls(lm1.form, weights = varIdent(form = ~1 | time.f), data = armd)



# ----------
summary(fm9.1)




# ----------
# variance function structure
fm9.1$modelStruct$varStruct



# ----------
# 95% CI for delta and sigma
intervals(fm9.1, which = "var-cov")




# ------------------------------------------------------------------------------
# REML-based LR test of homoscedasticity
# ------------------------------------------------------------------------------

# fm6.1 is nested within fm9.1, thus we can use the LR test
# fm9.1 is better

anova(fm6.1, fm9.1)



# -->
# We conclude that the data provide evidence for heterogeneous variances of visual acuity measurements
# at different measurement occasions.

