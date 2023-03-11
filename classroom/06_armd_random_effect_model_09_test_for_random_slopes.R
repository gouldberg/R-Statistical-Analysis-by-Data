
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
# Test for random slopes
# ------------------------------------------------------------------------------

# for illustrative purposes, we consider a model with uncorrelated subject-specific
# random intercepts and slopes and independent, homoscedastic residual errors.


fm16.7 <- update(fm16.4, weights = NULL, data = armd)              



# ----------
( an.res <- anova(fm16.1, fm16.7) )


# LR-test statistics
RLRT <- an.res[["L.Ratio"]][2]



# using 0.5 * X1^2 + 0.5 * X2^2 as the null distribution

.5 * pchisq(RLRT, 1, lower.tail = FALSE) + .5 * pchisq(RLRT, 2, lower.tail = FALSE) 




# ------------------------------------------------------------------------------
# Test for random slopes:  by exactRLRT() to simulate the null distribution
# ------------------------------------------------------------------------------

mAux <- update(fm16.1, random = ~ 0 + time | subject, data = armd)         


exactRLRT(m = mAux,           # Auxiliary model
          m0 = fm16.1,        # M16.1 (null)
          mA = fm16.7)        # M16.7 (alternative)




# ------------------------------------------------------------------------------
# Test for random slopes:  by simulate(9 to simulate the null distribution
# ------------------------------------------------------------------------------


# IT TAKES TIME !!!
# vis.lme2.sim <- simulate(fm16.1, m2 = fm16.7, nsim = 10000, seed = 654321)


## save(vis.lme2.sim , file = "ARMDsimLMM.dat")
## load("ARMDsimLMM.dat")


# plot(vis.lme2.sim, df = c(1, 2),  abline = c(0, 1, lty = 2))

