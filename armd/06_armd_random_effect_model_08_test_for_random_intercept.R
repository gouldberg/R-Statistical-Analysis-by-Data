
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
# model comparison
# ------------------------------------------------------------------------------

AIC(fm16.1, fm16.2, fm16.3, fm16.4)



# ----------
fm16.4ml <- update(fm16.4, method = "ML")

fm16.5ml <- update(fm16.5, method = "ML")


anova(fm16.4ml, fm16.5ml)



# -->
# lowest value of AIC is obtained from model fm16.5,
# suggesting that the model prodides the best overall fit to the data.



# ------------------------------------------------------------------------------
# Test for random intercept
# ------------------------------------------------------------------------------


# null model
vis.gls1a <- gls(lm2.form, data = armd)              



# ----------
# null model vs. fm16.1

( anova.res  <- anova(vis.gls1a, fm16.1) )

anova.res[["p-value"]]


# appropriate asymptotic distribution is a 50% - 50% mixture of the X0^2 + X1^2 distributions
(anova.res[["p-value"]][2]) / 2




# -->
# reject the null hypothesis that the variance of the distribution of random intercepts is equal to zero



# ------------------------------------------------------------------------------
# Test for random intercept:  alternative
# ------------------------------------------------------------------------------

library(RLRsim)    


# m:  alternative model
# we test a random effect in model fm16.1,
# which contains only a single random effect, we use the abbreviated form of the function call,
# with m as the only argument.

exactRLRT(m = fm16.1)



