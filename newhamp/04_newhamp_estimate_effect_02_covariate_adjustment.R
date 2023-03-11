
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\newhamp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  newhamp
# ------------------------------------------------------------------------------


dat <- read.csv("newhamp.txt", header = TRUE, sep = "\t")


str(dat)


car::some(dat)




# ----------
dat$trt <- ifelse(dat$votesys == "H", 1, 0)




# ------------------------------------------------------------------------------
# estimate average effect by covariate adjustment
# ------------------------------------------------------------------------------


# ----------
# 1.  pairs matching method
# note that by mean of pObama proportion by trt == 1 and trt == 0 is -0.016
mean(pdiff)

mean(pdiff2)




# ----------
# 2.  No covariate adjustment:  it seems that there are 4.2% higher proportion by 'H'
round(coef(lm(pObama ~ trt, data = dat)), 3)




# ----------
# 3.  covariate adjustment including 'Dean'
# almost no effect by 'H'  (coefficient for 'trt' is almost same by following two regression)
round(coef(lm(pObama ~ trt + Dean, data = dat)), 3)

round(coef(lm(pObama ~ trt + Dean + pci + white + absentee, data = dat)), 3)




# ----------
# 4.  covariate adjustment not including 'Dean'

round(coef(lm(pObama ~ trt + pci + white + absentee, data = dat)), 3)



