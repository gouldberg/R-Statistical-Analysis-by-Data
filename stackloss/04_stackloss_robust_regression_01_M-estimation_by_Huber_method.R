# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\stackloss")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  stackloss
# ------------------------------------------------------------------------------

dat <- read.csv("stackloss.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)





# ------------------------------------------------------------------------------
# M-estimation by Huber method
#   - weight(residual) =  1 if abs(residual) <= c,  otherwise c / abs(residual)
#   - downweight extreme cases and equal weight for the middle cases
# ------------------------------------------------------------------------------

library(MASS)


rlmod <- rlm(stack.loss ~ Air.Flow + poly(Water.Temp, 2) + Acid.Conc., data = dat)
rlmod <- rlm(stack.loss ~ Air.Flow + poly(Water.Temp, 2), data = dat)


summary(rlmod)



coef(rlmod)

coef(mod3)





# ------------------------------------------------------------------------------
# weights assigned by the model 
# ------------------------------------------------------------------------------

wts <- rlmod$w

names(wts) <- row.names(dat)


sort(wts)



# -->
# 4,3,21,20,2
# 4,3 have less than 0.5 weight

