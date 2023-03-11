# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\cheddar")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cheddar
# ------------------------------------------------------------------------------

dat <- read.csv("cheddar.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)




# ------------------------------------------------------------------------------
# M-estimation by Huber method
#   - weight(residual) =  1 if abs(residual) <= c,  otherwise c / abs(residual)
#   - downweight extreme cases and equal weight for the middle cases
# ------------------------------------------------------------------------------

library(MASS)


rlmod <- rlm(taste ~ Acetic + H2S + Lactic, data = dat)
rlmod <- rlm(taste ~ H2S + Lactic, data = dat)


summary(rlmod)



coef(rlmod)

coef(mod1)





# ------------------------------------------------------------------------------
# weights assigned by the model 
# ------------------------------------------------------------------------------

wts <- rlmod$w

names(wts) <- row.names(dat)


sort(wts)


