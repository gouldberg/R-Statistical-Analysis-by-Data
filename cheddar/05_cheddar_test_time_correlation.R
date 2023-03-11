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
# adding time variable
# ------------------------------------------------------------------------------


# time variable
dat$time <- 1:nrow(dat)




# -----------
library(nlme)


# form = ~1: the order of the observations in the data as a vocariate, and no group
mod_glsar <- gls(taste ~ H2S + Lactic, data = dat, method = "ML", correlation = corAR1(form = ~ 1))


summary(mod_glsar)



# -->
# Phi = 0.23




# ----------
mod_time <- lm(taste ~ H2S + Lactic + time, data = dat)


summary(mod_time)



# -->
# time variable is significant


car::residualPlots(mod_time, type = "response")


lmtest::dwtest(mod_time)





# ----------

fitp <- data.frame(dat, pred = predict(mod_time))

library(ggplot2)

graphics.off()

ggplot(fitp, aes(x = time, y = pred)) + geom_point() + geom_smooth()


