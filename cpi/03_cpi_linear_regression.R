# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\cpi")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CPI, EXR
#   - CPI (Consumer Price Index), Exchange Rage
# ------------------------------------------------------------------------------

dat <- read.csv("cpi_exr_dat.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)




# ----------
dat <- dat %>% mutate(yr_flg = ifelse(year >= 1979, 1, 0))




# ------------------------------------------------------------------------------
# simple linear regression
# ------------------------------------------------------------------------------


mod0 <- lm(cpi ~ exr, data = dat)


summary(mod0)



# -->
# regression coefficient is negative
# R^2 = 0.7812, quite high.



# ----------
car::residualPlots(mod0)




# ----------
par(mfrow = c(2,2))

plot(mod0)




# ----------
idx <- c(3,4,15)

dat[idx,]




# ------------------------------------------------------------------------------
# for reference:  log-log model
# ------------------------------------------------------------------------------

mod_loglog <- lm(log(cpi) ~ log(exr), data = dat)


summary(mod_loglog)



# -->
# Elasticity of Exchange Rate is -0.8372


