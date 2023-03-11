# setwd("//media//kswada//MyFiles//R//cpi_exr_wage")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\cpi")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CPI, EXR
#   - CPI (Consumer Price Index), Exchange Rage
#   - also Wage (Year 1995 = 100) in Japan from year 1970 to 1992
# ------------------------------------------------------------------------------

dat <- read.csv("cpi_exr_dat.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


car::some(dat)




# ----------
dat <- dat %>% mutate(yr_flg = ifelse(year >= 1979, 1, 0))


wage <- read.csv("wage_dat.txt", header = TRUE, colClasses = "numeric", sep = "\t")


( dat_w <- data.frame(dat, wage = wage[,2]) )


dat_w <- dat_w %>% mutate(exr_inv = 1 / exr)




# ------------------------------------------------------------------------------
# Check the difference of coefficient value of Exchange Rate in underidentification model and that in original model
# ------------------------------------------------------------------------------

# original log-log model
mod_loglog <- lm(log(cpi) ~ log(exr) + log(wage), data = dat_w)

b_exr <- coef(mod_loglog)[2]
b_wage <- coef(mod_loglog)[3]




# ----------
# Underidentification:  lacks log(wage)
mod_loglog_under <- lm(log(cpi) ~ log(exr), data = dat_w)

b_exr_under <- coef(mod_loglog_under)[2]




# ----------
# log(Wage) ~ log(exr)
mod_loglog_wage <- lm(log(wage) ~ log(exr), data = dat_w)

b_wage_exr <- coef(mod_loglog_wage)[2]




# ----------
# Check underidentification
# The coefficient for Exchange Ratio in underidentification model is larger than that for original model
# by d Wage / d Exr * b_wage  (effect of wage through Exchange Rage)

b_exr_under - b_exr

b_wage_exr * b_wage


