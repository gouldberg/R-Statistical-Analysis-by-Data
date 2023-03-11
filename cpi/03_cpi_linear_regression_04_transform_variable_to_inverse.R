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




# ------------------------------------------------------------------------------
# apply inverse of Exchange Rate (How much dollars are purchaged by one Yen)
#   - Note that, in the model of inverse of Exchange Rate, 
#     the marginal effect of Exchange Rate is not constant since d CPI / d EXR = 1 / EXP^2 (not constant)
# ------------------------------------------------------------------------------


dat_w <- dat_w %>% mutate(exr_inv = 1 / exr)



# ----------
# with wage
mod_lin <- lm(cpi ~ exr + wage, data = dat_w)

mod_lin_exrinv <- lm(cpi ~ exr_inv + wage, data = dat_w)




# ----------
# with wage and trend
mod_lin_trend <- lm(cpi ~ exr + wage + year, data = dat_w)

mod_lin_exrinv_trend <- lm(cpi ~ exr_inv + wage + year, data = dat_w)



# ----------
# without wage
mod_lin_exrinv_trend2 <- lm(cpi ~ exr_inv + year, data = dat_w)




# ----------
summary(mod_lin_exrinv)


# -->
# Including inverse of Exchange Rate, all coefficients are highly significant and Adjusted R^2 is larger than mod_lin
# Note that the coefficients of exr_inv is negative.

# exchange rate decreased (Yen power is increased)
# --> 1 / exr is increasing
# --> but effect to cpi is decreasing




# ----------
summary(mod_lin_exrinv_trend)



# -->
# trend is negative ... might be better trend is positive (CPI is increasing by year)



# ----------
summary(mod_lin_exrinv_trend2)



# -->
# better for interpretation: trend is positive 



# ----------
# but the model mod_lin_exrinv_trend is better in terms of diagnostics

car::residualPlots(mod_lin_exrinv)

car::residualPlots(mod_lin_exrinv_trend)

car::residualPlots(mod_lin_exrinv_trend2)

anova(mod_lin_exrinv, mod_lin_exrinv_trend)

anova(mod_lin_exrinv_trend2, mod_lin_exrinv_trend)




# ----------
AIC(mod_lin_trend, mod_lin_exrinv, mod_lin_exrinv_trend, mod_lin_exrinv_trend2)


car::compareCoefs(mod_lin_trend, mod_lin_exrinv, mod_lin_exrinv_trend, mod_lin_exrinv_trend2)


stargazer::stargazer(mod_lin_trend, mod_lin_exrinv, mod_lin_exrinv_trend, type = "text")



