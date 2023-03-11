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



# ------------------------------------------------------------------------------
# control by other variable: trend
# ------------------------------------------------------------------------------


mod_t <- lm(cpi ~ exr + year, data = dat)


summary(mod_t)



# -->
# exchange rate is NOT significant



# ----------
car::residualPlots(mod_t)



# ----------
par(mfrow = c(2,2))

plot(mod_t)




# ----------

fit.p <- data.frame(dat, pred = predict(mod_t))


ggplot(fit.p, aes(pred, cpi)) + geom_point() + stat_smooth(method = "lm")



