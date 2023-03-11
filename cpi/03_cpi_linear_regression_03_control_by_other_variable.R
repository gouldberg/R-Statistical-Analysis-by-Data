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
# control by other variable: wage
# ------------------------------------------------------------------------------


wage <- read.csv("wage_dat.txt", header = TRUE, colClasses = "numeric", sep = "\t")


( dat_w <- data.frame(dat, wage = wage[,2]) )



mod_w <- lm(cpi ~ exr + wage, data = dat_w)


mod_wt <- lm(cpi ~ exr + wage + year, data = dat_w)



# ----------
summary(mod_w)


# -->
# Now, exchange rate is significant, but coefficient for exr is negative




# ----------
summary(mod_wt)


car::vif(mod_wt)



# -->
# the absolute value of coefficient for wage and year is quite same




# ----------
car::residualPlots(mod_w)



# ----------
par(mfrow = c(2,2))

plot(mod_w)




# ----------

fit.p <- data.frame(dat, pred = predict(mod_w))


ggplot(fit.p, aes(pred, cpi)) + geom_point() + stat_smooth(method = "lm")



