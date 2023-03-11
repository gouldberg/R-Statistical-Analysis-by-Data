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
# data exploration:  time-series plot
# ------------------------------------------------------------------------------

summary(dat)




# ----------
myts <- ts(dat, start = c(1970,1), end = c(1992,1), frequency=1)


head(myts)



# plot series
MTS::MTSplot(myts[,-1])



# -->
# exchange rate is decreasing (Yen purchase power is increasing against dollars)
# but CPI is increasing




# ----------
apply(myts[,-1], 2, forecast::ndiffs)




# ------------------------------------------------------------------------------
# differences
# ------------------------------------------------------------------------------


par(mfrow = c(3,1))

plot(diff(dat$cpi), type = "o")

# takes 2 order differences
plot(diff(dat$cpi, differences = 2), type = "o")

plot(diff(dat$exr), type = "o")




# ------------------------------------------------------------------------------
# Relationship between Exchange Wage and CPI
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

plot(dat$exr, dat$cpi, type = "o")


