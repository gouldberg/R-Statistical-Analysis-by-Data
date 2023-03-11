# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//cpi_exr_wage")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "POE5Rdata")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CPI, EXR and WAGE
#   - CPI (Consumer Price Index), Exchange Rage, and Wage (Year 1995 = 100) in Japan from year 1970 to 1992
# ------------------------------------------------------------------------------

dat <- read.csv("cpi_exr_wage_dat.txt", header = TRUE, colClasses = "numeric", sep = "\t")


str(dat)


Hmisc::describe(dat)




# ------------------------------------------------------------------------------
# time-series plot
# ------------------------------------------------------------------------------

myts <- ts(dat, start = c(1970,1), end = c(1992,1), frequency=1)


head(myts)


# plot series
plot(myts)




# ----------
# Relationship between Exchange Rage, Wage and CPI
par(mfrow = c(1,2))
plot(dat$exr, dat$cpi, type = "l")
plot(dat$wage, dat$cpi, type = "l")


