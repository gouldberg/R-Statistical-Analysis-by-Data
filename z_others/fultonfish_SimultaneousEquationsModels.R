# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "plm", "systemfit")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  fultonfish
#
# lquan:  log of quantity
# lprice:  log of price
# stormy:  whether the catching day was stormy
#
# endogenous variable:  lprice and lquan
# exogenous variable:  the indicator variables for the day of the week and stormy
# identification variable for the demand equation:  stormy  (only show up in the supply equation)
# identification variable for the supply equation:  mon, tue, wd, thu
# 
# Simultaneous Equations Models
# ------------------------------------------------------------------------------
data("fultonfish", packages = "POE5Rdata")
data <- fultonfish
dim(data)
str(data)
describe(data)


# Quantity and Price
par(mfrow=c(1,1))
plot(data$lprice, data$lquan)


# the weekday indicators are not significant in fishP.ols, indicating 2SLS estimation of the supply equation unreliable
# stormy is significant in fishQ.ols, indicating the estimation of the (structural) demand equation will be reliable
fishQ.ols <- lm(lquan ~ mon + tue + wed + thu + stormy, data = data)
fishP.ols <- lm(lprice ~ mon + tue + wed + thu + stormy, data = data)

kable(tidy(fishQ.ols), digits = 4, caption = "Reduced Q equation for the Fulton Fish Market")
kable(tidy(fishP.ols), digits = 4, caption = "Reduced P equation for the Fulton Fish Market")


# Simultaneous Equations Models
# demand equation is identified (reliable), while the supply equation is not
# --> a solution might be to find better instruments, other than the weekdays for the demand equation
fish.D <- lquan ~ lprice + mon + tue + wed + thu
fish.S <- lquan ~ lprice + stormy
fish.eqs <- list(fish.D, fish.S)
fish.ivs <- ~ mon + tue + wed + thu + stormy
fish.sys <- systemfit(fish.eqs, method = "2SLS", inst = fish.ivs, data = data)
summary(fish.sys)



