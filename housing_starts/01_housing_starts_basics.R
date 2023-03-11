# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/housing_starts")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: housing starts  
#  - New privately owned housing units started 1960 to 2005:  http://www.census.gov/compendia/statab/construction_housing/authorizations_starts_and_completions/
#  - US housing sales data starting 1960, median sales price of new privately owned, one-family houses sold entire US from 1970 to 2005
#    http://www.census.gov/const/www/newressalesindex.html
# ------------------------------------------------------------------------------

starts <- c(1252, 1313, 1463, 1610, 1529, 1473, 1165, 1292, 1508, 1467,
           1434, 2052, 2357, 2045, 1338, 1160, 1538, 1987, 2020, 1745,
           1292, 1084, 1062, 1703, 1750, 1742, 1805, 1621, 1488, 1376,
           1193, 1014, 1200, 1288, 1457, 1354, 1477, 1474, 1617, 1641,
           1569, 1603, 1705, 1848, 1956, 2068)

sales <- c(485, 656, 718, 634, 519, 549, 646, 819, 817, 709, 545,
           436, 412, 623, 639, 688, 750, 671, 676, 650, 534, 509,
           610, 666, 670, 667, 757, 804, 886, 880, 877, 908, 973, 1086, 1203, 1283)


# ----------
# Note that sales data is leading 10 years
length(starts)
length(sales)



# ----------
# add NA to first 10 years to sales
sales2 <- c(rep(NA, 10), sales)
length(sales2)



# ----------
data <- data.frame(starts = starts, sales2 = sales2)

data.ts <- ts(data, start = c(1960), end = c(2005), frequency = 1)



# ------------------------------------------------------------------------------
# Chaeck the time series
# ------------------------------------------------------------------------------
# original each time series
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))

plot(starts, type = "l")
plot(sales, type = "l")



# ----------
plot.ts(data.ts)


# -->
# Note that after 1991, median starts and sales price is increasing