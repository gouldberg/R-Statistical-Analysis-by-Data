# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/q_gdpuempp")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  q_gdpuemp
#   - Quarterly time series of gdp and unemployment rate from year 1948 to 2011
# ------------------------------------------------------------------------------

da <- read.table(file = "/media/kswada/MyFiles/references/MultivariateTimeSeriesAnalysisWithRAndFinancialApplications/q-gdpunemp.txt", header = T)


glimpse(da)
str(da)


# ----------
# convert to ts object
is.ts(da)

da <- ts(da, start = c(1948, 1), end = c(2011, 4), frequency = 4)



# ------------------------------------------------------------------------------
# Visualize time series
# ------------------------------------------------------------------------------

graphics.off()

par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))

plot.ts(da[,c("gdp","rate")])

plot.ts(diff(da[,c("gdp","rate")]))


library(MTS)
MTSplot(da[,c("gdp","rate")])