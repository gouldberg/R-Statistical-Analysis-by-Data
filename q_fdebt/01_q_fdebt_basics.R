# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/q_fdebt")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  q-fdebt
#   - U.S. quarterly federal debts held by (a) foreign and international investors, (b) federal reserve banks, and 
#     (c) the public.  --> but no (c) ?? in the data set.
#   - The data are from the Federal Reserve Bank of St. Louis, from 1970 to 2012 for 171 observations, and not seasonally adjusted.
#     The debts are in billions of dollars.
# ------------------------------------------------------------------------------

da <- read.table(file = "/media/kswada/MyFiles/references/MultivariateTimeSeriesAnalysisWithRAndFinancialApplications/q-fdebt.txt", header = T)


glimpse(da)
str(da)


# ----------
# take the log
( debt <- log(da[, 3:4]) )


# time is converted to year
( tdx <- da[,1] + da[,2] / 12 )



# ------------------------------------------------------------------------------
# Visualize time series
# ------------------------------------------------------------------------------

graphics.off()


library(MTS)
MTSplot(debt, tdx)

plot.ts(debt)



# ----------
# take 1st diffence
zt <-diffM(debt)

plot.ts(zt, tdx[-1])


