setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\unemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  unemp
# ------------------------------------------------------------------------------

unemp <- read.table("unemp.txt", sep = "\t", header = T, stringsAsFactors = F)


unemp <- ts(unemp, start = 1948, end = 1978, frequency = 12)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


MTSplot(unemp)



# ------------------------------------------------------------------------------
# data exploration:  decompose
# ------------------------------------------------------------------------------


plot(decompose(unemp, type = "additive"))




# ------------------------------------------------------------------------------
# 1st difference, 2nd order difference, and detrended series
# ------------------------------------------------------------------------------

unemp_df1 <- diff(unemp)

unemp_df2 <- diff(diff(unemp))

unemp_dt1 <- resid(lm(unemp ~ time(unemp)))

unemp_dt2 <- unemp - decompose(unemp)$trend



graphics.off()

par(mfrow = c(3,2))

plot(unemp, type = "l")

plot(unemp_df1, type = "l")

plot(unemp_df2, type = "l")

plot(unemp_dt1, type = "l")

plot(unemp_dt2, type = "l")




# ------------------------------------------------------------------------------
# raw periodogram
# ------------------------------------------------------------------------------

graphics.off()


par(mfrow = c(3,2))

astsa::mvspec(unemp, log = "no")

astsa::mvspec(unemp_df1, log = "no")

astsa::mvspec(unemp_df2, log = "no")

astsa::mvspec(unemp_dt1, log = "no")

astsa::mvspec(na.omit(unemp_dt2), log = "no")


# -->
# unemp_df2:  over differenced series has large spectrum at high frequency (= 5)

# unemp_dt2:  detrended series has large spectrum at low frequency (= 1)




# ----------
# logged and smoothed

astsa::mvspec(unemp, log = "yes", spans = c(7,7), taper = 0.1)




astsa::SigExtract(unemp)

