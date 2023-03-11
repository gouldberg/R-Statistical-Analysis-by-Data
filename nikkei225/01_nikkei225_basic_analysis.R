setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\nikkei225")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  nikkei225
# ------------------------------------------------------------------------------


dat <- read.table("nikkei225.txt", sep = "", header = T, colClasses = "numeric")


head(dat)


dat <- dat$nikkei225



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


MTS::MTSplot(dat)


forecast::ndiffs(dat)



# ----------
# convert to log dif


dat <- diff(log(dat))


MTS::MTSplot(dat)




# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial-correaltion
# ------------------------------------------------------------------------------


graphics.off()


astsa::acf2(dat, max.lag = 200)





# ------------------------------------------------------------------------------
# Spectral analysis:  periodogram
# ------------------------------------------------------------------------------


nextn(nrow(dat))




# ----------
# Raw Periodogram

graphics.off()

par(mfrow = c(1,1))


astsa::mvspec(dat, log = "yes")





# ------------------------------------------------------------------------------
# Spectral analysis:  by spec.ar
# ------------------------------------------------------------------------------

spec.ar(dat)


