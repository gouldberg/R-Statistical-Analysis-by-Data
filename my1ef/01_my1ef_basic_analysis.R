setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\my1ef")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  MYE1F
# ------------------------------------------------------------------------------


dat <- read.table("MYE1F.txt", sep = "", header = T, colClasses = "numeric")


head(dat)


dat <- dat$x



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


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

par(mfrow = c(1,2))


astsa::mvspec(dat, log = "yes")


( ker <- kernel("modified.daniell", c(15,15)) )

plot(ker)

mvspec(dat, kernel = ker, taper = 0.5, log = "yes")




# ------------------------------------------------------------------------------
# Spectral analysis:  by spec.ar
# ------------------------------------------------------------------------------

spec.ar(dat)


spec.ar(dat[1:630])


spec.ar(dat[631:1026])


spec.ar(dat[1026:2000])
