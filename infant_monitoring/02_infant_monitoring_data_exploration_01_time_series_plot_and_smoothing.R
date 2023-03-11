setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\infant_monitoring")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Infant Monitoring
#   - Variable:
#       - Blood oxygen saturation, Pulse rate, and Respiration Rate
# ------------------------------------------------------------------------------


infm <- read.table("InfantMon.txt", sep = "", header = F, colClasses = "numeric")



colnames(gas) <- c("MetI", "gasSendout", "V3")


head(infm)




# ----------
colnames(infm) <- c("V1", "bos", "pulr", "respr")





# ------------------------------------------------------------------------------
# data exploration:  time series plot only up to 1000 seconds
# ------------------------------------------------------------------------------


MTS::MTSplot(infm[1:1000,])




# ------------------------------------------------------------------------------
# data exploration:  Smoothing
# Smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

graphics.off()

par(mfrow=c(1,1))


plot(infm$bos, type = "l")
lines(smooth.spline(time(infm$bos), infm$bos, spar = 0.05), lwd = 2, col = "blue")
lines(smooth.spline(time(infm$bos), infm$bos, spar = 0.1), lwd = 2, col = "red")



plot(infm$respr, type = "l")
lines(smooth.spline(time(infm$respr), infm$respr, spar = 0.05), lwd = 2, col = "blue")
lines(smooth.spline(time(infm$respr), infm$respr, spar = 0.1), lwd = 2, col = "red")



