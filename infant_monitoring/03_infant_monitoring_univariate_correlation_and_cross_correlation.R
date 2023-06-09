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





# smoothing by smoooth.spline
# bos_sm <- smooth.spline(time(infm$bos), infm$bos, spar = 0.1)$y

# respr_sm <- smooth.spline(time(infm$respr), infm$respr, spar = 0.1)$y


# sub-sampling by 10 seconds
bos_sm <- infm$bos[seq(0, 30000, by = 10)]

# arcsine transformation
bos_sm <- asin(sign(bos_sm) * sqrt(abs(bos_sm) / max(abs(bos_sm))))

bos_sm <- bos_sm - mean(bos_sm)

respr_sm <- infm$respr[seq(0, 30000, by = 10)]

respr_sm <- respr_sm - mean(respr_sm)





# ------------------------------------------------------------------------------
# Blood Oxygen Saturation:  Correlation analysis for univariate time series
# ------------------------------------------------------------------------------


graphics.off()

astsa::acf2(infm$bos, max.lag = 100)

astsa::acf2(bos_sm, max.lag = 100)

astsa::acf2(diff(bos_sm), max.lag = 100)




# ------------------------------------------------------------------------------
# Respiration Rate:  Correlation analysis for univariate time series
# ------------------------------------------------------------------------------


graphics.off()

astsa::acf2(infm$respr, max.lag = 100)

astsa::acf2(respr_sm, max.lag = 100)

astsa::acf2(diff(respr_sm), max.lag = 100)






# ------------------------------------------------------------------------------
# Correlation analysis:  cross-correlation
# ------------------------------------------------------------------------------


ccf(respr_sm, bos_sm, lag = 50)





