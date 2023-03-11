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
# Spectral analysis:  Raw periodogram
# ------------------------------------------------------------------------------


nextn(length(infm$bos))



# ----------
par(mfrow=c(2,2))


bos.per <- astsa::mvspec(infm$bos, log = "no")

bos.per <- astsa::mvspec(diff(infm$bos), log = "no")

bos.per <- astsa::mvspec(bos_sm, log = "yes")





# ----------
par(mfrow=c(2,2))


respr.per <- astsa::mvspec(infm$respr, log = "no")

respr.per <- astsa::mvspec(diff(infm$respr), log = "no")

respr.per <- astsa::mvspec(respr_sm, log = "yes")





# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------


# In order to achieve bandwidth = 0.1

( ker <- kernel("modified.daniell", c(100,100)) )


plot(ker)



# ----------
# taper:  generally have a shape that enhances the center of the data relative to the extremities, such as a cosine bell of the form
#  - taper = 0.1:  tapering the upper and lower 10% of the data

graphics.off()
par(mfrow=c(2,1))

bos.smo <- astsa::mvspec(bos_sm, kernel = ker, taper = 0.1, log = "yes")

respr.smo <- astsa::mvspec(respr_sm, kernel = ker, taper = 0.1, log = "yes")



