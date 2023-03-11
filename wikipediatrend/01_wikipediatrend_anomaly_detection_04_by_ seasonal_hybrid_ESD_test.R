rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\02_異常検知_時系列\\wikipediatrend")




# ------------------------------------------------------------------------------
# data:  wikipediatrend
# ------------------------------------------------------------------------------


data <- read.csv(file = "wikipediatrend.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)



str(data)


head(data)



# ----------

data$date <- as.Date(data$date)




# ------------------------------------------------------------------------------
# Anomaly Detection Using Seasonal Hybrid ESD Test
# ------------------------------------------------------------------------------


dat2 <- data %>% filter(date >= "2016-01-01", date < "2018-06-01") %>% dplyr::select(date, views)




# ---------
library(AnomalyDetection)

ano <- AnomalyDetectionTs(dat2, direction = "pos", plot = FALSE)


ano$anoms

idx <- which(substring(dat2$date,1,10) %in% substring(ano$anoms$timestamp,1,10))




# ----------
graphics.off()

par(mfrow = c(1,1))

plot(c(dat2$views), type = "l", main = "original validation series")

abline(v = idx, col = "blue", lty = 1)

