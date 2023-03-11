rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\02_異常検知_時系列\\qtdbsel102")



# ------------------------------------------------------------------------------
# data:  qtdbsel102
# ------------------------------------------------------------------------------


data <- read.csv(file = "qtdbsel102.txt", header = FALSE, sep = "")



str(data)


head(data)




# ----------
# add date variable

data$date <- seq.Date(from = as.Date('1900-01-01'), to = as.Date('2020-10-31'), by = 'days')[1:nrow(data)]


dat2 <- data %>% dplyr::select(date, V2)




# ------------------------------------------------------------------------------
# Anomaly Detection Using Seasonal Hybrid ESD Test
# ------------------------------------------------------------------------------


library(AnomalyDetection)



# set max_anom proportion

ano_pos <- AnomalyDetectionTs(dat2, direction = "pos", plot = FALSE, max_anoms = 0.001)

ano_neg <- AnomalyDetectionTs(dat2, direction = "neg", plot = FALSE, max_anoms = 0.001)



idx_pos <- which(substring(dat2$date,1,10) %in% substring(ano_pos$anoms$timestamp,1,10))

idx_neg <- which(substring(dat2$date,1,10) %in% substring(ano_neg$anoms$timestamp,1,10))




# ----------
graphics.off()

par(mfrow = c(2,1))

plot(c(dat2$V2), type = "l", main = "original series")

abline(v = idx_pos, col = "blue", lty = 1)


plot(c(dat2$V2), type = "l", main = "original series")

abline(v = idx_neg, col = "blue", lty = 1)



# -->
# negative anomaly is well detected !!






dat2 <- data %>% dplyr::select(date, V2)




# ------------------------------------------------------------------------------
# How about V3 ?
# ------------------------------------------------------------------------------

dat2 <- data %>% dplyr::select(date, V3)



ano_pos <- AnomalyDetectionTs(dat2, direction = "pos", plot = FALSE, max_anoms = 0.001)

ano_neg <- AnomalyDetectionTs(dat2, direction = "neg", plot = FALSE, max_anoms = 0.001)



idx_pos <- which(substring(dat2$date,1,10) %in% substring(ano_pos$anoms$timestamp,1,10))

idx_neg <- which(substring(dat2$date,1,10) %in% substring(ano_neg$anoms$timestamp,1,10))




# ----------
graphics.off()

par(mfrow = c(2,1))

plot(c(dat2$V3), type = "l", main = "original series and pos detected")

abline(v = idx_pos, col = "blue", lty = 1)


plot(c(dat2$V3), type = "l", main = "original series and neg detected")

abline(v = idx_neg, col = "blue", lty = 1)



# -->
# Only anomaly point IS NOT DETECTED


