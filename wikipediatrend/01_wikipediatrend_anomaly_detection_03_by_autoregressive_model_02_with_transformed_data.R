rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\wikipediatrend")



# ------------------------------------------------------------------------------
# data:  wikipediatrend
# ------------------------------------------------------------------------------


data <- read.csv(file = "wikipediatrend.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)



str(data)


head(data)



# ----------

data$date <- as.Date(data$date)


# dat2 <- data

dat2 <- data %>% filter(date >= "2016-01-01", date < "2018-06-01")




# ------------------------------------------------------------------------------
# Data Transformation:  Box-Cox Transformation
# ------------------------------------------------------------------------------


( trans <- caret::BoxCoxTrans(dat2[,"views"]) )


dat2 <- predict(trans, newdata = dat2[,"views"])




# ----------
graphics.off()

plot(dat2, type = "l")



# -->
# relatively larger changes at anomaly point



# ------------------------------------------------------------------------------
# Estimate Autoregressive Model for Training Data
# ------------------------------------------------------------------------------


# training data
Dtr <- dat2[1:500]


# validation data
xi <- dat2[501:length(dat2)]



Tt <- length(xi)



# ----------
# AR model, automatically selecting AR orders  --> AR(7)

ar.model <- ar(Dtr)


print(ar.model)




# ------------------------------------------------------------------------------
# Compute predicted values ofr validation data set 
# ------------------------------------------------------------------------------

( r <- ar.model$order )


alpha <- ar.model$ar


xmean <- ar.model$x.mean


sig2 <- ar.model$var.pred




# ----------
# compute predicted values for validation data (by each data point)

N <- Tt - r


X <- t(embed(xi - xmean, r))[,1:N]


dim(X)


ypred <- t(X) %*% alpha + xmean




# ------------------------------------------------------------------------------
# Compute anomaly score
# ------------------------------------------------------------------------------


y <- xi[(1 + r):Tt]



# anomaly score
a <- (y - as.numeric(ypred))^2 / sig2




# ----------

graphics.off()

par(mfrow = c(2,1))


plot(xi, type = "l", main = "original validation series")
lines(c(rep(NA, 7), ypred), lty = 1, col = "blue", lwd = 2)

plot(c(rep(NA, 7), a), type = "l", lty = 1, col = "blue", main = "anomaly score")


