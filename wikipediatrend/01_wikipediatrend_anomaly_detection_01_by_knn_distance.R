rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\�f�X�N�g�b�v\\�Z�p�͋���_���v���\\51_��̓X�N���v�g\\00_basics\\05_anomaly_detection\\02_�ُ팟�m_���n��\\wikipediatrend")



# ------------------------------------------------------------------------------
# data:  wikipediatrend
# ------------------------------------------------------------------------------


data <- read.csv(file = "wikipediatrend.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)



str(data)


head(data)



# ----------

data$date <- as.Date(data$date)



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


library(ggplot2)

library(dplyr)

ggplot(data %>% filter(date >= "2016-01-01", date < "2018-06-01"), aes(x=date, y=views, color=views)) + geom_line()





# ------------------------------------------------------------------------------
# subsetting time series
# ------------------------------------------------------------------------------

# dat2 <- data

dat2 <- data %>% filter(date >= "2016-01-01", date < "2018-06-01")






# ------------------------------------------------------------------------------
# Vectorize time series by slide window
# ------------------------------------------------------------------------------


# window length
w <- 7



# ----------
# training data set: Xtr

Xtr <- dat2[1:500, "views"]


# embedding time series
Dtr <- embed(Xtr, w)


dim(Dtr)



# both are same
Xtr[w:1]

Dtr[1,]


dim(Dtr)




# ----------
# validation data set: X

X <- dat2[501:nrow(dat2), "views"]


D <- embed(X, w)


dim(D)




# ------------------------------------------------------------------------------
# Compute distances between series and extract k nearest neighbour distance --> anomaly
# ------------------------------------------------------------------------------

# Fast Nearest Neighbor Search Algorithms and Applications
library(FNN)



# number of neighbours
nk <- 1



# compute distance between time series --> compute distance --> select nk nearest neibour distance
d <- knnx.dist(Dtr, D, k = nk)


dim(d)




# anomaly score = average distance to k nearest neighbour
( a <- rowMeans(d) )




# ----------

graphics.off()

par(mfrow = c(2,1))

plot(X, type = "l", main = "original validation series")

plot(c(rep(NA, w - 1), a), type = "l", lty = 1, col = "blue", main = "anomaly score")

