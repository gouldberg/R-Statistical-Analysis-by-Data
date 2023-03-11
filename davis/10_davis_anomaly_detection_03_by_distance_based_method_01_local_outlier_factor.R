rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\01_異常検知\\davis")


# ------------------------------------------------------------------------------
# data:  Davis
# ------------------------------------------------------------------------------


data <- read.csv("Davis.txt", header = T, sep = "\t")


str(data)


car::some(data)




# ------------------------------------------------------------------------------
# Anomaly Decetion by Local Outlier Factor (LOF)
#   - LOF compares the local density of an point to the local densities of its neighbors.
#     Points that have a substantially lower density than their neighbors are considered outliers.
#     A LOF score of approximately 1 indicates that density around the point is comparable to its neighbors.
#     Scores significantly larger than 1 indicate outliers.
#   - Note: If there are more than k duplicate points in the data, then lof can become NaN
#     caused by an infinite local density. 
#     In this case we set lof to 1. 
# ------------------------------------------------------------------------------

library(dbscan)


X <- as.matrix(data[,2:3])




# size of neighbouhood

k <- 3

lof_res <- lof(X, k = k)



# ----------
# LOF score
lof_res

summary(lof_res)


hist(lof_res, breaks = 10)



# ----------
graphics.off()

par(mfrow = c(1,1))
  
plot(X, pch = ".", main = "LOF score")

points(X, cex = sqrt(lof_res - 1), pch = 21, col = "red")

text(X[lof_res > 2,], labels = round(lof_res, 1)[lof_res > 2], pos = 3)


