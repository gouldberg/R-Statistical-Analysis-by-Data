rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\01_異常検知\\cars93")



# ------------------------------------------------------------------------------
# data:  Cars93
#    - data of 93 type cars
#    - MPG: mile per gallon
# ------------------------------------------------------------------------------


library(MASS)


str(Cars93)


car::some(Cars93)





# ------------------------------------------------------------------------------
# Select variables and transform data
# ------------------------------------------------------------------------------


# select 15 variables

cc <- c("Min.Price", "Price", "Max.Price", "MPG.city", "MPG.highway", "EngineSize", "Horsepower", "RPM", "Rev.per.mile",
        "Fuel.tank.capacity", "Length", "Wheelbase", "Width", "Turn.circle", "Weight")


Xc <- t(scale(Cars93[,cc]))

colnames(Xc) <- t(Cars93[,"Make"])


head(Xc)



# ----------
# column is Make and row is variable

summary(t(Xc))




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


X <- t(Xc)




# size of neighbouhood

k <- 5

lof_res <- lof(X, k = k)



# ----------
# LOF score
lof_res

summary(lof_res)


hist(lof_res, breaks = 10)




# ----------
graphics.off()

par(mfrow = c(3,3))

crit <- 1.5

for(i in 2:ncol(X)){
  plot(X[,i] ~ X[,i-1], pch = ".", main = paste0(colnames(X)[i], " : ", colnames(X)[i-1]))
  
  points(X[,c(i-1, i)], cex = sqrt(lof_res) * 2, pch = 21, col = "red")
  
  text(X[lof_res > crit, c(i-1, i)] + 0.1, labels = rownames(X)[lof_res > crit])
}



# pairs(t(Xc), cex = lof_res)

