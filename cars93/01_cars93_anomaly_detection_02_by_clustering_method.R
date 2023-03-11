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
# Anomaly Detection
# by clustering based on mixed normal distributions
# ------------------------------------------------------------------------------


# mclust select number of clusters by BIC
library(mclust)


# for demonstration, take only first 1-7 variables
X <- t(Xc)[,1:7]



# ----------
result <- Mclust(X)


result$BIC



print(summary(result, parameters = TRUE))



# -->
# 3 clusters are selected




# ----------
plot(result)




# ------------------------------------------------------------------------------
# Compute component densties
# ------------------------------------------------------------------------------


# cdens:  compute component densities for observations in MVN mixture models parameterized by eigen value decomposition

XX <- cdens(modelName = result$modelName, X, parameters = result$parameters)





# ------------------------------------------------------------------------------
# Compute anomaly scores
# ------------------------------------------------------------------------------

# mixing proportions
( pi <- result$parameters$pro )



# compute anomaly scores
a <- -log(as.matrix(XX) %*% as.matrix(pi))



graphics.off()
par(mfrow = c(1,1))

plot(a, ylab = "anomaly score")
abline(h = 4, lty = 2)


rownames(X)[a >= 4]

