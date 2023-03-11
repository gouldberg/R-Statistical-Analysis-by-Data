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
# Eigen Value Decomposition and select m
# ------------------------------------------------------------------------------

S <- Xc %*% t(Xc)

evd <- eigen(S)



# ----------
plot(evd$values, type = "b", xlab = "index", ylab = "eigenvalue")


round(cumsum(evd$values) / sum(evd$values), 3)


# -->
# m = 2  (81.7%)




# ------------------------------------------------------------------------------
# Compute normal space score
# ------------------------------------------------------------------------------


m <- 2


# scores for normal space:  2 scores for each car
( x2 <- t(evd$vectors[,1:m]) %*% Xc )


dim(x2)




# ----------
graphics.off()

par(mfrow = c(1,1))

plot(t(x2), xlab = "1st PCA", ylab = "2nd PCA")
text(t(x2)[,1]+0.1, t(x2)[,2]+0.1, colnames(x2), cex = 0.6)




# ------------------------------------------------------------------------------
# Compute anomaly scores
# ------------------------------------------------------------------------------

# anomaly scores = reconstructed errors = residuals ^ 2
a1 <- colSums(Xc * Xc) - colSums(x2 * x2)

a0 <- t(Xc) - rowSums(t(x2))




# ----------
# top 6
( idx <- order(a1, decreasing = T)[1:6] )


print(a1[idx])


a0[idx,]



# -->
# Chevrolet Corvette is outlier in EngineSize and HorsePower
# Mecedes-Benz 300E is outlier in terms of Max.Price





# ----------
colorcode <- rep(0, ncol(x2))

colorcode[idx] <- 1 



# note that this is NORMAL SPACE REPRESENTATION !!!
graphics.off()

par(mfrow = c(1,1))

plot(t(x2)[,1], t(x2)[,2], xlab = "1st PCA", ylab = "2nd PCA", pch = 21, bg = colorcode, cex = 1.2)
text(t(x2)[,1]+0.1, t(x2)[,2]+0.1, colnames(x2), cex = 0.6)


a0[idx]




# ------------------------------------------------------------------------------
# Compute Anomaly Scores, computing from Gramm Matrix (if nrow(data) < ncol(data))
# ------------------------------------------------------------------------------


G <- t(Xc) %*% Xc


evd <- eigen(G)


Lam_12 <- diag(evd$values[1:m] ^ (-0.5))


xx2 <- Lam_12 %*% t(evd$vectors[,1:m]) %*% t(Xc) %*% Xc


# anomaly scores
aa1 <- colSums(Xc * Xc) - colSums(xx2 * xx2)


( idx <- order(aa1, decreasing = T)[1:6] )


print(aa1[idx])




