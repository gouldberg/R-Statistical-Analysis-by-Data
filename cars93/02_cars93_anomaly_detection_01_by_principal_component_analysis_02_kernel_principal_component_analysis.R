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
# Anomaly Detection by Kernel Principal Component Analysis
#
#  --> this method is really sensitive for parameters (here we try 0.1 and 0.001)
# ------------------------------------------------------------------------------


library(kernlab)


# number of principal components
m <- 2


# kernel parameter
sig1 <- 1.1

sig2 <- 0.1

sig3 <- 0.001



# plot range
li1 <- c(-6, 7)

li2 <- c(-6, 7)

li3 <- c(-3, 3)




# ----------
# kernel principal component analysis

kpc1 <- kpca(t(Xc), kernel = "rbfdot", kpar = list(sigma = sig1), features = m)

kpc2 <- kpca(t(Xc), kernel = "rbfdot", kpar = list(sigma = sig2), features = m)

kpc3 <- kpca(t(Xc), kernel = "rbfdot", kpar = list(sigma = sig3), features = m)




# ----------
# principal component venctors

pcv(kpc3)





# ----------
# coodinates

Zt1 <- rotated(kpc1)

Zt2 <- rotated(kpc2)

Zt3 <- rotated(kpc3)




# ----------
graphics.off()

par(mfrow = c(1,1))


# idx (black:  detected by normal PCA
colorcode <- rep(0, 93)

colorcode[idx] <- 1


plot(Zt1[,1], Zt1[,2], pch = 21, bg = colorcode, xlab = "1st PC", ylab = "2nd PC", cex = 2, xlim = li1, ylim = li1, main = sig1)
text(Zt1[,1], Zt1[,2], rownames(t(Xc)), cex = 0.8)


plot(Zt2[,1], Zt2[,2], pch = 21, bg = colorcode, xlab = "1st PC", ylab = "2nd PC", cex = 2, xlim = li2, ylim = li2, main = sig2)
text(Zt2[,1], Zt2[,2], rownames(t(Xc)), cex = 0.8)



# ----------
# sigma = 0.001  -->  very similar to normal principal component
# when sigma << 1, the result of kernel principal component with RBF kernel is very similar to normal principal component analysis

plot(Zt3[,1], Zt3[,2], pch = 21, bg = colorcode, xlab = "1st PC", ylab = "2nd PC", cex = 2, xlim = li3, ylim = li3, main = sig3)
text(Zt3[,1], Zt3[,2], rownames(t(Xc)), cex = 0.8)



