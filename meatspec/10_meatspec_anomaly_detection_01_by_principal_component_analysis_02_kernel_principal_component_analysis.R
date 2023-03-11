
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\02_異常検知_入力出力データ\\meatspec")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  meatspec
# ------------------------------------------------------------------------------


# data(meatspec, package="faraway")

meatspec <- read.csv("meatspec.txt", header = T, sep = "\t")


names(meatspec)


# 100-channel spectrum of absorbances for 215 meat samples
dim(meatspec)




# ------------------------------------------------------------------------------
# Select variables and transform data
# ------------------------------------------------------------------------------


# select all variables

cc <- paste0("V", 1:100)


Xc <- scale(meatspec[,cc])


psych::describe(Xc)


head(Xc)


Xc <- t(Xc)




# ------------------------------------------------------------------------------
# Anomaly Detection by Kernel Principal Component Analysis
#
#  --> this method is really sensitive for parameters (here we try 0.1 and 0.001)
# ------------------------------------------------------------------------------


library(kernlab)


# number of principal components
m <- 2


# kernel parameter
sig1 <- 2.1

sig2 <- 0.1

sig3 <- 0.001



# plot range
li1 <- c(-15, 20)

li2 <- c(-15, 20)

li3 <- c(-15, 20)




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
text(Zt1[,1], Zt1[,2], rownames(t(Xc)), cex = 0.5)


plot(Zt2[,1], Zt2[,2], pch = 21, bg = colorcode, xlab = "1st PC", ylab = "2nd PC", cex = 2, xlim = li2, ylim = li2, main = sig2)
text(Zt2[,1], Zt2[,2], rownames(t(Xc)), cex = 0.5)



# ----------
# sigma = 0.001  -->  very similar to normal principal component  ??
# when sigma << 1, the result of kernel principal component with RBF kernel is very similar to normal principal component analysis

plot(Zt3[,1], Zt3[,2], pch = 21, bg = colorcode, xlab = "1st PC", ylab = "2nd PC", cex = 2, xlim = li3, ylim = li3, main = sig3)
text(Zt3[,1], Zt3[,2], rownames(t(Xc)), cex = 0.5)



