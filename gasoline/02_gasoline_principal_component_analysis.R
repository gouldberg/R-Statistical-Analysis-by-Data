rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\gasoline")



# ------------------------------------------------------------------------------
# data:  gasoline
#   - data set consisting of octane number (octane) and NIR spectra (NIR) of 60 gasoline samples
#     Each NIR spectrum consists of 401 diffuse reflectance measurements from 900 to 1700 nm
# ------------------------------------------------------------------------------


data(gasoline, package = "pls")


dim(gasoline)


gasoline$octane



# ----------
# 60 * 401

dim(gasoline$NIR)

str(gasoline$NIR)

dimnames(gasoline$NIR)[[1]]

dimnames(gasoline$NIR)[[2]]



# ------------------------------------------------------------------------------
# Principal Component Analysis
# ------------------------------------------------------------------------------


library(FactoMineR)


# need to convert for FactoMineR
tmp <- data.frame(matrix(gasoline$NIR, nrow = 60, ncol = 401))

str(tmp)

colnames(tmp) <- dimnames(gasoline$NIR)[[2]]
rownames(tmp) <- dimnames(gasoline$NIR)[[1]]


# ----------
res.pca0 <- FactoMineR::PCA(tmp)


head(res.pca0$eig)




# ----------
# extreme values

extremes <- c(15, 41, 45, 57)


Xextr <- scale(gasoline$NIR, scale = FALSE)[extremes,]

wavelengths <- seq(900, 1700, by = 2)

matplot(wavelengths, t(Xextr), type = "l", xlab = "Wavelength(nm)", ylab = "Intensity (mean-scaled)",
        lty = c(1,1,2,2), col = c(1, "gray", 1, "gray"))

abline(h = 0, col = "gray", lty = 3)






