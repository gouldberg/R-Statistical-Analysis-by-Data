rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\regression_basics")



# ------------------------------------------------------------------------------
# data:  Davis
# ------------------------------------------------------------------------------


data <- read.csv("Davis.txt", header = T, sep = "\t")


str(data)


car::some(data)




# ------------------------------------------------------------------------------
# Anomaly Detection:  basd on nonlinear model  (Support Vector Machine)
#   - Nonlinear transformation the data by RBF Kernel
# ------------------------------------------------------------------------------

X <- data[, c("weight", "height")]

# X <- rbind(matrix(rnorm(120), ncol = 2), matrix(rnorm(120, mean = 3), ncol = 2))


X <- scale(X)



# ----------
library(kernlab)


sigma <- 0.5

rbf <- rbfdot(sigma = sigma)


# approximately parameters for proportions for anomaly detected data
nu <- 0.1

ocsvm <- ksvm(X, type = "one-svc", kernel = rbf, nu = nu)


# support vectors (alpha vector)
ocsvm@alpha




# ----------
# anomaly detected data  (corresponding to support vector)

ocsvm@alphaindex

fitted(ocsvm)




# ----------
colorcode <- rep(0, nrow(X))

colorcode[ocsvm@alphaindex] <- 1


plot(X, pch = 21, bg = colorcode)

