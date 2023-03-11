# setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_tenstocks")

setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\02_異常検知_時系列\\m_tenstocks")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-tenstocks
# ------------------------------------------------------------------------------


data <- read.csv("m-tenstocks.txt", sep = " ", header = T)


str(data)


dim(data)


car::some(data)




# ------------------------------------------------------------------------------
# split data
# ------------------------------------------------------------------------------

data$date


# 200808 and before
train_idx <- 1:92


# 200809 and after
test_idx <- 93:132


train <- data[train_idx,]


test <- data[test_idx,]


data$class <- c(rep(0, length(train_idx)), rep(1, length(test_idx)))




# ------------------------------------------------------------------------------
# Estimate Latent Network Model by Eigenmodels
# ------------------------------------------------------------------------------

library(eigenmodel)


# use train data
cormat <- cor(train[,2:11])


diag(cormat) <- NA


# fit a 2-dimensional model and use 4000 MCMC iterations with a burn-in of 800 steps
fitEM <- eigenmodel_mcmc(cormat, R = 2, S = 4000, burn = 800, seed = 123)




# ----------
# posterior means of the eigenvalues

evals <- colMeans(fitEM$L_postsamp)

evals




# ----------
evecs <- eigen(fitEM$ULU_postmean)$vec[,1:2]


cols <- c("black", "blue", "red")


plot(evecs, type = "n", xlab = "Dimension 1", ylab = "Dimension 2")


# correlation threshold
corthresh <- 0.2


addlines(evecs, abs(cormat) > corthresh, col = "gray")

ind <- c(1,1,1,1,2,2,2,3,3,3)

text(evecs, labels = rownames(cormat), col = cols[ind], cex = 1.2)



