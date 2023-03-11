# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_tenstocks")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-tenstocks
# ------------------------------------------------------------------------------

mtenstocks <- read.csv("m-tenstocks.txt", sep = " ", header = T)


str(mtenstocks)


dim(mtenstocks)


car::some(mtenstocks)



# ----------
# log returns

rtn <- log(mtenstocks[,2:11] + 1)



# ------------------------------------------------------------------------------
# compute correlation matrix
# ------------------------------------------------------------------------------

polcor <- cor(rtn)


polcor



# ------------------------------------------------------------------------------
# Prepare data:  y(t) (TXN) and x(t)
# ------------------------------------------------------------------------------

# we predict y(t+1) for the first 5 components of z series using x(t)


# y(t):  TXN row 5 to 132

( y <- rtn[5:132, 1] )


length(y)



# ----------
# x(t) = c(z(t), z(t-1), z(t-2), z(t-3))
# note that this is including TXN itself

# only including TXN, MU, INTC, TSM
# ( x <- cbind(rtn[4:131,1:4], rtn[3:130,1:4], rtn[2:129,1:4], rtn[1:128,1:4]) )

( x <- cbind(rtn[4:131,], rtn[3:130,], rtn[2:129,], rtn[1:128,]) )


dim(x)




# ------------------------------------------------------------------------------
# One-Step Ahead predicion for "TXN" using Diffusion Index MOdel
# ------------------------------------------------------------------------------

library(MTS)


# the forecast origin = 100
# we use the first 350 observations of z(t) to construct x(t) and perform PCA on x(t)
# to derive the diffusion indeces.
# The components of x(t) are standardized individually before the PCA.

# f(it): diffusion indices
# y(t+1) = beta0 + beta(i) * f(it) + e(t),  t = 5, ..., 350


orig <- 50




# ----------
# number of diffusion indices: 10, 20, 30, 40, ...

m10 <- SWfore(y, x, orig, 3)


m11 <- SWfore(y, x, orig, 5)


m12 <- SWfore(y, x, orig, 7)


m13 <- SWfore(y, x, orig, 10)


m14 <- SWfore(rtn[5:132, 1], x, orig, 16)





# ------------------------------------------------------------------------------
# plot diffusion indices
# ------------------------------------------------------------------------------


# first 1-5, 6-10, 11-15 diffusion indices

par(mfrow = c(3,1))

matplot(m14$DFindex[,1:5], type = "l")


matplot(m14$DFindex[,6:10], type = "l")


matplot(m14$DFindex[,11:15], type = "l")




# ----------
# first 1-3 indices

par(mfrow = c(1,1))

matplot(m14$DFindex[,1:3], type = "l")





# ------------------------------------------------------------------------------
# Forecast and original time series
# ------------------------------------------------------------------------------


graphics.off()


par(mfrow = c(1,1), mar = c(2,2,2,2))


# black:  original time series
# blue:  number of diffusion indices = 10  (smallest means squares of error)

plot(m10$yhat, type = "l", lty = 1, lwd = 3, col = "blue", ylim = c(-0.2, 0.2))
lines(m11$yhat, type = "l", lty = 2, lwd = 2, col = "red")
lines(rtn[55:132,"TXN"], lty = 1, lwd = 1, col = "black")

