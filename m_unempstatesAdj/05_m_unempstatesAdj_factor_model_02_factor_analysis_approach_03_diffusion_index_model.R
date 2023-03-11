# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_unempstatesAdj")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-unempstatesAdj
# ------------------------------------------------------------------------------

da <- read.csv("m-unempstatesAdj.txt", sep = "", header = T)


str(da)


dim(da)


car::some(da)



# ----------
# first difference

drate <- diffM(da)



# ------------------------------------------------------------------------------
# compute correlation matrix
# ------------------------------------------------------------------------------

polcor <- cor(drate)


polcor




# ------------------------------------------------------------------------------
# Prepare data:  y(t) (Alabama) and x(t)
# ------------------------------------------------------------------------------

# we predict y(t+1) for the first 5 components of z series using x(t)


# y(t):  Alabama row 5 to 415

( y <- drate[5:415, 1] )


length(y)



# ----------
# x(t) = c(z(t), z(t-1), z(t-2), z(t-3))
# note that this is including Alabama itself

( x <- cbind(drate[4:414,], drate[3:413,], drate[2:412,], drate[1:411,]) )


dim(x)


# dependent variables are:  Alabama(AL), Alaska(AK), Arizona(AZ), Arkansas(AR), California(CA)




# ------------------------------------------------------------------------------
# One-Step Ahead predicion for "Alabama" using Diffusion Index MOdel
# ------------------------------------------------------------------------------

library(MTS)


# the forecast origin = 350
# we use the first 350 observations of z(t) to construct x(t) and perform PCA on x(t)
# to derive the diffusion indeces.
# The components of x(t) are standardized individually before the PCA.

# f(it): diffusion indices
# y(t+1) = beta0 + beta(i) * f(it) + e(t),  t = 5, ..., 350


orig <- 350




# ----------
# number of diffusion indices: 10, 20, 30, 40, ...

m10 <- SWfore(y, x, orig, 10)


m11 <- SWfore(y, x, orig, 20)


m12 <- SWfore(y, x, orig, 30)


m13 <- SWfore(y, x, orig, 40)


m14 <- SWfore(y, x, orig, 50)


m15 <- SWfore(drate[5:415, 1], x, orig, 200)





# ------------------------------------------------------------------------------
# plot diffusion indices
# ------------------------------------------------------------------------------


# first 1-10, 11-20, 21-30 diffusion indices

par(mfrow = c(3,1))

matplot(m14$DFindex[,1:10], type = "l")


matplot(m14$DFindex[,11:20], type = "l")


matplot(m14$DFindex[,21:30], type = "l")




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
# blue:  number of diffusion indices = 50  (smallest means squares of error)

plot(m14$yhat, type = "l", lty = 1, lwd = 3, col = "blue")
lines(m13$yhat, type = "l", lty = 2, lwd = 2, col = "red")
lines(m12$yhat, type = "l", lty = 2, lwd = 2, col = "darkgray")
lines(m11$yhat, type = "l", lty = 2, lwd = 2, col = gray(0.7))
lines(drate[354:415,"AL"], lty = 1, lwd = 1, col = "black")

