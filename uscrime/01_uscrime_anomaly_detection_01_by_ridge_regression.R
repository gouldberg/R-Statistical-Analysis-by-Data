rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\uscrime")



# ------------------------------------------------------------------------------
# data:  UScrime
#    - M:  proportion of men aged 14 to 24
#    - Ed:  average educated years
#    - Po1:  police budget at FY1960
#    - Po2:  police budget at FY1959
#    - LF:  ratio of labor force
#    - M.F:  number of men per 1000 women
#    - pop:  population at state
#    - NW:  non-white population per 1000
#    - U1:  unemployment ratio for men in urban area, aged 14 - 24
#    - U2:  unemployment ratio for men in urban araa, aged 35 to 39
#    - GDP:  GDP per person at state
#    - Ineq:  economical iequality index
#    - Prob:  proportion of imprisonment
#    - Time:  average years of imprisonment
# ------------------------------------------------------------------------------


library(MASS)


str(UScrime)


car::some(UScrime)





# ------------------------------------------------------------------------------
# Find variables with mean of absolute values of pair-wise correlation is large
# ------------------------------------------------------------------------------


res <- caret::findCorrelation(cor(UScrime), cutoff = 0.7)


colnames(UScrime)[res]



# -->
# the variables with mean of absolute values of pair-wise correlation >= 0.7 




# ------------------------------------------------------------------------------
# prediction by ridge regression
# ------------------------------------------------------------------------------


# removing So and y
X <- UScrime[,-c(2, 16)]


# y:  Time
y <- UScrime[,16]


M <- ncol(X)

N <- length(y)



# ----------
# set lambda and ridge regression selecting best lambda by cross validation

lambdas <- seq(0, 5, length = 50)


model <- lm.ridge(y ~ ., cbind(X, y), lambda = lambdas)



( bestIdx <- which.min(model$GCV) )

# selected lambda
lam <- model$lambda[bestIdx]



# ----------
# prediction

( coefs <- coef(model)[bestIdx,] )


ypred <- as.matrix(X) %*% as.matrix(coefs[2:15]) + coefs[1]





# ------------------------------------------------------------------------------
# Compute anomaly scores
# ------------------------------------------------------------------------------

sig2 <- (lam * sum(coefs[2:15] ^ 2) + sum(as.numeric(ypred) - y)^2) / N


X_ <- t(scale(X, scale = F))


H <- t(X_) %*% solve(X_ %*% t(X_) + lam * diag(M), X_)


TrHN <- sum(diag(H)) / N


# anomaly scores
a <- (as.numeric(ypred) - y) ^ 2 / ((1 - TrHN) ^ 2 * sig2)




# ----------
graphics.off()
par(mfrow = c(1,2))


plot(ypred ~ y, xlab = "original", ylab = "pred")

plot(a, xlab = "index", ylab = "anomaly score")

th <- sort(a)[N * (1 - 0.05)]

lines(0:50, rep(th, length(0:50)), col = "red", lty = 2)






