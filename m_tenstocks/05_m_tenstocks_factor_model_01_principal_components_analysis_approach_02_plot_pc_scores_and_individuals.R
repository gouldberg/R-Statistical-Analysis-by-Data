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
# standardize individual series
# ------------------------------------------------------------------------------


std <- diag(1 / sqrt(diag(cov(rtn))))


rtns <- as.matrix(rtn) %*% std




# ------------------------------------------------------------------------------
# plot PC scores + individual time series
# ------------------------------------------------------------------------------


# 10(company) * 10(Comp)
m1$loadings



# 132 * 10(Comp)
m1$scores



# ----------
# individuals scores:  1 - 5 components


( pred <- data.frame(m1$scores[,1:3] %*% t(m1$loadings[,1:3])) )

colnames(pred) <- colnames(rtn)


res_scores <- data.frame(m1$scores)





# ----------
graphics.off()

par(mfrow = c(3,1), mar = c(2,2,2,2))


# factor scores
plot(res_scores[,"Comp.1"], type = "l", lty = 1, col = "black", ylim = c(-5, 5))
lines(res_scores[,"Comp.2"], type = "l", lty = 2, col = gray(0.3))
lines(res_scores[,"Comp.3"], type = "l", lty = 3, col = gray(0.7))



# ----------
dimnames(rtns)[[2]] <- colnames(rtn)


# TXN and JPM
obj <- "TXN"
obj2 <- "JPM"


# blue: model   black: original time series

plot(pred[,obj], type = "l", lty = 1, lwd = 2, col = "blue", ylim = c(-4, 4))
lines(rtns[,obj], type = "l", lty = 2, lwd = 1, col = "black")

plot(pred[,obj2], typ = "l", lty = 1, lwd = 2, col = "blue", ylim = c(-4, 4))
lines(rtns[,obj2], type = "l", lty = 2, lwd = 1, col = "black")
