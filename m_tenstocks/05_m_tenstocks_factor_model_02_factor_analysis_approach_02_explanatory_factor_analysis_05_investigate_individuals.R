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
# Individual scores
# ------------------------------------------------------------------------------

# we apply unorthogonal solutions (resFA3)


# 10 * 3
round(resFA3$weights, 3)


# 10 * 3
resFA3$loadings


# 132 * 3
resFA3_scores <- factor.scores(rtn, f = resFA3, method = "Thurstone")$scores
# resFA3_scores <- factor.scores(rtn, f = resFA3, method = "Anderson")$scores


( res_scores <- data.frame(resFA3_scores) )




# ----------
# individuals scores

( pred <- resFA3_scores %*% t(resFA3$weights) )



# TXN
head(rtn[,"TXN"])

head(pred[,"TXN"])


resFA3$weights["TXN",]

resFA3$loadings["TXN",]

head(resFA3_scores)




# ------------------------------------------------------------------------------
# plot: original time series, individual scores, main factor scores
# ------------------------------------------------------------------------------

resFA3wts <- data.frame(resFA3$weights)


# Semiconductor: ML2
# black: model
# blue: original time series
# red: 1st factor loagings

graphics.off()

par(mfrow = c(2,2), mar = c(2,2,2,2))


obj <- c("TXN", "MU", "INTC", "TSM")
obj_fa <- "ML2"

sort(round(resFA3$communality, 4))

# the communality: 0.70, 0.50, 0.73, 0.67  (middle to larges

for(i in 1:length(obj)){
  
  plot(pred[,obj[i]], type = "l", lty = 1, lwd = 2, col = "black", ylim = c(-2, 2), 
       main = paste0(obj[i], " : ", obj_fa, " : weights ", round(resFA3wts[obj[i], obj_fa], 3)))
  abline(v = seq(0, 130, by = 10), lty = 2, col = gray(0.7))
  lines(rtn[,obj[i]], type = "l", lty = 1, lwd = 1, col = "blue")
  lines(res_scores[,obj_fa], type = "l", lty = 1, lwd = 1, col = "red", ylim = c(-2, 2))
}




# ----------
# Pharmaceutical: ML3
# black: model
# blue: original time series
# red: 2nd factor loagings

graphics.off()

par(mfrow = c(2,2), mar = c(2,2,2,2))


obj <- c("PFE", "MRK", "LLY")
obj_fa <- "ML3"

sort(round(resFA3$communality, 4))

# the communality: 0.50, 0.47, 0.57  (all are middle)

for(i in 1:length(obj)){
  
  plot(pred[,obj[i]], type = "l", lty = 1, lwd = 2, col = "black", ylim = c(-2, 2), 
       main = paste0(obj[i], " : ", obj_fa, " : weights ", round(resFA3wts[obj[i], obj_fa], 3)))
  abline(v = seq(0, 130, by = 10), lty = 2, col = gray(0.7))
  lines(rtn[,obj[i]], type = "l", lty = 1, lwd = 1, col = "blue")
  lines(res_scores[,obj_fa], type = "l", lty = 1, lwd = 1, col = "red", ylim = c(-2, 2))
}




# ----------
# Investment Bank: ML1
# black: model
# blue: original time series
# red: 3rd factor loagings

graphics.off()

par(mfrow = c(2,2), mar = c(2,2,2,2))


obj <- c("JPM", "MS", "GS")
obj_fa <- "ML1"

sort(round(resFA3$communality, 4))

# the communality: 0.45, 0.65, 0.90  (small, middle, large)

for(i in 1:length(obj)){
  
  plot(pred[,obj[i]], type = "l", lty = 1, lwd = 2, col = "black", ylim = c(-2, 2), 
       main = paste0(obj[i], " : ", obj_fa, " : weights ", round(resFA3wts[obj[i], obj_fa], 3)))
  abline(v = seq(0, 130, by = 10), lty = 2, col = gray(0.7))
  lines(rtn[,obj[i]], type = "l", lty = 1, lwd = 1, col = "blue")
  lines(res_scores[,obj_fa], type = "l", lty = 1, lwd = 1, col = "red", ylim = c(-2, 2))
}





