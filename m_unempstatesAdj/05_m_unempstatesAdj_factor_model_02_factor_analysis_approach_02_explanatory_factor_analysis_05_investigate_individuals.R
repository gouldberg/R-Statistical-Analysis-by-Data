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
# Individual scores
# ------------------------------------------------------------------------------

# we apply unorthogonal solutions (resFA3)


# 50 * 3
head(resFA3$weights)


# 50 * 3
resFA3$loadings


# 415 * 3
head(resFA3_scores)



# ----------
resFA3_scores <- factor.scores(da, f = resFA3, method = "Thurstone")$scores
resFA3_scores <- factor.scores(da, f = resFA3, method = "tenBerge")$scores


res_scores <- data.frame(resFA3_scores)



# ----------
# individuals scores

( pred <- resFA3_scores %*% t(resFA3$weights) )




# ----------
sort(round(resFA3$communality, 4))




# ------------------------------------------------------------------------------
# plot: original time series, individual scores, main factor scores
# ------------------------------------------------------------------------------

resFA3wts <- data.frame(resFA3$weights)


# black: model
# blue: original time series
# red: 1st factor loagings

graphics.off()

par(mfrow = c(2,1), mar = c(2,2,2,2))


obj <- c("MN", "RI")
obj_fa <- "ML1"

round(resFA3$communality, 4)

# the communality: 0.69

for(i in 1:length(obj)){
  
  plot(pred[,obj[i]], type = "l", lty = 1, lwd = 2, col = "black", ylim = c(-3, 3), 
       main = paste0(obj[i], " : ", obj_fa, " : weights ", round(resFA3wts[obj[i], obj_fa], 3)))
  abline(v = seq(0, 440, by = 40), lty = 2, col = gray(0.7))
  lines(drate[,obj[i]], type = "l", lty = 1, lwd = 1, col = "blue")
  lines(res_scores[,obj_fa], type = "l", lty = 1, lwd = 1, col = "red", ylim = c(-2, 2))
}




# ----------
# black: model
# blue: original time series
# red: 2nd factor loagings

graphics.off()

par(mfrow = c(2,1), mar = c(2,2,2,2))


obj <- c("MA", "CO")
obj_fa <- "ML2"

round(resFA3$communality, 4)

# the communality: 0.73

for(i in 1:length(obj)){
  
  plot(pred[,obj[i]], type = "l", lty = 1, lwd = 2, col = "black", ylim = c(-2, 2), 
       main = paste0(obj[i], " : ", obj_fa, " : weights ", round(resFA3wts[obj[i], obj_fa], 3)))
  abline(v = seq(0, 440, by = 40), lty = 2, col = gray(0.7))
  lines(drate[,obj[i]], type = "l", lty = 1, lwd = 1, col = "blue")
  lines(res_scores[,obj_fa], type = "l", lty = 1, lwd = 1, col = "red", ylim = c(-2, 2))
}




# ----------
# black: model
# blue: original time series
# red: 3rd factor loagings

graphics.off()

par(mfrow = c(2,1), mar = c(2,2,2,2))


obj <- c("WY", "NH")
obj_fa <- "ML3"

round(resFA3$communality, 4)

# the communality: 0.63

for(i in 1:length(obj)){
  
  plot(pred[,obj[i]], type = "l", lty = 1, lwd = 2, col = "black", ylim = c(-2, 2), 
       main = paste0(obj[i], " : ", obj_fa, " : weights ", round(resFA3wts[obj[i], obj_fa], 3)))
  abline(v = seq(0, 440, by = 40), lty = 2, col = gray(0.7))
  lines(drate[,obj[i]], type = "l", lty = 1, lwd = 1, col = "blue")
  lines(res_scores[,obj_fa], type = "l", lty = 1, lwd = 1, col = "red", ylim = c(-2, 2))
}



