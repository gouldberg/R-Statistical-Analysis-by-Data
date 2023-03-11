rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\02_異常検知_入力出力データ\\headsize")



# ------------------------------------------------------------------------------
# data:  headsize
#   - data on head lenght and head breadth for each of the 1st two adult sons in 25 families
# ------------------------------------------------------------------------------


data <- read.csv(file = "headsize.txt", header = T, sep = "\t")


names(data)



# ----------
str(data)



# ----------
# standardize data

data <- sweep(data, 2, apply(data, 2, sd), FUN = "/")





# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------


graphics.off()


par(mfrow = c(1,2))

plot(data$head2 ~ data$head1, main = "head: 2nd son vs 1st son")

plot(data$breadth1 ~ data$breadth2, main = "breadth: 2nd son vs 1st son")



# -->
# both are highly correlated


cor(data$head1, data$head2)

cor(data$breadth1, data$breadth2)




# ------------------------------------------------------------------------------
# Canonical Correlation Analysis
# ------------------------------------------------------------------------------

( R <- cor(data) )



# ----------
# decompose correlation matrix

( r11 <- R[1:2, 1:2] )

( r22 <- R[-(1:2), -(1:2)] )

( r12 <- R[1:2, -(1:2)] )

( r21 <- R[-(1:2), 1:2] )




# ----------
# eigenvectors of matrices

( E1 <- solve(r11) %*% r12 %*% solve(r22) %*% r21 )


( E2 <- solve(r22) %*% r21 %*% solve(r11) %*% r12 )




# ----------
# eigenvalues and eigenvectors
# column 1 of eigenvectors is for 1st variate
# column 2 of eigenvectors is for 2nd variate

( e1 <- eigen(E1) )


# -->
# f1 = +0.73 * head1 + 0.69 * breadth1
# f2 = -0.70 * head1 + 0.71 * breadth1


( e2 <- eigen(E2) )



# -->
# s1 = -0.68 * head2 - 0.70 * breadth2
# s2 = -0.71 * head2 + 0.71 * breadth2




# ----------
# linear functions for 1st son (f) and 2nd son (s)

f1 <- as.matrix(data[,1:2]) %*% e1$vectors[,1]

f2 <- as.matrix(data[,1:2]) %*% e1$vectors[,2]

s1 <- as.matrix(data[,3:4]) %*% e2$vectors[,1]

s2 <- as.matrix(data[,3:4]) %*% e2$vectors[,2]




# ----------
# correlation of first variate for 1st son and 2nd son  = -0.79
# now we can find one more correlated variate

cor(f1, s1)


# almost no corrleation of 2nd variate for 1st son and 2nd son = -0.05
cor(f2, s2)




# ----------
graphics.off()


par(mfrow = c(1,2))

plot(s1 ~ f1, main = "first variate: 2nd son vs 1st son")
abline(v = mean(f1), h = mean(s1), col = "darkgray", lty = 2)
text(f1, s1, label = 1:25, cex = 1.2)

plot(s2 ~ f2, main = "2nd variate: 2nd son vs 1st son")
abline(v = mean(f2), h = mean(s2), col = "darkgray", lty = 2)
text(f2, s2, label = 1:25, cex = 1.2)





# ----------
graphics.off()

par(mfrow = c(1,2))

plot(f2 ~ f1, main = "2nd variate vs 1st variate for 1st son of 25 family")
abline(v = mean(f1), h = mean(f2), col = "darkgray", lty = 2)
text(f1, f2, label = 1:25, cex = 1.2)


plot(s2 ~ s1, main = "2nd variate vs 1st variate for 2nd son of 25 family")
abline(v = mean(s1), h = mean(s2), col = "darkgray", lty = 2)
text(s1, s2, label = 1:25, cex = 1.2)





# ------------------------------------------------------------------------------
# anomaly detection by top 1 to 2 canonical covariate
# ------------------------------------------------------------------------------


X <- as.matrix(data[,1:2])

Y <- as.matrix(data[,3:4])



# anomaly scores for each canonical covariates (1 to 2)

a <- matrix(nrow = nrow(X), ncol = 2, 0)




for(i in 1:2){
  
  f <- X %*% e1$vectors[,i]
  
  g <- Y %*% e2$vectors[,i]
  
  D <- data.frame(f = f, g = g)
  
  
  mod <- lm(g ~ f, data = D)
  
  sig2 <- sum(resid(mod)^2) / nrow(D)
  
  a[,i] <- c((g - resid(mod))^2 / sig2)

}



# ----------
fall <- X %*% e1$vectors[,1:2]

gall <- Y %*% e2$vectors[,1:2]

Dall <- data.frame(f = fall, g = gall)

head(Dall)




# ----------
graphics.off()

par(mfcol = c(3,2))

plot(Dall[,"f.1"], type = "h", main = "1st variate:  1st son")
plot(Dall[,"g.1"], type = "h", main = "1st variate:  2nd son")
plot(a[,1], type = "h", main = "1st variate anomaly")

plot(Dall[,"f.2"], type = "h", main = "2nd variate:  1st son")
plot(Dall[,"g.2"], type = "h", main = "2nd variate:  2nd son")
plot(a[,2], type = "h", main = "2nd variate anomaly")


# -->
# 1st variate:  24, 6, 8, 25
# 2nd variate:  11, 25, 24, 13




# ----------
idx1 <- c(6,8,24,25)

idx2 <- c(11,13,25,24)


par(mfrow = c(2,1))

matplot(Y, type = "l", main = "2nd son of 25 family, head2 and breadth2")
abline(v = idx1)

matplot(Y, type = "l", main = "2nd son of 25 family, head2 and breadth2")
abline(v = idx2)




# ----------
# by variate

graphics.off()

par(mfrow = c(1,2))

idx <- unique(c(idx1))
colorcode <- rep(0, nrow(X))
colorcode[idx] <- 1

plot(s1 ~ f1, pch = 21, main = "1st variate: 2nd son vs 1st son", bg = colorcode)
abline(v = mean(f1), h = mean(s1), col = "darkgray", lty = 2)
text(f1+0.1, s1+0.1, label = 1:25, cex = 1.2)


idx <- unique(c(idx2))
colorcode <- rep(0, nrow(X))
colorcode[idx] <- 1

plot(s2 ~ f2, pch = 21, main = "2nd variate: 2nd son vs 1st son", bg = colorcode)
abline(v = mean(f2), h = mean(s2), col = "darkgray", lty = 2)
text(f2+0.1, s2+0.1, label = 1:25, cex = 1.2)




# ----------
# by son type

# 24th family moves from upper right (1st son) to bottom left (2nd son)

idx <- unique(c(idx1, idx2))
colorcode <- rep(0, nrow(X))
colorcode[idx] <- 1

graphics.off()

par(mfrow = c(1,2))

plot(f2 ~ f1, pch = 21, main = "1st son of 25 family: 2nd vs 1st variate", bg = colorcode, xlim = c(24, 32), ylim = c(0, 2.5))
abline(v = mean(f1), h = mean(f2), col = "darkgray", lty = 2)
text(f1+0.1, f2+0.1, label = 1:25, cex = 1.2)


plot(s2 ~ s1, pch = 21, main = "2nd son of 25 family: 2nd vs 1st variate", bg = colorcode, xlim = c(-32, -26), ylim = c(1.5, 4.0))
abline(v = mean(s1), h = mean(s2), col = "darkgray", lty = 2)
text(s1+0.1, s2+0.1, label = 1:25, cex = 1.2)


