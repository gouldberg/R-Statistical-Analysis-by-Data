# ------------------------------------------------------------------------------
# Placekicking データ
# 1,425 placekick during the 1995 NFL season
# PAT (point after touchdown)  1 for PAT attempt,  0 for a field goal attmpt
# 目的変数: good (successful  1   failed  0 pacekicks)
# ------------------------------------------------------------------------------
setwd("C:\\Users\\光世\\Desktop\\#R_ロジスティック回帰\\")
library(dplyr)

data <- read.csv(".\\AnalysisofCategoricalData\\Chapter2\\Placekick.csv", sep=",", header=T)


# ------------------------------------------------------------------------------
# 基礎分析：　scatter plot　など
# ------------------------------------------------------------------------------
plot(good ~ distance, data, xlab="distance", ylab="sucessful or failed")


# 目的変数が 0-1 なので、distance をグループに集計し、各distグループごとのsuccess比率を算出
data <- data %>% mutate(dist = ifelse(distance >=60, 6, ifelse(distance >= 50, 5, ifelse(distance >= 40, 4, ifelse(distance >= 30, 3, ifelse(distance >= 20, 2, 1))))))

data2 <- data %>% dplyr::select(dist, good) %>% mutate(n = 1) %>% group_by(dist) %>% summarize_all(funs(sum)) %>% mutate(Failed = n - good, Mean = round(good / n, 3))
colnames(data2) <- c("dist", "Success", "n", "Failed", "Mean")
data2 <- data2 %>% dplyr::select(1,3,2,4,5)

plot(Mean~dist, data2, ylim=c(0,1), xlab="dist", ylab="Success(Mean)")



# ------------------------------------------------------------------------------
# ロジスティック回帰
# ------------------------------------------------------------------------------
mod.fit <- glm(formula=good~distance, family=binomial(link=logit), data=data)

# Null Deviance：　切片モデルの完全モデルからの乖離　　Residual Deviance：　推定モデルの完全モデルからの乖離
# Residual Deviance < Null Deviance　なので回帰係数には意味がありそうだ
summary(mod.fit)



# ------------------------------------------------------------------------------
# 推定パラメータの共分散行列
# 共分散行列を実際に計算してみる
# ------------------------------------------------------------------------------
# 共分散行列
vcov(mod.fit)

# distance のロジスティック回帰係数の推定誤差
sqrt(vcov(mod.fit)[2,2])

# 共分散行列を計算してみる = (X'VX)^-1
pi.hat <- mod.fit$fitted.values
V <- diag(pi.hat * (1 - pi.hat))
X <- cbind(1, data$distance)

solve(t(X) %*% V %*%  X)


# ------------------------------------------------------------------------------
# パラメータ推定値　を実際に optim を使い、対数尤度最大化で計算してみる
# ------------------------------------------------------------------------------
logL <- function(beta, x, Y){
  pi <- exp(beta[1] + beta[2] * x) / (1 + exp(beta[1] + beta[2] * x))
  sum(Y * log(pi) + (1-Y) * log(1 - pi))
}

logL(beta = mod.fit$coefficients, x = data$distance, Y = data$good)

# まず初期値を通常の回帰で計算する
reg.mod <- lm(formula = good ~ distance, data=data)
reg.mod$coefficients

# optim で計算
mod.fit.optim <- optim(par=reg.mod$coefficients, fn=logL, hessian=TRUE, x=data$distance, Y=data$good, control=list(fnscale=-1), method="BFGS")
mod.fit.optim$convergence  # 収束したら 0

# 推定された係数の値
mod.fit.optim$par

# 対数尤度
mod.fit.optim$value
logLik(mod.fit)

# 推定パラメータの共分散行列 = hessian 行列の逆行列のマイナス
-solve(mod.fit.optim$hessian)
vcov(mod.fit)

# 対数尤度 曲面を再現し、最大対数尤度を算出
beta0.values <- seq(from = -5, to = 18, by = 0.1)
beta1.values <- seq(from = -0.65, to = 0.25, by = 0.01)
count <- 1
save.logL <- numeric(length(beta0.values)*length(beta1.values))
for (beta0 in beta0.values) {
  for (beta1 in beta1.values) {
    save.logL[count] <- logL(beta = c(beta0, beta1), x = data$distance, Y = data$good)
    count <- count + 1
  }
}
max(save.logL)

beta0.1.values <- expand.grid(beta0.values, beta1.values)
save.logL1 <- -apply(X = beta0.1.values, MARGIN = 1, FUN = neglogL, x = data$distance, Y = data$good)
max(save.logL1)

library(package = rgl)
open3d()
persp3d(x = beta1.values, y = beta0.values, z = save.logL, xlab = "beta1", ylab = "beta0", zlab = "log(L)", ticktype = "detailed", col="red")
grid3d(c("x", "y+", "z"))


# 対数尤度の等高線
save.logL2　<-　matrix(save.logL, nrow = length(beta0.values), ncol = length(beta1.values), byrow = T)
save.logL[2]
save.logL2[1:2,1:2]

x11(width = 7, height = 6, pointsize = 12)
# pdf(file = "c:\\figures\\Figure2.3color.pdf", width = 7, height = 6, colormodel = "cmyk")   # Create plot for book
par(pty = "s")
contour(x = beta0.values, y = beta1.values, z = save.logL2,  xlab =
  expression(beta[0]), ylab = expression(beta[1]), levels = -c(10000, 7500, 5000, 2500, 1000, 750, 500, 450, 400))
abline(h = mod.fit$coefficients[2], lty = "dashed", lwd = 2, col= "red")
abline(v = mod.fit$coefficients[1], lty = "dashed", lwd = 2, col= "red")

# dev.off()

# Show what a contour represents by finding log(L) approximately equalt to -500
beta0.1.values<-expand.grid(beta1.values, beta0.values)
save500<-data.frame(beta0.1.values[save.logL < -499 & save.logL > -501,], logL = save.logL[save.logL < -499 & save.logL > -501])
points(x = save500$Var2, y = save500$Var1, pch = 20, col = "darkblue")


# ------------------------------------------------------------------------------
# データを binomial response フォーマットに変換して、回帰  -->　同じ結果
# ------------------------------------------------------------------------------
( w <- aggregate(formula = good ~ distance, data=data, FUN=sum) )
( n <- aggregate(formula = good ~ distance, data=data, FUN=length) )
w.n <- data.frame(distance = w$distance, success = w$good, trials = n$good, proportion = round(w$good / n$good, 4))
head(w.n)

# weights を指定する -->　結果は同じ
mod.fit.bin <- glm(formula = success/trials ~ distance, weights = trials, family=binomial(link=logit), data=w.n)
summary(mod.fit.bin)


# ------------------------------------------------------------------------------
# パラメータに関する検定
# ------------------------------------------------------------------------------
# 回帰係数のワルド検定: 3通りのスクリプト
library(package = car)  # Anova() is in the car package

mod.fit2 <- glm(formula = good ~ change + distance, family = binomial(link = logit), data = data)
# 1
round(summary(mod.fit2)$coefficients, 4)
# 2
Anova(mod = mod.fit2, test.statistic="Wald")
# 3
Z0 <- mod.fit2$coefficients[2]/sqrt(vcov(mod.fit2)[2,2])
pvalue <- 2*(1 - pnorm(q = abs(Z0)))
round(data.frame(Z0, pvalue), 4)


# 尤度比検定
Anova(mod.fit2, test = "LR")  # Given other variables in model, "test = 'LR' is the default
anova(mod.fit2, test = "Chisq")  # Sequential testing of variables

# Another way to perform the test given other variables are in the model
drop1(object = mod.fit2, test = "LRT")
library(package = lmtest)
lrtest(mod.fit, mod.fit2)

 # Test Change from fits of two models
 mod.fit.Ho<-glm(formula = good ~ distance, family = binomial(link = logit), data = placekick)
 anova(mod.fit.Ho, mod.fit2, test = "Chisq")
 # Alternative way to perform the above test
 df<-mod.fit.Ho$df.residual-mod.fit2$df.residual
 stat<-mod.fit.Ho$deviance-mod.fit2$deviance
 pvalue<-1-pchisq(q = stat, df = df)
 data.frame(Ho.resid.dev = mod.fit.Ho$deviance, Ha.resid.dev = mod.fit2$deviance, df = df,
   stat = round(stat,4), pvalue = round(pvalue,4))


 # Test change from fits of two models
 mod.fit.Ho<-glm(formula = good ~ distance, family = binomial(link = logit), data = placekick)
 anova(mod.fit.Ho, mod.fit2, test = "Chisq")


 # Test of Ho: logit(pi) = beta_0  vs. Ha: logit(pi) = beta_0 + beta_1*change
 mod.fit.Ho<-glm(formula = good ~ 1, family = binomial(link = logit), data = placekick)
 mod.fit.Ha<-glm(formula = good ~ change, family = binomial(link = logit), data = placekick)
 anova(mod.fit.Ho, mod.fit.Ha, test = "Chisq")
 pi.hat.Ho<-mod.fit.Ho$fitted.values
 pi.hat.Ha<-mod.fit.Ha$fitted.values
 y<-placekick$good
 stat<--2*sum(y*log(pi.hat.Ho/pi.hat.Ha) + (1-y)*log((1-pi.hat.Ho)/(1-pi.hat.Ha)))  # -2log(Lambda)
 pvalue<-1-pchisq(q = stat, df = 1)
 data.frame(stat, pvalue)
 head(pi.hat.Ho)  # All pi^'s same for Ho model
 mean(y)  # Observed proportion of successes


#


# ------------------------------------------------------------------------------
# 予測
# ------------------------------------------------------------------------------
# 入力変数の値
predict.data <- data.frame(distance = c(20))

# 線形子の予測値
predict(mod.fit, newdata=predict.data, type="link")

# 最終的な予測値
predict(mod.fit, newdata=predict.data, type="response")

# ワルド信頼区間
alpha <- 0.5
linear.pred <- predict(mod.fit, newdata=predict.data, type="link", se=TRUE)
pi.hat <- exp(linear.pred$fit) / (1 + exp(linear.pred$fit))
CI.lin.pred <- linear.pred$fit + qnorm(p = c(alpha/2, 1-alpha/2)) * linear.pred$se
CI.pi <- exp(CI.lin.pred) / (1 + exp(CI.lin.pred))
data.frame(predict.data, pi.hat, lower=CI.pi[1], upper=CI.pi[2])


# プロファイルLR: サンプル数が大きければワルド信頼区間に近い
library(mcprofile)
K <- matrix(data=c(1,20), nrow=1, ncol=2) # 1 for intercept
( linear.combo <- mcprofile(mod.fit, CM=K) ) # CM: contrast matrix, calculate -2log(Lambda)
( ci.logit.profile <- confint(linear.combo, level=0.95) )
exp(ci.logit.profile$confint) / (1 + exp(ci.logit.profile$confint))



plot(x = w$distance, y = w$good/n$good, xlab="Distance(yeards)", ylab="Estimated probability", panel.first=grid(col="gray", lty="dotted"))
curve(expr = predict(mod.fit, newdata=data.frame(distance=x), type="response"), col="red", add=TRUE, xlim=c(18,66))

ci.pi <- function(newdata, mod.fit.obj, alpha){
  linear.pred <- predict(mod.fit.obj, newdata=newdata, type="link", se=TRUE)
  CI.lin.pred.lower <- linear.pred$fit - qnorm(p=1-alpha/2) * linear.pred$se
  CI.lin.pred.upper <- linear.pred$fit + qnorm(p=1-alpha/2) * linear.pred$se
  CI.pi.pred.lower <- exp(CI.lin.pred.lower) / (1 + exp(CI.lin.pred.lower))
  CI.pi.pred.upper <- exp(CI.lin.pred.upper) / (1 + exp(CI.lin.pred.upper))
  list(lower = CI.pi.lower, upper=CI.pi.upper)
}
