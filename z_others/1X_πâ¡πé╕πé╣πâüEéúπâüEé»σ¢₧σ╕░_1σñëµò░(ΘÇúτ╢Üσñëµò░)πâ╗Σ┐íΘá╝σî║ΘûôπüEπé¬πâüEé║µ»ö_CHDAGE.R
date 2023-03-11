# ------------------------------------------------------------------------------
# CHDAGEデータ
# 説明変数　AGE のみの1変数
# 目的変数　CHD(Coronary Heart Disease) = 0 or 1　の2値
# ------------------------------------------------------------------------------
setwd("C:\\Users\\光世\\Desktop\\#R_ロジスティック回帰\\")
library(dplyr)

data <- read.csv(".\\data\\CHDAGE\\CHDAGE.txt", sep="\t", header=T)


# ------------------------------------------------------------------------------
# 基礎分析：　scatter plot　など
# ------------------------------------------------------------------------------
plot(CHD ~ AGE, data, xlim=c(20,70), ylim=c(0,1), xlab="Age(years)", ylab="Coronary Heart Diseases")


# 目的変数が 0-1 なので、AGE を8グループに集計し、各AGEグループごとのCHD比率を算出
data <- data %>% mutate(AGEGRP = ifelse(AGE >=60, 8, ifelse(AGE >= 55, 7, ifelse(AGE >= 50, 6, ifelse(AGE >= 45, 5, ifelse(AGE >= 40, 4, ifelse(AGE >= 35, 3, ifelse(AGE >= 30, 2, 1))))))))

data2 <- data %>% dplyr::select(-ID, -AGE) %>% mutate(n = 1) %>% group_by(AGEGRP) %>% summarize_all(funs(sum)) %>% mutate(Absent = n - CHD, Mean = round(CHD / n, 3))
colnames(data2) <- c("AGEGRP", "Present", "n", "Absent", "Mean")
data2 <- data2 %>% dplyr::select(1,3,2,4,5)

plot(Mean~AGEGRP, data2, ylim=c(0,1), xlab="AGEGRP", ylab="Coronary Heart Diseases(Mean)")



# ------------------------------------------------------------------------------
# ロジスティック回帰:　いったん一通りのモデリング
# 目的変数が、CHDか、CHDでないかの2値　二項分布
# ------------------------------------------------------------------------------
result2 <- glm(CHD~AGE, family=binomial, data)

# Null Deviance：　切片モデルの完全モデルからの乖離　　Residual Deviance：　推定モデルの完全モデルからの乖離
# Residual Deviance < Null Deviance　なので回帰係数には意味がありそうだ
summary(result2)


# Nagelkerke によるロジスティック回帰モデルの決定係数
n <- nrow(data)
(1-(exp((result2$deviance-result2$null.deviance)/n)))/(1-exp(-result2$null.deviance/n))


# Deviance のカイ二乗検定
# 従属変数が二項分布に従い、サンプル数が多い場合は、Devianceは（サンプル数-パラメタ数）の自由度をもつ、カイ二乗分布に従う
# 棄却されないので、0.05より十分に大きいので、ロジスティック回帰モデルはよく適合しているといえる
pchisq(deviance(result2), df.residual(result2), lower=FALSE)


# 切片モデルは、棄却されてしまう
pchisq(result2$null.deviance, df.residual(result2)+1, lower=FALSE)


# Deviance の差での検定：　推定モデルが切片モデルより適合しているか？
# 棄却されるので、切片モデルが推定モデルよりも適合している、とは言えない
pchisq(result2$null.deviance-result2$deviance, result2$df.null-result2$df.residual, lower=FALSE)


# 回帰係数の信頼区間: 推定値 ± 1.96 * 標準誤差　の場合
library(broom)
tmp <- tidy(result2)

tmp[2,"estimate"] + 1.96 * tmp[2, "std.error"]
tmp[2,"estimate"] - 1.96 * tmp[2, "std.error"]

# 切片、回帰係数の信頼区間: MASSパッケージの confint()  尤度ベースでの計算であり、サンプル数が小さい場合の精度が高い
library(MASS)
confint(result2)


# 推定されたモデルおよび切片モデルのロジスティック曲線を描画
# ロジット変換する = exp() / ( 1 + exp() )
dev.new()
plot(CHD ~ AGE, data, xlim=c(20,70), ylim=c(0,1), xlab="Age(years)", ylab="Coronary Heart Diseases")
x <- seq(20, 70, 1)
lines(x, exp(tmp[1,"estimate"] + tmp[2,"estimate"] * x) / (1 + exp(tmp[1,"estimate"] + tmp[2,"estimate"] * x)), lty=2, col="blue")


# 推定値
fitted(result2)

library(faraway)
pred <- predict(result2, data=data)
ilogit(pred)  # ロジット変換


# ------------------------------------------------------------------------------
# ロジスティック回帰:　AGE の回帰係数を含めることの　尤度比検定　の詳細
# Deviance の差に基づく検定
# ------------------------------------------------------------------------------
# D = -2 * logLik
deviance(result2)
logLik(result2)

# G = D(model without the variable) - D(model with the variable) = -2 * log( 尤度比 )
n1 <- nrow(data %>% filter(CHD == 1))
n0 <- nrow(data %>% filter(CHD == 0))

lnull <- n1 * log(n1) + n0 * log(n0) - (n0 + n1) * log(n1 + n0)
lmod <- deviance(result2) / (-2)
G <- -2 * (lnull - lmod)
result2$null.deviance-result2$deviance　# これと同じ


# カイ二乗検定：　以下2つは同じ
pchisq(G, 1, lower=FALSE)
pchisq(result2$null.deviance-result2$deviance, result2$df.null-result2$df.residual, lower=FALSE)


# ------------------------------------------------------------------------------
# ロジスティック回帰:　AGE の回帰係数を含めることの　ワルド検定
# ワルド検定は、尤度比検定が棄却する（変数を含める方がよい）場合でも、棄却しない場合があり、尤度比検定の方がお薦め
# ------------------------------------------------------------------------------
library(broom)
tmp2 <- tidy(result2)

W <- tmp2[2,"estimate"] / tmp[2,"std.error"]
tmp2[2,"statistic"]

# 通常はあまり大きな差はない
W^2
G


# ------------------------------------------------------------------------------
# ロジスティック回帰:　信頼区間 = ilogit( 予測値　±  1.96　×　標準誤差 )
# ------------------------------------------------------------------------------
# 推定量の共分散行列
vmat <- vcov(result2)

library(faraway)  #ilogit でロジット変換
xa <- seq(20, 70, 1)

plot(CHD ~ AGE, data, xlim=c(20,70), ylim=c(0,1), xlab="Age(years)", ylab="Coronary Heart Diseases")
lines(x, ilogit(tmp2[1,"estimate"] + tmp2[2,"estimate"] * x), col="blue", lty=2)
lines(x, ilogit(tmp2[1,"estimate"] + tmp2[2,"estimate"] * x + 1.96 * sqrt(vmat[1,1] + xa^2 * vmat[2,2] + 2 * xa * vmat[1,2])), col="red", lty=2)
lines(x, ilogit(tmp2[1,"estimate"] + tmp2[2,"estimate"] * x - 1.96 * sqrt(vmat[1,1] + xa^2 * vmat[2,2] + 2 * xa * vmat[1,2])), col="green", lty=2)


# ------------------------------------------------------------------------------
# オッズ比:　説明変数が連続変数の場合
# とりあえず、logit が線形であると仮定しておく
# ------------------------------------------------------------------------------
# AGEグループを集約したロジット
# AGEGRP 3-6 あたりは線形であることが確認できる
plot(Mean~AGEGRP, data2, ylim=c(0,1), xlab="AGEGRP", ylab="Coronary Heart Diseases(Mean)")


# オリジナルのデータでロジ回帰の係数を確認
result <- glm(CHD ~ AGE, family=binomial, data)
summary(result)
tmp <- tidy(result)


# AGEGRP が3(平均37.5歳) と　6(平均52.5歳)　それぞれでの、CHD == 1 のオッズ　および　オッズ比
odds3 <- nrow(data %>% filter(AGEGRP == 3, CHD == 1)) / nrow(data %>% filter(AGEGRP == 3, CHD == 0))
odds6 <- nrow(data %>% filter(AGEGRP == 6, CHD == 1)) / nrow(data %>% filter(AGEGRP == 6, CHD == 0))
oddsratio <- odds6 / odds3

# AGEGRP 3-6 で年齢が15年離れているので、以下の計算
oddsratio
exp(tmp[2,"estimate"] * 15)
