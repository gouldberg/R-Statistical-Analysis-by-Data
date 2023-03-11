# ------------------------------------------------------------------------------
# GLOWデータ (The Global Longitudianl Study of osteoporosis in Women)
# 説明変数　複数
# 目的変数　Any fracture in first year = 0 or 1　の2値
# ------------------------------------------------------------------------------
setwd("C:\\Users\\光世\\Desktop\\#R_ロジスティック回帰\\")
library(dplyr)

data <- read.csv(".\\data\\GLOW\\GLOW500.txt", sep="\t", header=T)


# ------------------------------------------------------------------------------
# 基礎分析
# ------------------------------------------------------------------------------
par(mfrow=c(4,4))
plot(FRACTURE ~ ., data, ylim=c(0,1))


# ------------------------------------------------------------------------------
# ロジスティック回帰
# 1変数、2変数、2変数＋交互作用
# ------------------------------------------------------------------------------
mod1 <- glm(FRACTURE ~ PRIORFRAC, family=binomial, data)
mod2 <- glm(FRACTURE ~ PRIORFRAC + HEIGHT, family=binomial, data)
mod3 <- glm(FRACTURE ~ PRIORFRAC * HEIGHT, family=binomial, data)

summary(mod1)
summary(mod2)
summary(mod3)

tmp1 <- cbind(tidy(mod1), confint(mod1))
tmp2 <- cbind(tidy(mod2), confint(mod2))
tmp3 <- cbind(tidy(mod3), confint(mod3))


# ------------------------------------------------------------------------------
# 変数 HEIGHT が　変数 PRIORFRAC と confounder の関係にあるかどうかの判定　（confounder であれば変数に含めたい）
# delta-beta-hat-percent
# confounder でなければ、HEIGHT の平均は、PRIORFRAC の値によって大きく変わらない -->　HEIGHT は特に変数に含めなくてもよいのでは
# HEIGHT がないモデルのPRIORFRAC の回帰係数　＝　HEIGHTがあるモデルのPRIORFRACの回帰係数　＋　HEIGHTがあるモデルのHEIGHTの回帰係数　*  PRIORFRACによるHEIGHTの平均の差
# ------------------------------------------------------------------------------
# HEIGHT を変数に含めても、PRIORFRACの回帰係数は、5%しか変化しない　（20%以上であれば confounderの可能性）
# よって、HEIGHT の平均は、PRIORFRAC によって大きく変わらないはず
dh <- ( tmp1[2,"estimate"] - tmp2[2,"estimate"] ) / tmp2[2, "estimate"]

( h_mean <- aggregate(HEIGHT ~ PRIORFRAC, data=data, FUN=mean) )

tmp1[2,"estimate"]
tmp2[2,"estimate"] + tmp2[3,"estimate"] * ( h_mean[2,"HEIGHT"] - h_mean[1,"HEIGHT"] )



# ------------------------------------------------------------------------------
# ロジスティック回帰:　今度は AGE を変数とする  -->　交互作用が有意
# 1変数、2変数、2変数＋交互作用
# ------------------------------------------------------------------------------
mod1 <- glm(FRACTURE ~ PRIORFRAC, family=binomial, data)
mod2 <- glm(FRACTURE ~ PRIORFRAC + AGE, family=binomial, data)
mod3 <- glm(FRACTURE ~ PRIORFRAC * AGE, family=binomial, data)

summary(mod1)
summary(mod2)
summary(mod3)

tmp1 <- cbind(tidy(mod1), confint(mod1))
tmp2 <- cbind(tidy(mod2), confint(mod2))
tmp3 <- cbind(tidy(mod3), confint(mod3))


# delta-beta-hat-percent が 20%超であり、AGE は PRIORFRAC とは confounder であり、回帰係数を変える
dh <- ( tmp1[2,"estimate"] - tmp2[2,"estimate"] ) / tmp2[2, "estimate"]
( a_mean <- aggregate(AGE ~ PRIORFRAC, data=data, FUN=mean) )
tmp1[2,"estimate"]
tmp2[2,"estimate"] + tmp2[3,"estimate"] * ( a_mean[2,"AGE"] - a_mean[1,"AGE"] )
