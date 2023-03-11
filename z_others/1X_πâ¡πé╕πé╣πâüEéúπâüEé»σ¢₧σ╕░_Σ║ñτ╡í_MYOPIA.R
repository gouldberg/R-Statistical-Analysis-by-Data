# ------------------------------------------------------------------------------
# MYOPIAデータ
# 説明変数　複数
# 目的変数　MYOPIC (myopia within the first 5 years of follow up) = 0 or 1
# ------------------------------------------------------------------------------
setwd("C:\\Users\\光世\\Desktop\\#R_ロジスティック回帰\\")
library(dplyr)

data <- read.csv(".\\data\\MYOPIA\\MYOPIA.txt", sep="\t", header=T)


# ------------------------------------------------------------------------------
# 基礎分析
# ------------------------------------------------------------------------------
par(mfrow=c(4,5))
plot(MYOPIC ~ ., data, ylim=c(0,1))


# ------------------------------------------------------------------------------
# ロジスティック回帰
# 1変数、2変数、2変数＋交互作用
# ------------------------------------------------------------------------------
mod1 <- glm(MYOPIC ~ GENDER, family=binomial, data)
mod2 <- glm(MYOPIC ~ GENDER + SPHEQ, family=binomial, data)
mod3 <- glm(MYOPIC ~ GENDER * SPHEQ, family=binomial, data)

summary(mod1)
summary(mod2)
summary(mod3)

tmp1 <- cbind(tidy(mod1), confint(mod1))
tmp2 <- cbind(tidy(mod2), confint(mod2))
tmp3 <- cbind(tidy(mod3), confint(mod3))


# ------------------------------------------------------------------------------
# 変数 SPHEQ が　変数 GENDER と confounder の関係にあるかどうかの判定　（confounder であれば変数に含めたい）
# delta-beta-hat-percent
# confounder だと、SPHEQ の平均が GENDER によってあまり大きく変わらなくても、回帰係数の違いで GENDER の回帰係数を変えてしまう -->　変数として含めた方がよい
# ------------------------------------------------------------------------------
# SPHEQ を変数に含めると、GENDERの回帰係数は、34%も変化する（大きくなる）
dh <- ( tmp1[2,"estimate"] - tmp2[2,"estimate"] ) / tmp2[2, "estimate"]

# GENDER によって、SPHEQ の平均は大きくは変わらない
( s_mean <- aggregate(SPHEQ ~ GENDER, data=data, FUN=mean) )

# しかし、SPHEQの回帰係数が大きな値であり、SPHEQを含めると、GENDER の回帰係数が変わってしまう
tmp1[2,"estimate"]
tmp2[2,"estimate"] + tmp2[3,"estimate"] * ( s_mean[2,"SPHEQ"] - s_mean[1,"SPHEQ"] )



# ------------------------------------------------------------------------------
# Deviance の差での検定：　mod1 が mod2 より適合しているか？
# 棄却されるので、mod1 が mod2 よりも適合している、とは言えない
# ------------------------------------------------------------------------------
pchisq(mod1$deviance-mod2$deviance, mod1$df.residual-mod2$df.residual, lower=FALSE)
