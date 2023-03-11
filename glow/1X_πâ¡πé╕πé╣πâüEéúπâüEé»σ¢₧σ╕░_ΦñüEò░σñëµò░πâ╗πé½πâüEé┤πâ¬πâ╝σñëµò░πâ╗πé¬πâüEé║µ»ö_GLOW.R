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
# ロジスティック回帰: いったんこれまでの先行研究から重要と思われる変数のみ
# ------------------------------------------------------------------------------
data$RATERISK <- as.factor(data$RATERISK)
result2 <- glm(FRACTURE ~ AGE + WEIGHT + PRIORFRAC + PREMENO + RATERISK, family=binomial, data)

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


# ------------------------------------------------------------------------------
# クロス集計　および　オッズ比:　説明変数が2値の場合
# ------------------------------------------------------------------------------
result <- glm(FRACTURE ~ PRIORFRAC, family=binomial, data)
summary(result)
tmp <- tidy(result)

data <- data %>% mutate(n = 1)
addmargins(xtabs(n ~ FRACTURE + PRIORFRAC, data=data))

# PRIORFRAC が0 と　1　それぞれでの、FRACTURE == 1 のオッズ　および　オッズ比
odds0 <- nrow(data %>% filter(PRIORFRAC == 0, FRACTURE == 1)) / nrow(data %>% filter(PRIORFRAC == 0, FRACTURE == 0))
odds1 <- nrow(data %>% filter(PRIORFRAC == 1, FRACTURE == 1)) / nrow(data %>% filter(PRIORFRAC == 1, FRACTURE == 0))
oddsratio <- odds1 / odds0

# exp( FRACTURE ~ PRIORFRAC のロジ回帰係数 ) = オッズ比
# FRACTURE ~ PRIORFRAC のロジ回帰係数 = log(オッズ比)
oddsratio
exp(tmp[2,"estimate"])

log(oddsratio)
tmp[2,"estimate"]


# ------------------------------------------------------------------------------
# クロス集計　および　オッズ比:　説明変数が3値の場合
# ------------------------------------------------------------------------------
result <- glm(FRACTURE ~ RATERISK, family=binomial, data)
summary(result)
tmp <- tidy(result)

data <- data %>% mutate(n = 1)
addmargins(xtabs(n ~ FRACTURE + RATERISK, data=data))

# RATERISK が1 と　2　それぞれでの、FRACTURE == 1 のオッズ　および　オッズ比
odds1 <- nrow(data %>% filter(RATERISK == 1, FRACTURE == 1)) / nrow(data %>% filter(RATERISK == 1, FRACTURE == 0))
odds2 <- nrow(data %>% filter(RATERISK == 2, FRACTURE == 1)) / nrow(data %>% filter(RATERISK == 2, FRACTURE == 0))
oddsratio <- odds2 / odds1
oddsratio
exp(tmp[2,"estimate"])

# RATERISK が1 と　3　それぞれでの、FRACTURE == 1 のオッズ　および　オッズ比
odds1 <- nrow(data %>% filter(RATERISK == 1, FRACTURE == 1)) / nrow(data %>% filter(RATERISK == 1, FRACTURE == 0))
odds3 <- nrow(data %>% filter(RATERISK == 3, FRACTURE == 1)) / nrow(data %>% filter(RATERISK == 3, FRACTURE == 0))
oddsratio <- odds3 / odds1
oddsratio
exp(tmp[3,"estimate"])
