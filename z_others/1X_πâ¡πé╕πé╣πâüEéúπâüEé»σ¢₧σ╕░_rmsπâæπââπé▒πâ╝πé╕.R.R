# ------------------------------------------------------------------------------
# ロジスティック回帰　rmsパッケージ
# ------------------------------------------------------------------------------
setwd("C:\\Users\\光世\\Desktop\\#R_ロジスティック回帰\\")
library(dplyr)
library(rms)
library(Hmisc)


# ------------------------------------------------------------------------------
# データの準備
# ------------------------------------------------------------------------------
getHdata(sex.age.response)
d <- sex.age.response
head(d)

str(d)

dd <- datadist(d)
options(datadist="dd")


# ------------------------------------------------------------------------------
# 変数の状況確認
# ------------------------------------------------------------------------------
library(psych)

Hmisc::describe(d)
psych::describe(d)

pairs.panels(d)

hist(d$age)

# データを ageで <45, 45-54, >=55 の3分割し、それぞれ平均を代表点とする
( af <- with(d, cut2(age, c(45,55), levels.mean=TRUE)) )

# 各分割セグメントごとに、男女別に respoinse の平均を計算
( prop <- with(d, tapply(response, list(af, sex), mean, na.rm=TRUE)) )


# ------------------------------------------------------------------------------
# 元の生データで、ロジスティック回帰
# ------------------------------------------------------------------------------
f <- lrm(response ~ sex + age, data = d)
f


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
fasr <- f

w <- function(...){
  with(d, {
    m <- sex == "male"
    f <- sex == "female"
    lpoints(age[f], response[f], pch=1)
    lpoints(age[m], response[m], pch=2)
    af <- cut2(age, c(45,55), levels.mean=TRUE)
    prop <- tapply(response, list(af, sex), mean, na.rm=TRUE)
    agem <- as.numeric(row.names(prop))
    lpoints(agem, prop[,"female"], pch=4, cex=1.3, col="green")
    lpoints(agem, prop[,"male"], pch=5, cex=1.3, col="green")
    x <- rep(62, 4);  y <- seq(0.25, 0.1, length=4)
    lpoints(x, y, pch=c(1,2,4,5), col=rep(c("blue", "green"), each=2))
    ltext(x+5, y, c("F Observed", "M Observed", "F Proportion", "M Proportion"), cex=0.8)
    })
}

pred <- Predict(f, age=seq(34,70,length=200), sex, fun=plogis)

plot(pred, ylab="Pr[response]", ylim=c(-0.02, 1.02))
plot(pred, ylab="Pr[response]", ylim=c(-0.02, 1.02), addpanel=w)
