# ------------------------------------------------------------------------------
# ロジスティック回帰　rmsパッケージ
# タイタニック号の生存者
# 通常のロジスティック回帰　＋　欠損値補完後のモデルとの比較
# ------------------------------------------------------------------------------
setwd("C:\\Users\\光世\\Desktop\\#R_ロジスティック回帰\\")
library(dplyr)
library(rms)
library(Hmisc)


# ------------------------------------------------------------------------------
# データ読み込み　および　基礎統計量
# ------------------------------------------------------------------------------
getHdata(titanic3)

v <- c("pclass", "survived", "age", "sex", "sibsp", "parch")
t3 <- titanic3[,v]

units(t3$age) <- "years"
describe(t3)

dd <- datadist(t3);  options(datadist="dd")


# ------------------------------------------------------------------------------
# Univariable summaries
# 各変数ごとの生存率（目的変数）を確認
# 目的変数に対して重要変数のあたりをつける
# ------------------------------------------------------------------------------
s <- summary(survived ~ age + sex + pclass + cut2(sibsp, 0:3) + cut(parch, 0:3), data = t3)
s

# sex, pclass が大きく生存率に影響している
plot(s, main="", subtitles=FALSE)


# ------------------------------------------------------------------------------
# Univariable から　複数変数組み合わせでの生存率確認
# 同じ sex, pclass だが、agec, sibsp, parch の分類で生存率が異なるか　の確認
# age と sibsp, parch, sex, pclass との組み合わせ
# ------------------------------------------------------------------------------
# 目的変数への影響がないカテゴリーは再編、小さなカテゴリーは統合
tn <- transform(t3,
  agec = ifelse(age < 21, "child", "adult"),
  sibsp = ifelse(sibsp == 0, "no sib/sp", "sib/sp"),
  parch = ifelse(parch == 0, "no par/child", "par/child")
)


# 25人以下のカテゴリーは NA にしてしまう、その他は 平均を計算
g <- function(y) if(length(y) < 25) NA else mean(y)


# 各カテゴリーの組み合わせごとの、目的変数（生存率）を計算
s <- with(tn, summarize(survived, llist(agec, sex, pclass, sibsp, parch), g))


# 同じ sex, pclass だが、agec, sibsp * parch の分類で、生存率が異なるかを確認
ggplot(subset(s, agec != "NA"), aes(x=survived, y=pclass, shape=sex)) +
  geom_point() + facet_grid(agec ~ sibsp * parch) +
  xlab("Proportion Surviving") + ylab("Passenger Class") +
  scale_x_continuous(breaks=c(0, 0.5, 1))


# age と 生存率の関係
# age と sex, pclass　の生存率との関係
# 男性と女性では、年齢と生存率の関係がかなり異なる
b <- scale_size_discrete(range = c(0.1, 0.85))
yl <- ylab(NULL)
p1 <- ggplot(t3, aes(x=age, y=survived)) + histSpikeg(survived ~ age, lowess=TRUE, data=t3) + ylim(0,1) + yl
p2 <- ggplot(t3, aes(x=age, y=survived, color=sex)) + histSpikeg(survived ~ age + sex, lowess=TRUE, data=t3) + ylim(0,1) + yl
p3 <- ggplot(t3, aes(x=age, y=survived, size=pclass)) + histSpikeg(survived ~ age + pclass, lowess=TRUE, data=t3) + b + ylim(0,1) + yl
p4 <- ggplot(t3, aes(x=age, y=survived, color=sex, size=pclass)) + histSpikeg(survived ~ age + sex + pclass, lowess=TRUE, data=t3) + b + ylim(0,1) + yl
gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)


# age と sibsp, parch の生存率との関係
# confidence bands なしの lowess smoother
top <- theme(legend.position = "top")
p1 <- ggplot(t3, aes(x=age, y=survived, color=cut2(sibsp, 0:2))) + stat_plsmo() + b + ylim(0,1) + yl + top + scale_color_discrete(name="siblings/spouses")
p2 <- ggplot(t3, aes(x=age, y=survived, color=cut2(parch, 0:2))) + stat_plsmo() + b + ylim(0,1) + yl + top + scale_color_discrete(name="parents/children")
gridExtra::grid.arrange(p1, p2, ncol=2)


# ------------------------------------------------------------------------------
# モデル構築
# 各変数ごとの効果を描画
# ------------------------------------------------------------------------------
# age の 3-way interaction を含むモデル --> non significant
f1 <- lrm(survived ~ sex * pclass * rcs(age, 5) + rcs(age, 5) * (sibsp + parch), data = t3)
( anova(f1) )


#
f <- lrm(survived ~ sex + pclass + rcs(age, 5)^2 + rcs(age, 5) * sibsp, data = t3)
print(f)
( anova(f) )


# sibsp=0 とした際の、生存率および信頼区間を算出、描画
# ggplot(p)　だけで描画してくれる !!!
# age がx軸、sex, pclass がそれぞれウィンドウ、線で分かれる
( p <- Predict(f, age, sex, pclass, sibsp=0, fun=plogis) )
ggplot(p)

# sibsp がx軸、age で線が分かれる
( p2 <- Predict(f, sibsp, age=c(10,15,20,50), conf.int=FALSE) )
ggplot(p2)


# ------------------------------------------------------------------------------
# ブートストラップで、各指標（Discrimination Index, Rank Discrimination Index）の平均を算出
# バイアス補正もしてくれる
# ------------------------------------------------------------------------------
f <- update(f, x=TRUE, y=TRUE)
set.seed(131)
validate(f, B=200)


# ------------------------------------------------------------------------------
# calibration curve を確認　-->　オーバーフィッティングの度合いはあまり大きくないように見える
# Bootstrap overfitting-corrected loess nonparametric calibration curve
# ------------------------------------------------------------------------------
( cal <- calibrate(f, B=200) )
plot(cal, subtitles=FALSE)


# ------------------------------------------------------------------------------
# 欠損値の発生について、類似度行列を作成: naclus()
# 欠損値比率の大きい変数の特定: naplot()
# ------------------------------------------------------------------------------
na.patterns <- naclus(titanic3)
head(na.patterns)

# 各レコードごとの欠損値数
na.patterns$na.per.obs

# 各変数ごとの欠損値の比率
naplot(na.patterns, "na per var")

# 欠損値発生についての類似度行列
na.patterns$sim


# ------------------------------------------------------------------------------
# age の欠損値の発生メカニズムを探る
# -- 決定木:  強いパターンを見つける
# -- 各変数ごとの欠損比率
# ------------------------------------------------------------------------------
library(rpart)
who.na <- rpart(is.na(age) ~ sex + pclass + survived + sibsp + parch, data = titanic3, minbucket=15)
who.na
plot(who.na, margin=0.1);  text(who.na);

# 各変数ごとの欠損比率
plot(summary(is.na(age) ~ sex + pclass + survived + sibsp + parch, data=t3))


# ------------------------------------------------------------------------------
# 欠損値の発生確率を、ロジスティック回帰でモデリング
# ------------------------------------------------------------------------------
m <- lrm(is.na(age) ~ sex * pclass + survived + sibsp + parch, data = t3)
print(m)

anova(m)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
set.seed(17)

# n.impute:
# nk: number of knots
mi <- aregImpute(~ age + sex + pclass + sibsp + parch + survived, data=t3, n.impute=20, nk=4, pr=FALSE)

# 補完された値を確認
head(mi$imputed$age)

# 補完された値と、実際の値
plot(mi)
Ecdf(t3$age, add=TRUE, col="gray", lwd=2, subtitles=FALSE)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
f.mi <- fit.mult.impute(
  survived ~ (sex + pclass + rcs(age, 5))^2 + rcs(age, 5) * sibsp,
  lrm, mi, data=t3, pr=FALSE)

anova(f.mi)

p1 <- Predict(f, age, pclass, sex, sibsp=0, fun=plogis)
p2 <- Predict(f.mi, age, pclass, sex, sibsp=0, fun=plogis)
p <- rbind("Casewise Deletion"=p1, "Multiple Imputation"=p2)
ggplot(p, groups="sex", ylab="probability of Surviving")


s <- summary(f.mi, age=c(1,30), sibsp=0:1)
plot(s, log=TRUE, main="")


combos <- expand.grid(age=c(2, 21, 50), sex=levels(t3$sex), pclass=levels(t3$pclass), sibsp=0)
phat <- Predict(f.mi, age=c(2, 21, 50), sex, pclass, sibsp=0, fun=plogis)$yhat
data.frame(combos, phat)

( pred.logit <- Function(f.mi) )
