# ------------------------------------------------------------------
# 死刑判断調査：　直交データの分析
# ------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//capital_punishment")
library(dplyr)
library(ggplot2)
library(conjoint)


source(".//utils//generate_sikei_doe.R")
source(".//utils//utilities.R")



# ------------------------------------------------------------------
# データの読み込み
# ------------------------------------------------------------------
data <- read.csv("//media//kswada//MyFiles//data//capital_punishment//capital_punishment.csv", stringsAsFactors=F, header=T, fileEncoding="CP932")

tmp <- data[,1:8]
colnames(tmp)


# ----------
# DOE matrix
tmp2 <- generate_sikei_doe()
sikei_doe <- tmp2$sikei_doe
sikei_label <- tmp2$sikei_label



# ------------------------------------------------------------------
# 回答者個人に対する分析
# 各回答者ごとの部分効用値
# ------------------------------------------------------------------
( sikei_util <- caUtilities(y=tmp, x=sikei_doe, z=sikei_label) )
( sub_util <- caPartUtilities(y=tmp, x=sikei_doe, z=sikei_label) )



# ----------
# ちなみに全回答者の部分効用値を合計すると、回答者全体での部分効用値になる
colMeans(sub_util)
sikei_util



# ----------
# distribution of partial utilities by each respondents
par(mfrow=c(4,4))
for(i in 1:ncol(sub_util)){
  hist(sub_util[,i], xlab="", ylab="", main=paste0(colnames(sub_util)[i]), ylim=c(0,100), breaks=seq(-7,7,by=0.2))
}


# ----------
# 回答者番号を指定すると、plot.conjoint() で部分効用値のチャートを見ることができる
i <- 95
plot.Conjoint(utility=sub_util[i,], label=sikei_label, sd=FALSE)



# ----------
# conjyo.sim<-function(data,design,label,num=3){
#   x <- colMeans(caPartUtilities(data,design,label))
#   ss <- select.list(names(x),graphics=T,multiple= T)
#   print(round(x[rownames=ss],num))
#   print("総効用値")
#   print(round(sum(x[rownames=ss]),num))
# }

# conjyo.sim(data=tmp, design=sikei_doe, label=sikei_label)



# ------------------------------------------------------------------
# 個人差 vs. 全体傾向
# 死刑判断に与える影響は、個人差の影響が大きいか？　（各一つ一つの水準よりも個人差）
# ------------------------------------------------------------------
# 切片の標準偏差：　回答者集団における個人差の大きさ
( sd <- sd(sub_util[,"intercept"]) )
# sd <- apply(sub_util, MARGIN=2, FUN=sd)



# ----------
# 個人差が大きな影響があるのかの確認
# 部分効用値に対する個人差の相対的な大きさ
# いずれの部分効用値よりも、個人差が大きいことから、死刑判断において一つ一つの水準よりも、個人による信念や信条といった内的基準の方が死刑判断に大きな影響を与える傾向がある

# The standard deviation of intercept for all respondings are  1.22 > 1.0, that's why the value of this in bar chart (<= 1) is over 1 
par(mfrow=c(1,1))
plot.Conjoint(utility=sikei_util, label=sikei_label, sd=T)



# ------------------------------------------------------------------
# 回答者個人に対する分析
# 各個人ごとの各要因に対する相対的重要度: どの要因がどの程度重要視されているかを、水準数の違いに関係なく表す指標
# 例えば、すべてのプロファイルに対して「どちらともいえない」の回答だと、NAになる
# ------------------------------------------------------------------
( output <- sub.imp(data=tmp, design=sikei_doe, label=sikei_label) )


# ------
# check sum of all relative importance by each responding
apply(output, MARGIN=1, FUN=sum)



# ------------------------------------------------------------------
# Simulation for specified case (profile) and respondent 
#
# Specify respondent id by i
# Specify the factor and level of the interested case
# ------------------------------------------------------------------

# specify the respondent
i <- 65

x <- colMeans(caPartUtilities(tmp[i,], sikei_doe, sikei_label))


# select the factor and level of the interested case
ss <- select.list(names(x), graphics=T, multiple= T)


# total utility
print(round(x[rownames=ss], 3))
print(paste0("Total Utility : ", round(sum(x[rownames=ss]),3)))

