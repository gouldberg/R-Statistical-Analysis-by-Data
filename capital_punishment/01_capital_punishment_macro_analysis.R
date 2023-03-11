# ------------------------------------------------------------------
# 死刑判断調査：　直交データの分析
# 回答者全体に対する分析
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


# 141 respondings for each profile (combination of V1 to V8) and face attributes
dim(data)



# ----------
table(data$死刑)


# -->
# PLEASE NOTE THAT FOLLOWING MACRO ANALYSIS DOES NOT USE "死刑" LABEL



# ------------------------------------------------------------------
# DOE matrix
# ------------------------------------------------------------------
tmp <- generate_sikei_doe()

sikei_doe <- tmp$sikei_doe
sikei_label <- tmp$sikei_label

sikei_doe
sikei_label


# ----------
# 変数間相関:  check correlations are all zero
library(psych)
pairs.panels(sikei_doe, method="kendall")



# ------------------------------------------------------------------
# 死刑判断への影響が大きい項目は何か？
# 平均相対重要度 (Average Importance of factors (attributes))：　各個人ごとの求めた相対重要度を要因について平均した値
# ------------------------------------------------------------------
tmp <- data[,1:8]
colnames(tmp)


# y:　選好　x: プロファイル  z:　水準ラベル
Conjoint(y=tmp, x=sikei_doe, z=sikei_label)


# Average Importance of factors (attributes)　を確認すると
# 「被害者人数」 = 29.11  が最も大きい



# ------------------------------------------------------------------
# 回答者全体での回帰切片
# 回答者全体の部分効用値 (Part worths (utilities) of levels):  回答者全体での各要因ごとの切片
#
#  --> what is most important factor and level ?
#  --> what is the profile with largest utilities ?
# ------------------------------------------------------------------
# 部分効用値（回答者全体の切片）のみを取りだし、一枚のチャートにまとめる
sikei_util <- caUtilities(y=tmp, x=sikei_doe, z=sikei_label)

sikei_util



# ----------
# partial utility (relative importance) by each factor and level
# partial utility is the interpretable partial coefficients of regression
sikei_util[2:15]
sum(sikei_util[2:15])



# ----------
# partial utility of intercept for all factor and level
sikei_util[1]



# ----------
plot.Conjoint(utility=sikei_util, label=sikei_label, sd=FALSE)
# plot.Conjoint(utility=sikei_util, label=sikei_label, sd=TRUE)


# what is the profile with largest utilities:  male + old + over1 + taiho + planned + zenka + kanjo_ari 
