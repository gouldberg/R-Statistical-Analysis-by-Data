# ------------------------------------------------------------------
# 死刑判断調査：　直交データの分析
# ------------------------------------------------------------------
setwd("//media//kswada//MyFiles//R//capital_punishment")
library(dplyr)
library(ggplot2)
library(conjoint)


source(".//utils//generate_sikei_doe.R")
source(".//utils//utilities.R")
source(".//stan//rstan_func.R")



# ------------------------------------------------------------------
# データの読み込み
# ------------------------------------------------------------------
data <- read.csv("//media//kswada//MyFiles//data//capital_punishment//capital_punishment_2.csv", stringsAsFactors=F, header=T, fileEncoding="CP932")


tmp <- data[,1:8]
colnames(tmp)


# ----------
# DOE matrix
tmp2 <- generate_sikei_doe()
sikei_doe <- tmp2$sikei_doe
sikei_label <- tmp2$sikei_label



# ------------------------------------------------------------------
# data by each respondent
#  - Partial utility by each factor, level and interecept
#  - pos/neg for capital punishment
# ------------------------------------------------------------------
sub_util <- caPartUtilities(y=tmp, x=sikei_doe, z=sikei_label)
tmp <- cbind(as.data.frame(sub_util), sikei=data[,"死刑"]) %>% filter(sikei %in% c("賛成", "反対"))



# ------------------------------------------------------------------
# 集団差の分析:  効果量 (effect size)
#
# 群のバラツキも考慮した差の指標（平均値差が標準偏差の何倍か）
# ------------------------------------------------------------------
# 賛成と反対ごとの、部分効用値（切片）の分布
ggplot(tmp, aes(x=intercept)) + geom_histogram(fill="white", colour="black") + facet_grid(sikei~.) + theme_bw()

ggplot(tmp, aes(x=intercept, fill=sikei, colour=sikei)) + geom_histogram(position="identity", alpha=0.8) + theme_bw() +
  scale_colour_manual(values = c(賛成 = "tomato",　反対 = "darkgreen"))



# ----------
# 賛成と反対のそれぞれの群の平均および平均値の差
( df_mean <- aggregate(. ~ sikei, data = tmp, FUN=mean)[,2:16] )
df_mean[3,] <-  df_mean[1,] - df_mean[2,]
row.names(df_mean) <- c("mean_賛成", "mean_反対", "dif_mean")

# ( df <- by(tmp %>% dplyr::select(-sikei), tmp[,"sikei"], summary) )



# ----------
# 賛成と反対のそれぞれの群の standard deviation and common standard deviation
( df_sd <- aggregate(. ~ sikei, data = tmp, FUN=stats::sd)[,2:16] )
( df_sd_common <- apply(tmp[,2:16], MARGIN=2, FUN=stats::sd) )
df_sd <- rbind(df_sd, df_sd_common)
row.names(df_sd) <- c("sd_賛成", "sd_反対", "sd_common")

output <- round(rbind(df_mean, df_sd), digits=3)



# ----------
# 標準偏差が賛成と反対で異なるため、効果量 = 平均値 ÷ 標準偏差　を算出
# 効果量でみた場合、賛成では被害者人数複数が、反対で逮捕経歴逮捕　が最も重要
x1 <- tmp %>% filter(sikei=="賛成") %>% dplyr::select(intercept, male, old, over1, taiho, planned, zenka, kanjo_ari)
x2 <- tmp %>% filter(sikei=="反対") %>% dplyr::select(intercept, male, old, over1, taiho, planned, zenka, kanjo_ari)

col_name <- c("切片","性別","年齢","被害者人数","逮捕経緯","殺害経緯","前科","処罰感情")


tmp0 <- effectsize(x1, x2, toubunsan=TRUE, col_name = col_name)
tmp1 <- effectsize(x1, x2, toubunsan=FALSE, col_name = col_name)

output2 <- rbind(tmp0, tmp1)



# ----------
# mean, difference of mean, standard deviation, and effect size
output
output2

# --> "taiho" has largest effect size: 0.749


# ----------
# ベイズ推定(stan)
# 標準偏差が共通の場合と異なる場合
outEQU <- G2Ind(x1[,"taiho"], x2[,"taiho"], EQU=1, prior=F)
outDEF <- G2Ind(x1[,"taiho"], x2[,"taiho"], EQU=0, prior=F)

outEQU2 <- print(outEQU, 3)
outDEF2 <- print(outDEF, 3)

outEQU2$Gc
outDEF2$Gc

# -->
# "taiho"'s EAP is 0.551 (if standard deviasion is that of group1)
# "taiho"'s EAP is 0.712 (if standard deviasion is that of group2)
# ALSO, CREDIBLE INTERVAL IS AVAILABLE



# ------------------------------------------------------------------
# 集団差の分析:  非重複度 (Cohen's U3, third measure of nonoverlap)
# 
# 第1群の平均値が、第2群の何%点に相当するかを示す指標
# 非重複度が 0.5 から離れ 1.0 に近づくほど2群の平均値差が大きいと解釈
#
# 平均値の差が群によって感じ方が異なるか？
# 死刑に賛成でも、情状酌量の余地ありとする要因は何か？
# ------------------------------------------------------------------
# 共通した標準偏差
cohen_u3(x1, x2, toubunsan=TRUE, col_name = col_name)


# 異なる標準偏差
cohen_u3(x1, x2, toubunsan=FALSE, col_name = col_name)


# --->
# U31:  group 2's nonoverlap against group 1 --> taiho 0.773
# U32:  group 1's nonoverlap against group 2 --> taiho 0.712
# 死刑に賛成する人は、被告人への厳しい罰を望みながら、「逮捕経歴」が自首である場合は、死刑に反対する人より、情状酌量の余地ありと考えている



# ----------
# ベイズ推定(stan)
outEQU2 <- print(outEQU, 3)
outDEF2 <- print(outDEF, 3)

outEQU2$Gc
outDEF2$Gc

# -->
# "taiho"'s EAP is 0.754 (if standard deviasion is that of group1)
# "taiho"'s EAP is 0.706 (if standard deviasion is that of group2)
# ALSO, CREDIBLE INTERVAL IS AVAILABLE


outEQU2 <- print(outEQU, 3, pr1=0.60)
outDEF2 <- print(outDEF, 3, pr1=0.60)

outEQU2$Uc
outDEF2$Uc



# ------------------------------------------------------------------
# 集団差の分析:  優越率 (probability of dominance)
# 
# ある要因水準に関する部分効用値を比較した際に、属性内の一方の群に所属する回答者の部分効用値が、
# もう一方の群に所属する回答者の部分効用値を上回る確率
# （回答者を属性（群）別に無作為に選んだ場合）
#
# 属性ごとの観測値の違いを検討
#  - 要因「逮捕経歴」の水準が「逮捕」であることによって、
#    「死刑にしたい程度」に対してこの要因が対応する部分効用値がが正の方向に大きくなる確率は、
#    死刑制度に賛成する回答者は、死刑制度に反対する回答者と比べてどの程度大きくなるか？
# ------------------------------------------------------------------
# 2群の標準偏差が等しい場合の優越率
pid(x1, x2, toubunsan=TRUE, col_name)


# 2群の標準偏差が等しいという仮定を置かない場合の優越率
pid(x1, x2, toubunsan=FALSE, col_name)


# --> 67.3%


# ----------
# ベイズ推定(stan)
outEQU2 <- print(outEQU, 3)
outDEF2 <- print(outDEF, 3)

outEQU2$Gc
outDEF2$Gc


# -->
# "taiho"'s EAP is 0.665

outEQU2 <- print(outEQU, 3, pr2=0.7)
outDEF2 <- print(outDEF, 3, pr2=0.7)

outEQU2$Uc
outDEF2$Uc



# ------------------------------------------------------------------
# 集団差の分析:  閾上率 (probability beyond threshold)  -->  優越率の拡張
# 
# ある要因水準に関する部分効用値を比較した際に、属性内の一方の群に所属する回答者の部分効用値が、
# もう一方の群に所属する回答者の部分効用値を、[閾値cよりも] 上回る確率
# （回答者を属性（群）別に無作為に選んだ場合）
# ------------------------------------------------------------------
# 2群の標準偏差が等しい場合の閾上率
pic(x1, x2, c=0.5, toubunsan=TRUE, col_name)


# 2群の標準偏差が等しいという仮定を置かない場合の閾上率
pic(x1, x2, c=0.5, toubunsan=FALSE, col_name)


pic(x1, x2, c=0.25, toubunsan=FALSE, col_name)
pic(x1, x2, c=0.50, toubunsan=FALSE, col_name)
pic(x1, x2, c=0.75, toubunsan=FALSE, col_name)



# ----------
# ベイズ推定(stan)
outEQU2 <- print(outEQU, 3, cr2=0.5)
outDEF2 <- print(outDEF, 3, cr2=0.5)

outEQU2$Uc
outDEF2$Uc

