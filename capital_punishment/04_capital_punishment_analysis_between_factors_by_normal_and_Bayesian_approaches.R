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


# extract only a level of factors with positive average utilities
usl <- sub_util[,c(1,2,5,7,8,10,12,14)] 
colnames(usl)


col_name <- c("切片","性別","年齢","被害者人数","逮捕経緯","殺害経緯","前科","処罰感情")



# ------------------------------------------------------------------
# 個人内差の分析:  Basic Analysis
# 要因, 水準の部分効用値の平均, 標準偏差, 相関行列
# ------------------------------------------------------------------
round(apply(usl, MARGIN=2, FUN=mean), digits=3)

round(apply(usl, MARGIN=2, FUN=stats::sd), digits=3)

round(cor(usl),3)



# ------------------------------------------------------------------
# 個人内差の分析:  差得点の効果量
# 
# 個人内で最も部分効用の差が大きい要因は何か？
# 個人内で価値観の差が最も大きくなる要因は何か？
# ------------------------------------------------------------------
diff_effectsize(usl, toubunsan=TRUE, col_name)
diff_effectsize(usl, toubunsan=FALSE, col_name)


# -->
# The largest effect size of difference among 2 utilities is 1.082, which is for "taiho" and "kanjo_ari" 



# ------------------------------------------------------------------
# 個人内差の分析:  差得点の優越率
#
# 個人内で、ある要因・水準の部分効用が、ある他の要因・水準の部分効用の値より大きくなる確率
# ------------------------------------------------------------------
diff_pid(usl, toubunsan=TRUE, col_name)
diff_pid(usl, toubunsan=FALSE, col_name)


# -->
# The largest probability of dominance is 0.860, which is for "higai" and "kanjo_ari" 



# ------------------------------------------------------------------
# 個人内差の分析:  差得点の閾上率
#
# 個人内で、ある要因・水準の部分効用値が、ある他の要因・水準の部分効用値より1.0以上大きくなる確率
# ------------------------------------------------------------------
diff_pic(usl, 1.0, toubunsan=TRUE, col_name)
diff_pic(usl, 1.0, toubunsan=FALSE, col_name)


# -->
# The largest probability of beyond threshold = 1.0 is 0.268, which is for "higai" and "kanjo_ari" 



# ------------------------------------------------------------------
# 個人内差の分析
# Bayesian Appraoch
#
# onlydiff = F:  差得点以外の指標も出力
# cr2:  差得点の閾上率の閾値
# ------------------------------------------------------------------
outEQU <- G2pair(cbind(usl[,"over1"], usl[,"kanjo_ari"]), EQU=1)
outDEF <- G2pair(cbind(usl[,"over1"], usl[,"kanjo_ari"]), EQU=0)

EQU2 <- print(outEQU, onlydiff=F, 3)
EQU3 <- print(outEQU, onlydiff=F, 3, cr2=1)

DEF2 <- print(outDEF, onlydiff=F, 3)
DEF3 <- print(outDEF, onlydiff=F, 3, cr2=1)





