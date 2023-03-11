# ------------------------------------------------------------------------------
# ロジスティック回帰　rmsパッケージ
# ------------------------------------------------------------------------------
setwd("C:\\Users\\光世\\Desktop\\#R_ロジスティック回帰\\")
library(dplyr)
library(rms)
library(Hmisc)

# ------------------------------------------------------------------------------
# データ変換　および　欠損補完
# ------------------------------------------------------------------------------
getHdata(prostate)

summary(prostate)
datadist(prostate)

prostate <- within(prostate, {
  levels(ekg)[levels(ekg) %in% c("old MI", "recent MI")] <- "MI"
  ekg.norm <- 1 * (ekg %in% c("normal", "benign"))
  levels(ekg) <- abbreviate(levels(ekg))
  pfn <- as.numeric(pf)
  levels(pf) <- levels(pf)[c(1,2,3,3)]
  cvd <- status %in% c("dead - heart or vascular", "dead - cerebrovascular")
  rxn = as.numeric(rx)
  }
)

unique(prostate$status)

# Use transcan to compute optimal pre-transformations
ptrans <- transcan(~ sz + sg + ap + sbp + dbp + age + wt + hg + ekg + pf + bm + hx + dtime + rx, imputed = TRUE, transformed = TRUE, data = prostate, pl=FALSE, pr=FALSE)

# Use transcan single imputations
imp <- impute(ptrans, data=prostate, list.out=TRUE)


NAvars <- all.vars(~ sz + sg + age + wt + ekg)
prostate[["sz"]]
for(x in NAvars) prostate[[x]] <- imp[[x]]
prostate[["sz"]]

subset <- prostate$status %in% c("dead - heart or vascular", "dead - cerebrovascular", "dead - prostatic ca")
trans <- ptrans$transformed[subset,]
psub <- prostate[subset,]


# ------------------------------------------------------------------------------
# データ圧縮効果の確認：　主成分でどこまで予測精度あるか？
# ------------------------------------------------------------------------------
sum(prostate$status %in% c("dead - heart or vascular", "dead - cerebrovascular"))

# 15:1 rule of thumb で、主成分数をまず8としてみる
pcn <- round(sum(prostate$status %in% c("dead - heart or vascular", "dead - cerebrovascular")) / 15, 0)

# 最初の k 主成分を取得する関数
ipc <- function(x, k=1, ...) princomp(x, ..., cor=TRUE)$scores[,1:k]

# 最初の 8 主成分でロジスティック回帰
pc8 <- ipc(~sz + sg + log(ap) + sbp + dbp + age + wt + hg + ekg.norm + pfn + bm + hx + rxn + dtime, data=psub, k=pcn)
f8 <- lrm(cvd ~ pc8, data=psub)

pc8t <- ipc(trans, k=pcn)
f8t <- lrm(cvd ~ pc8t, data=psub)

# オリジナルの変数でロジスティック回帰
f <- lrm(cvd ~ sz + sg + log(ap) + sbp + dbp + age + wt + hg + ekg + pf + bm + hx + rx + dtime, data=psub)

# スプライン
g <- lrm(cvd ~ rcs(sz,4) + rcs(sg,4) + rcs(log(ap),4) + rcs(sbp,4) + rcs(dbp,4) + rcs(age,4) + rcs(wt,4) + rcs(hg,4) + ekg + pf + bm + hx + rx + rcs(dtime,4), data=psub)

# 変換後の変数を使う
h <- lrm(cvd ~ trans, data=psub)

# AICで比較 -- 小さいほど良い
c(f8=AIC(f8), f8t=AIC(f8t), f=AIC(f), g=AIC(g), h=AIC(h))


# ------------------------------------------------------------------------------
# Wald-ANOVA table
# 重要度でのランキング
# ------------------------------------------------------------------------------
print(f)
( an <- anova(f) )
plot(an)


# ------------------------------------------------------------------------------
# 汎化性能？
# van Houwelingen and leCessie's heuristic shrinkage estimate
# ------------------------------------------------------------------------------
# モデル f の、新しいデータに対する精度は、現在学習しているデータでの精度の 85%程度　（15%悪化する）
f$stats
( gamma.hat <- ( f$stats["Model L.R."] - f$stats["d.f."] ) / f$stats["Model L.R."] )



dd <- datadist(psub);  options(datadist="dd");
ggplot(Predict(f), sepdiscrete="vertical", vnames="names", rdata=psub, histSpike.opts=list(frac=function(f) .1*f/max(f)))


# ------------------------------------------------------------------------------
# オッズ比の信頼区間
# 連続変数：　Interquartile-range オッズ比
# カテゴリー変数：　通常のオッズ比
# ------------------------------------------------------------------------------
summary(f)

# オッズ比(90%, 95%, 99%信頼区間)　ただし軸は対数オッズ比
plot(summary(f), log=TRUE)


# ------------------------------------------------------------------------------
# 変数選択： Fast Backward Step-Down
# ------------------------------------------------------------------------------
fastbw(f)

fred <- lrm(cvd ~ sz + log(ap) + age + hx, data=psub)
fred


# ------------------------------------------------------------------------------
# ノモグラム
# ------------------------------------------------------------------------------
nom <- nomogram(fred, ap=c(0.1, 0.5, 1, 5, 10, 50), fun=plogis, funlabel="Probability", fun.at=c(.01, .05, .1, .25, .5, .75, .9, .95, .99))
plot(nom, xfrac=.45)


# ------------------------------------------------------------------------------
# 変数選択：　
# ------------------------------------------------------------------------------
f <- update(f, x=TRUE, y=TRUE)
v <- validate(f, B=200, bw=TRUE)


# ------------------------------------------------------------------------------
# 変数選択：　
# ------------------------------------------------------------------------------
cal <- calibrate(f, B=200, bw=TRUE)
plot(cal)
