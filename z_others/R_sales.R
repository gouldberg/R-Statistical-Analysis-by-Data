# 状態空間モデル　by dlm
# アパレル販売データ
# 周期・季節性のあるローカルレベルモデル
# - レベルと季節の攪乱項をゼロに固定
# - レベルと季節要素が時間で変化

setwd("C:\\Users\\R_work")
require(dlm)
source("C:\\Users\\光世\\Desktop\\#Py_R_信号解析_時系列分析\\R_状態空間モデル_サポート関数.R")


###############################################################################
# データ読み込み、基礎俯瞰
###############################################################################
# データ読み込み
dfData <- read.csv("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\sales.csv")
head(dfData)

# 緩やかなレベル変動と強い周期・季節性あり
par(mfrow=c(2,1))
ts.plot(dfData[,2]); ts.plot(log(dfData[,2]));

# ts型にしなくても dlm では扱えるが、ここではしておく
# ts型にすると decompose が使える、plot() でx軸を時間指定することができる
tsData <- dfData[,2]
tsData <- ts(tsData, start=c(2002,1), frequency=12)
plot(tsData)

n <- length(tsData)


###############################################################################
# 基礎分析:　自己相関、偏自己相関など
###############################################################################
# 6ヶ月と12ヶ月の周期性が確認できる
# 周期・季節性が、6ヶ月と12ヶ月の2パターンあることが確認できる
dev.new();  par(mfrow=c(2,1));  acf(tsData, lag.max=30);  pacf(tsData, lag.max=30);
dev.new();  plot(decompose(tsData, type="additive"))

# 一階差分をとって、同様に確認
plot(diff(tsData), type="o", col=c("darkgrey"), xlab="", ylab="Level")
dev.new();  par(mfrow=c(2,1));  acf(diff(tsData), lag.max=30);  pacf(diff(tsData), lag.max=30);
dev.new();  plot(decompose(diff(tsData), type="additive"))


###############################################################################
# モデル1:  レベルと季節の攪乱項はゼロに固定するモデル (nStatevar=12,  nHyperPram=1)
# モデル2:  レベルと季節要素が時間で変化する (nStatevar=12, nHyperparam=3)、システムノイズとして表現
#
# モデル改善には定常AR成分を織り込むとよい
###############################################################################
# -----------------------------------------------------------------------------
# モデル1:  レベルと季節の攪乱項はゼロに固定するモデル
# nStatevar:  状態方程式の数、1本はレベル、11本は季節要素
nStateVar <- 12;  nHyperParam <- 1;
funModel <- function(parm){
  m1 <- dlmModPoly(order = 1, dV=exp(parm), dW=0)
  m2 <- dlmModSeas(12, dV = 0, dW=rep(0,11))
  return( m1 + m2 )
}
oMLE.DLM <- dlmMLE(tsData, parm=log(c(var(tsData))), build=funModel, hessian=T)

oMLE.DLM$convergence
oMLE.DLM$par;  exp(oMLE.DLM$par);

oFitted.DLM <- funModel(oMLE.DLM$par)
drop(oFitted.DLM$V)

# -----------------------------------------------------------------------------
# モデル2:  レベルと季節要素が時間で変化する -->  システムノイズとして表現
nStateVar2 <- 12;  nHyperParam2 <- 3;
funModel2 <- function(parm){
  m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
  m2 <- dlmModSeas(12, dV=0, dW=c(exp(parm[3]), rep(0,10)))
  return( m1 + m2 )
}
oMLE2.DLM <- dlmMLE(tsData, parm=log(c(var(tsData), 0.001, 0.001)), build=funModel2, hessian=T)
oMLE2.DLM$convergence

# システムノイズを設定したが、推定されたノイズは非常に小さい
oMLE2.DLM$par;  exp(oMLE2.DLM$par[1]);  exp(oMLE2.DLM$par[2]);  exp(oMLE2.DLM$par[3]);

oFitted2.DLM <- funModel2(oMLE2.DLM$par)
diag(oFitted2.DLM$W)[1];  diag(oFitted2.DLM$W)[2];

# -----------------------------------------------------------------------------
# フィルタリング　および　モデル診断:  どちらも残差になんらかの相関・構造が残ってしまっている
# モデル診断:　フィルタ化残差になんらかの相関・構造が残っていないか
# 検定:  独立性 / 均一分散性 / 正規性
# nMaxLag:     (integer scalar) max lag (for BoxLjung)
# anACLag:     (integer vector) two number of lag (for ACF)
oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
oFiltered2.DLM <- dlmFilter(tsData, oFitted2.DLM)

qqnorm(residuals(oFiltered.DLM, sd=FALSE));  qqline(residuals(oFiltered.DLM, sd=FALSE));  dev.new();  tsdiag(oFiltered.DLM);
agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)
sub.ShowDiagnostics(agStdPredErr.DLM, nStateVar, nHyperParam, nMaxLag=15, anACLag=c(1,12))

qqnorm(residuals(oFiltered2.DLM, sd=FALSE));  qqline(residuals(oFiltered2.DLM, sd=FALSE));  dev.new();  tsdiag(oFiltered2.DLM);
agStdPredErr2.DLM  <- residuals(oFiltered2.DLM, type = c("standardized"), sd=FALSE)
lPredErr2.DLM      <- residuals(oFiltered2.DLM, type = c("raw"), sd=TRUE)
sub.ShowDiagnostics(agStdPredErr2.DLM, nStateVar2, nHyperParam2, nMaxLag=15, anACLag=c(1,12))


# -----------------------------------------------------------------------------
# モデルの当てはまりの良さ:  AICではむしろモデル1の方がよい、またAICそのものの値が大きすぎる

# モデルの対数尤度　および　AIC
( gLLbyFun.DLM <- -oMLE.DLM$value - 0.5*n*log(2*pi) )
sub.AIC(gLLbyFun.DLM, nStateVar, nHyperParam)
sub.AIC(gLLbyFun.DLM, nStateVar, nHyperParam)/n

# 予測誤差？？の対数尤度 および AIC
( gLLbyErr.DLM <- sub.LogLik(lPredErr.DLM$res, lPredErr.DLM$sd^2, nStateVar) )
sub.AIC(gLLbyFun.DLM, nStateVar, nHyperParam)
sub.AIC(gLLbyFun.DLM, nStateVar, nHyperParam)/n

# モデルの対数尤度　および　AIC
( gLLbyFun2.DLM <- -oMLE2.DLM$value - 0.5*n*log(2*pi) )
sub.AIC(gLLbyFun2.DLM, nStateVar2, nHyperParam2)
sub.AIC(gLLbyFun2.DLM, nStateVar2, nHyperParam2)/n

# 予測誤差？？の対数尤度 および AIC
( gLLbyErr2.DLM <- sub.LogLik(lPredErr.DLM2$res, lPredErr2.DLM$sd^2, nStateVar2) )
sub.AIC(gLLbyFun2.DLM, nStateVar2, nHyperParam2)
sub.AIC(gLLbyFun2.DLM, nStateVar2, nHyperParam2)/n


# -----------------------------------------------------------------------------
# 平滑化
oSmoothed.DLM <- dlmSmooth(oFiltered.DLM)
oSmoothed2.DLM <- dlmSmooth(oFiltered2.DLM)

# 平滑化分布の平均(s)、平滑化分布の分散を特異値分解(SVD)したもの
oSmoothed.DLM$s
oSmoothed2.DLM$s


# -----------------------------------------------------------------------------
# プロット
# 時系列、フィルタ後推定値、平滑化レベルのプロット
par(mfrow=c(1,1))
plot(tsData, type="o", col="darkgrey", main="モデル1: レベルと季節の攪乱項はゼロ固定")
lines(dropFirst(oFiltered.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")
legend("topright", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

dev.new()
plot(tsData, type="o", col="darkgrey", main="モデル2: レベルと季節要素が変動")
lines(dropFirst(oFiltered2.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed2.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered2.DLM$f, lty=3, col="red")
legend("topright", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")


# 平滑化レベルと95%信頼区間のプロット --> 再考（エラーになる）
# var <- dlmSvd2var(oSmoothed.DLM$U.S, oSmoothed.DLM$D.S)
# hwid <- qnorm(0.025, lower=FALSE) * sqrt(unlist(var))
# smooth <- cbind(oSmoothed.DLM$s, as.vector(oSmoothed.DLM$s)+hwid%o%c(-1,1))
# plot(dropFirst(smooth), plot.type="s", type="l", lty=c(1,5,5), xlab="", ylab="Level", ylim=range(tsData))
# lines(tsData, type="o", col="darkgrey")
# legend("bottomleft", legend=c("data", "smoothed level", "95% probability limits"), col=c("darkgrey", rep("black", 2)), lty=c(1,1,5), pch=c(1,NA,NA), bty="n")

# -----------------------------------------------------------------------------
# 先期間の予測
oFcst.DLM <- dlmForecast(oFiltered.DLM, nAhead=24, sampleNew=3)
oFcst2.DLM <- dlmForecast(oFiltered2.DLM, nAhead=24, sampleNew=3)

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル1: レベルと季節の攪乱項はゼロ固定", xlim=c(2002, 2015))
invisible(lapply(oFcst.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル2: レベルと季節要素が変動", xlim=c(2002, 2015))
invisible(lapply(oFcst2.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst2.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst2.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered2.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed2.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered2.DLM$f, lty=3, col="red")
