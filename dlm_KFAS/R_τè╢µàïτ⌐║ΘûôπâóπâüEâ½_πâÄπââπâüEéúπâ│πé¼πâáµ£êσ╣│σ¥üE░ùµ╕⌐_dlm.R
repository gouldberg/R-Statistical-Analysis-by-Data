# 状態空間モデル　by dlm
# ノッティンガム月平均気温
# 周期・季節性のあるローカルレベルモデル
# - 季節要素が時間変化するモデル
# - フーリエ形式の周期性あるモデル（全調和項）
# - フーリエ形式の周期性あるモデル　縮小モデル（高次調和項を除き、2調和項のみ）

setwd("C:\\Users\\R_work")
require(dlm)
source("C:\\Users\\光世\\Desktop\\#Py_R_信号解析_時系列分析\\R_状態空間モデル_サポート関数.R")


###############################################################################
# データ読み込み、基礎俯瞰
###############################################################################
# データ読み込み
data(nottem);  dfData <- nottem;

# 非常に明瞭な周期性がわかる
# それほど高周波数成分はないようだ
par(mfrow=c(2,1))
ts.plot(dfData); ts.plot(log(dfData));

tsData <- dfData

n <- length(tsData)


###############################################################################
# 基礎分析:　自己相関、偏自己相関など
###############################################################################
# 強い綺麗な周期性あり
dev.new();  par(mfrow=c(2,1));  acf(tsData, lag.max=30);  pacf(tsData, lag.max=30);
dev.new();  plot(decompose(tsData, type="additive"))

plot(diff(tsData), type="o", col=c("darkgrey"), xlab="", ylab="Level")
dev.new();  par(mfrow=c(2,1));  acf(diff(tsData), lag.max=30);  pacf(diff(tsData), lag.max=30);
dev.new();  plot(decompose(diff(tsData), type="additive"))


###############################################################################
# モデル1:  季節要素が時間で変化する (nStatevar=12, nHyperparam=2)、システムノイズとして表現
# モデル2:  フーリエ形式の周期モデル
# モデル3:　フーリエ形式の周期モデル　縮小モデル（高次調和項を除き、2調和項のみ）
#
# 高次調和項は、データにおける雑音への適合に用いられ、標本外の1期先予測の適合には汎用性がない
# 以下の例では、フィルタ化推定値での推定誤差の MAPE はモデル2 --> 1 --> 3 の順で改善
# 縮小モデル3 のモデル診断では、残差の自己相関を改善するが、正規性はあやしくなる
# 縮小モデル3 では、予測線が若干滑らかになった
# ###############################################################################
# -----------------------------------------------------------------------------
# モデル1:  季節要素が時間で変化する -->  システムノイズとして表現
nStateVar <- 12;  nHyperParam <- 2;
funModel <- function(parm){
  m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=0)
  m2 <- dlmModSeas(12, dV=0, dW=c(exp(parm[2]), rep(0,10)))
  return( m1 + m2 )
}
oMLE.DLM <- dlmMLE(tsData, parm=log(c(var(tsData), 0.001)), build=funModel, hessian=T)

oMLE.DLM$convergence
oMLE.DLM$par;  exp(oMLE.DLM$par[1]);  exp(oMLE.DLM$par[2]);

oFitted.DLM <- funModel(oMLE.DLM$par)
diag(oFitted.DLM$W)[1];  diag(oFitted.DLM$W)[2];

# -----------------------------------------------------------------------------
# モデル2: フーリエ形式の周期モデル
# nStatevar2 は確認のこと
nStateVar2 <- 12;  nHyperParam2 <- 2;
funModel2 <- function(parm){
  m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=0)
  m2 <- dlmModTrig(s=12, dV=0, dW=exp(parm[2]))
  return( m1 + m2 )
}
oMLE2.DLM <- dlmMLE(tsData, parm=log(c(var(tsData), 0.001)), build=funModel2, hessian=T)

oMLE2.DLM$convergence
oMLE2.DLM$par;  exp(oMLE2.DLM$par[1]);  exp(oMLE2.DLM$par[2]);

oFitted2.DLM <- funModel2(oMLE2.DLM$par)

# -----------------------------------------------------------------------------
# 平滑化し、周期性成分をプロット
# dlmSmooth は、モデルそのものを与えると内部で dlmFilter も呼び出される
# 2番目以降の調和項は、相対的に振幅が小さい
oSmoothed2.DLM <- dlmSmooth(tsData, oFitted2.DLM)
plot(ts(oSmoothed2.DLM$s[2:13, c(1,3,5,7,9,11)], names=paste("S", 1:6, sep="_")), oma.multi=c(2,0,1,0), pch=16, nc=1, yax.flip=TRUE, type="o", xlab="", main="")

# -----------------------------------------------------------------------------
# モデル3: 縮小モデル
# 2番目以降の調和項は、相対的に振幅が小さく、最初の2つの調和項だけをローカルレベルに加えたモデルを試してみる
# q で調和項に残す数を指定する
# nStatevar3 は確認のこと
nStateVar3 <- 12;  nHyperParam3 <- 2;
funModel3 <- function(parm){
  m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=0)
  m2 <- dlmModTrig(s=12, q = 2, dV=0, dW=exp(parm[2]))
  return( m1 + m2 )
}
oMLE3.DLM <- dlmMLE(tsData, parm=log(c(var(tsData), 0.001)), build=funModel3, hessian=T)
oMLE3.DLM$convergence
oMLE3.DLM$par;  exp(oMLE3.DLM$par[1]);  exp(oMLE3.DLM$par[2]);

oFitted3.DLM <- funModel3(oMLE3.DLM$par)

# MAPE　を比較すると、悪化せず、逆にわずかに減少していることがわかる
# 高次調和項を除いたことで、1期先予測能力が若干改善している(はず)
# 高次調和項は、データにおける雑音への適合に用いられ、標本外の1期先予測の適合には汎用性がない
mean(abs(residuals(dlmFilter(tsData, oFitted.DLM), type="raw", sd=FALSE)) / tsData)
mean(abs(residuals(dlmFilter(tsData, oFitted2.DLM), type="raw", sd=FALSE)) / tsData)
mean(abs(residuals(dlmFilter(tsData, oFitted3.DLM), type="raw", sd=FALSE)) / tsData)

# -----------------------------------------------------------------------------
# フィルタリング　および　モデル診断:  モデル1-2はほぼ同じで残差に若干に相関が残る、モデル3は残差の自己相関は改善したが正規性があやしくなった
# モデル診断:　フィルタ化残差になんらかの相関・構造が残っていないか
# 検定:  独立性 / 均一分散性 / 正規性
# nMaxLag:     (integer scalar) max lag (for BoxLjung)
# anACLag:     (integer vector) two number of lag (for ACF)

oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
oFiltered2.DLM <- dlmFilter(tsData, oFitted2.DLM)
oFiltered3.DLM <- dlmFilter(tsData, oFitted3.DLM)

qqnorm(residuals(oFiltered.DLM, sd=FALSE));  qqline(residuals(oFiltered.DLM, sd=FALSE));  dev.new();  tsdiag(oFiltered.DLM);
agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)
sub.ShowDiagnostics(agStdPredErr.DLM, nStateVar, nHyperParam, nMaxLag=20, anACLag=c(1,12))

qqnorm(residuals(oFiltered2.DLM, sd=FALSE));  qqline(residuals(oFiltered2.DLM, sd=FALSE));  dev.new();  tsdiag(oFiltered2.DLM);
agStdPredErr2.DLM  <- residuals(oFiltered2.DLM, type = c("standardized"), sd=FALSE)
lPredErr2.DLM      <- residuals(oFiltered2.DLM, type = c("raw"), sd=TRUE)
sub.ShowDiagnostics(agStdPredErr2.DLM, nStateVar2, nHyperParam2, nMaxLag=20, anACLag=c(1,12))

qqnorm(residuals(oFiltered3.DLM, sd=FALSE));  qqline(residuals(oFiltered3.DLM, sd=FALSE));  dev.new();  tsdiag(oFiltered3.DLM);
agStdPredErr3.DLM  <- residuals(oFiltered3.DLM, type = c("standardized"), sd=FALSE)
lPredErr3.DLM      <- residuals(oFiltered3.DLM, type = c("raw"), sd=TRUE)
sub.ShowDiagnostics(agStdPredErr3.DLM, nStateVar3, nHyperParam3, nMaxLag=20, anACLag=c(1,12))

# -----------------------------------------------------------------------------
# モデルの当てはまりの良さ

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

# モデルの対数尤度　および　AIC
( gLLbyFun3.DLM <- -oMLE3.DLM$value - 0.5*n*log(2*pi) )
sub.AIC(gLLbyFun3.DLM, nStateVar3, nHyperParam3)
sub.AIC(gLLbyFun3.DLM, nStateVar3, nHyperParam3)/n

# 予測誤差？？の対数尤度 および AIC
( gLLbyErr3.DLM <- sub.LogLik(lPredErr.DLM3$res, lPredErr3.DLM$sd^2, nStateVar3) )
sub.AIC(gLLbyFun3.DLM, nStateVar3, nHyperParam3)
sub.AIC(gLLbyFun3.DLM, nStateVar3, nHyperParam3)/n


# -----------------------------------------------------------------------------
# 平滑化
oSmoothed.DLM <- dlmSmooth(oFiltered.DLM)
oSmoothed2.DLM <- dlmSmooth(oFiltered2.DLM)
oSmoothed3.DLM <- dlmSmooth(oFiltered3.DLM)

# -----------------------------------------------------------------------------
# プロット
# 時系列、フィルタ後推定値、平滑化レベルのプロット
par(mfrow=c(1,1))
plot(tsData, type="o", col="darkgrey", main="モデル1: 季節要素が時間で変化")
lines(dropFirst(oFiltered.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

dev.new()
plot(tsData, type="o", col="darkgrey", main="モデル2: フーリエ形式の周期モデル")
lines(dropFirst(oFiltered2.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed2.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered2.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

dev.new()
plot(tsData, type="o", col="darkgrey", main="モデル3: フーリエ形式の周期モデル　縮小モデル")
lines(dropFirst(oFiltered3.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed3.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered3.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")


# 平滑化レベルと95%信頼区間のプロット --> 再考（エラーになる）
# var <- dlmSvd2var(oSmoothed.DLM$U.S, oSmoothed.DLM$D.S)
# hwid <- qnorm(0.025, lower=FALSE) * sqrt(unlist(var))
# smooth <- cbind(oSmoothed.DLM$s, as.vector(oSmoothed.DLM$s)+hwid%o%c(-1,1))
# plot(dropFirst(smooth), plot.type="s", type="l", lty=c(1,5,5), xlab="", ylab="Level", ylim=range(tsData))
# lines(tsData, type="o", col="darkgrey")
# legend("bottomleft", legend=c("data", "smoothed level", "95% probability limits"), col=c("darkgrey", rep("black", 2)), lty=c(1,1,5), pch=c(1,NA,NA), bty="n")

# -----------------------------------------------------------------------------
# 先期間の予測:　モデル3は若干予測線が滑らかになった
oFcst.DLM <- dlmForecast(oFiltered.DLM, nAhead=24, sampleNew=3)
oFcst2.DLM <- dlmForecast(oFiltered2.DLM, nAhead=24, sampleNew=3)
oFcst3.DLM <- dlmForecast(oFiltered3.DLM, nAhead=24, sampleNew=3)

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル1: 季節要素が時間で変化", xlim=c(1920, 1941))
invisible(lapply(oFcst.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル2: フーリエ形式の周期モデル", xlim=c(1920, 1941))
invisible(lapply(oFcst2.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst2.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst2.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered2.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed2.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered2.DLM$f, lty=3, col="red")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル2: フーリエ形式の周期モデル　縮小モデル", xlim=c(1920, 1941))
invisible(lapply(oFcst3.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst3.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst3.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered3.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed3.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered3.DLM$f, lty=3, col="red")
