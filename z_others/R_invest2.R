# 状態空間モデル　by dlm
# スペイン年間投資額

setwd("C:\\Users\\R_work")
require(dlm)
source("C:\\Users\\光世\\Desktop\\#Py_R_信号解析_時系列分析\\R_状態空間モデル_サポート関数.R")


###############################################################################
# データ読み込み、基礎俯瞰
###############################################################################
dfData <- read.table("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_statespace_petris\\invest2.dat")

# レベルに関して概ね直線的に増加もしくは減少、傾きは数年ごとに変化
par(mfrow=c(2,1))
ts.plot(dfData[,2]); ts.plot(log(dfData[,2]));

tsData <- ts(dfData[,2], start=c(1960), frequency=1)

n <- length(tsData)


###############################################################################
# 基礎分析:　自己相関、偏自己相関など
###############################################################################
dev.new();  par(mfrow=c(2,1));  acf(tsData, lag.max=30);  pacf(tsData, lag.max=30);

plot(diff(tsData), type="o", col=c("darkgrey"), xlab="", ylab="Level")
dev.new();  par(mfrow=c(2,1));  acf(diff(tsData), lag.max=30);  pacf(diff(tsData), lag.max=30);


###############################################################################
# モデル1:  ローカル線形トレンドモデル (nStatevar=2,  nHyperPram=3)
# モデル2:  和分ランダムウォークモデル (nStatevar=2,  nHyperPram=2)
# モデル3:  ローカル線形トレンドモデル　MCMC近似によるパラメータ推定
###############################################################################
# -----------------------------------------------------------------------------
# モデル1:　ローカル線形トレンドモデル
# nStatevar:  状態方程式の数
nStateVar <- 2;  nHyperParam <- 3;
funModel <- function(parm){ dlmModPoly(order=2, dV=exp(parm[1]), dW=exp(parm[2:3])) }
oMLE.DLM <- dlmMLE(tsData, parm=log(c(var(tsData), var(tsData), var(tsData))), build=funModel, hessian=T)

# 観測ノイズが小さく、システムノイズがかなり大きい
oMLE.DLM$convergence
oMLE.DLM$par;  exp(oMLE.DLM$par[1]);  exp(oMLE.DLM$par[2]);  exp(oMLE.DLM$par[3]);

exp(oMLE.DLM$par[2]) / exp(oMLE.DLM$par[1])

oFitted.DLM <- funModel(oMLE.DLM$par)
drop(oFitted.DLM$V);  drop(oFitted.DLM$W);

# -----------------------------------------------------------------------------
# モデル2:  和分ランダムウォークモデル -- Wのパラメータが一つ少ない
nStateVar2 <- 2;  nHyperParam2 <- 2;
funModel2 <- function(parm){ dlmModPoly(order=2, dV=exp(parm[1]), dW=c(0,exp(parm[2]))) }
oMLE2.DLM <- dlmMLE(tsData, parm=log(c(var(tsData), var(tsData))), build=funModel2, hessian=T)

oMLE2.DLM$convergence
oMLE2.DLM$par;  exp(oMLE2.DLM$par[1]);  exp(oMLE2.DLM$par[2]);

exp(oMLE2.DLM$par[2]) / exp(oMLE2.DLM$par[1])

oFitted2.DLM <- funModel2(oMLE2.DLM$par)
drop(oFitted2.DLM$V);  drop(oFitted2.DLM$W);

# -----------------------------------------------------------------------------
# モデル3:  ローカル線形トレンドモデル　MCMC近似によるパラメータ推定
# 元の時系列を 1/1000　にする　（でないとパラメータの移動エルゴード平均が収束しない）
tsData2 <- tsData/1000

nStateVar3 <- 2;  nHyperParam3 <- 4;

set.seed(5672)
n.sample <- 12000
# 観測ノイズ、システムノイズの精度に対する事前分布は、(平均 a.y 分散 b.y) (平均 a.theta  分散 b.theta) を持つ独立なガンマ分布
# thin=1 で、24,000本のサンプルを生成し、1つおきに出力
gibbsOut <- dlmGibbsDIG(tsData2, mod = dlmModPoly(2), a.y=1, b.y=1000, a.theta=10, b.theta=1000, n.sample=n.sample, thin=1, save.states=FALSE)

# 最初の 2000回はバーンイン期間
burnin <- 2000;  use <- n.sample - burnin;  from <- 0.05 * use
at <- pretty(c(0, use), n=3);  at <- at[at>=from];

# 各パラメータの移動エルゴード平均、推定値の自己相関をプロット
# 収束には達しているようだが、分散の自己相関はそれほど早く減衰していない
v <- gibbsOut$dV[-(1:burnin)];  w1 <- gibbsOut$dW[-(1:burnin),1];  w2 <- gibbsOut$dW[-(1:burnin),2];

par(mfrow=c(2,3))
plot(ergMean(v, from), type="l", xaxt="n", xlab="", ylab="", main="V: 移動エルゴード平均")
axis(1, at=at-from, labels=format(at))
plot(ergMean(w1, from), type="l", xaxt="n", xlab="", ylab="", main="W11: 移動エルゴード平均")
axis(1, at=at-from, labels=format(at))
plot(ergMean(w2, from), type="l", xaxt="n", xlab="", ylab="", main="W22: 移動エルゴード平均")
axis(1, at=at-from, labels=format(at))
acf(v);  acf(w1);  acf(w2);

# V, W の事後平均を推定、標準偏差とともに出力
mcmcMean(cbind(gibbsOut$dV[-(1:burnin)], gibbsOut$dW[-(1:burnin),]))
v_mean <- mcmcMean(v);  w1_mean <- mcmcMean(w1);  w2_mean <- mcmcMean(w2);

# パラメータ同士の散布図すると、モデルの候補がわかる
# W1, W2 が同時にゼロになることはない、V, W1 は同時にゼロになることがある
# V=0 / W1=0 / V=W1=0 / W2=0 の4つのモデル候補となる
par(mfrow=c(1,3))
plot(v, w1);  plot(v, w2);  plot(w1, w2);

# mcmcMean の値を用いて、モデル3を設定する
# ただし、tsData が 1/1000 にされていることに注意 !!!
oFitted3.DLM <- dlmModPoly(order=2, dV=v_mean[1]*1000, dW=c(w1_mean[1]*1000, w2_mean[1]*1000))

# -----------------------------------------------------------------------------
# フィルタリング　および　モデル診断 --> どのモデルも診断は問題なし
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
sub.ShowDiagnostics(agStdPredErr.DLM, nStateVar, nHyperParam, nMaxLag=15, anACLag=c(1,12))

qqnorm(residuals(oFiltered2.DLM, sd=FALSE));  qqline(residuals(oFiltered2.DLM, sd=FALSE));  dev.new();  tsdiag(oFiltered2.DLM);
agStdPredErr2.DLM  <- residuals(oFiltered2.DLM, type = c("standardized"), sd=FALSE)
lPredErr2.DLM      <- residuals(oFiltered2.DLM, type = c("raw"), sd=TRUE)
sub.ShowDiagnostics(agStdPredErr2.DLM, nStateVar2, nHyperParam2, nMaxLag=15, anACLag=c(1,12))

qqnorm(residuals(oFiltered3.DLM, sd=FALSE));  qqline(residuals(oFiltered3.DLM, sd=FALSE));  dev.new();  tsdiag(oFiltered3.DLM);
agStdPredErr3.DLM  <- residuals(oFiltered3.DLM, type = c("standardized"), sd=FALSE)
lPredErr3.DLM      <- residuals(oFiltered3.DLM, type = c("raw"), sd=TRUE)
sub.ShowDiagnostics(agStdPredErr3.DLM, nStateVar3, nHyperParam3, nMaxLag=15, anACLag=c(1,12))

# -----------------------------------------------------------------------------
# モデル評価:  MAD / MSE / MAPE / Theil-U
# Theil-U:  モデルのMSEと自明な「変化のない」モデル（次の観測値が現在の観測値と同じであると予測する）のMSEを比較する

# MAD
mean(abs(oFiltered.DLM$f - tsData));  mean(abs(oFiltered2.DLM$f - tsData));  mean(abs(oFiltered3.DLM$f - tsData));
# MSE
mean((oFiltered.DLM$f - tsData)^2);  mean((oFiltered2.DLM$f - tsData)^2);  mean((oFiltered3.DLM$f - tsData)^2);
# MAPE
mean(abs(oFiltered.DLM$f - tsData) / tsData);  mean(abs(oFiltered2.DLM$f - tsData) / tsData);  mean(abs(oFiltered3.DLM$f - tsData) / tsData);
# Theil-U: 両モデルとも変化のないモデルよりは性能がよく、MSEの平方根が7%減少
sqrt(sum((oFiltered.DLM$f - tsData)[-(1:5)]^2) / sum(diff(tsData[-(1:4)])^2))
sqrt(sum((oFiltered2.DLM$f - tsData)[-(1:5)]^2) / sum(diff(tsData[-(1:4)])^2))
sqrt(sum((oFiltered3.DLM$f - tsData)[-(1:5)]^2) / sum(diff(tsData[-(1:4)])^2))

# -----------------------------------------------------------------------------
# モデルの当てはまりの良さ -->　わずかにモデル2の方がよい

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
( gLLbyErr2.DLM <- sub.LogLik(lPredErr2.DLM$res, lPredErr2.DLM$sd^2, nStateVar2) )
sub.AIC(gLLbyFun2.DLM, nStateVar2, nHyperParam2)
sub.AIC(gLLbyFun2.DLM, nStateVar2, nHyperParam2)/n

# -----------------------------------------------------------------------------
# 平滑化
oSmoothed.DLM <- dlmSmooth(oFiltered.DLM)
oSmoothed2.DLM <- dlmSmooth(oFiltered2.DLM)
oSmoothed3.DLM <- dlmSmooth(oFiltered3.DLM)

# -----------------------------------------------------------------------------
# 時系列、フィルタ後推定値、平滑化レベルのプロット
# フィルタ化推定値、平滑化後値ともに、元の時系列にほぼ合致してしまう・・・

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", main="モデル1: ローカル線形トレンドモデル")
lines(dropFirst(oFiltered.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", main="モデル2: 和分ランダムウォークモデル")
lines(dropFirst(oFiltered2.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed2.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered2.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", main="モデル3: ローカル線形トレンドモデル MCMC近似によるパラメータ推定")
lines(dropFirst(oFiltered3.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed3.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered3.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

# -----------------------------------------------------------------------------
# 先期間の予測
oFcst.DLM <- dlmForecast(oFiltered.DLM, nAhead=5, sampleNew=3)
oFcst2.DLM <- dlmForecast(oFiltered2.DLM, nAhead=5, sampleNew=3)
oFcst3.DLM <- dlmForecast(oFiltered3.DLM, nAhead=5, sampleNew=3)

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル1: ローカル線形トレンドモデル", xlim=c(1960, 2005), ylim=c(0,22000))
invisible(lapply(oFcst.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル2: 和分ランダムウォークモデル", xlim=c(1960, 2005), ylim=c(0,22000))
invisible(lapply(oFcst2.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst2.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst2.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered2.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed2.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered2.DLM$f, lty=3, col="red")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル3: ローカル線形トレンドモデル MCMC近似によるパラメータ推定", xlim=c(1960, 2005), ylim=c(0,22000))
invisible(lapply(oFcst3.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst3.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst3.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered3.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed3.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered3.DLM$f, lty=3, col="red")
