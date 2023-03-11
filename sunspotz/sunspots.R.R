# 状態空間モデル　by dlm
# 太陽黒点数
# 周期・季節性あるローカルレベルモデル
# - 周期・季節要素が時間で変化するモデル
# - フーリエ形式の周期モデル（全調和項）
# - 一般周期成分モデル
# - 一般周期成分モデル：パラメータを正しく推定した場合

setwd("C:\\Users\\R_work")
require(dlm)
source("C:\\Users\\光世\\Desktop\\#Py_R_信号解析_時系列分析\\R_状態空間モデル_サポート関数.R")


###############################################################################
# データ読み込み、基礎俯瞰
###############################################################################
# データ読み込み
data(sunspots);  dfData <- sunspots;

# カウントデータであり、平方根の時系列も確認
par(mfrow=c(2,1))
ts.plot(dfData); ts.plot(sqrt(dfData));

# 平方根変換したデータを扱うこととする
tsData <- sqrt(dfData)

n <- length(tsData)


###############################################################################
# 基礎分析:　自己相関、偏自己相関など
###############################################################################
# 高周波が含まれており、生データでは分析が難しい
dev.new();  par(mfrow=c(2,1));  acf(tsData, lag.max=30);  pacf(tsData, lag.max=30);
dev.new();  plot(decompose(tsData, type="additive"))

# 1階差分をとると、自己相関がわかりやすくなる
plot(diff(tsData), type="o", col=c("darkgrey"), xlab="", ylab="Level")
dev.new();  par(mfrow=c(2,1));  acf(diff(tsData), lag.max=30);  pacf(diff(tsData), lag.max=30);
dev.new();  plot(decompose(diff(tsData), type="additive"))


###############################################################################
# モデル1:  季節要素が時間で変化する (nStatevar=12, nHyperparam=2)、システムノイズとして表現
# モデル2:  フーリエ形式の周期モデル
# モデル3:　一般周期成分モデル:　観測値に内在する過程の周期が、連続する観測時点間の整数倍でない場合
# モデル4:　一般周期成分モデル　パラメータが正しい場合
#
# 一般周期成分モデルは複雑な高周波データからレベル（シグナル）を抽出するために使える
# 人間が目で見てわかるが、高周波成分の多いデータからのレベル抽出・予測は、フーリエ形式の周期モデルでは対応できない
# 一般周期成分モデルでは、多少パラメータの推定がうまくいかずフィルタ化・平滑化後もノイズを含んでいても、予測は正しいパラメータの場合と似た波形を出す
# 一般周期成分モデルのパラメータ推定が課題（正しいパラメータがわからない）
# ###############################################################################
# -----------------------------------------------------------------------------
# モデル1:  季節要素が時間で変化する -->  システムノイズとして表現
nStateVar <- 12;  nHyperParam <- 2;
funModel <- function(parm){
  m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=0)
  m2 <- dlmModSeas(12, dV=0, dW=c(exp(parm[2]), rep(0,10)))
  return( m1 + m2 )
}
oMLE.DLM <- dlmMLE(tsData, parm=log(c(var(tsData), 0.1)), build=funModel, hessian=T)

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
oMLE2.DLM <- dlmMLE(tsData, parm=log(c(var(tsData), 0.1)), build=funModel2, hessian=T)

oMLE2.DLM$convergence
oMLE2.DLM$par;  exp(oMLE2.DLM$par[1]);  exp(oMLE2.DLM$par[2]);

oFitted2.DLM <- funModel2(oMLE2.DLM$par)

# -----------------------------------------------------------------------------
# 平滑化し、周期性成分をプロット
# 高次の調和項も低次と同じ程度の値のレンジであり、縮小モデルは検討できない
oSmoothed2.DLM <- dlmSmooth(tsData, oFitted2.DLM)
plot(ts(oSmoothed2.DLM$s[2:13, c(1,3,5,7,9,11)], names=paste("S", 1:6, sep="_")), oma.multi=c(2,0,1,0), pch=16, nc=1, yax.flip=TRUE, type="o", xlab="", main="")

# -----------------------------------------------------------------------------
# モデル3: 一般周期成分モデル
# モデル4: うちパラメータを正しく推定したもの
# 一般的に q=2: 周期成分2つ（調和項2つ分）　とし、周期 tau をパラメータとする
# nStatevar3 は確認のこと
nStateVar3 <- 4;  nHyperParam3 <- 5;
funModel3 <- function(parm){
  m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
  m2 <- dlmModTrig(q=2, tau=exp(parm[3]), dV=0, dW=rep(c(exp(parm[4]), exp(parm[5])),each=2))
  return( m1 + m2 )
}

# パラメータの最尤推定 -->　どうも正解に近いパラメータが推定されない・・・
# tau に対する初期値は、11年周期を考慮し近い値に設定する
oMLE3.DLM <- dlmMLE(tsData, parm=log(c(var(tsData), 0.1, 12*11, 10, 0.1)), build=funModel3, hessian=T)
oMLE3.DLM$convergence
oMLE3.DLM$par;  exp(oMLE3.DLM$par[1]);  exp(oMLE3.DLM$par[2]);  exp(oMLE3.DLM$par[3]);  exp(oMLE3.DLM$par[4]);  exp(oMLE3.DLM$par[5]);

oFitted3.DLM <- funModel3(oMLE3.DLM$par)

# 本当はこちらが正解らしい
nStateVar4 <- 4;  nHyperParam4 <- 5;
oFitted4.DLM <- dlmModTrig(q=2, tau=130.51, dV=0, dW=rep(c(1765e-2, 3102e-4), each=2)) + dlmModPoly(1, dV=0.7452, dW=0.1606)

# MAPE　はすべてInfとなってしまう
mean(abs(residuals(dlmFilter(tsData, oFitted.DLM), type="raw", sd=FALSE)) / tsData)
mean(abs(residuals(dlmFilter(tsData, oFitted2.DLM), type="raw", sd=FALSE)) / tsData)
mean(abs(residuals(dlmFilter(tsData, oFitted3.DLM), type="raw", sd=FALSE)) / tsData)
mean(abs(residuals(dlmFilter(tsData, oFitted4.DLM), type="raw", sd=FALSE)) / tsData)

# -----------------------------------------------------------------------------
# フィルタリング　および　モデル診断:  モデル4が最もよい
# モデル診断:　フィルタ化残差になんらかの相関・構造が残っていないか
# 検定:  独立性 / 均一分散性 / 正規性
# nMaxLag:     (integer scalar) max lag (for BoxLjung)
# anACLag:     (integer vector) two number of lag (for ACF)

oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
oFiltered2.DLM <- dlmFilter(tsData, oFitted2.DLM)
oFiltered3.DLM <- dlmFilter(tsData, oFitted3.DLM)
oFiltered4.DLM <- dlmFilter(tsData, oFitted4.DLM)

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

qqnorm(residuals(oFiltered4.DLM, sd=FALSE));  qqline(residuals(oFiltered4.DLM, sd=FALSE));  dev.new();  tsdiag(oFiltered4.DLM);
agStdPredErr4.DLM  <- residuals(oFiltered4.DLM, type = c("standardized"), sd=FALSE)
lPredErr4.DLM      <- residuals(oFiltered4.DLM, type = c("raw"), sd=TRUE)
sub.ShowDiagnostics(agStdPredErr4.DLM, nStateVar4, nHyperParam4, nMaxLag=20, anACLag=c(1,12))

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

# モデルの対数尤度　および　AIC
( gLLbyFun4.DLM <- -oMLE4.DLM$value - 0.5*n*log(2*pi) )
sub.AIC(gLLbyFun4.DLM, nStateVar4, nHyperParam4)
sub.AIC(gLLbyFun4.DLM, nStateVar4, nHyperParam4)/n

# 予測誤差？？の対数尤度 および AIC
( gLLbyErr4.DLM <- sub.LogLik(lPredErr.DLM4$res, lPredErr4.DLM$sd^2, nStateVar4) )
sub.AIC(gLLbyFun4.DLM, nStateVar4, nHyperParam4)
sub.AIC(gLLbyFun4.DLM, nStateVar4, nHyperParam4)/n


# -----------------------------------------------------------------------------
# 平滑化
oSmoothed.DLM <- dlmSmooth(oFiltered.DLM)
oSmoothed2.DLM <- dlmSmooth(oFiltered2.DLM)
oSmoothed3.DLM <- dlmSmooth(oFiltered3.DLM)
oSmoothed4.DLM <- dlmSmooth(oFiltered4.DLM)


# -----------------------------------------------------------------------------
# 元データ、レベル(フィルタ化推定値)、一般的な確率周期成分　のプロット --> モデル3はレベル変動がデータに引きずられて大きい
# レベルは状態ベクトルの5番目、周期成分は2つの調和項（1番目と3番目）を加える
tmp3 <- tcrossprod(dropFirst(oSmoothed3.DLM$s[,c(1,3,5)]), matrix(c(0,0,1,1,1,0), nr=2, byrow=TRUE))
y3 <- cbind(tsData, tmp3)
colnames(y3) <- c("Sunspots", "Level", "Periodic")
plot(y3, yax.flip=TRUE, oma.multi=c(2,0,1,0))

dev.new()
tmp4 <- tcrossprod(dropFirst(oSmoothed4.DLM$s[,c(1,3,5)]), matrix(c(0,0,1,1,1,0), nr=2, byrow=TRUE))
y4 <- cbind(tsData, tmp4)
colnames(y4) <- c("Sunspots", "Level", "Periodic")
plot(y4, yax.flip=TRUE, oma.multi=c(2,0,1,0))

# -----------------------------------------------------------------------------
# プロット
# 時系列、フィルタ後推定値、平滑化レベルのプロット
# 一般周期成分モデルは、フィルタ推定値、平滑化後値が比較的滑らか
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

# モデル3,4 は5列目(最後)がフィルタ後推定値
dev.new()
plot(tsData, type="o", col="darkgrey", main="モデル3: 一般周期成分モデル")
lines(dropFirst(oFiltered3.DLM$m[,5]), lty=1, col="black")
lines(dropFirst(oSmoothed3.DLM$s[,5]), lty=2, col="blue")
lines(oFiltered3.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

# モデル3,4 は5列目(最後)がフィルタ後推定値
dev.new()
plot(tsData, type="o", col="darkgrey", main="モデル4: 一般周期成分モデル　正しいパラメータ")
lines(dropFirst(oFiltered4.DLM$m[,5]), lty=1, col="black")
lines(dropFirst(oSmoothed4.DLM$s[,5]), lty=2, col="blue")
lines(oFiltered4.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")


# 平滑化レベルと95%信頼区間のプロット --> 再考（エラーになる）
# var <- dlmSvd2var(oSmoothed.DLM$U.S, oSmoothed.DLM$D.S)
# hwid <- qnorm(0.025, lower=FALSE) * sqrt(unlist(var))
# smooth <- cbind(oSmoothed.DLM$s, as.vector(oSmoothed.DLM$s)+hwid%o%c(-1,1))
# plot(dropFirst(smooth), plot.type="s", type="l", lty=c(1,5,5), xlab="", ylab="Level", ylim=range(tsData))
# lines(tsData, type="o", col="darkgrey")
# legend("bottomleft", legend=c("data", "smoothed level", "95% probability limits"), col=c("darkgrey", rep("black", 2)), lty=c(1,1,5), pch=c(1,NA,NA), bty="n")

# -----------------------------------------------------------------------------
# 先期間の予測:　フーリエ形式の周期モデルでは予測はまったく×、一般周期成分モデルは高周波の中でも周期性をうまくとらえている
oFcst.DLM <- dlmForecast(oFiltered.DLM, nAhead=60, sampleNew=3)
oFcst2.DLM <- dlmForecast(oFiltered2.DLM, nAhead=60, sampleNew=3)
oFcst3.DLM <- dlmForecast(oFiltered3.DLM, nAhead=60, sampleNew=3)
oFcst4.DLM <- dlmForecast(oFiltered4.DLM, nAhead=60, sampleNew=3)

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル1: 季節要素が時間で変化", xlim=c(1749, 1988))
invisible(lapply(oFcst.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル2: フーリエ形式の周期モデル", xlim=c(1920, 1988))
invisible(lapply(oFcst2.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst2.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst2.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered2.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed2.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered2.DLM$f, lty=3, col="red")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル3: 一般周期成分モデル", xlim=c(1920, 1988))
invisible(lapply(oFcst3.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst3.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst3.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered3.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed3.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered3.DLM$f, lty=3, col="red")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル4: 一般周期成分モデル　正しいパラメータ", xlim=c(1920, 1988))
invisible(lapply(oFcst4.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst4.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst4.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered4.DLM$m[,5]), lty=1, col="black")
lines(dropFirst(oSmoothed4.DLM$s[,5]), lty=2, col="blue")
lines(oFiltered4.DLM$f, lty=3, col="red")
