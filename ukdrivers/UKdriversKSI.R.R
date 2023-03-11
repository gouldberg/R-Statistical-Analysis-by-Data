# 状態空間モデル　by dlm
# 英国ドライバー死傷者数
# 周期・季節性のあるローカルレベルモデル　＋　干渉・説明変数
# - レベルと季節の攪乱項をゼロに固定
# - レベルと季節要素が時間で変化
# - レベルが時間で変化 + 季節要素あり（攪乱項はゼロ固定）+ 説明変数（対数石油価格） + 干渉変数（英国シートベルト法によるレベルシフト）

setwd("C:\\Users\\R_work")
require(dlm)
source("C:\\Users\\光世\\Desktop\\#Py_R_信号解析_時系列分析\\R_状態空間モデル_サポート関数.R")


###############################################################################
# データ読み込み、基礎俯瞰
###############################################################################
dfData <- read.table("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\OxCodeIntroStateSpaceBook\\Chapter_4\\UKdriversKSI.txt", skip=1)

# 周期性、レベルシフトが見られる
par(mfrow=c(2,1))
ts.plot(dfData); ts.plot(log(dfData));

# 対数変換したデータを使う (平方根でもよいように思うが）
tsData <- ts(log(dfData), frequency=12, start=c(1969, 1))
ts.plot(tsData)

n <- length(tsData)

# 対数石油価格とシートベルト法適用 ("1")
dfX1Data <- read.table("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\OxCodeIntroStateSpaceBook\\Chapter_7\\logUKpetrolprice.txt", skip=1)
ts.plot(dfX1Data)

x2 <- rep(0, n);  x2[170:n] <- 1;
X <- ts(cbind(dfX1Data[,1], x2), frequency=12, start=c(1969, 1))
colnames(X) <- c("x1", "x2")

par(mfrow=c(3,1));  plot(tsData);  plot(X[,1]);  plot(X[,2]);

###############################################################################
# 基礎分析:　自己相関、偏自己相関など
###############################################################################
# 強い周期性あり
# decompose の trend でレベルシフトも見られる
dev.new();  par(mfrow=c(2,1));  acf(tsData, lag.max=30);  pacf(tsData, lag.max=30);
dev.new();  plot(decompose(tsData, type="additive"))

# 一階差分をとっても、12ヶ月周期性のサイクルあり
plot(diff(tsData), type="o", col=c("darkgrey"), xlab="", ylab="Level")
dev.new();  par(mfrow=c(2,1));  acf(diff(tsData), lag.max=30);  pacf(diff(tsData), lag.max=30);
dev.new();  plot(decompose(diff(tsData), type="additive"))


###############################################################################
# モデル1:  レベルと季節の攪乱項はゼロに固定するモデル (nStatevar=12,  nHyperPram=1)
# モデル2:  レベルと季節要素が時間で変化する (nStatevar=12, nHyperparam=3)、システムノイズとして表現
# モデル3:  レベルが時間で変化 + 季節要素あり（攪乱項はゼロ固定）+ 説明変数（対数石油価格） + 干渉変数（英国シートベルト法によるレベルシフト）
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

# 観測ノイズ dV　は小さい
oMLE.DLM$convergence
oMLE.DLM$par;  exp(oMLE.DLM$par);

oFitted.DLM <- funModel(oMLE.DLM$par)

# -----------------------------------------------------------------------------
# モデル2:  レベルと季節要素が時間で変化する -->  システムノイズとして表現
nStateVar2 <- 12;  nHyperParam2 <- 3;
funModel2 <- function(parm){
  m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
  m2 <- dlmModSeas(12, dV=0, dW=c(exp(parm[3]), rep(0,10)))
  return( m1 + m2 )
}
oMLE2.DLM <- dlmMLE(tsData, parm=log(c(var(tsData), 0.001, 0.001)), build=funModel2, hessian=T)

# 観測ノイズもシステムノイズに吸収され、さらに小さくなった
# システムノイズもかなり小さい、W/V比もそこそこ
oMLE2.DLM$convergence
oMLE2.DLM$par;  exp(oMLE2.DLM$par[1]);  exp(oMLE2.DLM$par[2]);  exp(oMLE2.DLM$par[3]);
exp(oMLE2.DLM$par[2]) / exp(oMLE2.DLM$par[1])

oFitted2.DLM <- funModel2(oMLE2.DLM$par)
diag(oFitted2.DLM$W)

# -----------------------------------------------------------------------------
# モデル3:  レベルが時間で変化 + 季節要素あり（攪乱項はゼロ固定）+ 説明変数（対数石油価格） + 干渉変数（英国シートベルト法によるレベルシフト）
# 石油価格が高くなると道路を走る車の数が減り、死傷者数が減少する、という仮説
# 1983年2月に英国にシートベルト法が導入された
# 説明変数、干渉変数の数だけ、状態方程式が増える
nStateVar3 <- 14;  nHyperParam3 <- 2;
funModel3 <- function(parm){
  m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
  m2 <- dlmModSeas(12, dV=0, dW=rep(0,11))
  m3 <- dlmModReg(X, dV=0, addInt=FALSE)
  return( m1 + m2 + m3 )
}
oMLE3.DLM <- dlmMLE(tsData, parm=log(c(var(tsData), 0.001)), build=funModel3, hessian=T)

oMLE3.DLM$convergence
oMLE3.DLM$par;  exp(oMLE3.DLM$par[1]);  exp(oMLE3.DLM$par[2])
exp(oMLE3.DLM$par[2]) / exp(oMLE3.DLM$par[1])

oFitted3.DLM <- funModel3(oMLE3.DLM$par)
diag(oFitted3.DLM$W)

# -----------------------------------------------------------------------------
# フィルタリング　および　モデル診断:  モデル2,3がよい
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
# モデルの当てはまりの良さ:  モデル2が最もよい

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

# モデルの対数尤度　および　AIC
( gLLbyFun3.DLM <- -oMLE3.DLM$value - 0.5*n*log(2*pi) )
sub.AIC(gLLbyFun3.DLM, nStateVar3, nHyperParam3)
sub.AIC(gLLbyFun3.DLM, nStateVar3, nHyperParam3)/n

# 予測誤差？？の対数尤度 および AIC
( gLLbyErr3.DLM <- sub.LogLik(lPredErr3.DLM$res, lPredErr3.DLM$sd^2, nStateVar3) )
sub.AIC(gLLbyFun3.DLM, nStateVar3, nHyperParam3)
sub.AIC(gLLbyFun3.DLM, nStateVar3, nHyperParam3)/n


# -----------------------------------------------------------------------------
# 平滑化
oSmoothed.DLM <- dlmSmooth(oFiltered.DLM)
oSmoothed2.DLM <- dlmSmooth(oFiltered2.DLM)
oSmoothed3.DLM <- dlmSmooth(oFiltered3.DLM)

# 平滑化分布の平均(s)、平滑化分布の分散を特異値分解(SVD)したもの
oSmoothed.DLM$s
oSmoothed2.DLM$s
oSmoothed3.DLM$s


# -----------------------------------------------------------------------------
# プロット
# 時系列、フィルタ後推定値、平滑化レベルのプロット -->　干渉変数を含めたモデル3が変動によくキャッチアップしている
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

dev.new()
m <- dropFirst(oFiltered3.DLM$m[,1])+dropFirst(oFiltered3.DLM$m[,13])*X[,1]+dropFirst(oFiltered3.DLM$m[,14])*X[,2]
s <- dropFirst(oSmoothed3.DLM$s[,1])+dropFirst(oSmoothed3.DLM$s[,13])*X[,1]+dropFirst(oSmoothed3.DLM$s[,14])*X[,2]
plot(tsData, type="o", col="darkgrey", main="モデル3: 季節要素が変動＋説明・干渉変数")
lines(m, lty=1, col="black")
lines(s, lty=2, col="blue")
lines(oFiltered3.DLM$f, lty=3, col="red")
legend("topright", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")


# 平滑化レベルと95%信頼区間のプロット --> 再考（エラーになる）
# var <- dlmSvd2var(oSmoothed.DLM$U.S, oSmoothed.DLM$D.S)
# hwid <- qnorm(0.025, lower=FALSE) * sqrt(unlist(var))
# smooth <- cbind(oSmoothed.DLM$s, as.vector(oSmoothed.DLM$s)+hwid%o%c(-1,1))
# plot(dropFirst(smooth), plot.type="s", type="l", lty=c(1,5,5), xlab="", ylab="Level", ylim=range(tsData))
# lines(tsData, type="o", col="darkgrey")
# legend("bottomleft", legend=c("data", "smoothed level", "95% probability limits"), col=c("darkgrey", rep("black", 2)), lty=c(1,1,5), pch=c(1,NA,NA), bty="n")

# -----------------------------------------------------------------------------
# 先期間の予測 --> モデル3は干渉・説明変数が先区間にないため使えない
oFcst.DLM <- dlmForecast(oFiltered.DLM, nAhead=24, sampleNew=3)
oFcst2.DLM <- dlmForecast(oFiltered2.DLM, nAhead=24, sampleNew=3)

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル1: レベルと季節の攪乱項はゼロ固定", xlim=c(1969, 1986))
invisible(lapply(oFcst.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル2: レベルと季節要素が変動", xlim=c(1969, 1986))
invisible(lapply(oFcst2.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst2.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst2.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered2.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed2.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered2.DLM$f, lty=3, col="red")
