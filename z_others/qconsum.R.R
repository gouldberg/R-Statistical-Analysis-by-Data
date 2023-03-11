# 状態空間モデル　by dlm
# イギリスにおける耐久財の1958年のポンド価格での消費支出の四半期時系列
# 周期・季節性のあるローカルレベルモデル
# - レベルと季節要素が時間で変化

setwd("C:\\Users\\R_work")
require(dlm)
source("C:\\Users\\光世\\Desktop\\#Py_R_信号解析_時系列分析\\R_状態空間モデル_サポート関数.R")


###############################################################################
# データ読み込み、基礎俯瞰
###############################################################################
dfData <- read.table("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_statespace_petris\\qconsum.dat", skip=4, colClasses="numeric")

# トレンドと周期・季節性あり、しかも季節成分は動的変化している
# 観測期間の初期に正負の値が交互する状況から、系列の後半では正の値が2つ続いた後に負の値が続く、といったパターンに移行している
# 上下の振幅も大きくなってきている
par(mfrow=c(2,1))
ts.plot(dfData[,1]); ts.plot(log(dfData[,1]));

tsData <- ts(dfData[,1], frequency=4, start=c(1957, 1))
plot(tsData)

n <- length(tsData)

###############################################################################
# 基礎分析:　自己相関、偏自己相関など
###############################################################################
# 強い周期性あり
dev.new();  par(mfrow=c(2,1));  acf(tsData, lag.max=30);  pacf(tsData, lag.max=30);
dev.new();  plot(decompose(tsData, type="additive"))

# 一階差分をとると、変化が大きくなってきていることがわかる
plot(diff(tsData), type="o", col=c("darkgrey"), xlab="", ylab="Level")
dev.new();  par(mfrow=c(2,1));  acf(diff(tsData), lag.max=30);  pacf(diff(tsData), lag.max=30);
dev.new();  plot(decompose(diff(tsData), type="additive"))


###############################################################################
# モデル1:  レベルと季節の攪乱項はゼロに固定するモデル (nStatevar=4,  nHyperPram=1)
# モデル2:  レベルと季節要素が時間で変化する (nStatevar=4, nHyperparam=3)、システムノイズとして表現
#
# 時系列は季節性以外にトレンドも見られるが、モデル2ではレベルも時間で変化するためトレンドにフィルター推定値がおいついている
# 季節性の波形が系列の初期と後半で異なるが、モデル2では季節成分も時間で変化するため、後半ではパターンが変わる
# 先期間予測も比較的まとも
###############################################################################
# -----------------------------------------------------------------------------
# モデル:  レベルと季節要素が時間で変化する -->  システムノイズとして表現、システムノイズ側の初期値を大きく設定
nStateVar <- 4;  nHyperParam <- 3;
funModel <- function(parm){
  m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
  m2 <- dlmModSeas(4, dV=0, dW=c(exp(parm[3]), rep(0,2)))
  return( m1 + m2 )
}
# システムノイズ側を大きく設定する
oMLE.DLM <- dlmMLE(tsData, parm=log(c(0.001, var(tsData), var(tsData))), build=funModel, hessian=T)
oMLE.DLM$convergence
oMLE.DLM$par;  exp(oMLE.DLM$par[1]);  exp(oMLE.DLM$par[2]);  exp(oMLE.DLM$par[3]);

oFitted.DLM <- funModel(oMLE.DLM$par)
diag(oFitted.DLM$W)[1];  diag(oFitted.DLM$W)[2]

# -----------------------------------------------------------------------------
# フィルタリング　および　モデル診断:
# モデル診断:　フィルタ化残差になんらかの相関・構造が残っていないか
# 検定:  独立性 / 均一分散性 / 正規性
# nMaxLag:     (integer scalar) max lag (for BoxLjung)
# anACLag:     (integer vector) two number of lag (for ACF)
oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)

qqnorm(residuals(oFiltered.DLM, sd=FALSE));  qqline(residuals(oFiltered.DLM, sd=FALSE));  dev.new();  tsdiag(oFiltered.DLM);
agStdPredErr.DLM  <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)
lPredErr.DLM      <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)
sub.ShowDiagnostics(agStdPredErr.DLM, nStateVar, nHyperParam, nMaxLag=15, anACLag=c(1,12))

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

# -----------------------------------------------------------------------------
# 平滑化
oSmoothed.DLM <- dlmSmooth(oFiltered.DLM)

# 平滑化分布の平均(s)、平滑化分布の分散を特異値分解(SVD)したもの
oSmoothed.DLM$s

# -----------------------------------------------------------------------------
# プロット
# 時系列、フィルタ後推定値、平滑化レベルのプロット
par(mfrow=c(1,1))
plot(tsData, type="o", col="darkgrey", main="モデル: レベルと季節要素が変動")
lines(dropFirst(oFiltered.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")


# 平滑化レベルと95%信頼区間のプロット --> 再考（エラーになる）
# var <- dlmSvd2var(oSmoothed.DLM$U.S, oSmoothed.DLM$D.S)
# hwid <- qnorm(0.025, lower=FALSE) * sqrt(unlist(var))
# smooth <- cbind(oSmoothed.DLM$s, as.vector(oSmoothed.DLM$s)+hwid%o%c(-1,1))
# plot(dropFirst(smooth), plot.type="s", type="l", lty=c(1,5,5), xlab="", ylab="Level", ylim=range(tsData))
# lines(tsData, type="o", col="darkgrey")
# legend("bottomleft", legend=c("data", "smoothed level", "95% probability limits"), col=c("darkgrey", rep("black", 2)), lty=c(1,1,5), pch=c(1,NA,NA), bty="n")

# -----------------------------------------------------------------------------
# 先期間の予測
oFcst.DLM <- dlmForecast(oFiltered.DLM, nAhead=12, sampleNew=3)

# 先予測も周期・季節要素のパターンを再現している
dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル: レベルと季節要素が変動", xlim=c(1957, 1970), ylim=c(200,1000))
invisible(lapply(oFcst.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")
