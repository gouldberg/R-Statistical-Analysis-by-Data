# 状態空間モデル　by dlm
# ナイル河水量
# 1871～1970年のナイル河の年間流量
# アスワンの最初のダム建設は1898年に開始、二番目のダムは1971年に完成している

setwd("C:\\Users\\R_work")
require(dlm)
source("C:\\Users\\光世\\Desktop\\#Py_R_信号解析_時系列分析\\R_状態空間モデル_サポート関数.R")


###############################################################################
# データ読み込み、基礎俯瞰
###############################################################################
data(Nile);  dfData <- Nile;

# 1890年あたりから推移レベルが低下しているようでもある
# また振幅が近年は小さくなっている
par(mfrow=c(2,1))
ts.plot(dfData); ts.plot(log(dfData));

tsData <- dfData

n <- length(tsData)


###############################################################################
# 基礎分析:　自己相関、偏自己相関など
###############################################################################
# 自己相関は減衰、ラグ=3程度まであり　（ラグ=4以降はゼロが推測される）　⇒　MA(3)が想定される
# 偏自己相関は1次はあり、2次以降はゼロ　⇒　AR(1)が想定される
# 以上より、ARMA(p=1, q=3) が想定される
# 若干の自己相関あり
dev.new();  par(mfrow=c(2,1));  acf(tsData, lag.max=30);  pacf(tsData, lag.max=30);

# 1階差分は定常過程とみることができそう
# 自己相関は減衰、ラグ=1まであり　（ラグ=2以降はゼロが推測される）　⇒　MA(1)が想定される
# 偏自己相関は2次まであり、3次以降はゼロ　⇒　AR(2)が想定される
# 以上より、ARIMA(p=2, d=1, q=1) が想定される
plot(diff(tsData), type="o", col=c("darkgrey"), xlab="", ylab="Level")
dev.new();  par(mfrow=c(2,1));  acf(diff(tsData), lag.max=30);  pacf(diff(tsData), lag.max=30);


###############################################################################
# ARMA / ARIMA  モデル
###############################################################################
# ARIMA(1,0,3)モデル
p<-1; d<-0; q<-3
result <- arima(tsData, order=c(p,d,q), transform.pars=FALSE)
result
# モデルの残差の分散、対数尤度、AIC、残差
result$sigma2;  result$loglik;  result$aic;  result$res;

# -----------------------------------------------------------------------------
# モデル診断
dev.new(); tsdiag(result);
ahat<-result$resid; result_hat<-tsData - ahat;
dev.new(); par(mfrow=c(1,1)); plot(tsData, type="o", col=c("darkgrey"), xlab="", ylab="Level", main="ARIMAモデルの推定値"); lines(result_hat, lty=2, col=2)

tsdiag(result, gof.lag=20)

# ARIMA(1,0,3)モデル　パラメータ推定値の有意性検定　（t検定）　-->  MA要素は有意にならなかった
# パラメータ (phi, theta) 推定値
( b <- result$coef )
# パラメータ推定値の分散、共分散行列　（対角要素の平方根はパラメータ推定値の標準誤差）
( V <- result$var.coef )
# パラメータ推定値のt値計算
t <- numeric(4)
for(j in 1:4) t[j] <- b[j]/sqrt(V[j,j])
names(t) <- c("t_ar1", "t_ma1", "t_ma2", "t_ma3")
t
hantei <-( (t<0) & (pnorm(t)<0.05) ) | ( (t>0) & (pnorm(t)>0.95) )
hantei

# -----------------------------------------------------------------------------
# 予測
( pred <- predict(result, n.ahead=10) )
pred$pred; pred$se;
se1 <- pred$pred+2*pred$se;  se2 <- pred$pred-2*pred$se;
dev.new(); par(mfrow=c(1,1));
ts.plot(result_hat, pred$pred, se1, se2, gpars=list(lt=c(2,3,4), col=c(2,3,4), ylim=range(tsData), main="ARIMAモデルでの先期間予測"))
lines(tsData, type="o", col=c("darkgrey")); abline(v=1898, lty=2);  legend(locator(1), c("観測値","モデル推定値","モデル予測値", "2*se"), lty=c(1,2,3,4), col=c("darkgrey",2,3,4));


###############################################################################
# モデル1:  ランダムウォークプラスノイズ　ローカルレベルモデル (nStatevar=1,  nHyperPram=2)
# モデル2:  システムノイズをさらに小さくする -- W/V=0.05
# モデル3:  システムノイズを観測ノイズに対して大きく設定 -- W/V=0.50
# モデル4:　システムノイズの調整 -- 大きな変動のあった時点の（のみの）システムノイズを大きく（12倍）設定して、変動にキャッチアップ
#
# ランダムウォークプラスノイズモデルのフィルタ化推定値は、当該時刻の値を考慮するため変化のタイミングがARIMAモデルよりも1時点速い
# モデル4は、ランダムウォークプラスノイズモデルと比較して一時的なレベル変化への追随がよく、またその他の時点ではランダムウォークプラスノイズの滑らかさを維持
# モデル3は、システムノイズを大きくしたことで、変動キャッチアップが速いが、大きく変動してしまう
# ランダムウォークプラスノイズモデルの予測は、フィルタ化推定値（=1期先予測）の最終観測時点の値を延長させたものになる
# 一方、ARIMAモデルは若干の変動を考慮することから、AICはARIMAの方がよい？
#
# 信号対雑音比 (W/V) を大きくする（システムノイズを大きくする）ことで、更に変動へのキャッチアップを速くかつ大きくすることができる　（が予測のブレも大きくなる）
# 最尤推定で求められたシステムノイズと観測ノイズの分散が、データ変動を最も捉えるわけではなく、W/Vの調整が必要
# 変化点が事前にわかっていれば、信号対雑音比 を一部マニュアルで調整することで、大きな変化点へ対応することが可能　（事前にわかっていること、マニュアルでの調整）
###############################################################################
# -----------------------------------------------------------------------------
# モデル1:　ランダムウォークプラスノイズ　ローカルレベルモデル
# nStatevar:  状態方程式の数
nStateVar <- 1;  nHyperParam <- 2;

# 初期値はゼロで与えた　（KFASでは時系列全体の分散を与えるが、これを与えるとエラーになった）
funModel <- function(parm){ dlmModPoly(order=1, dV=exp(parm[1]), dW=exp(parm[2])) }
oMLE.DLM <- dlmMLE(tsData, parm=rep(0,2), build=funModel, hessian=T)

oMLE.DLM$convergence
oMLE.DLM$par;  exp(oMLE.DLM$par[1]);  exp(oMLE.DLM$par[2]);
oMLE.DLM$value;

# 観測ノイズがかなり大きい（システムノイズで吸収しきれていない
exp(oMLE.DLM$par[2]) / exp(oMLE.DLM$par[1])

# t=0時点の 状態ベクトルの正規事前分布　theta0 ~ N(m0, C0)
# 観測方程式:  Yt = Ft * thetat + vt,  vt ~ N(0, Vt)
# システム方程式:  thetat ~ Gt * thetat-1 + wt,  wt ~ N(0, Wt)
# FF(Ftを指す), GG(Gtを指す)　にそれぞれ1が入っている
# t=0での状態ベクトルが N(m0, C0) --> C0 = 10^7 と随分大きな値がデフォルトで設定されている
oFitted.DLM <- funModel(oMLE.DLM$par)
oFitted.DLM$FF;  oFitted.DLM$GG;
drop(oFitted.DLM$C0)
drop(oFitted.DLM$V);  drop(oFitted.DLM$W);

# -----------------------------------------------------------------------------
# モデル2:  システムノイズをさらに小さくする -- W/V=0.05
nStateVar2 <- 1;  nHyperParam2 <- 2;
oFitted2.DLM <- dlmModPoly(order=1, dV=exp(oMLE.DLM$par[1]), dW=exp(oMLE.DLM$par[1])*0.05)
drop(oFitted2.DLM$W) / drop(oFitted2.DLM$V)

# -----------------------------------------------------------------------------
# モデル3:  システムノイズを観測ノイズに対して大きく設定 -- W/V=0.50
nStateVar3 <- 1;  nHyperParam3 <- 2;
oFitted3.DLM <- dlmModPoly(order=1, dV=exp(oMLE.DLM$par[1]), dW=exp(oMLE.DLM$par[1])*0.50)
drop(oFitted3.DLM$W) / drop(oFitted3.DLM$V)

# -----------------------------------------------------------------------------
# モデル4:　システムノイズの調整 -- 大きな変動のあった時点の（のみの）システムノイズを大きく（12倍）設定して、変動にキャッチアップ
# nStatevar は要確認
nStateVar4 <- 1;  nHyperParam4 <- 2;
X <- ts(matrix(oFitted.DLM$W, nc=1, nr=length(tsData)), start=start(tsData))
window(X, 1898, 1899) <- 12*oFitted.DLM$W

# 固定モデル に対して、X, JWを追加する -->　詳細は未だ不明
oFitted4.DLM <- oFitted.DLM;  oFitted4.DLM$X <- X;  oFitted4.DLM$JW <- matrix(1,1,1);

# -----------------------------------------------------------------------------
# モデル5:　ランダムウォークプラスノイズモデル  ギブス・サンプリングによる観測ノイズ、システムノイズの推定
# 分散についての融通の利く事前分布族は逆ガンマ分布族であり、# V = 1/phi1, W = 1/phi2, phi1 ~ gamma(a1, b1), phi2 ~ gamma(a2, b2) とする
nStateVar <- 1;  nHyperParam <- 4;

a1 <- 2;  b1 <- 0.0001;  a2 <- 2;  b2 <- 0.0001
# 初期値
phi1 <- 1;  phi2 <- 1;
oFitted5.DLM <- dlmModPoly(order=1, dV=1/phi1, dW=1/phi2)

n.sample <- 1500;  phi1_save <- numeric(n.sample);  phi2_save <- numeric(n.sample);
sh1 <- a1 + n/2;  sh2 <- a2 + n /2;

set.seed(10)
# ギブスサンプリング
for(it in 1:n.sample){
  filt <- dlmFilter(tsData, oFitted5.DLM)
  level <- dlmBSample(filt)
  rate <- b1 + crossprod(tsData - level[-1]) / 2
  phi1 <- rgamma(1, shape=sh1, rate=rate)
  rate <- b2 + crossprod(level[-1] - level[-n]) / 2
  phi2 <- rgamma(1, shape=sh2, rate=rate)
  V(oFitted5.DLM) <- 1/phi1
  W(oFitted5.DLM) <- 1/phi2
  phi1_save[it] <- phi1
  phi2_save[it] <- phi2
}

# 収束状況の確認
par(mfrow=c(2,1))
plot(phi1_save, type="l");  plot(phi2_save, type="l");

# モデル1とギブスサンプリングによる推定の比較
drop(oFitted.DLM$V);  drop(oFitted.DLM$W);
drop(oFitted5.DLM$V);  drop(oFitted5.DLM$W);

# -----------------------------------------------------------------------------
# フィルタリング　および　モデル診断 --> どのモデルも診断は問題なし
# モデル診断:　フィルタ化残差になんらかの相関・構造が残っていないか
# 検定:  独立性 / 均一分散性 / 正規性
# nMaxLag:     (integer scalar) max lag (for BoxLjung)
# anACLag:     (integer vector) two number of lag (for ACF)

oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
oFiltered2.DLM <- dlmFilter(tsData, oFitted2.DLM)
oFiltered3.DLM <- dlmFilter(tsData, oFitted3.DLM)
oFiltered4.DLM <- dlmFilter(tsData, oFitted4.DLM)
oFiltered5.DLM <- dlmFilter(tsData, oFitted5.DLM)

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

qqnorm(residuals(oFiltered4.DLM, sd=FALSE));  qqline(residuals(oFiltered4.DLM, sd=FALSE));  dev.new();  tsdiag(oFiltered4.DLM);
agStdPredErr4.DLM  <- residuals(oFiltered4.DLM, type = c("standardized"), sd=FALSE)
lPredErr4.DLM      <- residuals(oFiltered4.DLM, type = c("raw"), sd=TRUE)
sub.ShowDiagnostics(agStdPredErr4.DLM, nStateVar4, nHyperParam4, nMaxLag=15, anACLag=c(1,12))

# -----------------------------------------------------------------------------
# モデルの当てはまりの良さ -->  ARIMAモデルの方がよい
# モデル2～3　はモデル1と同じパラメータをマニュアルで調整しているので AIC は同じ？
# モデル4　はおそらくパラメータが1つ増えているので・・・

# ARIMAモデル
result$loglik;  result$aic;

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
oSmoothed2.DLM <- dlmSmooth(oFiltered2.DLM)
oSmoothed3.DLM <- dlmSmooth(oFiltered3.DLM)
oSmoothed4.DLM <- dlmSmooth(oFiltered4.DLM)
oSmoothed5.DLM <- dlmSmooth(oFiltered5.DLM)

# -----------------------------------------------------------------------------
# 時系列、フィルタ後推定値、平滑化レベルのプロット
# ランダムウォークプラスノイズモデルのフィルタ化推定値は、当該時刻の値を考慮するため変化のタイミングがARIMAモデルよりも1時点速い
# モデル4は、ランダムウォークプラスノイズモデルと比較して一時的なレベル変化への追随がよく、またその他の時点ではランダムウォークプラスノイズの滑らかさを維持
# モデル3は、システムノイズを大きくしたことで、変動キャッチアップが速いが、大きく変動してしまう

par(mfrow=c(1,1))
ts.plot(result_hat, gpars=list(lt=c(2), col=c(2), lwd=c(1), ylim=range(tsData), main="ARIMAモデルでの先期間予測 vs. ランダムウォークプラスノイズ"))
lines(tsData, type="o", col="darkgrey")
lines(dropFirst(oFiltered.DLM$m), col="blue", lwd=2)
legend(locator(1), c("観測値","ARIMA推定値","ランダムウォークプラスノイズ"), lty=c(1,2,1), lwd=c(1,1,2), col=c("darkgrey",2,"blue"));

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", main="モデル1: ランダムウォークプラスノイズモデル")
lines(dropFirst(oFiltered.DLM$m), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", main="モデル2: W/V=0.05")
lines(dropFirst(oFiltered2.DLM$m), lty=1, col="black")
lines(dropFirst(oSmoothed2.DLM$s), lty=2, col="blue")
lines(oFiltered2.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", main="モデル3: W/V=0.50")
lines(dropFirst(oFiltered3.DLM$m), lty=1, col="black")
lines(dropFirst(oSmoothed3.DLM$s), lty=2, col="blue")
lines(oFiltered3.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", main="モデル4: レベル変化時点のみのシステムノイズを調整")
lines(dropFirst(oFiltered4.DLM$m), lty=1, col="black")
lines(dropFirst(oSmoothed4.DLM$s), lty=2, col="blue")
lines(oFiltered4.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", main="モデル5: ランダムウォークプラスノイズモデル　ギブスサンプリングによるパラメータ推定")
lines(dropFirst(oFiltered5.DLM$m), lty=1, col="black")
lines(dropFirst(oSmoothed5.DLM$s), lty=2, col="blue")
lines(oFiltered5.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

# -----------------------------------------------------------------------------
# 平滑化レベルと95%信頼区間のプロット
dev.new(); par(mfrow=c(1,1));
var <- dlmSvd2var(oSmoothed.DLM$U.S, oSmoothed.DLM$D.S)
hwid <- qnorm(0.025, lower=FALSE) * sqrt(unlist(var))
smooth <- cbind(oSmoothed.DLM$s, as.vector(oSmoothed.DLM$s) + hwid %o% c(-1,1))
plot(dropFirst(smooth), plot.type="s", type="l", lty=c(1,5,5), xlab="", ylab="Level", ylim=range(tsData), main="モデル1: ランダムウォークプラスノイズモデル")
lines(tsData, type="o", col="darkgrey")
legend("bottomleft", legend=c("data", "smoothed level", "95% probability limits"), col=c("darkgrey", rep("black", 2)), lty=c(1,1,5), pch=c(1,NA,NA), bty="n")

dev.new(); par(mfrow=c(1,1));
var2 <- dlmSvd2var(oSmoothed2.DLM$U.S, oSmoothed2.DLM$D.S)
hwid2 <- qnorm(0.025, lower=FALSE) * sqrt(unlist(var2))
smooth2 <- cbind(oSmoothed2.DLM$s, as.vector(oSmoothed2.DLM$s) + hwid2 %o% c(-1,1))
plot(dropFirst(smooth2), plot.type="s", type="l", lty=c(1,5,5), xlab="", ylab="Level", ylim=range(tsData), main="モデル2: W/V=0.05")
lines(tsData, type="o", col="darkgrey")
legend("bottomleft", legend=c("data", "smoothed level", "95% probability limits"), col=c("darkgrey", rep("black", 2)), lty=c(1,1,5), pch=c(1,NA,NA), bty="n")

dev.new(); par(mfrow=c(1,1));
var3 <- dlmSvd2var(oSmoothed3.DLM$U.S, oSmoothed3.DLM$D.S)
hwid3 <- qnorm(0.025, lower=FALSE) * sqrt(unlist(var3))
smooth3 <- cbind(oSmoothed3.DLM$s, as.vector(oSmoothed3.DLM$s) + hwid3 %o% c(-1,1))
plot(dropFirst(smooth3), plot.type="s", type="l", lty=c(1,5,5), xlab="", ylab="Level", ylim=range(tsData), main="モデル3: W/V=0.50")
lines(tsData, type="o", col="darkgrey")
legend("bottomleft", legend=c("data", "smoothed level", "95% probability limits"), col=c("darkgrey", rep("black", 2)), lty=c(1,1,5), pch=c(1,NA,NA), bty="n")

dev.new(); par(mfrow=c(1,1));
var4 <- dlmSvd2var(oSmoothed4.DLM$U.S, oSmoothed4.DLM$D.S)
hwid4 <- qnorm(0.025, lower=FALSE) * sqrt(unlist(var4))
smooth4 <- cbind(oSmoothed4.DLM$s, as.vector(oSmoothed4.DLM$s) + hwid4 %o% c(-1,1))
plot(dropFirst(smooth4), plot.type="s", type="l", lty=c(1,5,5), xlab="", ylab="Level", ylim=range(tsData), main="モデル4: レベル変化時点のみのシステムノイズを調整")
lines(tsData, type="o", col="darkgrey")
legend("bottomleft", legend=c("data", "smoothed level", "95% probability limits"), col=c("darkgrey", rep("black", 2)), lty=c(1,1,5), pch=c(1,NA,NA), bty="n")

# -----------------------------------------------------------------------------
# 先期間の予測 --> モデル4は constant models でないので予測できない
# ランダムウォークプラスノイズモデルの予測は、フィルタ化推定値（=1期先予測）の最終観測時点の値を延長させたものになる
# ARIMAモデルは若干の変動を考慮する
oFcst.DLM <- dlmForecast(oFiltered.DLM, nAhead=5, sampleNew=3)
oFcst2.DLM <- dlmForecast(oFiltered2.DLM, nAhead=5, sampleNew=3)
oFcst3.DLM <- dlmForecast(oFiltered3.DLM, nAhead=5, sampleNew=3)
# oFcst4.DLM <- dlmForecast(oFiltered4.DLM, nAhead=5, sampleNew=3)
oFcst5.DLM <- dlmForecast(oFiltered5.DLM, nAhead=5, sampleNew=3)

dev.new(); par(mfrow=c(1,1));
ts.plot(result_hat, pred$pred, se1, se2, gpars=list(lt=c(2,3,4), col=c(2,3,4), ylim=range(tsData), main="ARIMAモデルでの先期間予測"))
lines(tsData, type="o", col=c("darkgrey"))
abline(v=1898, lty=2)
legend(locator(1), c("観測値","モデル推定値","モデル予測値", "2*se"), lty=c(1,2,3,4), col=c("darkgrey",2,3,4));

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル1: ランダムウォークプラスノイズ", xlim=c(1871, 1975), ylim=c(400,1400))
invisible(lapply(oFcst.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered.DLM$m), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル2: W/V=0.05", xlim=c(1871, 1975), ylim=c(400,1400))
invisible(lapply(oFcst2.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst2.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst2.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered2.DLM$m), lty=1, col="black")
lines(dropFirst(oSmoothed2.DLM$s), lty=2, col="blue")
lines(oFiltered2.DLM$f, lty=3, col="red")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル3: W/V=0.50", xlim=c(1871, 1975), ylim=c(400,1400))
invisible(lapply(oFcst3.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst3.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst3.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered3.DLM$m), lty=1, col="black")
lines(dropFirst(oSmoothed3.DLM$s), lty=2, col="blue")
lines(oFiltered3.DLM$f, lty=3, col="red")

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル5: ランダムウォークプラスノイズモデル ギブスサンプリングによるパラメータ推定", xlim=c(1871, 1975), ylim=c(400,1400))
invisible(lapply(oFcst5.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst5.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst5.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered5.DLM$m), lty=1, col="black")
lines(dropFirst(oSmoothed5.DLM$s), lty=2, col="blue")
lines(oFiltered5.DLM$f, lty=3, col="red")
