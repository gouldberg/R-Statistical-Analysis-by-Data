# 状態空間モデル　by KFAS
# ナイル河水量
# 1871～1970年のナイル河の年間流量
# アスワンの最初のダム建設は1898年に開始、二番目のダムは1971年に完成している

setwd("C:\\Users\\R_work")
require(KFAS)
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
###############################################################################
# -----------------------------------------------------------------------------
# モデル1:　ランダムウォークプラスノイズ　ローカルレベルモデル
# nStatevar:  状態方程式の数
nStateVar <- 1;  nHyperParam <- 2;

# 初期値を、時系列全体での分散の対数　として設定 -->  初期値をゼロで与えると全く異なる分散となるため注意、また対数にしないとオーバーフローなどでうまく計算できない
( oModel.KFAS <- SSModel(tsData ~ SSMtrend(1, Q = list(matrix(NA))), H = matrix(NA)) )
( oFitted.KFAS <- fitSSM(oModel.KFAS, init=c(log(var(tsData)), log(var(tsData))), method = "BFGS") )

oFitted.KFAS$optim.out
drop(oFitted.KFAS$model$H);  drop(oFitted.KFAS$model$Q);

# 信号対雑音比確認 W/V --> 観測ノイズが随分と大きい
drop(oFitted.KFAS$model$Q) / drop(oFitted.KFAS$model$H)

# -----------------------------------------------------------------------------
# フィルタリング　および　平滑化
# KFS は、指数族の状態空間モデルに対して univariate approach で散漫初期化 exact diffuse initialization を用いてカルマンフィルタリングと平滑化を実施
# filtering:  ガウスモデルではデフォルトは "state"　（signal は mean)　　非ガウスモデルでは "none"
# smoothing:  デフォルトは "state" and "mean"　（ガウスモデルでは "mean" は "signal" と同じ）　　非ガウスモデルでは "disturbance"　は適用なし

( oEstimated.KFAS <- KFS(oFitted.KFAS$model, filtering = "state", smoothing = "state") )

# 状態の1期先予測
oEstimated.KFAS$a
# 状態の平滑化推定値
oEstimated.KFAS$alphahat

# -----------------------------------------------------------------------------
# モデル診断:　平滑化後残差になんらかの相関・構造が残っていないか

# rstandard() で標準化誤差をKFSの結果から取り出す
# state: 平滑化状態攪乱項に基づく誤差　　pearson: ガウスモデルの場合は standardized smoothed ε disturbance residuals  一般線形モデルの場合は標準化ピアソン残差
( oEstimated.KFAS <- KFS(oFitted.KFAS$model, smoothing = c("state", "mean", "disturbance")) )
plot(cbind(state = rstandard(oEstimated.KFAS, "state"), recursive = rstandard(oEstimated.KFAS), irregular = rstandard(oEstimated.KFAS, "pearson")), main = "recursive and auxiliary residuals")

# revursive, irregular については　以下と同じ
# "SSModel" クラスを引数にplot() すると、1期先予測と、平滑化後誤差をプロットしてくれる
# 非ガウスモデルでは、nsim= を引数に設定すると、その数だけインポータンス・サンプリングをしてくれる
dev.new(); plot(oFitted.KFAS$model);

( agStdPredErr.KFAS <- oEstimated.KFAS$v[,1] / sqrt(t(oEstimated.KFAS$F)) )
sub.ShowDiagnostics(agStdPredErr.KFAS, nStateVar, nHyperParam, nMaxLag=15, anACLag=c(1,12))


# -----------------------------------------------------------------------------
# モデルの当てはまりの良さ -->  ARIMAモデルの方がよい
# モデル2～3　はモデル1と同じパラメータをマニュアルで調整しているので AIC は同じ？
# モデル4　はおそらくパラメータが1つ増えているので・・・

# ARIMAモデル
result$loglik;  result$aic;

# モデルの対数尤度　および　AIC
( gLLbyFun.KFAS <- oEstimated.KFAS$logLik )
sub.AIC(gLLbyFun.KFAS, nStateVar, nHyperParam)
sub.AIC(gLLbyFun.KFAS, nStateVar, nHyperParam)/n


# 予測誤差？？の対数尤度 および AIC
# v[,1]: 予測誤差  F:  予測誤差分散
( gLLbyErr.KFAS <- sub.LogLik(oEstimated.KFAS$v[,1], t(oEstimated.KFAS$F), nStateVar) )
sub.AIC(gLLbyErr.KFAS, nStateVar, nHyperParam)
sub.AIC(gLLbyErr.KFAS, nStateVar, nHyperParam)/n

# -----------------------------------------------------------------------------
# 平滑化レベルと95%信頼区間のプロット
# predict() には、KFSの結果ではなく、fitSSMの結果を渡す
( conf <- predict(oFitted.KFAS$model, interval = "confidence", level = 0.95) )
( pred <- predict(oFitted.KFAS$model, interval = "prediction", level = 0.95) )
ts.plot(cbind(tsData, pred, conf[, -1]), col = c(1:2, 3, 3, 4, 4), ylab = "Predicted Annual flow", main = "River Nile")

# -----------------------------------------------------------------------------
# 先期間の予測
# predict() で先区間を予測、結果をそのままts.plot() に渡すと描画
oModel_c.KFAS <- SSModel(tsData ~ SSMtrend(1, Q=list(oFitted.KFAS$model$Q)),H=oFitted.KFAS$model$H)
( oFcst.KFAS <- predict(oModel_c.KFAS, n.ahead=5, interval="prediction",　level=0.9) )
plot()
ts.plot(oFcst.KFAS, col=c(1,2,3));
ts.plot(cbind(tsData, pred, conf[, -1]), col = c(1:2, 3, 3, 4, 4), ylab = "", main = "モデル1: ランダムウォークプラスノイズ")


plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="モデル1: ランダムウォークプラスノイズ", xlim=c(1871, 1975), ylim=c(400,1400))
lines(pred)

, lty=1, col="black")
lines(conf[,-1], lty=1, col="blue")
lines(oFcst.KFAS, col=c(1,2,3))
