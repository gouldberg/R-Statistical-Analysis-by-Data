# ローカル線形トレンドモデル

setwd("C:\\Users\\R_work")
require(dlm)
require(KFAS)

###############################################################################
## ----------------------------------------------------------------------------
## サポート関数
## ----------------------------------------------------------------------------
###############################################################################
sub.LogLik <- function(tsNu, tsF, nStateVar){
  # purpose: get log likelihood.
  #          See Section 8.4
  # args:    tsNu:      (ts object)      time series of predicion error
  #          tsF:       (ts object)      time series of prediction error variance
  #          nStateVar: (numeric scalar) number of state variables
  # return:  (numeric scalar) log likelihood

  # trap: the length should be same
  stopifnot(length(tsF) == length(tsNu))

  # remove the first (nStateVar) timepoints
  agNu <- tsNu[-(1:nStateVar)]
  agF  <- tsF[-(1:nStateVar)]

  - 0.5 * length(tsNu) * log(2*pi) - 0.5 * sum( log(agF) + (agNu^2 / agF) )
}

sub.AIC <- function(gLogLik, nStateVar, nHyperParam){
  # purpose: get AIC
  #          See Section 2.1.
  # args:    gLogLik:     (numeric scalar) log likelihood
  #          nStateVar:   (integer scalar) number of state variables
  #          nHyperParam: (integer scalar) number of hyper params
  # return:  (numeric scalar) AIC

  - 2 * gLogLik + 2 * ( nStateVar + nHyperParam )
}

sub.BoxLjung <- function(agStdRes, nStateVar, nHyperParam, nLag=15){
  # purpose: Independence test for standardized residuals
  #          See Section 8.5.
  # args:    agStdRes:    (numeric vector) standardized residuals
  #          nStateVar:   (integer scalar) number of state variables
  #          nHyperParam: (integer scalar) number of hyper params
  #          nLag:        (integer scalar) max lag
  # return:  (list)
  #            stat:     (numeric scalar) Q(nLag)
  #            critical: (numeric scalar) 5% critical value
  # notes:   Though the C&K book suggests all residuals can be used,
  #          their ssfpack program removes the first (nStateVar)
  #          residuals.

  # call Box.test
  gStat <- Box.test(
    agStdRes[-(1:nStateVar)], # remove the first (nStateVar) timepoints
    lag     = nLag,
    type    = "Ljung-Box"
  )$statistic

  # get critical value
  gCritical <- qchisq(0.95, nLag-nHyperParam+1)

  list(
    stat     = gStat,
    critical = gCritical
  )
}

sub.ResidualAcf <- function(agStdRes, nStateVar, nLag=15){
  # purpose: autocorrelation of standardized residuals
  #          See Section 8.5.
  # args:    agStdRes:    (numeric vector) standardized residuals
  #          nStateVar:   (integer scalar) number of state variables
  #          nLag:        (integer scalar) max lag
  # return:  (list)
  #            acf:      (numeric vector) acf (1 ... nLag)
  #            critical: (numeric scalar) 95% confidence limit
  # notes:   Though the C&K book suggests all residuals can be used,
  #          their ssfpack program removes the first (nStateVar)
  #          residuals.

  list(
    acf = acf(agStdRes[-(1:nStateVar)], plot=FALSE)$acf[-1],
    critical = 2 / sqrt(length(agStdRes))
  )
}

sub.Homoscedasticity <- function(agStdRes, nStateVar){
  # purpose: Homoscedasticity test of standardized residuals
  #          See the C&K Book, Section 8.5.
  # args:    agStdRes:    (numeric vector) standardized residuals
  #          nStateVar:   (integer scalar) number of state variables
  # return:  (list)
  #            size:  (integer scalar) block size
  #            stat:  (numeric scalar) H(block size)
  #            upper: (numeric scalar) 5% critical value (upper)
  #            lower: (numeric scalar) 5% critical value (lower)

  # get leength
  n <- length(agStdRes)

  # defint blocksize
  nBlockSize <- round((n - nStateVar)/3)
  gValue     <- sum(agStdRes[(n-nBlockSize+1):n]^2) /
                  sum(agStdRes[(nStateVar+1):(nStateVar+nBlockSize)]^2)

  list(
    size  = nBlockSize,
    stat  = gValue,
    upper = qf(0.975, nBlockSize, nBlockSize),
    lower = qf(0.025, nBlockSize, nBlockSize)
  )
}

sub.JarqueBera <- function(agStdRes, nStateVar){
  # purpose: Normality test of standardized residuals
  #          See Section 8.5.
  # args:    agStdRes:    (numeric vector) standardized residuals
  #          nStateVar:   (integer scalar) number of state variables
  # return:  (list)
  #            stat:     (numeric vector) statistic
  #            critical: (numeric vector) 5% critical value (=5.99)
  # notes:   Though the C&K book suggests all residuals can be used,
  #          their ssfpack program removes the first (nStateVar)
  #          residuals.

  require(tseries)

  list(
    stat     = jarque.bera.test(agStdRes[-(1:nStateVar)])$statistic,
    critical = qchisq(0.95,2)
  )
}

sub.ShowDiagnostics <- function(
  agStdRes, nStateVar, nHyperParam, nMaxLag, anACLag
){
  # purpose: Show Diagnostics Table
  # args:    agStdRes:    (numeric vector) standardized residuals
  #          nStateVar:   (integer scalar) number of state variables
  #          nHyperParam: (integer scalar) number of hyper params
  #          nMaxLag:     (integer scalar) max lag (for BoxLjung)
  #          anACLag:     (integer vector) two number of lag (for ACF)
  # return:  NULL

  # get various statistics
  lBoxLjung <- sub.BoxLjung(agStdRes, nStateVar, nHyperParam, nMaxLag)
  lACF      <- sub.ResidualAcf(agStdRes, nStateVar, nMaxLag)
  lHomo     <- sub.Homoscedasticity(agStdRes, nStateVar)
  lJB       <- sub.JarqueBera(agStdRes, nStateVar)

  # gHomoStat: H statistics, or its reciprocal
  gHomoStat <- ifelse(lHomo$stat > 1, lHomo$stat, 1/lHomo$stat)

  asTemplate <- c(
    "------------------------------------------------------",
    "                   stat     value  critical satisfied",
    "------------------------------------------------------",
    "independence      Q  (%2d)  %7.3f   %5.2f     %1s",  # BoxLJung, 4 args
    "                  r  (%2d)  %7.3f  +-%4.2f     %1s", # ACF,      4 args
    "                  r  (%2d)  %7.3f  +-%4.2f     %1s", # ACF,      4 args
    "homoscedasticity  %-3s(%2d)  %7.3f   %5.2f     %1s", # Homo,     5 args
    "normality         N        %7.3f   %5.2f     %1s",   # N,        3 args
    "------------------------------------------------------"
  )

  cat(
    sprintf(
      paste(asTemplate, collapse="\n"),
      # BoxLjung, 4 args
      nMaxLag,
      lBoxLjung$stat,
      lBoxLjung$critical,
      ifelse(lBoxLjung$stat < lBoxLjung$critical, "+", "-"),
      # ACF, 4 args
      anACLag[1],
      lACF$acf[anACLag[1]],
      lACF$critical,
      ifelse(abs(lACF$acf[anACLag[1]]) < lACF$critical, "+", "-"),
      # ACF, 4 args
      anACLag[2],
      lACF$acf[anACLag[2]],
      lACF$critical,
      ifelse(abs(lACF$acf[anACLag[2]]) < lACF$critical, "+", "-"),
      # Homo, 5 args
      ifelse(lHomo$stat > 1, "H", "1/H"),
      lHomo$size,
      gHomoStat,
      lHomo$upper,
      ifelse(gHomoStat < lHomo$upper, "+", "-"),
      # N, 3 args
      lJB$stat,
      lJB$critical,
      ifelse(lJB$stat < lJB$critical, "+", "-")
    )
  )
  cat("\n")
}


###############################################################################
# ローカル線形トレンドモデル by ARMA / ARIMA
# フィンランドの年次道路交通事故数
###############################################################################
# データ読み込み
dfData <- read.table("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\OxCodeIntroStateSpaceBook\\Chapter_3\\NorwayFinland.txt", skip=1)
anTime <- as.integer(dfData[,1])

par(mfrow=c(2,1))
ts.plot(dfData[,3]); ts.plot(log(dfData[,3]));

# 対数変換したデータを使う
tsData <- ts(log(dfData[,3]), start=anTime[1])
# tsData <- ts(dfData[,3], start=anTime[1])

# 原系列の時系列プロット、自己相関係数、偏自己相関係数を確認
# 自己相関は減衰、ラグ=4程度まであり　⇒　MA(4)が想定される
# 偏自己相関は1次はあり、2次以降はゼロ　⇒　AR(1)が想定される
# 以上より、ARMA(p=1, q=4) が想定される
dev.new();  par(mfrow=c(2,1));  acf(tsData, lag.max=30);  pacf(tsData, lag.max=30);

# 一階差分をとって、同様に確認
# 1階差分は定常過程とみることができそう
plot(diff(tsData), type="o", col=c("darkgrey"), xlab="", ylab="Level")
dev.new();  par(mfrow=c(2,1));  acf(diff(tsData), lag.max=30);  pacf(diff(tsData), lag.max=30);

# -----------------------------------------------------------------------------
# ARIM(1,0,3)モデルの構築および診断
p<-1; d<-0; q<-4
result <- arima(tsData, order=c(p,d,q), transform.pars=FALSE)
result
# モデルの残差の分散、対数尤度、AIC、残差
result$sigma2;  result$loglik;  result$aic;  result$res;

# -----------------------------------------------------------------------------
# モデル診断
# ARIM(1,0,4)モデルの診断
dev.new(); tsdiag(result);
ahat<-result$resid; result_hat<-tsData - ahat;
dev.new(); par(mfrow=c(1,1)); plot(tsData, type="o", col=c("darkgrey"), xlab="", ylab="Level", main="ARIMAモデルの推定値"); lines(result_hat, lty=2, col=2)

# ARIM(1,0,4)モデル　パラメータ推定値の有意性検定　（t検定）
# パラメータ (phi, theta) 推定値
( b <- result$coef )
# パラメータ推定値の分散、共分散行列　（対角要素の平方根はパラメータ推定値の標準誤差）
( V <- result$var.coef )
# パラメータ推定値のt値計算
t <- numeric(5)
for(j in 1:5) t[j] <- b[j]/sqrt(V[j,j])
names(t) <- c("t_ar1", "t_ma1", "t_ma2", "t_ma3", "t_ma4")
t
hantei <-( (t<0) & (pnorm(t)<0.05) ) | ( (t>0) & (pnorm(t)>0.95) )
hantei

# -----------------------------------------------------------------------------
# 予測
# ARIMA(1,0,4)モデルでの先期間の予測
( pred <- predict(result, n.ahead=10) )
pred$pred; pred$se;
se1 <- pred$pred+2*pred$se;  se2 <- pred$pred-2*pred$se;
dev.new(); par(mfrow=c(1,1));
ts.plot(result_hat, pred$pred, se1, se2, gpars=list(lt=c(2,3,4), col=c(2,3,4), ylim=range(tsData), main="ARIMAモデルでの先期間予測"))
lines(tsData, type="o", col=c("darkgrey")); abline(v=1898, lty=2);  legend(locator(1), c("観測値","モデル推定値","モデル予測値", "2*se"), lty=c(1,2,3,4), col=c("darkgrey",2,3,4));


###############################################################################
# ローカル線形トレンドモデル by dlm
# フィンランドの年次道路交通事故数
###############################################################################
# データ読み込み
dfData <- read.table("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\OxCodeIntroStateSpaceBook\\Chapter_3\\NorwayFinland.txt", skip=1)
anTime <- as.integer(dfData[,1])

par(mfrow=c(2,1))
ts.plot(dfData[,3]); ts.plot(log(dfData[,3]));

# 対数変換したデータを使う
tsData <- ts(log(dfData[,3]), start=anTime[1])
# tsData <- ts(dfData[,3], start=anTime[1])

n <- length(tsData)

# -----------------------------------------------------------------------------
funModel <- function(parm){ dlmModPoly(order=2, dV=exp(parm[1]), dW=exp(parm[2:3])) }
# oMLE.DLM <- dlmMLE(tsData, parm=rep(0,3), build=funModel, hessian=T)
oMLE.DLM <- dlmMLE(tsData, parm=log(c(var(tsData),0.001,0.001)), build=funModel, hessian=T)

oMLE.DLM$convergence
oMLE.DLM$par;  exp(oMLE.DLM$par[1]);  exp(oMLE.DLM$par[2]);  exp(oMLE.DLM$par[3]);

# t=0時点の 状態ベクトルの正規事前分布　theta0 ~ N(m0, C0)
# 観測方程式:  Yt = Ft * thetat + vt,  vt ~ N(0, Vt)
# システム方程式:  thetat ~ Gt * thetat-1 + wt,  wt ~ N(0, Wt)
# F(Ftを指す), GG(Gtを指す)　を確認
oFitted.DLM <- funModel(oMLE.DLM$par)

agW <- diag(oFitted.DLM$W)
cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
cat(sprintf("hat(sigma^2_xi)      = %e\n", agW[1]))
cat(sprintf("hat(sigma^2_zeta)    = %f\n", agW[2]))

# -----------------------------------------------------------------------------
# フィルタリング　および　モデル診断
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
cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2,1]))
cat(sprintf("hat(nu_1)            = %f\n", oSmoothed.DLM$s[2,2]))


# -----------------------------------------------------------------------------
# プロット
# 時系列、フィルタ後推定値、平滑化レベルのプロット
par(mfrow=c(1,1))
plot(tsData, type="o", col="darkgrey")
lines(dropFirst(oFiltered.DLM$m), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")
legend("topright", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")


# 平滑化レベルと95%信頼区間のプロット --> 再考（エラーになる）
# var <- dlmSvd2var(oSmoothed.DLM$U.S, oSmoothed.DLM$D.S)
# hwid <- qnorm(0.025, lower=FALSE) * sqrt(unlist(var))
# smooth <- cbind(oSmoothed.DLM$s, as.vector(oSmoothed.DLM$s)+hwid%o%c(-1,1))
# plot(dropFirst(smooth), plot.type="s", type="l", lty=c(1,5,5), xlab="", ylab="Level", ylim=range(tsData))
# lines(tsData, type="o", col="darkgrey")
# legend("bottomleft", legend=c("data", "smoothed level", "95% probability limits"), col=c("darkgrey", rep("black", 2)), lty=c(1,1,5), pch=c(1,NA,NA), bty="n")

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 先期間の予測
# フィルタリング結果を用いて、先10期間について予測、予測はsampleNewに指定した回数実施
oFcst.DLM <- dlmForecast(oFiltered.DLM, nAhead=10, sampleNew=3)

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="動的線形モデルでの先期間予測", xlim=c(1990, 2013), ylim=c(5,7))
invisible(lapply(oFcst.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst.DLM$f, type="o", lwd=1, pch=4)
abline(v=mean(c(time(oFcst.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered.DLM$m), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")



###############################################################################
## ----------------------------------------------------------------------------
## 動的線形モデル:　ローカル線形トレンドモデル:  KFAS　パッケージ
## 参照:  野村本
## ----------------------------------------------------------------------------
###############################################################################
# nStatevar:  状態方程式の数
nStateVar <- 2;  nHyperParam <- 3;
n <- length(tsData)

# -----------------------------------------------------------------------------
# システムノイズ、観測ノイズを最尤推定法で推定

# ローカル線形トレンドモデルは、SSMtrend の次数=2　とする
# H: 観測ノイズの分散　　Q: システムノイズの分散
# fitSSM にて最尤推定
# 初期値を、時系列全体での分散の対数　として設定 -->  初期値をゼロで与えると全く異なる分散となるため注意、また対数にしないとオーバーフローなどでうまく計算できない
( oModel.KFAS <- SSModel(tsData ~ SSMtrend(2, Q = list(matrix(NA), matrix(NA))), H = matrix(NA)) )
( oFitted.KFAS <- fitSSM(oModel.KFAS, init=log(c(var(tsData),0.001,0.001)), method = "BFGS") )

# 最尤推定が収束していることを確認 （$convergence=0 となっていることを確認）
oFitted.KFAS$optim.out

# 推定された観測ノイズ、システムノイズの分散
drop(oFitted.KFAS$model$H);  drop(oFitted.KFAS$model$Q);

# 信号対雑音比確認 W/V --> 観測ノイズが随分と大きい
diag(drop(oFitted.KFAS$model$Q))[1] / drop(oFitted.KFAS$model$H)
diag(drop(oFitted.KFAS$model$Q))[2] / drop(oFitted.KFAS$model$H)

# -----------------------------------------------------------------------------
# 予測分布　および　フィルタ化分布　の算出:　すでに fitSSM にて終了


# -----------------------------------------------------------------------------
# フィルタリング　および　平滑化
# KFS は、指数族の状態空間モデルに対して univariate approach で散漫初期化 exact diffuse initialization を用いてカルマンフィルタリングと平滑化を実施
# filtering:  ガウスモデルではデフォルトは "state"　（signal は mean)　　非ガウスモデルでは "none"
# smoothing:  デフォルトは "state" and "mean"　（ガウスモデルでは "mean" は "signal" と同じ）　　非ガウスモデルでは "disturbance"　は適用なし
( oEstimated.KFAS <- KFS(oFitted.KFAS$model, filtering = "state", smoothing = "state") )

# 状態の1期先予測（水準と傾きの2列）
oEstimated.KFAS$a
# 状態の平滑化推定値（水準と傾きの2列）
oEstimated.KFAS$alphahat

# 水準成分と傾き成分のプロット
par(mfrow=c(2,1));
plot(tsData, lty=3, type="o", ylab="水準成分")
lines(oEstimated.KFAS$alphahat[,"level"], lwd=2)
plot(oEstimated.KFAS$alphahat[,"slope"], lwd=2, col=8, ylab="傾き成分")


# -----------------------------------------------------------------------------
# モデル診断:　平滑化後残差になんらかの相関・構造が残っていないか

# rstandard() で標準化誤差をKFSの結果から取り出す
# state: 平滑化状態攪乱項に基づく誤差　　pearson: ガウスモデルの場合は standardized smoothed ε disturbance residuals  一般線形モデルの場合は標準化ピアソン残差
( oEstimated2.KFAS <- KFS(oFitted.KFAS$model, smoothing = c("state", "mean", "disturbance")) )
plot(cbind(state = rstandard(oEstimated2.KFAS, "state"), recursive = rstandard(oEstimated2.KFAS), irregular = rstandard(oEstimated2.KFAS, "pearson")), main = "recursive and auxiliary residuals")

# revursive, irregular については　以下と同じ
# "SSModel" クラスを引数にplot() すると、1期先予測と、平滑化後誤差をプロットしてくれる
# 非ガウスモデルでは、nsim= を引数に設定すると、その数だけインポータンス・サンプリングをしてくれる
dev.new(); plot(oFitted.KFAS$model);

( agStdPredErr.KFAS <- oEstimated.KFAS$v[,1] / sqrt(t(oEstimated.KFAS$F)) )

# 検定:  independence / homoscedasticity / normality
# nMaxLag:     (integer scalar) max lag (for BoxLjung)
# anACLag:     (integer vector) two number of lag (for ACF)
sub.ShowDiagnostics(agStdPredErr.KFAS, nStateVar, nHyperParam, nMaxLag=15, anACLag=c(1,12))


# -----------------------------------------------------------------------------
# モデルの当てはまりの良さ:  -->  AICはガウスモデルのみ

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
par(mfrow=c(1,1))
ts.plot(cbind(tsData, pred, conf[, -1]), col = c(1:2, 3, 3, 4, 4), ylab = "Predicted", main = "フィンランド年次道路交通事故数")
# alphahat と pred/conf の fit が同じ値であることを確認
check <- cbind(data.frame(a=oEstimated.KFAS$a[-1,], alphahat=oEstimated.KFAS$alphahat[,]), as.data.frame(conf), as.data.frame(pred))
head(check);  tail(check);

# -----------------------------------------------------------------------------
# 先期間の予測 -->　エラーになるので検討、Qにどうやって渡せばよいのか
# predict() で先区間を予測、結果をそのままts.plot() に渡すと描画
# q1 <- diag(drop(oFitted.KFAS$model$Q))[1]
# q2 <- diag(drop(oFitted.KFAS$model$Q))[2]
# oModel2.KFAS <- SSModel(tsData ~ SSMtrend(2, Q=list(drop(oFitted.KFAS$model$Q))),H=list(oFitted.KFAS$model$H))
# ( oFcst.KFAS <- predict(oModel2.KFAS, n.ahead=10, interval="prediction",　level=0.9) )
# ts.plot(oFcst.KFAS, col=c(1,2,3));
