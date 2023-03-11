# ナイル河水量:　ARMA　および　ローカルレベルモデル（ランダムウォーク・プラスノイズ）　など
# 1871～1970年のナイル河の年間流量
# アスワンの最初のダム建設は1898年に開始、二番目のダムは1971年に完成している

setwd("C:\\Users\\R_work")
require(dlm)
require(KFAS)

# TS型のデータが必要
data(Nile)
tsData <- Nile

# 時系列プロットを見る限り、定常的な様相であるが、1890年あたりから推移レベルが低下しているようでもある
ts.plot(tsData)


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
## ----------------------------------------------------------------------------
## 自己相関、偏自己相関　の確認
## ----------------------------------------------------------------------------
###############################################################################
# 原系列の時系列プロット、自己相関係数、偏自己相関係数を確認
# 自己相関は減衰、ラグ=3程度まであり　（ラグ=4以降はゼロが推測される）　⇒　MA(3)が想定される
# 偏自己相関は1次はあり、2次以降はゼロ　⇒　AR(1)が想定される
# 以上より、ARMA(p=1, q=3) が想定される
dev.new();  par(mfrow=c(2,1));  acf(tsData, lag.max=30);  pacf(tsData, lag.max=30);

# 一階差分をとって、同様に確認
# 1階差分は定常過程とみることができそう
# 自己相関は減衰、ラグ=1まであり　（ラグ=2以降はゼロが推測される）　⇒　MA(1)が想定される
# 偏自己相関は2次まであり、3次以降はゼロ　⇒　AR(2)が想定される
# 以上より、ARIMA(p=2, d=1, q=1) が想定される
plot(diff(tsData), type="o", col=c("darkgrey"), xlab="", ylab="Level")
dev.new();  par(mfrow=c(2,1));  acf(diff(tsData), lag.max=30);  pacf(diff(tsData), lag.max=30);


###############################################################################
## ----------------------------------------------------------------------------
## ARMA / ARIMA モデル
## ----------------------------------------------------------------------------
###############################################################################
# ARIM(1,0,3)モデルの構築および診断
p<-1; d<-0; q<-3
result <- arima(tsData, order=c(p,d,q), transform.pars=FALSE)
result
# モデルの残差の分散、対数尤度、AIC、残差
result$sigma2;  result$loglik;  result$aic;  result$res;

# -----------------------------------------------------------------------------
# モデル診断
# ARIM(1,0,3)モデルの診断
dev.new(); tsdiag(result);
ahat<-result$resid; result_hat<-tsData - ahat;
dev.new(); par(mfrow=c(1,1)); plot(tsData, type="o", col=c("darkgrey"), xlab="", ylab="Level", main="ARIMAモデルの推定値"); lines(result_hat, lty=2, col=2)

# ARIM(1,0,3)モデル　パラメータ推定値の有意性検定　（t検定）
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
# ARIMA(1,0,3)モデルでの先期間の予測
( pred <- predict(result, n.ahead=10) )
pred$pred; pred$se;
se1 <- pred$pred+2*pred$se;  se2 <- pred$pred-2*pred$se;
dev.new(); par(mfrow=c(1,1));
ts.plot(result_hat, pred$pred, se1, se2, gpars=list(lt=c(2,3,4), col=c(2,3,4), ylim=range(tsData), main="ARIMAモデルでの先期間予測"))
lines(tsData, type="o", col=c("darkgrey")); abline(v=1898, lty=2);  legend(locator(1), c("観測値","モデル推定値","モデル予測値", "2*se"), lty=c(1,2,3,4), col=c("darkgrey",2,3,4));


###############################################################################
## ----------------------------------------------------------------------------
## 動的線形モデル:　ローカルレベルモデル（ランダムウォーク・プラスノイズモデル）:  dlm　パッケージ
## 参照:  Petris本
##
## ARMAモデルよりも、ランダムウォーク・プラスノイズモデルの方が比較的変動へのキャッチアップが速くかつ大きい
## 信号対雑音比 (W/V) を大きくする（システムノイズを大きくする）ことで、更に変動へのキャッチアップを速くかつ大きくすることができる　（が予測のブレも大きくなる）
## 最尤推定で求められたシステムノイズと観測ノイズの分散が、データ変動を最も捉えるわけではなく、W/Vの調整が必要
## 変化点が事前にわかっていれば、信号対雑音比 を一部マニュアルで調整することで、大きな変化点へ対応することが可能　（事前にわかっていること、マニュアルでの調整）
## ----------------------------------------------------------------------------
###############################################################################
# nStatevar:  状態方程式の数
nStateVar <- 1;  nHyperParam <- 2;
n <- length(tsData)

# -----------------------------------------------------------------------------
# システムノイズ、観測ノイズを最尤推定法で推定

# dV: 観測ノイズの分散　　dW: システムノイズの分散
# パラメタの負の値を探索しないようにするため、対数をパラメタとしたく、exp()変換している
# dlmMLE()は optim() を呼び出し、最尤推定する
# rep(0,2) は初期値 --> ゼロで与えてよい　（KFASでは時系列全体の分散を与えるが、これを与えるとエラーになった）
# hessian=T　でヘッセ行列を返り値として利用することができる、ヘッセ行列はパラメタの値の標準誤差を計算するのに使う
funModel <- function(parm){ dlmModPoly(order=1, dV=exp(parm[1]), dW=exp(parm[2])) }
oMLE.DLM <- dlmMLE(tsData, parm=rep(0,2), build=funModel, hessian=T)

#ゼロになっていることを確認、非零であれば最小値への収束がされていない
oMLE.DLM$convergence

# 最尤推定されたパラメタ値（観測ノイズの分散、システムノイズの分散）および対数尤度
# パラメタは exp() 変換が必要
# value　は対数尤度（らしい）
oMLE.DLM$par;  exp(oMLE.DLM$par[1]);  exp(oMLE.DLM$par[2]);
oMLE.DLM$value;

# 推定値の標準誤差　（ヘッセ行列を使って計算、推定値がexp()変換されていることに注意）
sqrt(diag(solve(oMLE.DLM$hessian)))

# 信号対雑音比確認 W/V --> 観測ノイズが随分と大きい
exp(oMLE.DLM$par[2]) / exp(oMLE.DLM$par[1])


# -----------------------------------------------------------------------------
# 動的線形モデルの設定
# 推定した、システムノイズ、観測ノイズを設定
# t=0での状態ベクトルが N(m0, C0) --> C0 = 10^7 と随分大きな値がデフォルトで設定されている
# モデル設定した時点で、V:観測ノイズ、W:システムノイズ　が exp() 変換されて見える

# t=0時点の 状態ベクトルの正規事前分布　theta0 ~ N(m0, C0)
# 観測方程式:  Yt = Ft * thetat + vt,  vt ~ N(0, Vt)
# システム方程式:  thetat ~ Gt * thetat-1 + wt,  wt ~ N(0, Wt)
# FF(Ftを指す), GG(Gtを指す)　にそれぞれ1が入っている
oFitted.DLM <- funModel(oMLE.DLM$par)
str(oFitted.DLM); unlist(oFitted.DLM);
oFitted.DLM$FF;  oFitted.DLM$GG;
drop(oFitted.DLM$V);  drop(oFitted.DLM$W);

cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.DLM$V)))
cat(sprintf("hat(sigma^2_xi)      = %f\n", drop(oFitted.DLM$W)))

# -----------------------------------------------------------------------------
# 予測分布とフィルタ化分布を算出 by カルマンフィルタ
oFiltered.DLM <- dlmFilter(tsData, oFitted.DLM)
str(oFiltered.DLM, 1)

# y:  入力された時系列データ(観測値)
# a:  予測分布の平均
# U.R, D.R: 予測誤差(a - theta)の分散を特異値分解した際の行列 --> dlmSvd2var(U.R, D.R) で分散計算
# m:  モデルにおいては状態ベクトルが従う分布の平均、フィルタリング後は事後状態推定値、0時点が起点となっているので注意、ランダムウォークプラスノイズモデルだと、m0 = E(y1) = E(theta)
# U.C, D.C: 事後状態推定値の推定誤差(m - theta)の分散を特異値分解した際の行列 --> dlmSvd2var(U.C, D.C) で分散計算

# f:  1期先予測が従う分布の平均、予測分布の期待値、時刻t-1まで観測された際の時刻tの事前観測推定値 = F %*% a
# y - f:  予測誤差(イノベーション)

oFiltered.DLM$a;  oFiltered.DLM$m;  oFiltered.DLM$f;
oFiltered.DLM$U.R
oFiltered.DLM$D.R
oFiltered.DLM$U.C
oFiltered.DLM$D.C

check_m_theta <- as.numeric(n);  check_m_theta2 <- as.numeric(n);
for(t in 2:(n+1)) check_m_theta[t-1] <- oFiltered.DLM$U.C[[t-1]] %*% oFiltered.DLM$D.C[t-1,]^2 %*% t(oFiltered.DLM$U.C[[t-1]])
for(t in 2:(n+1)) check_m_theta2[t-1] <- drop(dlmSvd2var(oFiltered.DLM$U.C[[t-1]], oFiltered.DLM$D.C[t-1,]))

check_a_theta <- as.numeric(n); check_a_theta2 <- as.numeric(n);
for(t in 1:n) check_a_theta[t] <- oFiltered.DLM$U.R[[t]] %*% oFiltered.DLM$D.R[t,]^2 %*% t(oFiltered.DLM$U.R[[t]])
for(t in 1:n) check_a_theta2[t] <- drop(dlmSvd2var(oFiltered.DLM$U.R[[t]], oFiltered.DLM$D.R[t,]))

innov <- oFiltered.DLM$y - oFiltered.DLM$f
( check <- data.frame(a=oFiltered.DLM$a, f=oFiltered.DLM$f, tsData=tsData, y=oFiltered.DLM$y, innov=innov, m=dropFirst(oFiltered.DLM$m), var_m_theta=check_m_theta, var_m_theta2=check_m_theta2, var_a_theta=check_a_theta, var_a_theta2=check_a_theta2) )

# ARIMAモデルによる推定 vs. ランダムウォーク・プラスノイズモデルのフィルタ化推定値と比較
# フィルタ化推定値 filt$m は　t-1 時点が起点になっているので、dropFirst で　t-1 時点をおとす
# ランムウォーク・プラスノイズモデルの方が、ARIMAモデルよりも比較的変動への追いつきが早くかつ大きい
ts.plot(result_hat, pred$pred, se1, se2, gpars=list(lt=c(2,3,4), col=c(2,3,4), ylim=range(tsData), main="ARIMAモデルでの先期間予測"))
lines(tsData, type="o", col=c("darkgrey")); abline(v=1898, lty=2);  legend(locator(1), c("観測値","モデル推定値","モデル予測値", "2*se"), lty=c(1,2,3,4), col=c("darkgrey",2,3,4));
lines(dropFirst(oFiltered.DLM$m), lty="dotdash")

# -----------------------------------------------------------------------------
# 予測分布とフィルタ化分布を算出 by カルマンフィルタ:　観測ノイズの分散は変えずに、システムノイズの分散を大きくしてみる
# mod2: W/V=0.05   mod3: W/V=0.50 に調整
oFitted2.DLM <- dlmModPoly(order=1, dV=exp(oMLE.DLM$par[1]), dW=exp(oMLE.DLM$par[1])*0.05);  oFiltered2.DLM <- dlmFilter(tsData, oFitted2.DLM);
oFitted3.DLM <- dlmModPoly(order=1, dV=exp(oMLE.DLM$par[1]), dW=exp(oMLE.DLM$par[1])*0.50);  oFiltered3.DLM <- dlmFilter(tsData, oFitted3.DLM);

# W/Vを大きくする（システムノイズを大きくする）ことで、フィルタ化値がよりデータの変動に近づいている
# 最尤推定で求められたシステムノイズと観測ノイズの分散が、データ変動を最も捉えるわけではない
dev.new(); plot(tsData, type="o", col=c("darkgrey"), xlab="", ylab="Level", main="動的線形モデルによる推定");
lines(dropFirst(oFiltered.DLM$m), lty="solid");  lines(dropFirst(oFiltered2.DLM$m), lty="longdash"); lines(dropFirst(oFiltered3.DLM$m), lty="dotdash"); abline(v=1898, lty=2)
legend <- c("data", paste("filtered,  W/V=", format(c(W(oFitted.DLM)/V(oFitted.DLM), W(oFitted2.DLM)/V(oFitted2.DLM), W(oFitted3.DLM)/V(oFitted3.DLM)))))
legend("bottomright", legend=legend, col=c("darkgrey", "black", "black", "black"), lty=c("solid", "solid", "longdash", "dotdash"), pch=c(1,NA,NA,NA), bty="n")

# -----------------------------------------------------------------------------
# モデル診断:　フィルタ化残差になんらかの相関・構造が残っていないか
# residuals() でフィルタ化結果から残差を取り出せる
qqnorm(residuals(oFiltered.DLM, sd=FALSE));  qqline(residuals(oFiltered.DLM, sd=FALSE));  dev.new();  tsdiag(oFiltered.DLM);
qqnorm(residuals(oFiltered2.DLM, sd=FALSE)); qqline(residuals(oFiltered2.DLM, sd=FALSE));  dev.new();  tsdiag(oFiltered2.DLM);
qqnorm(residuals(oFiltered3.DLM, sd=FALSE));  qqline(residuals(oFiltered3.DLM, sd=FALSE));  dev.new();  tsdiag(oFiltered3.DLM);

lPredErr.DLM     <- residuals(oFiltered.DLM, type = c("raw"), sd=TRUE)
agStdPredErr.DLM <- residuals(oFiltered.DLM, type = c("standardized"), sd=FALSE)

# 検定:  独立性 / 均一分散性 / 正規性
# nMaxLag:     (integer scalar) max lag (for BoxLjung)
# anACLag:     (integer vector) two number of lag (for ACF)
sub.ShowDiagnostics(agStdPredErr.DLM, nStateVar, nHyperParam, nMaxLag=15, anACLag=c(1,12))

# -----------------------------------------------------------------------------
# モデルの当てはまりの良さ:  -->  AICはガウスモデルのみ

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
# dlmsmooth() は、dlmFilter の結果を入力してもよいし、最初からデータ・モデルを与えてもよい（その場合は内部で dlmFlterが呼び出される）
oSmoothed.DLM <- dlmSmooth(oFiltered.DLM)
str(oSmoothed.DLM, 1)

# 平滑化分布の平均(s)、平滑化分布の分散を特異値分解(SVD)したもの
oSmoothed.DLM$s
cat(sprintf("hat(mu_1)            = %f\n", oSmoothed.DLM$s[2]))
oSmoothed.DLM$U.S
oSmoothed.DLM$D.S

# 観測期間の最終時点でのフィルタリング分散および平滑化分散 -->  一致する
# 多くの時不変モデルでは、フィルタリング分散は極限値に近づく傾向にある
drop(dlmSvd2var(oFiltered.DLM$U.C[[n+1]], oFiltered.DLM$D.C[n+1, ]))
drop(dlmSvd2var(oSmoothed.DLM$U.S[[n+1]], oSmoothed.DLM$D.S[n+1, ]))

# 観測期間の半分時点でのフィルタリング分散および平滑化分散 -->　平滑化は観測値すべてを利用するため平滑化分散の方が小さい
drop(dlmSvd2var(oFiltered.DLM$U.C[[n/2+1]], oFiltered.DLM$D.C[n/2+1, ]))
drop(dlmSvd2var(oSmoothed.DLM$U.S[[n/2+1]], oSmoothed.DLM$D.S[n/2+1, ]))

# 平滑化レベルと95%信頼区間のプロット
var <- dlmSvd2var(oSmoothed.DLM$U.S, oSmoothed.DLM$D.S)
hwid <- qnorm(0.025, lower=FALSE) * sqrt(unlist(var))
smooth <- cbind(oSmoothed.DLM$s, as.vector(oSmoothed.DLM$s)+hwid%o%c(-1,1))
plot(dropFirst(smooth), plot.type="s", type="l", lty=c(1,5,5), xlab="", ylab="Level", ylim=range(tsData))
lines(tsData, type="o", col="darkgrey")
legend("bottomleft", legend=c("data", "smoothed level", "95% probability limits"), col=c("darkgrey", rep("black", 2)), lty=c(1,1,5), pch=c(1,NA,NA), bty="n")

( check2 <- data.frame(a=oFiltered.DLM$a, f=oFiltered.DLM$f, tsData=tsData, y=oFiltered.DLM$y, innov=innov, m=dropFirst(oFiltered.DLM$m), smoothed=oSmoothed.DLM$s[2:(n+1)], var_m_theta=check_m_theta, var_m_theta2=check_m_theta2, var_a_theta=check_a_theta, var_a_theta2=check_a_theta2) )

# 時系列、フィルタ後推定値、平滑化レベルのプロット
plot(tsData, type="o", col="darkgrey")
lines(dropFirst(oFiltered.DLM$m), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="orange")
legend("topright", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "orange"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

# -----------------------------------------------------------------------------
# 大きな変動のあった時点のシステムノイズを大きく設定して、変動にキャッチアップするモデル
# システムノイズはマニュアルで調整して入れる
X <- ts(matrix(oFitted.DLM$W, nc=1, nr=length(tsData)), start=start(tsData))
window(X, 1898, 1899) <- 12*oFitted.DLM$W

# 固定モデル に対して、X, JWを追加する -->　詳細は未だ不明
oFitted4.DLM <- oFitted.DLM;  oFitted4.DLM$X <- X;  oFitted4.DLM$JW <- matrix(1,1,1);
oFiltered4.DLM <- dlmFilter(tsData, oFitted4.DLM)

a <- window(cbind(tsData, oFiltered.DLM$f, oFiltered4.DLM$f), start=1871, end=1920)
plot(a[,1], type="o", col="darkgrey", xlab="", ylab="Level", main="動的線形モデル　変動キャッチアップ")
lines(a[,2], lty="longdash"); lines(a[,3], lty="dotdash"); abline(v=1898, lty=2)
legend<-c("data", paste("one-step-ahead forecast -", c("mod", "mod4")))
legend("bottomleft", legend=legend, col=c("darkgrey", "black", "black"), lty=c("solid", "longdash", "dotdash"), pch=c(1,NA,NA), bty="n")

qqnorm(residuals(oFiltered4.DLM, sd=FALSE));  qqline(residuals(oFiltered4.DLM, sd=FALSE)); dev.new();  tsdiag(oFiltered4.DLM);

# -----------------------------------------------------------------------------
# 先期間の予測
# 信号対雑音比 W/V が最も小さい最尤推定を使った、当初の filt を使って予測　（システムノイズを小さくしたい）
# フィルタリング結果を用いて、先10期間について予測、予測はsampleNewに指定した回数実施
oFcst.DLM <- dlmForecast(oFiltered.DLM, nAhead=10, sampleNew=3)

# 状態の予測分布の期待値(a)、分散(R)
oFcst.DLM$a; oFcst.DLM$R;
# 観測対象の予測分布の期待値(f)、分散(Q)
oFcst.DLM$f; oFcst.DLM$Q;
# 状態の予測値(newStates)、観測対象の予測値(newObs)、いずれも予測実施回数分の系列
oFcst.DLM$newStates
oFcst.DLM$newObs

# ランダムウォーク・プラスノイズモデルなので観測対象の予測分布の期待値は一定値であるが、予測値そのものはノイズのため実施回数ごとに大きく変わる
dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="動的線形モデルでの先期間予測", xlim=c(1950, 1980))
invisible(lapply(oFcst.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst.DLM$f, type="o", lwd=1, pch=4)
abline(v=mean(c(time(oFcst.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")

# 90%予測区間 --> toDo

# -----------------------------------------------------------------------------
# 補足:　欠測値の補間 --> toDo


###############################################################################
## ----------------------------------------------------------------------------
## 動的線形モデル:　ローカルレベルモデル（ランダムウォーク・プラスノイズモデル）:  KFAS　パッケージ
## 参照:  野村本
## ----------------------------------------------------------------------------
###############################################################################
# nStatevar:  状態方程式の数
nStateVar <- 1;  nHyperParam <- 2;
n <- length(tsData)

# -----------------------------------------------------------------------------
# システムノイズ、観測ノイズを最尤推定法で推定

# ランダムウォーク・プラスノイズモデルは、SSMtrend の次数=1　とする
# H: 観測ノイズの分散　　Q: システムノイズの分散
# fitSSM にて最尤推定
# 初期値を、時系列全体での分散の対数　として設定 -->  初期値をゼロで与えると全く異なる分散となるため注意、また対数にしないとオーバーフローなどでうまく計算できない
( oModel.KFAS <- SSModel(tsData ~ SSMtrend(1, Q = list(matrix(NA))), H = matrix(NA)) )
( oFitted.KFAS <- fitSSM(oModel.KFAS, init=c(log(var(tsData)), log(var(tsData))), method = "BFGS") )

# 最尤推定が収束していることを確認 （$convergence=0 となっていることを確認）
oFitted.KFAS$optim.out

# 推定された観測ノイズ、システムノイズの分散
drop(oFitted.KFAS$model$H);  drop(oFitted.KFAS$model$Q);

# 信号対雑音比確認 W/V --> 観測ノイズが随分と大きい
drop(oFitted.KFAS$model$Q) / drop(oFitted.KFAS$model$H)

# -----------------------------------------------------------------------------
# 予測分布　および　フィルタ化分布　の算出:　すでに fitSSM にて終了


# -----------------------------------------------------------------------------
# フィルタリング　および　平滑化
# KFS は、指数族の状態空間モデルに対して univariate approach で散漫初期化 exact diffuse initialization を用いてカルマンフィルタリングと平滑化を実施
# filtering:  ガウスモデルではデフォルトは "state"　（signal は mean)　　非ガウスモデルでは "none"
# smoothing:  デフォルトは "state" and "mean"　（ガウスモデルでは "mean" は "signal" と同じ）　　非ガウスモデルでは "disturbance"　は適用なし
( oEstimated.KFAS <- KFS(oFitted.KFAS$model, filtering = "state", smoothing = "state") )

# 状態の1期先予測
oEstimated.KFAS$a
# 状態の平滑化推定値
oEstimated.KFAS$alphahat; oEstimated.KFAS$alphahat[1];

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

# 検定:  独立性 / 均一分散性 / 正規性
# nMaxLag:     (integer scalar) max lag (for BoxLjung)
# anACLag:     (integer vector) two number of lag (for ACF)
sub.ShowDiagnostics(agStdPredErr.KFAS, nStateVar, nHyperParam, nMaxLag=15, anACLag=c(1,12))

# -----------------------------------------------------------------------------
# モデルの当てはまりの良さ:  -->  AICはガウスモデルのみ
nStateVar <- 1;  nHyperParam <- 2;
n <- length(tsData)

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
# alphahat と pred/conf の fit が同じ値であることを確認
check <- cbind(data.frame(a=oEstimated.KFAS$a[-1,], alphahat=oEstimated.KFAS$alphahat[,]), as.data.frame(conf), as.data.frame(pred))
head(check);  tail(check);

# -----------------------------------------------------------------------------
# 先期間の予測
# predict() で先区間を予測、結果をそのままts.plot() に渡すと描画
oModel2.KFAS <- SSModel(tsData ~ SSMtrend(1, Q=list(oFitted.KFAS$model$Q)),H=oFitted.KFAS$model$H)
( oFcst.KFAS <- predict(oModel2.KFAS, n.ahead=10, interval="prediction",　level=0.9) )
ts.plot(oFcst.KFAS, col=c(1,2,3));


# -----------------------------------------------------------------------------
# 補足:　欠測値の補間
tsDataNA <- tsData
tsDataNA[c(21:40, 61:80)] <- NA
ts.plot(tsDataNA)

# 欠測値を除いた時系列の分散を初期値に与え、再度観測ノイズとシステムノイズの分散を推定
# KFS() の結果を fitted() に入力すると、補間された観測値(fit)、フィルタ化推定値(fit_filt)を出力
oModel_NA.KFAS <- SSModel(tsDataNA ~ SSMtrend(1, Q = list(matrix(NA))), H = matrix(NA))
oFitted_NA.KFAS <- fitSSM(oModel_NA.KFAS, init=c(log(var(tsDataNA, na.rm=TRUE)), log(var(tsDataNA, na.rm=TRUE))), method = "BFGS")
oFitted_NA.KFAS$model$Q;  oFitted_NA.KFAS$model$H;

oEstimated_NA.KFAS <- KFS(oFitted_NA.KFAS$model, filtering="mean", smoothing="mean")
( fit <- fitted(oEstimated_NA.KFAS, filtered = TRUE) )
( fit_filt <- fitted(oEstimated_NA.KFAS) )
ts.plot(tsDataNA, fit, fit_filt, col=1:3, lty=1:3, ylab = "Predicted Annual flow", main = "River Nile")

( check <- cbind(data.frame(tsData=tsData, fit=fit, fit_filt=fit_filt)) )
