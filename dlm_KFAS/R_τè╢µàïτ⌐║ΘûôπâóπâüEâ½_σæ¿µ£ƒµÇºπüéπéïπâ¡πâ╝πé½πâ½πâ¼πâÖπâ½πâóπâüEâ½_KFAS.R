# 周期性あるローカルレベルモデル

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
# フーリエ形式の周期性あるローカルレベルモデル
# ノッティンガムの月平均気温
#
# モデル1:　全調和項モデル
# モデル2:　縮小モデル（2調和項のみ、高次調和項を除く）
#
# 高次調和項は、データにおける雑音への適合に用いられ、標本外の1期先予測の適合には汎用性がない
# ###############################################################################
data(nottem);  dfData <- nottem;

# 非常に明瞭な周期性がわかる
# それほど高周波数成分はないようだ
par(mfrow=c(2,1))
ts.plot(dfData); ts.plot(log(dfData));

tsData <- dfData

# -----------------------------------------------------------------------------
# 自己相関、偏自己相関の確認 -->　強い周期性は差分でも消えない
dev.new();  par(mfrow=c(2,1));  acf(tsData, lag.max=30);  pacf(tsData, lag.max=30);
dev.new();  plot(decompose(tsData, type="additive"))

plot(diff(tsData), type="o", col=c("darkgrey"), xlab="", ylab="Level")
dev.new();  par(mfrow=c(2,1));  acf(diff(tsData), lag.max=30);  pacf(diff(tsData), lag.max=30);
dev.new();  plot(decompose(diff(tsData), type="additive"))

n <- length(tsData)

# -----------------------------------------------------------------------------
nStateVar <- 4????;  nHyperParam <- 2;

# ModTrig でフーリエ周期モデル
funModel <- function(parm){
  m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=0)
  m2 <- dlmModTrig(s=12, dV=0, dW=exp(parm[2]))
  return( m1 + m2 )
}
oMLE.DLM <- dlmMLE(tsData, parm=log(c(var(tsData), 0.001)), build=funModel, hessian=T)

oMLE.DLM$convergence
oMLE.DLM$par;  exp(oMLE.DLM$par[1]);  exp(oMLE.DLM$par[2]);

oFitted.DLM <- funModel(oMLE.DLM$par)

# -----------------------------------------------------------------------------
# 平滑化し、周期性成分をプロット
# dlmSmooth は、モデルそのものを与えると内部で dlmFilter も呼び出される
oSmoothed.DLM <- dlmSmooth(tsData, oFitted.DLM)
plot(ts(oSmoothed.DLM$s[2:13, c(1,3,5,7,9,11)], names=paste("S", 1:6, sep="_")), oma.multi=c(2,0,1,0), pch=16, nc=1, yax.flip=TRUE, type="o", xlab="", main="")

# -----------------------------------------------------------------------------
# モデル見直し:　縮小モデル
# 2番目以降の調和項は、相対的に振幅が小さく、最初の2つの調和項だけをローカルレベルに加えたモデルを試してみる
# q で調和項に残す数を指定する
funModel2 <- function(parm){
  m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=0)
  m2 <- dlmModTrig(s=12, q = 2, dV=0, dW=exp(parm[2]))
  return( m1 + m2 )
}
oMLE2.DLM <- dlmMLE(tsData, parm=log(c(var(tsData), 0.001)), build=funModel2, hessian=T)
oMLE2.DLM$convergence
oMLE2.DLM$par;  exp(oMLE2.DLM$par[1]);  exp(oMLE2.DLM$par[2]);

oFitted2.DLM <- funModel2(oMLE2.DLM$par)

# MAPE　を比較すると、悪化せず、逆にわずかに減少していることがわかる
# 高次調和項を除いたことで、1期先予測能力が若干改善している
# 高次調和項は、データにおける雑音への適合に用いられ、標本外の1期先予測の適合には汎用性がない
mean(abs(residuals(dlmFilter(tsData, oFitted.DLM), type="raw", sd=FALSE)) / tsData)
mean(abs(residuals(dlmFilter(tsData, oFitted2.DLM), type="raw", sd=FALSE)) / tsData)

# -----------------------------------------------------------------------------
# 縮小モデルの設定
oFiltered.DLM <- dlmFilter(tsData, oFitted2.DLM)
oSmoothed.DLM <- dlmSmooth(oFiltered.DLM)

# 時系列、フィルタ後推定値、平滑化レベルのプロット　および　季節要素
par(mfrow=c(2,1))
plot(tsData, type="o", col="darkgrey")
lines(dropFirst(oFiltered.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

plot(dropFirst(oSmoothed.DLM$s[,2]), type="o", xlab="", ylab="seasonal component")
abline(h=0, lty=2, col="darkgrey")

dev.new()
par(mfrow=c(2,1))
plot(dropFirst(oSmoothed.DLM$s[,3]), type="o", xlab="", ylab="seasonal component")
abline(h=0, lty=2, col="darkgrey")
plot(dropFirst(oSmoothed.DLM$s[,4]), type="o", xlab="", ylab="seasonal component")
abline(h=0, lty=2, col="darkgrey")

# -----------------------------------------------------------------------------
# 先期間の予測
# フィルタリング結果を用いて、先8期間について予測、予測はsampleNewに指定した回数実施
oFcst.DLM <- dlmForecast(oFiltered.DLM, nAhead=24, sampleNew=3)

# 予測値そのものはノイズのため実施回数ごとに大きく変わる
dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="先期間予測", xlim=c(1920, 1942))
invisible(lapply(oFcst.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst.DLM$f, type="o", lwd=1, pch=1)
abline(v=mean(c(time(oFcst.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")


###############################################################################
# 一般周期成分モデル:　観測値に内在する過程の周期が、連続する観測時点間の整数倍でない場合
# 太陽黒点データ
# データからレベル(シグナル)を抽出する
###############################################################################
data(sunspots);  dfData <- sunspots;

# 平方根の時系列も確認
par(mfrow=c(2,1))
ts.plot(dfData); ts.plot(sqrt(dfData));

tsData <- dfData

# -----------------------------------------------------------------------------
# 自己相関、偏自己相関の確認
dev.new();  par(mfrow=c(2,1));  acf(tsData, lag.max=12*20);  pacf(tsData, lag.max=30);
dev.new();  plot(decompose(tsData, type="additive"))

plot(diff(tsData), type="o", col=c("darkgrey"), xlab="", ylab="Level")
dev.new();  par(mfrow=c(2,1));  acf(diff(tsData), lag.max=12*20);  pacf(diff(tsData), lag.max=30);
dev.new();  plot(decompose(diff(tsData), type="additive"))

n <- length(tsData)

# -----------------------------------------------------------------------------
nStateVar <- 4????;  nHyperParam <- 4;

# 一般的に q=2: 周期成分2つ（調和項2つ分）　とし、周期 tau をパラメータとする
funModel <- function(parm){
  m1 <- dlmModPoly(order = 1, dV=exp(parm[1]), dW=exp(parm[2]))
  m2 <- dlmModTrig(q=2, tau=exp(parm[3]), dV=0, dW=rep(c(exp(parm[4]), exp(parm[5])),each=2))
  return( m1 + m2 )
}

# パラメータの最尤推定 -->　どうも正解に近いパラメータが推定されない・・・
# データはオリジナルではなく、平方根スケールにする
# tau に対する初期値は、11年周期を考慮し近い値に設定する
oMLE.DLM <- dlmMLE(sqrt(tsData), parm=log(c(var(sqrt(tsData)), 0.1, 12*11, 10, 0.1)), build=funModel, hessian=T)
oMLE.DLM$convergence
oMLE.DLM$par;  exp(oMLE.DLM$par[1]);  exp(oMLE.DLM$par[2]);  exp(oMLE.DLM$par[3]);  exp(oMLE.DLM$par[4]);  exp(oMLE.DLM$par[5]);

oFitted.DLM <- funModel(oMLE.DLM$par)

# 本当はこちらが正解らしい
oFitted.DLM <- dlmModTrig(q=2, tau=130.51, dV=0, dW=rep(c(1765e-2, 3102e-4), each=2)) + dlmModPoly(1, dV=0.7452, dW=0.1606)


# -----------------------------------------------------------------------------
# フィルタリング、平滑化
oFiltered.DLM <- dlmFilter(sqrt(tsData), oFitted.DLM)
oSmoothed.DLM <- dlmSmooth(oFiltered.DLM)

tmp <- tcrossprod(dropFirst(oSmoothed.DLM$s[,c(1,3,5)]), matrix(c(0,0,1,1,1,0), nr=2, byrow=TRUE))
y <- cbind(sqrt(tsData), tmp)
colnames(y) <- c("Sunspots", "Level", "Periodic")
plot(y, yax.flip=TRUE, oma.multi=c(2,0,1,0))

# 時系列、フィルタ後推定値、平滑化レベルのプロット　および周期要素
par(mfrow=c(2,1))
plot(sqrt(tsData), type="o", col="darkgrey")
lines(dropFirst(oFiltered.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")
legend("topleft", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

plot(dropFirst(oSmoothed.DLM$s[,2]), type="o", xlab="", ylab="seasonal component")
abline(h=0, lty=2, col="darkgrey")

dev.new()
par(mfrow=c(2,1))
plot(dropFirst(oSmoothed.DLM$s[,3]), type="o", xlab="", ylab="seasonal component")
abline(h=0, lty=2, col="darkgrey")
plot(dropFirst(oSmoothed.DLM$s[,4]), type="o", xlab="", ylab="seasonal component")
abline(h=0, lty=2, col="darkgrey")
