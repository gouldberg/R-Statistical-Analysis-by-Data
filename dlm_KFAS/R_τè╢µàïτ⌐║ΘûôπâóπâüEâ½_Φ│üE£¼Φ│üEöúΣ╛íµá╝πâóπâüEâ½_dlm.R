# 状態空間モデル　by dlm
# 資本資産価格モデル
# 動的回帰モデル:  CAPMモデルの係数αとβが時間で変化 yt = αt + βt * xt + vt,  vt ~ N(0, sigma^2)

setwd("C:\\Users\\R_work")
require(dlm)
source("C:\\Users\\光世\\Desktop\\#Py_R_信号解析_時系列分析\\R_状態空間モデル_サポート関数.R")


###############################################################################
# データ読み込み、基礎俯瞰
###############################################################################
dfData <- read.table("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_statespace_petris\\P.dat", header=T)
dftsData <- ts(dfData, start=c(1978,1), frequency=12)

par(mfrow=c(1,1));  plot(dftsData);

# IBM株データを取り上げる
tsData <- dftsData[,"IBM"] - dftsData[, "RKFREE"]
X <- dftsData[,"MARKET"] - dftsData[, "RKFREE"]

par(mfrow=c(2,1));  plot(tsData);  plot(X);

n <- length(tsData)


###############################################################################
# 基礎分析:　自己相関、偏自己相関など
###############################################################################
# 自己相関はないが、若干の周期性がみられる
dev.new();  par(mfrow=c(2,1));  acf(tsData, lag.max=30);  pacf(tsData, lag.max=30);
dev.new();  plot(decompose(tsData, type="additive"))

plot(diff(tsData), type="o", col=c("darkgrey"), xlab="", ylab="Level")
dev.new();  par(mfrow=c(2,1));  acf(diff(tsData), lag.max=30);  pacf(diff(tsData), lag.max=30);
dev.new();  plot(decompose(diff(tsData), type="additive"))


###############################################################################
# 古典的回帰モデル: CAPMモデル
###############################################################################
outLM <- lm(tsData ~ X)
outLM$coef

plot(outLM)

dev.new();  par(mfrow=c(2,1));  acf(outLM$res, lag.max=30);  pacf(outLM$res, lag.max=30);
qqnorm(outLM$res);  qqline(outLM$res);


###############################################################################
# 状態空間モデルによる回帰係数のベイズ推定
###############################################################################
# 説明変数 X についてモデル生成
mod <- dlmModReg(X, dV=var(tsData), m0=c(0, 1.5), C0=diag(c(1e+07, 1)))

# 従属変数について、上記モデルでフィルタリング
outF <- dlmFilter(tsData, mod)

# 観測期間の最終時点でのフィルタリング推定値が、二次損失関数の下での回帰係数のベイズ推定となる
outF$m[1 + n, ]

# 古典的回帰モデルでのOLS推定値と非常に近い値になっている
outLM$coef
outF$m[1 + n, ]


###############################################################################
# 回帰係数は独立にランダムウォークに従うモデル
# 動的回帰モデル:  CAPMモデルの係数αとβが時間で変化 yt = αt + βt * xt + vt,  vt ~ N(0, sigma^2)
###############################################################################
# 説明変数 X についてモデル記述
# パラメータの初期値は、parm=log(c(0,0,0)) ではなく c(0,0,0)　とする
nStateVar <- 2;  nHyperParam <- 3;
funModel <- function(parm){ dlmModReg(X, dV=exp(parm[1]), dW=exp(parm[2:3])) }
oMLE.DLM <- dlmMLE(tsData, parm=c(0,0,0), build=funModel, hessian=T)

oMLE.DLM$convergence
oMLE.DLM$par;  exp(oMLE.DLM$par);

oFitted.DLM <- funModel(oMLE.DLM$par)

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
sub.ShowDiagnostics(agStdPredErr.DLM, nStateVar, nHyperParam, nMaxLag=20, anACLag=c(1,12))

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
oSmoothed.DLM$s


# -----------------------------------------------------------------------------
# 回帰係数をプロット
plot(dropFirst(oSmoothed.DLM$s))


# -----------------------------------------------------------------------------
# 説明変数X と　従属変数 tsData のプロット
# 時系列、フィルタ後推定値、平滑化レベルのプロット
dev.new()
par(mfrow=c(2,1))
m <- dropFirst(oFiltered.DLM$m[,1]) + dropFirst(oFiltered.DLM$m[,2])*X + oFitted.DLM$V
s <- dropFirst(oSmoothed.DLM$s[,1]) + dropFirst(oSmoothed.DLM$s[,2])*X + oFitted.DLM$V
plot(tsData, type="o", col="darkgrey", main="CAPMの回帰係数αとβが時間変化するモデル")
lines(m, lty=1, col="black")
lines(s, lty=2, col="blue")
lines(oFiltered3.DLM$f, lty=3, col="red")
legend("topright", legend=c("data", "filtered", "smoothed", "one-step-ahead fcst"), col=c("darkgrey", "black", "blue", "red"), pch=c(1,NA,NA,NA), lty=c(1,1,2,3), bty="n")

plot(X)
