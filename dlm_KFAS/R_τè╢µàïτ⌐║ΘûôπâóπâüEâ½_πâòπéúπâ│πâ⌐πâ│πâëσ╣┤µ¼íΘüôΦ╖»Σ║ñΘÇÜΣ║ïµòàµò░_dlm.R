# 状態空間モデル　by dlm
# フィンランドの年次道路交通事故数
# ARMAモデル　vs.　ローカル線形トレンドモデル

setwd("C:\\Users\\R_work")
require(dlm)
source("C:\\Users\\光世\\Desktop\\#Py_R_信号解析_時系列分析\\R_状態空間モデル_サポート関数.R")


###############################################################################
# データ読み込み、基礎俯瞰
###############################################################################
# データ読み込み
dfData <- read.table("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\OxCodeIntroStateSpaceBook\\Chapter_3\\NorwayFinland.txt", skip=1)
anTime <- as.integer(dfData[,1])

par(mfrow=c(2,1))
ts.plot(dfData[,3]); ts.plot(log(dfData[,3]));

# 減少トレンドがあり、対数変換したデータを使う
tsData <- ts(log(dfData[,3]), start=anTime[1])
plot(tsData)

n <- length(tsData)


###############################################################################
# 基礎分析:　自己相関、偏自己相関など
###############################################################################
# 1～4での自己相関がみられる
dev.new();  par(mfrow=c(2,1));  acf(tsData, lag.max=30);  pacf(tsData, lag.max=30);

# 一階差分には自己相関なし、定常過程とみることができそう --> ARMA(1,0,4)を想定
plot(diff(tsData), type="o", col=c("darkgrey"), xlab="", ylab="Level")
dev.new();  par(mfrow=c(2,1));  acf(diff(tsData), lag.max=30);  pacf(diff(tsData), lag.max=30);


###############################################################################
# ARMA(1,0,4)モデル
###############################################################################
p<-1; d<-0; q<-4
result <- arima(tsData, order=c(p,d,q), transform.pars=FALSE)
result
# モデルの残差の分散、対数尤度、AIC、残差
result$sigma2;  result$loglik;  result$aic;  result$res;

# -----------------------------------------------------------------------------
# モデル診断
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
# ローカル線形トレンドモデル
###############################################################################
nStateVar <- 1;  nHyperParam <- 3;
funModel <- function(parm){ dlmModPoly(order=2, dV=exp(parm[1]), dW=exp(parm[2:3])) }
oMLE.DLM <- dlmMLE(tsData, parm=log(c(var(tsData),0.001,0.001)), build=funModel, hessian=T)

oMLE.DLM$convergence
oMLE.DLM$par;  exp(oMLE.DLM$par[1]);  exp(oMLE.DLM$par[2]);  exp(oMLE.DLM$par[3]);

# t=0時点の 状態ベクトルの正規事前分布　theta0 ~ N(m0, C0)
# 観測方程式:  Yt = Ft * thetat + vt,  vt ~ N(0, Vt)
# システム方程式:  thetat ~ Gt * thetat-1 + wt,  wt ~ N(0, Wt)
# F(Ftを指す), GG(Gtを指す)　を確認
oFitted.DLM <- funModel(oMLE.DLM$par)

drop(oFitted.DLM$V)
diag(oFitted.DLM$W)[1];  diag(oFitted.DLM$W)[2];

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
# モデルの当てはまりの良さ -->　ARMAモデルの方がよい??（ARMAも　0.5*n*log(2*pi) を調整する必要あり？）

# ARMAモデルの対数尤度とAIC
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

# 平滑化分布の平均(s)、平滑化分布の分散を特異値分解(SVD)したもの
oSmoothed.DLM$s


# -----------------------------------------------------------------------------
# プロット
# 時系列、フィルタ後推定値、平滑化レベルのプロット
par(mfrow=c(1,1))
plot(tsData, type="o", col="darkgrey")
lines(dropFirst(oFiltered.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s[,1]), lty=2, col="blue")
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
# 先期間の予測
# ARMAモデルではトレンドを考慮しない予測をしてしまうが、ローカル線形トレンドモデルではトレンドが反映される（あまり面白くない予測になる）
oFcst.DLM <- dlmForecast(oFiltered.DLM, nAhead=10, sampleNew=3)

dev.new(); par(mfrow=c(1,1));
plot(tsData, type="o", col="darkgrey", xlab="", ylab="Level", main="ローカル線形トレンドモデルでの先期間予測", xlim=c(1970, 2013), ylim=c(5,7))
invisible(lapply(oFcst.DLM$newObs, function(x) lines(x, col="lightgrey", type="o", pch=4)))
lines(oFcst.DLM$f, type="o", lwd=1, pch=4)
abline(v=mean(c(time(oFcst.DLM$f)[1], time(tsData)[length(tsData)])), lty="dashed")
lines(dropFirst(oFiltered.DLM$m[,1]), lty=1, col="black")
lines(dropFirst(oSmoothed.DLM$s[,1]), lty=2, col="blue")
lines(oFiltered.DLM$f, lty=3, col="red")

dev.new(); par(mfrow=c(1,1));
ts.plot(result_hat, pred$pred, se1, se2, gpars=list(lt=c(2,3,4), col=c(2,3,4), ylim=range(tsData), main="ARIMAモデルでの先期間予測"))
lines(tsData, type="o", col=c("darkgrey")); abline(v=1898, lty=2);  legend(locator(1), c("観測値","モデル推定値","モデル予測値", "2*se"), lty=c(1,2,3,4), col=c("darkgrey",2,3,4));
