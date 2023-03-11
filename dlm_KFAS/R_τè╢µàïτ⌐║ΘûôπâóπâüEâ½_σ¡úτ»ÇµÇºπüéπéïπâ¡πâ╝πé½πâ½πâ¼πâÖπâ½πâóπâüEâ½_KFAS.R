# 季節性あるローカルレベルモデル by KFAS

setwd("C:\\Users\\R_work")
require(KFAS)

source("C:\\Users\\光世\\Desktop\\#Py_R_信号解析_時系列分析\\R_状態空間モデル_サポート関数.R")

###############################################################################
# 季節性あるローカルレベルモデル by KFAS
# 英国のドライバー死傷者数
# モデル1:  レベルと季節の攪乱項はゼロに固定するモデル (nStatevar=12,  nHyperPram=1)
# モデル2:  レベルと季節要素が時間で変化する (nStatevar=12, nHyperparam=3)、システムノイズとして表現
#
# 残念ながらモデル2でも、後半のレベル変化に1期先予測が追いついていっていない
###############################################################################
# データ読み込み
dfData <- read.table("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\OxCodeIntroStateSpaceBook\\Chapter_4\\UKdriversKSI.txt", skip=1)

par(mfrow=c(2,1))
ts.plot(dfData[,1]); ts.plot(log(dfData[,1]));

# 対数変換したデータを使う (平方根でもよいように思うが）
tsData <- ts(log(dfData[,1]), frequency=12, start=c(1969, 1))
n <- length(tsData)

# -----------------------------------------------------------------------------
# 12ヶ月サイクルが確認できる
dev.new();  par(mfrow=c(2,1));  acf(tsData, lag.max=30);  pacf(tsData, lag.max=30);
dev.new();  plot(decompose(tsData, type="additive"))

# 一階差分をとって、同様に確認
plot(diff(tsData), type="o", col=c("darkgrey"), xlab="", ylab="Level")
dev.new();  par(mfrow=c(2,1));  acf(diff(tsData), lag.max=30);  pacf(diff(tsData), lag.max=30);
dev.new();  plot(decompose(diff(tsData), type="additive"))


# -----------------------------------------------------------------------------
# モデル1:  レベルと季節の攪乱項はゼロに固定するモデル
# nStatevar:  状態方程式の数、1本はレベル、11本は季節要素
nStateVar <- 12;  nHyperParam <- 1;

# H: 観測ノイズの分散　　Q: システムノイズの分散
( oModel.KFAS <- SSModel( tsData ~ SSMtrend(degree=1, Q=list(matrix(0))) + SSMseasonal(12, sea.type="dummy"), H=matrix(NA)) )
( oFitted.KFAS <- fitSSM(oModel.KFAS, init=log(c(var(tsData))), method = "BFGS") )

# $convergence=0 を確認
oFitted.KFAS$optim.out

drop(oFitted.KFAS$model$H);  drop(oFitted.KFAS$model$Q);
cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted.KFAS$model$H)))

# 信号対雑音比確認 W/V --> システムノイズはゼロ固定のため関係なし

# -----------------------------------------------------------------------------
# モデル2:  レベルと季節要素が時間で変化する -->  システムノイズとして表現
nStateVar2 <- 12;  nHyperParam2 <- 3;

( oModel2.KFAS <- SSModel( tsData ~ SSMtrend(degree=1, Q=list(matrix(NA))) + SSMseasonal(12, sea.type="dummy", Q=matrix(NA), n=n), H=matrix(NA)) )
( oFitted2.KFAS <- fitSSM(oModel2.KFAS, init=log(c(var(tsData), 0.001, 0.001)), method = "BFGS") )

oFitted2.KFAS$optim.out

drop(oFitted2.KFAS$model$H);  drop(oFitted2.KFAS$model$Q);
agQ            <- diag(oFitted2.KFAS$model$Q[,,1])
cat(sprintf("hat(sigma^2_epsilon) = %f\n", drop(oFitted2.KFAS$model$H)))
cat(sprintf("hat(sigma^2_xi)      = %e\n", agQ[1]))
cat(sprintf("hat(sigma^2_omega)   = %e\n", agQ[2]))

# 信号対雑音比確認 W/V
diag(drop(oFitted2.KFAS$model$Q))[1] / drop(oFitted2.KFAS$model$H)
diag(drop(oFitted2.KFAS$model$Q))[2] / drop(oFitted2.KFAS$model$H)

# -----------------------------------------------------------------------------
# フィルタリング　および　平滑化
# KFS は、指数族の状態空間モデルに対して univariate approach で散漫初期化 exact diffuse initialization を用いてカルマンフィルタリングと平滑化を実施
# filtering:  ガウスモデルではデフォルトは "state"　（signal は mean)　　非ガウスモデルでは "none"
# smoothing:  デフォルトは "state" and "mean"　（ガウスモデルでは "mean" は "signal" と同じ）　　非ガウスモデルでは "disturbance"　は適用なし
( oEstimated.KFAS <- KFS(oFitted.KFAS$model, filtering = "state", smoothing = "state") )
cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated.KFAS)[1,1]))
cat(sprintf("hat(gamma_1)         = %f\n", coef(oEstimated.KFAS)[1,2]))

( oEstimated2.KFAS <- KFS(oFitted2.KFAS$model, filtering = "state", smoothing = "state") )
cat(sprintf("hat(mu_1)            = %f\n", coef(oEstimated2.KFAS)[1,1]))
cat(sprintf("hat(gamma_1)         = %f\n", coef(oEstimated2.KFAS)[1,2]))


# 水準成分と季節ダミーのプロット
par(mfrow=c(2,1));
plot(tsData, lty=3, type="o", ylab="水準成分")
lines(oEstimated.KFAS$alphahat[,"level"], lwd=2)
ts.plot(oEstimated.KFAS$alphahat[,2:11], ylab="季節ダミー")

dev.new()
par(mfrow=c(2,1));
plot(tsData, lty=3, type="o", ylab="水準成分")
lines(oEstimated2.KFAS$alphahat[,"level"], lwd=2)
ts.plot(oEstimated2.KFAS$alphahat[,2:11], ylab="季節ダミー")

# -----------------------------------------------------------------------------
# モデル診断:　平滑化後残差になんらかの相関・構造が残っていないか
# 検定:  独立性 / 均一分散性 / 正規性
# nMaxLag:     (integer scalar) max lag (for BoxLjung)
# anACLag:     (integer vector) two number of lag (for ACF)

# state: 平滑化状態攪乱項に基づく誤差　　pearson: ガウスモデルの場合は standardized smoothed ε disturbance residuals  一般線形モデルの場合は標準化ピアソン残差
( oEstimated.KFAS <- KFS(oFitted.KFAS$model, smoothing = c("state", "mean", "disturbance")) )
plot(cbind(state = rstandard(oEstimated.KFAS, "state"), recursive = rstandard(oEstimated.KFAS), irregular = rstandard(oEstimated.KFAS, "pearson")), main = "recursive and auxiliary residuals")
# dev.new(); plot(oFitted.KFAS$model);

( oEstimated2.KFAS <- KFS(oFitted2.KFAS$model, smoothing = c("state", "mean", "disturbance")) )
plot(cbind(state = rstandard(oEstimated2.KFAS, "state"), recursive = rstandard(oEstimated2.KFAS), irregular = rstandard(oEstimated2.KFAS, "pearson")), main = "recursive and auxiliary residuals")
# dev.new(); plot(oFitted2.KFAS$model);

( agStdPredErr.KFAS <- oEstimated.KFAS$v[,1] / sqrt(t(oEstimated.KFAS$F)) )
( agStdPredErr2.KFAS <- oEstimated2.KFAS$v[,1] / sqrt(t(oEstimated2.KFAS$F)) )

sub.ShowDiagnostics(agStdPredErr.KFAS, nStateVar, nHyperParam, nMaxLag=15, anACLag=c(1,12))
sub.ShowDiagnostics(agStdPredErr2.KFAS, nStateVar2, nHyperParam2, nMaxLag=15, anACLag=c(1,12))


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

# モデルの対数尤度　および　AIC
( gLLbyFun2.KFAS <- oEstimated2.KFAS$logLik )
sub.AIC(gLLbyFun2.KFAS, nStateVar2, nHyperParam2)
sub.AIC(gLLbyFun2.KFAS, nStateVar2, nHyperParam2)/n

# 予測誤差？？の対数尤度 および AIC
# v[,1]: 予測誤差  F:  予測誤差分散
( gLLbyErr2.KFAS <- sub.LogLik(oEstimated2.KFAS$v[,1], t(oEstimated2.KFAS$F), nStateVar2) )
sub.AIC(gLLbyErr2.KFAS, nStateVar2, nHyperParam2)
sub.AIC(gLLbyErr2.KFAS, nStateVar2, nHyperParam2)/n


# -----------------------------------------------------------------------------
# 平滑化レベルと95%信頼区間のプロット
( conf <- predict(oFitted.KFAS$model, interval = "confidence", level = 0.95) )
( pred <- predict(oFitted.KFAS$model, interval = "prediction", level = 0.95) )
( conf2 <- predict(oFitted2.KFAS$model, interval = "confidence", level = 0.95) )
( pred2 <- predict(oFitted2.KFAS$model, interval = "prediction", level = 0.95) )

par(mfrow=c(2,1))
ts.plot(cbind(tsData, pred, conf[, -1]), col = c("black", "darkgrey", "red", "red", "blue", "blue"), lty=c(1,1,3,3,2,2), lwd=c(1,2,1,1,1,1), ylab = "Predicted", main = "モデル1")
ts.plot(cbind(tsData, pred, conf2[, -1]), col = c("black", "darkgrey", "red", "red", "blue", "blue"), lty=c(1,1,3,3,2,2), lwd=c(1,2,1,1,1,1), ylab = "Predicted", main = "モデル2")


# -----------------------------------------------------------------------------
# 先期間の予測 -->　エラーになるので検討、Qにどうやって渡せばよいのか
# predict() で先区間を予測、結果をそのままts.plot() に渡すと描画
# q1 <- diag(drop(oFitted.KFAS$model$Q))[1]
# q2 <- diag(drop(oFitted.KFAS$model$Q))[2]
# oModel2.KFAS <- SSModel(tsData ~ SSMtrend(2, Q=list(drop(oFitted.KFAS$model$Q))),H=list(oFitted.KFAS$model$H))
# ( oFcst.KFAS <- predict(oModel2.KFAS, n.ahead=10, interval="prediction",　level=0.9) )
# ts.plot(oFcst.KFAS, col=c(1,2,3));


###############################################################################
# 季節性あるローカルレベルモデル by KFAS
# アパレル販売データ
# モデル1:  季節要素は固定のモデル
# モデル2:  レベルと季節要素が時間で変化する
###############################################################################
# データ読み込み
dfData <- read.csv("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\sales.csv")
head(dfData)
plot(ts(dfData))

par(mfrow=c(2,1))
ts.plot(dfData[,2]); ts.plot(log(dfData[,2]));

# ts型にしなくても dlm では扱えるが、ここではしておく
# ts型にすると decompose が使える、plot() でx軸を時間指定することができる
tsData <- dfData[,2]
tsData <- ts(tsData, start=c(2002,1), frequency=12)

n <- length(tsData)

# -----------------------------------------------------------------------------
# 6ヶ月と12ヶ月の周期性が確認できる
dev.new();  par(mfrow=c(2,1));  acf(tsData, lag.max=30);  pacf(tsData, lag.max=30);
dev.new();  plot(decompose(tsData, type="additive"))

# 一階差分をとって、同様に確認
plot(diff(tsData), type="o", col=c("darkgrey"), xlab="", ylab="Level")
dev.new();  par(mfrow=c(2,1));  acf(diff(tsData), lag.max=30);  pacf(diff(tsData), lag.max=30);
dev.new();  plot(decompose(diff(tsData), type="additive"))

# -----------------------------------------------------------------------------
# モデル1:  ダミー変数型(固定) の季節成分モデル(季節変動が固定の場合)
( oModel.KFAS <- SSModel(tsData ~ SSMtrend(degree=2, Q = c(list(0), list(NA))) + SSMseasonal(12, sea.type="dummy"), H = NA) )
( oFitted.KFAS <- fitSSM(oModel.KFAS, numeric(2), method = "BFGS") )
oFitted.KFAS$optm.out
( oEstimated.KFAS <- KFS(oFitted.KFAS$model, filtering = "state", smoothing = "state") )

# -----------------------------------------------------------------------------
# モデル2:  ダミー変数型(変化) の季節成分モデル(季節変動が変化する場合)
( oModel2.KFAS <- SSModel(tsData ~ SSMtrend(degree=2, Q = c(list(0), list(NA))) + SSMseasonal(12, sea.type="dummy", Q=NA), H = NA) )
( oFitted2.KFAS <- fitSSM(oModel2.KFAS, numeric(3), method = "BFGS") )
oFitted2.KFAS$optm.out
( oEstimated2.KFAS <- KFS(oFitted2.KFAS$model, filtering = "state", smoothing = "state") )


# -----------------------------------------------------------------------------
# 水準成分と季節ダミーのプロット
par(mfrow=c(2,1));
plot(tsData, lty=3, type="o", ylab="水準成分")
lines(oEstimated.KFAS$alphahat[,"level"], lwd=2)
ts.plot(oEstimated.KFAS$alphahat[,2:11], ylab="季節ダミー")

dev.new()
par(mfrow=c(2,1));
plot(tsData, lty=3, type="o", ylab="水準成分")
lines(oEstimated2.KFAS$alphahat[,"level"], lwd=2)
ts.plot(oEstimated2.KFAS$alphahat[,2:11], ylab="季節ダミー")


# -----------------------------------------------------------------------------
# モデル診断:　平滑化後残差になんらかの相関・構造が残っていないか
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



likSeasDummy0 <- kfsSeasDummy0$logLik - sum(kfsSeasDummy0$Finf>0) * log(2*pi)/2
likSeasDummy <- kfsSeasDummy$logLik - sum(kfsSeasDummy$Finf>0) * log(2*pi)/2
likSeasTri0 <- kfsSeasTri0$logLik - sum(kfsSeasTri0$Finf>0) * log(2*pi)/2
likSeasTri <- kfsSeasTri$logLik - sum(kfsSeasTri$Finf>0) * log(2*pi)/2

# 1 期先予測の平均二乗誤差
mseSeasDummy0 <- sum(kfsSeasDummy0$v[14:144]^2) / 131
mseSeasDummy <- sum(kfsSeasDummy$v[14:144]^2) / 131
mseSeasTri0 <- sum(kfsSeasTri0$v[14:144]^2) / 131
mseSeasTri <- sum(kfsSeasTri$v[14:144]^2) / 131

# 散漫対数尤度の修正（季節変動を固定した同等なモデルの尤度を揃える）
likSeasTri <- likSeasTri - (likSeasTri0 - likSeasDummy0)
likSeasTri0 <- likSeasDummy0

# AIC (赤池情報量規準)
aicSeasDummy0 <- -2*likSeasDummy0 + 2*(2+13)
aicSeasDummy <- -2*likSeasDummy + 2*(3+13)
aicSeasTri0 <- -2*likSeasTri0 + 2*(2+13)
aicSeasTri <- -2*likSeasTri + 2*(3+13)
