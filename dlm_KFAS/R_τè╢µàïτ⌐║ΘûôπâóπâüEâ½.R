setwd("C:\\Users\\R_work")
library(KFAS)


## ----------------------------------------------------------------------------
## ローカルレベルモデル
## ----------------------------------------------------------------------------

# どうも ts(scan())　で読み込まないと、以降の処理がうまくいかない
Weight <- ts(scan("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\Weight.DAT"))
plot(Weight, type="l")

# ローカルモデルの場合は、SSMtrend を使い、次数=1 を指定
# Q: 状態攪乱項の分散　　H:　観測値攪乱項の分散
mod <- SSModel(Weight ~ SSMtrend(1, Q=NA), H=NA)


# 未知パラメータの最尤推定
# numeric(2) は未知パラメータの初期値（0ふたつ）  "BFGS": 準ニュートン法
# fit$model は、predict に入力することで、平滑化状態の信頼区間や、長期予測に使える
fit <- fitSSM(mod, numeric(2), method="BFGS")
fit$model$H
fit$model$Q


# カルマンフィルタと平滑化の実行
kfs <- KFS(fit$model)


# 状態αの1期先予測 a　および　その予測誤差分散 P
kfs$a
kfs$P

# 観測値yの1期先予測誤差 v（イノベーション） および その予測誤差分散 F
kfs$v
kfs$F

# 平滑化状態α-hat、平滑化状態分散 V
kfs$alphahat
kfs$V

# フィルタ化推定量: 1期先予測　および　信頼区間
afilt <- kfs$a[-1]
Pfilt <- kfs$P[,,-1] - fit$model$Q
afiltconf <- cbind(afilt + sqrt(Pfilt) * pnorm(0.025), afilt+sqrt(Pfilt)*pnorm(0.975))


# 平滑化状態の信頼区間　（計算が一致しない）
kfs$alphahat + as.vector(sqrt(kfs$V)*pnorm(0.025))
alphahatconf <- predict(fit$model, interval="confidence", level=0.95)
plot(alphahatconf)


# 長期予測
mod50 <- SSModel(Weight[1:50] ~ SSMtrend(1, Q=NA), H=NA)
fit50 <- fitSSM(mod50, numeric(2), method="BFGS")
pre50 <- predict(fit50$model, interval="prediction", n.ahead=10, level=0.95)


# 欠測値の補間
modNA <- SSModel(Weight[c(1:20, rep(NA,20), 41:60)] ~ SSMtrend(1, Q=NA), H=NA)
fitNA <- fitSSM(modNA, numeric(2), method="BFGS")
preNA <- predict(fitNA$model, interval="prediction", level=0.95)



## ----------------------------------------------------------------------------
## 線形ガウスモデル
## ローカルレベルモデル、2次のトレンドモデル、ローカル線形トレンドガウスモデル
## ----------------------------------------------------------------------------
# どうも ts(scan())　で読み込まないと、以降の処理がうまくいかない
Weight <- ts(scan("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\Weight.DAT"))
plot(Weight, type="l")


# ローカルレベルモデル
modLocallevel <- SSModel(Weight ~ SSMtrend(1, Q=NA), H=NA)
fitLocallevel <- fitSSM(modLocallevel, numeric(2), method="BFGS")
kfsLocallevel <- KFS(fitLocallevel$model)


# 2次のトレンドモデル:　状態攪乱項分散の一方がゼロ
modTrend <- SSModel(Weight ~ SSMtrend(2, Q=c(list(0), list(NA))), H=NA)
fitTrend <- fitSSM(modTrend, numeric(2), method="BFGS")
kfsTrend <- KFS(fitTrend$model)


# ローカル線形トレンドモデル
modLocalTrend <- SSModel(Weight ~ SSMtrend(2, Q=c(list(NA), list(NA))), H=NA)
fitLocalTrend <- fitSSM(modLocalTrend, numeric(3), method="BFGS")
kfsLocalTrend <- KFS(fitLocalTrend$model)
# 実際は、傾き成分の状態攪乱項分散はゼロに近い
fitLocalTrend$model$Q


# 水準成分"level"と傾き成分"slope"の平滑化状態
# ローカル線形トレンドモデルは、傾き成分がずっと一定であり、傾き成分の状態攪乱項分散がゼロ推定され、時間比例の線形トレンド項が加わったローカルレベルモデルになった
# 一方、2次のトレンドモデルは、傾き成分が時点ごとに変動
plot(Weight, lty=3, type="o", ylab="水準成分")
lines(kfsLocallevel$alphahat[,"level"], lwd=2)
lines(kfsTrend$alphahat[,"level"], lwd=2, lty=2)
lines(kfsLocalTrend$alphahat[,"level"], lwd=2, col=8)

dev.new()
plot(kfsTrend$alphahat[,"slope"], lwd=2, col=8, ylab="傾き成分")
lines(kfsLocalTrend$alphahat[,"slope"], lwd=2)


# モデルの当てはまりの良さの比較
# 最大対数尤度: KFSの対数尤度は、散漫対数尤度は F > 0 となる時点数に log(2*pi)/2 を掛けた分だけ過大となっているため修正
likLocallevel <- kfsLocallevel$logLik - sum(kfsLocallevel$Finf > 0) * log(2*pi)/2
likTrend <- kfsTrend$logLik - sum(kfsTrend$Finf > 0) * log(2*pi)/2
likLocalTrend <- kfsLocalTrend$logLik - sum(kfsLocalTrend$Finf > 0) * log(2*pi)/2
# AIC
aicLocallevel <- -2 * likLocallevel + 2*(2+1)
aicTrend <- -2 * likTrend + 2*(2+2)
aicLocalTrend <- -2 * likLocalTrend + 2*(2+3)
# 1期先予測の平均二乗誤差: 誤差が非常に大きくなる散漫な初期時点を除いた平均をとっている
mseLocallevel <- sum(kfsLocallevel$v[3:60]^2/58)
mseTrend <- sum(kfsTrend$v[3:60]^2/58)
mseLocalTrend <- sum(kfsLocalTrend$v[3:60]^2/58)


## ----------------------------------------------------------------------------
## 基本構造時系列モデル
## ----------------------------------------------------------------------------
sales <- read.csv("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\sales.csv")
head(sales)
plot(ts(sales))

#ダミー変数型(固定) の季節成分モデル(季節変動が固定の場合)
modSeasDummy0 <- SSModel(sales$Fabric ~ SSMtrend(2, Q = c(list(0), list(NA)))
  + SSMseasonal(12, sea.type="dummy"), H = NA)
fitSeasDummy0 <- fitSSM(modSeasDummy0, numeric(2), method = "BFGS")
kfsSeasDummy0 <- KFS(fitSeasDummy0$model)

#ダミー変数型(変化) の季節成分モデル(季節変動が変化する場合)
modSeasDummy <- SSModel(sales$Fabric ~ SSMtrend(2, Q = c(list(0), list(NA)))
  + SSMseasonal(12, sea.type="dummy", Q = NA), H = NA)
fitSeasDummy <- fitSSM(modSeasDummy, numeric(3), method = "BFGS")
kfsSeasDummy <- KFS(fitSeasDummy$model)

#三角関数型(固定) の季節成分モデル(季節変動が固定の場合)
modSeasTri0 <- SSModel(sales$Fabric ~ SSMtrend(2, Q = c(list(0), list(NA)))
  + SSMseasonal(12, sea.type="trigonometric"), H = NA)
fitSeasTri0 <- fitSSM(modSeasTri0, numeric(2), method = "BFGS")
kfsSeasTri0 <- KFS(fitSeasTri0$model)

#三角関数型(変化) の季節成分モデル(季節変動が変化する場合)
#三角関数型モデルの状態攪乱項分散を推定する場合は、Q=NAだけでは分散を周波数ごとの分散を共通にすることはできないため、updatefn を使っている
#updatefn: 未知パラメータ(NAでなくてもよい)を含むモデルと、未知パラメータに与える候補値を引数に与えて、未知パラメータに候補値を代入したモデルを返す関数
#分散パラメータは負値をとれないため、候補値の引数 pars に対して指数関数をとり正値に変換している
#pars[2]は二次トレンドモデル、pars[3:8]は季節成分モデルの状態攪乱項分散: fitSeasTri$model　で状態変数の格納順を確認しておくこと
modSeasTri <- SSModel(sales$Fabric ~ SSMtrend(2, Q = c(list(0), list(NA)))
  + SSMseasonal(12, sea.type="trigonometric", Q = NA), H = NA)
updatefn <- function(pars, model){
  model$H[] <- exp(pars[1])
  diag(model$Q[,,1]) <- c(0,exp(pars[2]),rep(exp(pars[3:8]),c(rep(2,5),1)))
  return(model)
}
fitSeasTri <- fitSSM(modSeasTri, c(6,0,1,2,0,0,0,0), updatefn, method="BFGS")
kfsSeasTri <- KFS(fitSeasTri$model)

# 最大対数尤度
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


## ----------------------------------------------------------------------------
## 定常AR成分を加えたモデル
## 基本構造時系列モデルで残差に自己相関がみられる場合、定常ARモデル成分を加えることでモデルの当てはまりを改善できる可能性がある
## ----------------------------------------------------------------------------
sales <- read.csv("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\sales.csv")
head(sales)
plot(ts(sales))

# artransform() は実数ベクトル空間から定常条件を満たす領域全体への写像
# AR(1) 成分モデル
modAR1 <- SSModel(sales$Fabric ~ SSMtrend(2, Q = c(list(0), list(NA)))
  + SSMarima(ar = 0, Q = 0)
  + SSMseasonal(12, sea.type="dummy"), H = NA)
updatefn <- function(pars, model){
model <- SSModel(sales$Fabric ~
  SSMtrend(2, Q = c(list(0), list(exp(pars[1]))))
    + SSMarima(ar = artransform(pars[2]), Q = exp(pars[3]))
    + SSMseasonal(12, sea.type="dummy"), H = exp(pars[4]))
  return(model)
}
fitAR1 <- fitSSM(modAR1, c(-1,0,6,3), updatefn, method = "BFGS")
kfsAR1 <- KFS(fitAR1$model)

# AR(2) 成分モデル
modAR2 <- SSModel(sales$Fabric ~ SSMtrend(2, Q = c(list(0), list(NA)))
  + SSMarima(ar = c(0, 0), Q = 0)
  + SSMseasonal(12, sea.type="dummy"), H = NA)
updatefn <- function(pars, model){
  model <- SSModel(sales$Fabric ~ SSMtrend(2, Q = c(list(0), list(exp(pars[1]))))
    + SSMarima(ar = artransform(pars[2:3]), Q = exp(pars[4]))
    + SSMseasonal(12, sea.type="dummy"), H = exp(pars[5]))
  return(model)
}
fitAR2 <- fitSSM(modAR2, c(-1,0.1,0,6,3), updatefn, method = "BFGS")
kfsAR2 <- KFS(fitAR2$model)

# AR(12) 成分モデル(ラグ12 以外の自己回帰係数はゼロとする)
modAR12 <- SSModel(sales$Fabric ~ SSMtrend(2, Q = c(list(0), list(NA)))
  + SSMarima(ar = rep(0, 12), Q = 0)
  + SSMseasonal(12, sea.type="dummy"), H = NA)
updatefn <- function(pars, model){
  model <- SSModel(sales$Fabric ~ SSMtrend(2, Q = c(list(0), list(exp(pars[1]))))
    + SSMarima(ar = c(rep(0,11), artransform(pars[2])), Q = exp(pars[3]))
    + SSMseasonal(12, sea.type="dummy"), H = exp(pars[4]))
return(model)
}
fitAR12 <- fitSSM(modAR12, c(-1,0.4,6,0), updatefn, method = "BFGS")


## ----------------------------------------------------------------------------
## 回帰成分のあるモデル：　回帰係数を固定、回帰係数を時間変化
## ----------------------------------------------------------------------------
sales <- read.csv("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\sales.csv")
Gasoline <- ts(scan("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\Gasoline.dat"))
plot(Gasoline)

par(mfrow=c(2,1))
plot(sales$Fuel, type="l")
plot(log(sales$Fuel), type="l")

# 販売額の規模が大きく変化しており季節変動の規模もそれに応じて変化しているため、対数をとることで、推定および予測を安定させる
# 回帰係数を時間変化させる場合
modRegression <- SSModel(log(sales$Fuel) ~ SSMtrend(1, Q = NA)
  + SSMseasonal(12, sea.type="dummy")
  + SSMregression(~ log(Gasoline), Q = NA), H = NA)
fitRegression <- fitSSM(modRegression, numeric(3), method = "BFGS")
kfsRegression <- KFS(fitRegression$model)

# 回帰係数を固定する場合
modRegression0 <- SSModel(log(sales$Fuel) ~ SSMtrend(1, Q = NA)
  + SSMseasonal(12, sea.type="dummy")
  + log(Gasoline), H = NA)
fitRegression0 <- fitSSM(modRegression0, numeric(2), method = "BFGS")
kfsRegression0 <- KFS(fitRegression0$model)


## ----------------------------------------------------------------------------
## カレンダー効果のあるモデル
## 月次データしかない場合に、各月の曜日の（日曜日に対する）多少を加味　＋　うるう年の加味
## ----------------------------------------------------------------------------
sales <- read.csv("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\sales.csv")

# 各月の曜日集計
dates <- seq(as.Date("2002-01-01"), as.Date("2013-12-31"), by = 1)
weeks <- table(substr(dates,1,7), weekdays(dates, T))
sun <- weeks[,"日"]
mon <- weeks[,"月"]-sun; tue <- weeks[,"火"]-sun; wed <- weeks[,"水"]-sun
thu <- weeks[,"木"]-sun; fry <- weeks[,"金"]-sun; sat <- weeks[,"土"]-sun
calendar <- cbind(mon, tue, wed, thu, fry, sat)

# うるう年２月のダミー変数
leapyear <- rownames(weeks) %in% c("2004-02","2008-02","2012-02")

# カレンダー効果(曜日・うるう年) のあるモデル
modCalender <- SSModel(sales$Fabric ~ SSMtrend(2, Q = c(list(0), list(NA)))
  + SSMseasonal(12, sea.type="dummy")
  + leapyear + calendar, H = NA)
fitCalender <- fitSSM(modCalender, numeric(2), method = "BFGS")
kfsCalender <- KFS(fitCalender$model)


## ----------------------------------------------------------------------------
## 外れ値と構造変化の検討
## 干渉変数
## ----------------------------------------------------------------------------
sales <- read.csv("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\sales.csv")

# 2010年11月，12月のデータを除外
salesNA <- sales$Machinery
salesNA[sales$month %in% c("2010年11月","2010年12月")] <- NA

# 2011年8月以降の水準シフト干渉変数の定義
( ShiftLevel <- (1:nrow(sales) >= which(sales$month=="2011年8月")) )

# 水準シフト干渉変数を加えたモデル
# 補助残差に必要な平滑化攪乱項を得るためにには、KFSの引数 smoothing に "disturbance" を指定する
modShift <- SSModel(log(salesNA) ~ SSMtrend(1, Q = NA)
  + SSMseasonal(12, Q = NA, sea.type="dummy")
  + ShiftLevel, H = NA)
fitShift <- fitSSM(modShift, numeric(3))
kfsShift <- KFS(fitShift$model, smoothing=c("state","mean","disturbance"))

# 補助残差のプロット例
plot(rstandard(kfsShift, "pearson")) # 観測値撹乱項
plot(rstandard(kfsShift, "state")[,1]) # 状態撹乱項(水準成分)


## ----------------------------------------------------------------------------
## 多変量時系列モデル
## ----------------------------------------------------------------------------
Weight <- ts(scan("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\Weight.dat"))   # 体重データの読み込み
Bodyfat <- ts(scan("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\Bodyfat.dat")) # 体脂肪率データの読み込み

WeightNA <- Weight
WeightNA[21:40] <- NA

# 多変量の場合には引数 type に水準成分を要素ごとに別々に与える "distinct"　か 全要素に共通の水準成分を与える "common" かを指定できる
# 攪乱項分散 H, Q には 2*2 行列を指定することになるが、非対角成分をもつ正定値対象行列として推定するためには行列の全成分をNAに指定することで、内部でコレスキー分解を用いた推定を行ってくれ宇
# 2*2 行列をコレスキー分解したときの上三角行列は、三つの非ゼロ成分をもつため、初期値には、Q, H それぞれ3つの値を与える必要がある
modSUTSE <- SSModel(cbind(WeightNA, Bodyfat) ~
  SSMtrend(1, Q = matrix(NA,2,2), type = "distinct"), H = matrix(NA,2,2))
fitSUTSE <- fitSSM(modSUTSE, numeric(6), method="BFGS")
kfsSUTSE <- KFS(fitSUTSE$model)


## ----------------------------------------------------------------------------
## 線形非ガウス状態空間モデル
## 東京都における1日の火災件数の予測
## ----------------------------------------------------------------------------
library(Nippon)

fire     <- ts(scan("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\fire.dat"))     # 火災件数データの読み込み
humidity <- ts(scan("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\humidity.dat")) # 日平均湿度データの読み込み
celsius  <- ts(scan("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\celsius.dat"))  # 日平均気温データの読み込み
rain     <- ts(scan("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\rain.dat"))     # 日降水量データの読み込み

par(mfrow=c(4,1))
plot(fire);  plot(humidity);  plot(celsius);  plot(rain);

dates <- seq(as.Date("2005-01-01"), as.Date("2014-12-31"), by=1) # 日付列作成
weekday <- weekdays(dates, T) # 曜日判別関数("月"～"日"を返す)
weekday[is.jholiday(dates)] <- "祝" # パッケージNippon の祝日判別関数
weekday <- factor(weekday, c("月","火","水","木","金","土","日","祝"))
weekday


# 季節成分は、うるう年を考慮して周期 365.25日とおいた三角関数型の季節成分モデルを仮定
# 季節成分を表現する三角関数の周波数は、最大数を用いると過適合となるため、月単位の季節変動が表現できる程度で j = 1,...,6程度
modPois <- SSModel(fire ~ SSMtrend(2, Q = c(list(0), list(NA)))
  + SSMcycle(365.25) + SSMcycle(365.25/2) + SSMcycle(365.25/3)
  + SSMcycle(365.25/4) + SSMcycle(365.25/5) + SSMcycle(365.25/6)
  + weekday + humidity + celsius + rain,
distribution = "poisson")

# これはエラーになる
fitPois <- fitSSM(modPois, 0, method="BFGS", nsim=100)
# これも警告がある
fitPois <- fitSSM(modPois, 0, method="Brent", nsim=100, lower=-40, upper=0)


# 散漫初期化で安定的な結果が得られなかったため、散漫初期化を止めて P1inf をゼロとし、代わりに初期分散P1を十分大きく（各状態成分の推定値を十分大きく上回る大きさ）とった
diag(modPois$P1inf) <- 0 # 散漫初期化が上手くいかなかったため，
diag(modPois$P1) <- 100^2 # 代わりに十分大きな初期分散を用いる

# 非ガウスモデルではシミュレーション数nsim を設定する
# KFS は平滑化状態とその誤差分散を返す
fitPois <- fitSSM(modPois, 0, method="Brent", lower=-40, upper=0, nsim=100)
kfsPois <- KFS(fitPois$model, nsim=1000)

# 任意の状態の関数に対する平滑化推定値を求めたい場合は、インポータンス・サンプルを利用する
# インポータンス・サンプリングと利用例(湿度効果(第8 状態成分) の95%信頼区間)
impPois <- importanceSSM(fitPois$model, type="states", nsim=4000)
impPois$samples
impPois$weights
emp <- cumsum(impPois$weights[order(impPois$samples[1,8,])])/sum(impPois$weight)
conf <- sort(impPois$samples[1,8,])[rank(c(0.025, 0.975, emp))[1:2]]

# 観測値の98％予測区間
prePois <- predict(fitPois$model, interval="prediction", level=0.98, nsim=10000)


# 有意な効果のない降水量を回帰成分から外し、代わりに湿度と温度の交互作用すなわち湿度×温度を回帰成分に加える
# 2011年3月11日の異常値を欠測値に買えることで除去する
# ポアソン分布の代わりに、拡散パラメータ u により分散が調整可能な負の二項分布を適用する（建物ごとの火災確率が一律でないため観測値の分散がポアソン分布より大きい）
hum_cel = humidity * celsius # 湿度と温度の交互作用
fireNA <- fire; fireNA[dates=="2011-03-11"] <- NA # 外れ値を欠測値に替え除外
modNegbin <- SSModel(fireNA ~ SSMtrend(2 , Q = c(list(0), list(NA)))
  + SSMcycle(365.25) + SSMcycle(365.25/2) + SSMcycle(365.25/3)
  + SSMcycle(365.25/4) + SSMcycle(365.25/5) + SSMcycle(365.25/6)
  + weekday + humidity + celsius + hum_cel,
  distribution = "negative binomial", u = 1)
diag(modNegbin$P1inf) <- 0; diag(modNegbin$P1) <- 100^2
updatefn <- function(pars, model){
  model$Q[2,2,] = exp(pars[1])
  model$u[,] = exp(pars[2])
  return(model)
}
fitNegbin <- fitSSM(modNegbin, c(-28,0), updatefn, method="BFGS", nsim=1000)


## ----------------------------------------------------------------------------
## 非線形非ガウス状態空間モデル
## 金利の期間構造モデルの推定
## 同じ時点の金利でも、満期までの年数によって金利が異なるため、満期に依存する金利を共通ファクターで表したものが期間構造モデル
## ここでは、均衡モデルの単純な例として、1ファクターのCIRモデルを扱う: 連続時間t上の微小区間における金利を年率で表した瞬時的短期金利をファクターとして、その推移について確率微分方程式を仮定
## ----------------------------------------------------------------------------
libor <- matrix(scan("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_kalmanfilter\\libor.dat"),ncol=2,byrow=T) # 金利データの読み込み

# サンプルサイズN, 離散近似の分割数J および分割区間長Dt=1/J
N <- 10^6; J <- 10; Dt <- 1/J

# libor は1,…,n 週目における満期tau[1],…,tau[p] の金利をもつn × p 行列
n <- nrow(libor); p <- ncol(libor); tau <- c(3,6)/12 # 満期は3 ヶ月と6 ヶ月

# 初期分布からのサンプリング
set.seed(1)
alp <- runif(N)*0.001; kap <- runif(N) ; the <- runif(N)*0.25
sig <- runif(N)*0.25 ; lam <- runif(N)*-1 ; sigeps <- runif(N)*0.0001

# CIR モデルによる理論式の係数
gam <- sqrt((kap+lam)^2+2*sig^2)
Atau <- Btau <- matrix(0,N,p)
for(i in 1:p){
  Atau[,i] <- 2*kap*the/sig^2*log(2*gam*exp((gam+kap+lam)/2*tau[i])/
              ((gam+kap+lam)*(exp(gam*tau[i])-1)+2*gam))
  Btau[,i] <- 2*(exp(gam*tau[i])-1)/((gam+kap+lam)*(exp(gam*tau[i])-1)+2*gam)
}
alpPsi <- cbind(alp,kap,the,sig,lam,sigeps,Atau,Btau) # 拡大状態ベクトル

# 粒子フィルタ
for(s in 1:n){
  # 尤度による重みw の算出
  sigeps <- alpPsi[,6]; Atau <- alpPsi[,7:8]; Btau <- alpPsi[,9:10]
  w <- ifelse(alp>0, 1, 0) # 瞬時的短期金利が0 以下であるものは重み0 とする
  for(i in 1:p) w <- w*dnorm(libor[s,i],(-Atau[,i]+Btau[,i]*alp)/tau[i], sigeps
  # インポータンス・リサンプリング
  r <- rank(c((1:N-runif(1))/N,cumsum(w)/sum(w)),ties="random")[1:N]-1:N+1
  alpPsi <- alpPsi[r,]
  # 1 期先予測サンプリング
  alp <- alpPsi[,1]; kap <- alpPsi[,2]; the <- alpPsi[,3];
  sig <- alpPsi[,4]
  for(j in 1:J) alp <- alp+kap*(the-alp)*Dt+sig*sqrt(alp)*rnorm(N,0,sqrt(Dt))
  alpPsi[,1] <- alp <- ifelse(is.nan(alp), 0, alp) # NaN は0 におきかえる
}
