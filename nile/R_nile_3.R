setwd("C:\\Users\\R_work")
library(KFAS)


## ----------------------------------------------------------------------------
## ナイル河　ローカルレベルモデル
##
## フィルタリング、平滑化、信頼区間・予測区間の表示、予測　など
## 欠測値補間: ただし欠測値がない状態で推定した Q, H の値が必要・・・・
## ----------------------------------------------------------------------------
# Nile データは ts型
data(Nile)

( model_Nile0 <- SSModel(Nile ~ SSMtrend(1, Q = list(matrix(NA))), H = matrix(NA)) )
( model_Nile <- fitSSM(model_Nile0, c(log(var(Nile)), log(var(Nile))), method = "BFGS")$model )

# fitSSM により、Q, H が推定された
class(model_Nile)
model_Nile0$Q;  model_Nile$Q;
model_Nile0$H;  model_Nile$H;

# "SSModel" クラスを引数にplot() すると、1期先予測と、平滑化後誤差をプロットしてくれる
# 非ガウスモデルでは、nsim= を引数に設定すると、その数だけインポータンス・サンプリングをしてくれる
plot(model_Nile)

# Filtering and state smoothing
# KFS は、指数族の状態空間モデルに対して univariate approach で散漫初期化 exact diffuse initialization を用いてカルマンフィルタリングと平滑化を実施
# filtering:  ガウスモデルではデフォルトは "state"　（signal は mean)　　非ガウスモデルでは "none"
# smoothing:  デフォルトは "state" and "mean"　（ガウスモデルでは "mean" は "signal" と同じ）　　非ガウスモデルでは "disturbance"　は適用なし
( out_Nile <- KFS(model_Nile, filtering = "state", smoothing = "state") )
names(out_Nile)

# 対数尤度（ガウスモデルのみ）
out_Nile$logLik
# 状態の1期先予測
out_Nile$a
# 状態の平滑化推定値、観測値の平滑化推定値
out_Nile$alphahat;  out_Nile$muhat;

# rstandard() で標準化誤差をKFSの結果から取り出す
# state: 平滑化状態攪乱項に基づく誤差　　pearson: ガウスモデルの場合は standardized smoothed ε disturbance residuals  一般線形モデルの場合は標準化ピアソン残差
out_Nile2 <- KFS(model_Nile, smoothing = c("state", "mean", "disturbance"))
plot(cbind(state = rstandard(out_Nile2, "state"), recursive = rstandard(out_Nile2), irregular = rstandard(out_Nile2, "pearson")), main = "recursive and auxiliary residuals")
# 基本的には以下と同じ
dev.new()
plot(model_Nile)

# Confidence and prediction intervals for the expected value and the observations.
# Note that predict uses original model object, not the output from KFS.
( conf_Nile <- predict(model_Nile, interval = "confidence", level = 0.9) )
( pred_Nile <- predict(model_Nile, interval = "prediction", level = 0.9) )
ts.plot(cbind(Nile, pred_Nile, conf_Nile[, -1]), col = c(1:2, 3, 3, 4, 4), ylab = "Predicted Annual flow", main = "River Nile")

# Missing observations, using the same parameter estimates
NileNA <- Nile
NileNA[c(21:40, 61:80)] <- NA
# 推定したQ, H を使う
model_Nile$Q;  model_Nile$H;
model_NileNA <- SSModel(NileNA ~ SSMtrend(1, Q = list(model_Nile$Q)), H = model_Nile$H)
out_NileNA <- KFS(model_NileNA, "mean", "mean")
# Filtered and smoothed states
ts.plot(NileNA, fitted(out_NileNA, filtered = TRUE), fitted(out_NileNA), col = 1:3, ylab = "Predicted Annual flow", main = "River Nile")

# 注意!!!
# 推定したQ, H を使わず、matrix(NA) だと計算できない
NileNA <- Nile
NileNA[c(21:40, 61:80)] <- NA
model_NileNA <- SSModel(NileNA ~ SSMtrend(1, Q = list(matrix(NA))), H = matrix(NA))
out_NileNA <- KFS(model_NileNA, "mean", "mean")


# 先10年の予測
model_Nile_pre <- SSModel(Nile ~ SSMtrend(1, Q=list(model_Nile$Q)),H=model_Nile$H)
pred <- predict(model_Nile_pre, n.ahead=10, interval="prediction",　level=0.9)
pred;  ts.plot(pred, col=c(1,2,3));


## ----------------------------------------------------------------------------
## 予測：　新しいxの値を指定してyを予測する場合
##
## 新しいxの値を xnew として作成
## SSModel を、従来の y~X と、NA~xnew で2つ生成し、前者をモデル、後者をnewdataとして、両方をpredictに入力する
## matlines で、予測値・上下限の3つの曲線を一気に描画できる
## ----------------------------------------------------------------------------
set.seed(1)
x <- runif(n=100, min=1, max=3)
y <- rpois(n=100, lambda=exp(x-1))
plot(x=x,y=y,pch=19,ylim=c(0,25),xlim=c(0.5,3.5))

model <- SSModel(y~x,distribution="poisson")
xnew <- seq(0.5,3.5,by=0.1)
newdata <- SSModel(rep(NA,length(xnew))~xnew,distribution="poisson")
pred <- predict(model,newdata=newdata,interval="prediction",level=0.9,nsim=100)
matlines(x=xnew,y=pred,col=c(2,2,2),lty=c(1,2,2),type="l")


## ----------------------------------------------------------------------------
## "signal" 関数を使う例：　いまいちわからない
## Function signal returns the signal of a state space model using only subset of states.
## ----------------------------------------------------------------------------
Seatbelts <- read.table("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_durbinkoopman\\Seatbelt.dat", skip=2, sep="")
Van <- read.table("C:\\Users\\光世\\Desktop\\#DataforAnalysis\\data_durbinkoopman\\Van.dat", skip=1, sep="")
colnames(Seatbelts) <- c("drivers", "front", "rear", "kilometers", "PetrolPrice")
colnames(Van) <- c("van", "law")

Seatbelts <- cbind(Seatbelts, Van)

par(mfrow=c(3,1)); plot(log(Seatbelts$drivers), type="l"); plot(Seatbelts$PetrolPrice, type="l"); plot(Seatbelts$law, type="l");

# model <- SSModel(log(drivers) ~ SSMtrend(1, Q = NA) +　SSMseasonal(12, sea.type = 'trigonometric', Q = NA) +　log(PetrolPrice) + law,data = Seatbelts, H = NA)
model <- SSModel(log(drivers) ~ SSMtrend(1, Q = NA) +　SSMseasonal(12, sea.type = 'trigonometric', Q = NA) +　PetrolPrice + law, data = Seatbelts, H = NA)

ownupdatefn <- function(pars,model,...){
  model$H[] <- exp(pars[1])
  diag(model$Q[,,1]) <- exp(c(pars[2], rep(pars[3], 11)))
  model
}

fit <- fitSSM(inits = log(c(var(log(Seatbelts[,'drivers'])), 0.001, 0.0001)),　model = model, updatefn = ownupdatefn, method = 'BFGS')
out <- KFS(fit$model, smoothing = c('state', 'mean'))
par(mfrow=c(1,1))
ts.plot(cbind(out$model$y, fitted(out)),lty = 1:2, col = 1:2,　main = 'Observations and smoothed signal with and without seasonal component')
lines(signal(out, states = c('regression', 'trend'))$signal, col = 4, lty = 1)
legend('bottomleft',legend = c('Observations', 'Smoothed signal','Smoothed level'),　col = c(1, 2, 4), lty = c(1, 2, 1))


## ----------------------------------------------------------------------------
## Oxford-Cambridge boat race results 1829 - 2011
## 勝ち負けの0-1　二項分布をベースとした状態空間モデル
## ----------------------------------------------------------------------------
data("boat")
boat
plot(boat, type="l")
length(which(boat=="1"));  length(which(boat=="0"));  length(which(is.na(boat)));

# Model from DK2012, bernoulli response based on random walk
model <- SSModel(boat ~ SSMtrend(1, Q = NA), distribution = "binomial")
fit_nosim <- fitSSM(model, inits = log(0.25), method = "BFGS", hessian = TRUE)
# nsim set to small for faster execution of example
# doesn't matter here as the model/data is so poor anyway
fit_sim <- fitSSM(model, inits = log(0.25), method = "BFGS", hessian = TRUE, nsim = 100)

# Compare with the results from DK2012
model_DK <- SSModel(boat ~ SSMtrend(1, Q = 0.33), distribution = "binomial")
# Big difference in variance parameters:
fit_nosim$model["Q"]
fit_sim$model["Q"]

# approximate 95% confidence intervals for variance parameter:
# very wide, there really isn't enough information in the data
# as a comparison, a fully Bayesian approach (using BUGS) with [0, 10] uniform prior for sigma
# gives posterior mode for Q as 0.18, and 95% credible interval [0.036, 3.083]
exp(fit_nosim$optim.out$par + c(-1, 1)*qnorm(0.975)*sqrt(1/fit_nosim$optim.out$hessian))
exp(fit_sim$optim.out$par + c(-1, 1)*qnorm(0.975)*sqrt(1/fit_sim$optim.out$hessian))
# 95% confidence intervals for probability that Cambridge wins
pred_nosim <- predict(fit_nosim$model, interval = "confidence")
pred_sim <- predict(fit_sim$model, interval = "confidence")
ts.plot(pred_nosim, pred_sim, col = c(1, 2, 2, 3, 4, 4), lty = c(1, 2, 2), ylim = c(0, 1))
points(x = time(boat), y = boat, pch = 15, cex = 0.5)
# if we trust the approximation, fit_nosim gives largest log-likelihood:
logLik(fit_nosim$model)
logLik(fit_sim$model)
logLik(model_DK)
# and using importance sampling fit_sim is the best:
logLik(fit_nosim$model, nsim = 100)
logLik(fit_sim$model, nsim = 100)
logLik(model_DK, nsim = 100)
## Not run:
# only one unknown parameter, easy to check the shape of likelihood:
# very flat, as was expected based on Hessian
ll_nosim <- Vectorize(function(x) {
model["Q"] <- x
coef.SSModel 7
logLik(model)
})
ll_sim <- Vectorize(function(x) {
model["Q"] <- x
logLik(model, nsim = 100)
})
curve(ll_nosim(x), from = 0.1, to = 0.5, ylim = c(-106, -104.5))
curve(ll_sim(x), from = 0.1, to = 0.5, add = TRUE, col = "red")


## ----------------------------------------------------------------------------
## updatefn
## ----------------------------------------------------------------------------
# 以下がデフォルトの updatefn
# Example function for updating covariance matrices H and Q
# (also used as a default function in fitSSM)
updatefn <- function(pars, model){
  if(any(is.na(model$Q))){
    Q <- as.matrix(model$Q[,,1])
    naQd <- which(is.na(diag(Q)))
    naQnd <- which(upper.tri(Q[naQd,naQd]) & is.na(Q[naQd,naQd]))
    Q[naQd,naQd][lower.tri(Q[naQd,naQd])] <- 0
    diag(Q)[naQd] <- exp(0.5 * pars[1:length(naQd)])
    Q[naQd,naQd][naQnd] <- pars[length(naQd)+1:length(naQnd)]
    model$Q[naQd,naQd,1] <- crossprod(Q[naQd,naQd])
  }
  if(!identical(model$H,'Omitted') && any(is.na(model$H))){
    H<-as.matrix(model$H[,,1])
    naHd <- which(is.na(diag(H)))
    naHnd <- which(upper.tri(H[naHd,naHd]) & is.na(H[naHd,naHd]))
    H[naHd,naHd][lower.tri(H[naHd,naHd])] <- 0
    diag(H)[naHd] <-
    exp(0.5 * pars[length(naQd)+length(naQnd)+1:length(naHd)])
    H[naHd,naHd][naHnd] <-
    pars[length(naQd)+length(naQnd)+length(naHd)+1:length(naHnd)]
    model$H[naHd,naHd,1] <- crossprod(H[naHd,naHd])
  }
model
}

# デフォルトの checkhn
# Example function for checking the validity of covariance matrices.
checkfn <- function(model){
  #test positive semidefiniteness of H and Q
  inherits(try(ldl(model$H[,,1]),TRUE),'try-error') || inherits(try(ldl(model$Q[,,1]),TRUE),'try-error')
}

#function for updating the model
update_model <- function(pars, model) {
  model["H"] <- pars[1]
  model["Q"] <- pars[2]
  model
}

#check that variances are non-negative
check_model <- function(model) {
  (model["H"] > 0 && model["Q"] > 0)
}

data("Nile")
model <- SSModel(Nile ~ SSMtrend(1, Q = list(matrix(NA))), H = matrix(NA))

# updatefn:  パラメータが与えられた際のモデル更新関数、function(pars, model)の形式をとる、デフォルトのupdatefn は、NA とされた Q, H の分散共分散行列を推定する
# checkfn:  モデルの validity チェック、valid なら TRUE
fit <- fitSSM(inits = rep(var(Nile)/5, 2), model = model, updatefn = update_model, checkfn = check_model)
