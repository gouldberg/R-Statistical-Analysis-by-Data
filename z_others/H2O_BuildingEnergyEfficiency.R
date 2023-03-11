## R　H2O
## データ:  Building Energy Efficiency
##
## 目的変数:　Heating Load あるいは Cooling Load
## 説明変数:　全8変数（連続6 + カテゴリー2)
##
## データの特性
##　- データの分布は正規ではない、非線形モデルが必要
##  - 全変数で同じ値をとるデータがなく、すべてが違う値
##  - ノイズが少ない
##
## 結果
## - Deep Learning が最も性能がよく、次いでGBM, randomForest
## - randomForest はデフォルトモデルがチューニングモデルと性能があまり変わらない
## - 一般化線形回帰は性能が悪い
## ----------------------------------------------------------------------------
setwd("C:/users/R_work")
library(h2o)
library(tidyverse)

# -----------------------------------------------------------------------------
# 初期化
# -----------------------------------------------------------------------------
h2o.init(nthreads = -1)

# -----------------------------------------------------------------------------
# データ準備
# -----------------------------------------------------------------------------
path <- "C:\\DataforAnalysis2\\"
data <- h2o.importFile(paste0(path, "ENB2012_data.csv"))
attr(data, "id")
head(data)
str(data)

data[, c("X6","X8")] <- as.factor(data[,c("X6", "X8")])

splits <- h2o.splitFrame(data, 0.8)
train <- splits[[1]];  test <- splits[[2]];

x <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")
y <- "Y2"
# y <- "Y1"

# -----------------------------------------------------------------------------
# 基礎俯瞰
# -----------------------------------------------------------------------------
#　基本統計量
h2o.describe(train)

#　変数間の相関確認
cor_col <- setdiff(colnames(train), c("X6", "X8"))
d <- round( h2o.cor(train[,cor_col]), 2)
rownames(d) <- colnames(d)
d

# 目的変数との相関関係
barplot(d[,"Y2"], horiz=TRUE, names.arg=colnames(d))

# 訓練データのヒストグラム
par(mar=c(5.1, 6.0, 4.1, 2.1))  #Changed 4.1 to 6.0
par(oma=c(1,0,0,0))  #Def was all zeroes
par(mfrow = c(2,5))
#ylim <- c(0,350)
ylim <- NULL
dummy <- lapply(colnames(train), function(col){
   h <- h2o.hist(train[,col], breaks=30, plot = FALSE)
   plot(h, main = col, xlab = "", ylim = ylim,
     ylab = ifelse(col %in% c("X1","X6"), "Frequency", ""),
     cex.lab=2.0, cex.axis=2.0, cex.main=2.5, cex.sub=2.0, cex=2.0
  )
})

# 同じ値をもつ行があるか確認 -->　同じ値をもつ行がない
# -->　正則化の効果がないかもしれない、テストデータの分割の seed によって影響が大きいかもしれない
tmp <- as.data.frame(data)
tmp %>% distinct() %>% count()
nrow(tmp)

# 乱数の設定
seed = 999

# -----------------------------------------------------------------------------
# ランダムフォレスト
# -----------------------------------------------------------------------------
# 訓練データに対して、テストデータの MSE が若干上がっている
# RMSE（元データと単位が同じ）は元データのレンジ（10.90～48.03に比べて小さい
RFd <- h2o.randomForest(x, y, train, nfolds=10, model_id="RF_defaults", seed=seed)
RFd; summary(RFd);
RFd@model$cross_validation_metrics_summary
par(mfrow=c(1,1)); barplot(h2o.varimp(RFd)$percentage, horiz=TRUE, names.arg=h2o.varimp(RFd)$variable, xlab="percentage")
( pRFd <- h2o.performance(RFd, test) )

# パラメータのグリッドサーチ
# デフォルトモデルよりも residual_deviance がよいものがある
g <- h2o.grid("randomForest",
  hyper_params=list(ntrees=c(10,100,120), max_depth=c(40,60), min_rows=c(1,2)),
  x=x, y=y, training_frame = train, nfolds=10)
g
g@summary_table$residual_deviance

# 決定係数でモデルをソート
g_r2 <- h2o.getGrid(g@grid_id, sort_by="r2", decreasing=TRUE)

as.data.frame( g_r2@summary_table )

# パラメータのランダムサーチ:  search_criteria および hyper_params を設定する
# 直近10モデルのベストMSEが、それまでのベストよりも0.1%以上向上しなければストップ
# また、全実行時間は120秒
g <- h2o.grid("randomForest",
  search_criteria=list(strategy="RandomDiscrete", stopping_metric="mse", stopping_tolerance=0.001, stopping_rounds=10, max_runtime_secs=120),
  hyper_params=list(ntrees=c(50, 100, 150, 200, 250), mtries=c(2, 3, 4, 5), sample_rate=c(0.5, 0.632, 0.8, 0.95), col_sample_rate_per_tree=c(0.5, 0.9, 1.0)),
  x=x, y=y, training_frame=train, nfolds=5, max_depth=40, stopping_metric="deviance", stopping_tolerance=0, stopping_rounds=4, score_tree_interval=3)

# デフォルトモデルとチューンモデルで、シードの影響を確認
seeds <- c(101, 109, 373, 571, 619, 999)
defaultModels <- lapply(seeds, function(seed){
  h2o.randomForest(x, y, train, nfolds=10, seed=seed)
})

tunedModels <- lapply(seeds, function(seed){
  h2o.randomForest(x, y, train, nfolds=10, seed=seed, max_depth=40, ntrees=200, sample_rate=0.7, mtries=4, col_sample_rate_per_tree=0.9,
    stopping_metric="deviance", stopping_tolerance=0, stopping_rounds=5, score_tree_interval=3)
})

def <- sapply(defaultModels, h2o.rmse, xval=TRUE)
tuned <- sapply(tunedModels, h2o.rmse, xval=TRUE)
boxplot(c(def, tuned) ~ c(rep(1,6), rep(2,6)))

# ベストモデルで予測
RFt <- h2o.randomForest(x, y, train, nfolds = 10, model_id = "RF_tuned", seed = seed,
  max_depth = 40, ntrees = 200, sample_rate = 0.9, mtries = 4, col_sample_rate_per_tree = 0.9, score_tree_interval = 10)
( pRFt <- h2o.performance(RFt, test) )

# テストデータで予測し、デフォルトモデルとベストモデルを比較
pred_RFd <- h2o.predict(RFd, test)
pred_RFt <- h2o.predict(RFt, test)
df <- as.data.frame( h2o.cbind(pred_RFd$predict, pred_RFt$predict, test$Y2) )
plot(df$Y2, pch=0, ylim=c(0,50))
par(new=T); plot(df$predict, pch=17, col="red", ylab="", ylim=c(0,50));
par(new=T); plot(df$predict0, pch=16, col="blue", ylab="", ylim=c(0,50));


# -----------------------------------------------------------------------------
# Gradient Boosting Machines (GBM)
# -----------------------------------------------------------------------------
GBMd <- h2o.gbm(x, y, train, nfolds=10, model_id="GBM_defaults", seed=seed)
GBMd; summary(GBMd);
GBMd@model$cross_validation_metrics_summary
par(mfrow=c(1,1)); barplot(h2o.varimp(GBMd)$percentage, horiz=TRUE, names.arg=h2o.varimp(GBMd)$variable, xlab="percentage")
( pGBMd <- h2o.performance(GBMd, test) )

# パラメータのランダムサーチ
g2 <- h2o.grid("gbm",
  search_criteria=list(strategy="RandomDiscrete", stopping_metric="mse", stopping_tolerance=0.001, stopping_rounds=10, max_runtime_secs=60*10),
  hyper_params=list(max_depth=c(5,10,15), min_rows=c(1,2,5,10), sample_rate=c(0.67, 0.8, 0.9, 0.95, 1.0), col_sample_rate_per_tree=c(0.7, 0.9, 1.0), nbins=c(8,12,16,24,32)),
  x=x, y=y, training_frame=train, nfolds=5, ntrees=1000, stopping_metric="deviance", stopping_tolerance=0, stopping_rounds=4, score_tree_interval=5)

g <- h2o.grid("gbm",
  hyper_params=list(max_depth=c(5,10,15), min_rows=c(1,2,5,10), sample_rate=c(0.67, 0.8, 0.9, 0.95, 1.0), col_sample_rate_per_tree=c(0.7, 0.9, 1.0), nbins=c(8,12,16,24,32)),
  x=x, y=y, training_frame=train, nfolds=5, ntrees=1000, stopping_metric="deviance", stopping_tolerance=0, stopping_rounds=4, score_tree_interval=5)

models <- lapply(g@model, h2o.getModel)
mse_sd <- t(as.numeric(sapply(models, function(m){
  m@model$cross_validation_metrics_summary["mse", c("mean", "sd")]
})))

cbind(as.data.frame(g@summary_table), mes_sd)

# ベストモデル
GBMt <- h2o.gbm(x, y, train, nfolds=10, model_id="GBM_tuned", seed=373,
# #max_depth = 5,  #Default
    min_rows = 1, sample_rate = 0.9, col_sample_rate = 0.9, learn_rate = 0.01, ntrees = 1000, stopping_tolerance = 0, stopping_rounds = 4, score_tree_interval = 5)
( pGBMt <- h2o.performance(GBMt, test) )

# テストデータで予測し、デフォルトモデルとベストモデルを比較
pred_GBMd <- h2o.predict(GBMd, test)
pred_GBMt <- h2o.predict(GBMt, test)
df <- as.data.frame( h2o.cbind(pred_GBMd$predict, pred_GBMt$predict, test$Y2) )
plot(df$Y2, pch=0, ylim=c(0,50))
par(new=T); plot(df$predict, pch=17, col="red", ylab="", ylim=c(0,50));
par(new=T); plot(df$predict0, pch=16, col="blue", ylab="", ylim=c(0,50));


# -----------------------------------------------------------------------------
# Generalized Linear Models (GLM)
# -----------------------------------------------------------------------------
# GLM　ではseed の不要
GLMd <- h2o.glm(x, y, train, nfolds=10, model_id="GLM_defaults")
GLMd; summary(GLMd);
h2o.varimp(GLMd)
( pGLMd <- h2o.performance(GLMd, test) )

solvers <- c("IRLSM", "L_BFGS", "COORDINATE_DESCENT_NAIVE", "COORDINATE_DESCENT")
families <- c("gaussian", "poisson", "gamma")
gaussianLinks <- c("identity", "log", "inverse")
poissonLinks <- c("log")
gammaLinks <- c("identity", "log", "inverse")
gammaLinks_CD <- c("identity", "log")

allGrids <- lapply(solvers, function(solver){
  lapply(families, function(family){

    if(family=="gaussian") theLinks <- gaussianLinks
    else if(family=="poisson") theLinks <- poissonLinks
    else{
      if(solver=="COORDINATE_DESCENT") theLinks <- gammaLinks_CD
      else theLinks <- gammaLinks
    }

    lapply(theLinks, function(link){
      grid_id <- paste("GLM", solver, family, link, sep="_")
      h2o.grid("glm", grid_id = grid_id, hyper_params=list(alpha=c(0, 0.1, 0.5, 0.99)), x=x, y=y, training_frame=train, nfolds=10, lambda_search=TRUE, solver=solver, family=family, link=link, max_iterations=100)
    })
  })
})

g_tweedie <- h2o.grid("glm", grid_id="GLM_tweedie",
  hyper_params=list(tweedie_variance_power=c(1.0, 1.25, 1.50, 1.75, 2.0, 2.33, 2.67, 3.0, 3.5, 4.0), tweedie_link_power=c(0, 0.33, 0.67, 1.0, 1.33, 1.67, 2)),
  x=x, y=y, training_frame=train, nfolds=10, lambda_search=TRUE, solver="IRLSM", family="tweedie", alpha=0.5, stopping_tolerance=0, stopping_rounds=5, max_iterations=100)

models <- lapply(g_tweedie@model, h2o.getModel)
mse_sd <- t(as.numeric(sapply(models, function(m){
m@model$cross_validation_metrics_summary["mse", c("mean", "sd")]
})))

cbind(as.data.frame(g@summary_table), mes_sd)

# ベストモデル
GLMt <- h2o.glm(x, y, train, nfolds = 10, model_id="GLM_tuned",
  tweedie_variance_power = 1.55, tweedie_link_power = 0, alpha = 0.33, lambda_search = TRUE, solver = "IRLSM", family = "tweedie", link = "family_default", max_iterations = 100)
( pGLMt <- h2o.performance(GLMt, test) )

# テストデータで予測し、デフォルトモデルとベストモデルを比較
pred_GLMd <- h2o.predict(GLMd, test)
pred_GLMt <- h2o.predict(GLMt, test)
df <- as.data.frame( h2o.cbind(pred_GLMd$predict, pred_GLMt$predict, test$Y2) )
plot(df$Y2, pch=0, ylim=c(0,50))
par(new=T); plot(df$predict, pch=17, col="red", ylab="", ylim=c(0,50));
par(new=T); plot(df$predict0, pch=16, col="blue", ylab="", ylim=c(0,50));


# -----------------------------------------------------------------------------
# Deep Learning
# 1.　デフォルト設定で実施し、MSEを確認　（他の手法でのベストモデルと比較し、どれだけの改善余地があるか確認）
# 2.　epoch数を大きくする、そのためにearly stopping を実施　-->　epoch数の影響を確認
# 3.　データが非線形であることを確認し、隠れ層を追加、その際に入力層のサイズにあわせてサイズを設定、計算時間短縮のため nfolds を小さくする
# 4.　隠れ層について他のパターンを確認し、隠れ層の追加、サイズの影響を確認
# 5.　ドロップアウト、正則化、活性化関数をハイパーパラメタとし、ランダムサーチによるパラメータ探索
# 6.　隠れ層数を変えたもので再度パラメータ探索
# 7.　ベストモデルの設定
# -----------------------------------------------------------------------------
# まずデフォルトでやってみる -->　MSEが大きく、randomForest, GBM よりも悪い
# 入力層で18のニューロンが生成される = 6（連続変数の数） + (4:X6カテゴリー変数のレベル + 1:スペア)　+ (6:X8カテゴリー変数のレベル + 1:スペア)
DLd <- h2o.deeplearning(x, y, train, nfolds=10, model_id="DL_defaults", seed=seed)
DLd@parameters$epochs
DLd
h2o.scoreHistory(DLd)
( pDLd <- h2o.performance(DLd, test) )

# epoch数を大きくするため、early stopping を導入 -->　随分MSEが改善された
# train_samples_per_iteration = 0:  デフォルトの-2から0に変更、各epochごとにscoringする
# score_interval = 3:  デフォルトの５秒から3秒に短縮、early stopping がもっと早く反応
m2 <- h2o.deeplearning(x, y, train, nfolds=10, model_id="DL_tuned",
  stopping_metric="MSE", stopping_tolerance=0.0005, stopping_rounds=3, epochs=1000, train_samples_per_iteration=0, score_interval=3)
m2@parameters$epochs
m2
h2o.scoreHistory(m2)

# 非線形なので、隠れ層を3層にしてみる、入力層のサイズが18なので、その倍数（3倍、9倍、18倍で設定、だんだん大きくする）で設定
# 計算時間短縮のため、nfolds=6　に小さくする
m3 <- h2o.deeplearning(x, y, train, nfolds=6, model_id="DL_tuned2",
  hidden=c(54,162,324),
  stopping_metric="MSE", stopping_tolerance=0.0005, stopping_rounds=3, epochs=1000, train_samples_per_iteration=0, score_interval=3)
m3@parameters$epochs
m3
h2o.scoreHistory(m3)

# 隠れ層を3層のうち、1層目を半分、3層目を2倍にする -->　どうも隠れ層の効果はあまりないようである、それよりもエポック数が多い方がよいようだ
m4 <- h2o.deeplearning(x, y, train, nfolds=6, model_id="DL_tuned3",
  hidden=c(27,162,648),
  stopping_metric="MSE", stopping_tolerance=0.0005, stopping_rounds=3, epochs=1000, train_samples_per_iteration=0, score_interval=3)
m4@parameters$epochs
m4
h2o.scoreHistory(m4)

# パラメータのランダムサーチ: 隠れ層サイズ2　で max_models = 50
# hidden_dropout_ratios は2つの隠れ層に対する比率の組み合わせを設定する
# 正則化は、変数数も少ないこと、ノイズが大きいわけではないことからあまり必要ないと思われるが、0以外の値のみ試行
g2 <- h2o.grid("deeplearning",
  search_criteria=list(strategy="RandomDiscrete", max_models=50),
  hyper_params=list(hidden=list(c(54,54), c(162,162)), activation=c("TanhWithDropout", "RectifierWithDropout", "MaxoutWithDropout"),
    hidden_dropout_ratios=list(c(0, 0), c(0, 0.1), c(0, 0.2), c(0, 0.5), c(0.1, 0), c(0.2, 0), c(0.5, 0)), input_dropout_ratio=c(0, 0.1), l1=c(0, 1e-05), l2=c(0, 1e-05)),
  x=x, y=y, training_frame=train, nfolds=6, stopping_metric="MSE", stopping_tolerance=0.0005, stopping_rounds=3, train_samples_per_iteration=0, score_interval=3)
g2
g2@summary_table$residual_deviance
( g2_smry <- as.data.frame( g2@summary_table ) )

# パラメータのランダムサーチ: 隠れ層サイズ3　で max_models = 50
# hidden は一つのパターンにするので、hyper_params ではなく外に出す
g3 <- h2o.grid("deeplearning",
  search_criteria=list(strategy="RandomDiscrete", max_models=50),
  hyper_params=list(activation=c("TanhWithDropout", "RectifierWithDropout", "MaxoutWithDropout"),
    hidden_dropout_ratios=list(c(0, 0, 0), c(0.1, 0, 0), c(0, 0.1, 0), c(0, 0, 0.1), c(0, 0.1, 0.1)), input_dropout_ratio=c(0, 0.1), l1=c(0, 1e-05), l2=c(0, 1e-05)),
  hidden=c(324,162,162), x=x, y=y, training_frame=train, nfolds=6, stopping_metric="MSE", stopping_tolerance=0.0005, stopping_rounds=3, train_samples_per_iteration=0, score_interval=3)
g3
g3@summary_table$residual_deviance
( g3_smry <- as.data.frame( g3@summary_table ) )

# 隠れ層が2と3でパラメータを比較
g2_smry
g3_smry

# ベストモデルの設定:　ドロップアウトがゼロのモデルを選択
# エポック数を大きくする、nfolds=10　に戻す
DLt <- h2o.deeplearning(x, y, train, nfolds=10, model_id="DL_best", seed=seed,
  activation="Tanh", l2=1e-05, hidden=c(162,162),
  stopping_metric="MSE", stopping_tolerance=0.0005, stopping_rounds=5, epochs=2000, train_samples_per_iteration=0, score_interval=3)
DLt@parameters$epochs
DLt
h2o.scoreHistory(DLt)
( pDLt <- h2o.performance(DLt, test) )

# テストデータで予測し、デフォルトモデルとベストモデルを比較
pred_DLd <- h2o.predict(DLd, test)
pred_DLt <- h2o.predict(DLt, test)
df <- as.data.frame( h2o.cbind(pred_DLd$predict, pred_DLt$predict, test$Y2) )
plot(df$Y2, pch=0, ylim=c(0,50))
par(new=T); plot(df$predict, pch=17, col="red", ylab="", ylim=c(0,50));
par(new=T); plot(df$predict0, pch=16, col="blue", ylab="", ylim=c(0,50));


# -----------------------------------------------------------------------------
# 各手法でのベストモデルの比較:  GLM含む
# -----------------------------------------------------------------------------
res <- sapply(c(RFd, RFt, GBMd, GBMt, GLMd, GLMt, DLd, DLt), function(m){
  p <- h2o.performance(m, test)
  c(h2o.mse(p), as.numeric(m@model$cross_validation_metrics_summary['mse',c('mean','sd')]))
})

rownames(res) <- c("Test-MSE", "CV-MSE-mean", "CV-MSE-sd")
colnames(res) <- c("RFd", "RFt", "GBMd", "GBMt", "GLMd", "GLMt", "DLd", "DLt")

par(mfrow=c(1,1))
barplot(res["Test-MSE",], ylab="MSE", ylim=c(0,10), main="Model Performance (lower is better)", cex.lab=1.2, cex.axis=1.2, cex.main=1.2, cex.sub=1.2, cex.names=1.2)


# -----------------------------------------------------------------------------
# 各手法でのベストモデルの比較:  GLM含まない
# -----------------------------------------------------------------------------
res <- sapply(c(RFd, RFt, GBMd, GBMt, DLd, DLt), function(m){
  p <- h2o.performance(m, test)
  c(h2o.mse(p), as.numeric(m@model$cross_validation_metrics_summary['mse',c('mean','sd')]))
})

rownames(res) <- c("Test-MSE", "CV-MSE-mean", "CV-MSE-sd")
colnames(res) <- c("RFd", "RFt", "GBMd", "GBMt", "DLd", "DLt")
res

par(mfrow=c(1,1))
barplot(res["Test-MSE",], ylab="MSE", ylim=c(0,10), main="Model Performance (lower is better)", cex.lab=1.2, cex.axis=1.2, cex.main=1.2, cex.sub=1.2, cex.names=1.2)


# -----------------------------------------------------------------------------
# 様々な手法をアンサンブル -->　R ver3.3.1 では使えない
# -----------------------------------------------------------------------------
library(h2oEnsemble)


# -----------------------------------------------------------------------------
# シャットダウン
# -----------------------------------------------------------------------------
h2o.shutdown()
