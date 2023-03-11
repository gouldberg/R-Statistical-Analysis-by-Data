## R　H2O
## データ:  MNIST
## ----------------------------------------------------------------------------
setwd("C:/users/R_work")
library(h2o)
library(tidyverse)
library(data.table)

# -----------------------------------------------------------------------------
# 初期化
# -----------------------------------------------------------------------------
h2o.init(nthreads = -1, max_mem_size="4G")

# -----------------------------------------------------------------------------
# オリジナルデータの読み込み、データ準備
# -----------------------------------------------------------------------------
path <- "C:\\DataforAnalysis2\\MNISt\\"
train_all <- h2o.importFile(paste0(path, "mnist.train.csv.gz"))
test <- h2o.importFile(paste0(path, "mnist.test.csv.gz"))
dim(train_all)

x <- 1:784
y <- 785

train_all[, y] <- as.factor(train_all[, y])
test[, y] <- as.factor(test[, y])

parts <- h2o.splitFrame(train_all, 1.0/6.0)
valid <- parts[[1]]
train <- parts[[2]]
rm(parts)

# -----------------------------------------------------------------------------
# 基礎俯瞰
# -----------------------------------------------------------------------------
train_allt <- as.data.table(train_all)

# 各文字のデータ件数、比率確認　-->　各文字ごとでサンプル数に大きな違いはない
train_allt %>% group_by(C785) %>% count() %>% mutate(ratio=n/sum(n))

# 各文字ごとにデータ分割し、平均的なイメージを描く
# 時計回りに90度回転 t(apply, x, rev)　で向きを揃える
train_allts <- split(train_allt, by="C785")
avg <- lapply(1:10, function(n) sapply(train_allts[[n]], mean))
par(mfrow=c(3,4))
lapply(1:10, function(n) image(t(apply(matrix(avg[[n]][x], ncol=28, byrow=T), 2, rev)), col=grey(255:0/255)))

# 参考: データテーブルに落とさずに、直接 h2o で平均・標準偏差を求める場合
# h2o.mean(), h2o.sd()

# 乱数の設定
seed = 450

# -----------------------------------------------------------------------------
# ランダムフォレスト
# -----------------------------------------------------------------------------
# デフォルトモデル:  検証データが別途用意されているので交差検証はせず、直接検証データを指定する
# 各クラスごとに50の木を生成し、全部で500を生成した
RFd <- h2o.randomForest(x, y, train, validation_frame=valid, model_id="RF_defaults", seed=seed)
RFd; summary(RFd);
RFd@model$training_metrics
RFd@model$validation_metrics
# 混同行列および精度
h2o.confusionMatrix(RFd, valid=TRUE)
h2o.hit_ratio_table(RFd, valid=TRUE)

( pRFd <- h2o.performance(RFd, test) )

# パラメータのランダムサーチ:　時間がかかるので注意
# デフォルトでは、max_depth が上限に達していたので増やしてみる、またそのために early stopping を導入
# 訓練データ数が多いので、min_rows は増やしてみてもよい
# mtries はデフォルトでは 28 となるが、増やしてみる
g1 <- h2o.grid("randomForest",
  search_criteria=list(strategy="RandomDiscrete", max_models=20),
  hyper_params=list(max_depth=c(40, 60), mtries=c(28, 42, 56), sample_rate=c(0.5, 0.7, 0.9), col_sample_rate_per_tree=c(0.75, 0.9, 1.0), min_rows=c(1, 2, 5)),
  x=x, y=y, training_frame=train, validation_frame=valid, ntrees=500, stopping_tolerance=0.0001, stopping_rounds=3, score_tree_interval=3)

# ベストモデルで予測
RFt <- h2o.randomForest(x, y, train, validation_frame = valid, model_id = "RF_tuned", seed = 999,
  min_rows = 2, mtries = 56, col_sample_rate_per_tree = 0.9, sample_rate = 0.9, max_depth = 40, ntrees = 500, stopping_tolerance = 0.0001, stopping_rounds=3, score_tree_interval = 3)
h2o.confusionMatrix(RFt, valid=TRUE)
h2o.hit_ratio_table(RFt, valid=TRUE)

( pRFt <- h2o.performance(RFt, test) )

# デフォルトモデルなどと正解率を比較
cbind(h2o.hit_ratio_table(RFd), h2o.hit_ratio_table(RFd, valid=TRUE), h2o.hit_ratio_table(RFt, valid=TRUE), h2o.hit_ratio_table(pRFt))


# -----------------------------------------------------------------------------
# Gradient Boosting Machines (GBM)
# -----------------------------------------------------------------------------
# デフォルトモデル:  検証データが別途用意されているので交差検証はせず、直接検証データを指定する
GBMd <- h2o.gbm(x, y, train, validation_frame=valid, model_id="GBM_defaults", seed=seed)
GBMd; summary(GBMd);
GBMd@model$training_metrics
GBMd@model$validation_metrics
h2o.confusionMatrix(GBMd, valid=TRUE)
h2o.hit_ratio_table(GBMd, valid=TRUE)

( pGBMd <- h2o.peGBMormance(GBMd, test) )

# パラメータのランダムサーチ:　時間がかかるので注意
g1 <- h2o.grid("gbm", grid_id="GBM_BigStew",
  search_criteria=list(strategy="RandomDiscrete", max_models=50),
  hyper_params=list(max_depth=c(5, 20, 50), min_rows=c(2, 5, 10), sample_rate=c(0.5, 0.8, 0.95, 1.0), col_sample_rate=c(0.5, 0.8, 0.95, 1.0), col_sample_rate_per_tree=c(0.8, 0.99, 1.0), learn_rate=c(0.1), seed=c(701)),
  x=x, y=y, training_frame=train, validation_frame=valid, ntrees=400, stopping_tolerance=0.001, stopping_rounds=3, score_tree_interval=10)

# パラメータのランダムサーチ:　時間がかかるので注意
g2 <- h2o.grid("gbm", grid_id="GBM_Better",
  search_criteria=list(strategy="RandomDiscrete", max_models=9),
  hyper_params=list(max_depth=c(5), min_rows=c(10), sample_rate=c(0.5, 0.8, 0.95), col_sample_rate=c(0.5, 0.8, 0.95), col_sample_rate_per_tree=c(0.8, 0.99), learn_rate=c(0.1), seed=c(701)),
  x=x, y=y, training_frame=train, validation_frame=valid, ntrees=400, stopping_tolerance=0.001, stopping_rounds=3, score_tree_interval=10)

# ベストモデル
GBMt <- h2o.gbm(x, y, train, validation_frame = valid,  model_id="GBM_tuned", seed=seed,
  max_depth = 5,  #Default
  min_rows = 10,  #Default
  sample_rate = 0.95, col_sample_rate = 0.8, col_sample_rate_per_tree = 0.8, learn_rate = 0.01, stopping_tolerance = 0.001, stopping_rounds=3, score_tree_interval = 10, ntrees = 400)

h2o.confusionMatrix(GBMt, valid=TRUE)
h2o.hit_ratio_table(GBMt, valid=TRUE)
( pGBMt <- h2o.performance(GBMt, test) )

# デフォルトモデルなどと正解率を比較
cbind(h2o.hit_ratio_table(GBMd), h2o.hit_ratio_table(GBMd, valid=TRUE), h2o.hit_ratio_table(GBMt, valid=TRUE), h2o.hit_ratio_table(pGBMt))

# -----------------------------------------------------------------------------
# Generalized Linear Models (GLM)
# -----------------------------------------------------------------------------
# GLM
# GLMでも、ゼロとそれ以外の数字を区別できる
# 4 と 9、7 と 9　の区別が難しいようだ
GLMd <- h2o.glm(x, y, train, model_id="GLM_defaults", validation_frame=valid, family="multinomial")
h2o.confusionMatrix(GBMd, valid=TRUE)
h2o.hit_ratio_table(GBMd, valid=TRUE)
( pGLMd <- h2o.performance(GLMd, test) )

# チューニング  -->　結果は改善せず、デフォルトモデルがベスト
# alpha=0: ridge regression   alpha=1: lasso regression   alpha=0.5: elastic net
solvers <- c("IRLSM", "L_BFGS", "COORDINATE_DESCENT_NAIVE", "COORDINATE_DESCENT")
allGrids <- lapply(solvers, function(solver){
  grid_id = paste("GLM", solver, sep="_")
  cat("GRID:", grid_id, "\n")
  h2o.grid("glm", grid_id = grid_id, hyper_params=list(
    alpha=c(0, 0.1, 0.5, 0.99)), x=x, y=y, training_frame=train, validation_frame=valid, lambda_search=TRUE, solver=solver, family="multinomial", max_iterations=100)
})


# -----------------------------------------------------------------------------
# Deep Learning
# 注意!:  通常のPCでは非常に時間がかかる !!!
# -----------------------------------------------------------------------------
# デフォルトモデル --> epoch数が少ない
DLd <- h2o.deeplearning(x, y, train, model_id="DL_defaults", validation_frame=valid, seed=seed)
DLd; summary(DLd);
DLd@model$training_metrics
DLd@model$validation_metrics
h2o.confusionMatrix(DLd, valid=TRUE)
h2o.hit_ratio_table(DLd, valid=TRUE)
h2o.scoreHistory(DLd)

tmp <- as.data.frame(h2o.scoreHistory(DLd))
par(mfrow=c(1,2))
plot(tmp$training_logloss, type="b", lty=1, col="black", ylab="LOGLOSS")
lines(tmp$validation_logloss, type="b", lty=2, col="red")
plot(tmp$training_rmse, type="b", lty=1, col="black", ylab="RMSE")
lines(tmp$validation_rmse, type="b", lty=2, col="red")

( pDLd <- h2o.performance(DLd, test) )

# ベストモデルの設定
system.time(
  DLt <- h2o.deeplearning(x, y, train, validation_frame = valid, model_id = "DL_tuned", seed=seed,
  hidden = c(300,400,500,600), activation = "RectifierWithDropout", hidden_dropout_ratios = c(0.1, 0.1, 0.1, 0.1), input_dropout_ratio = 0.2, l1 = 0.00001,
  classification_stop=-1, stopping_metric = "misclassification", stopping_tolerance = 0.001, stopping_rounds = 8, epochs = 2000)
)

h2o.confusionMatrix(DLt, valid=TRUE)
h2o.hit_ratio_table(DLt, valid=TRUE)

( pDLt <- h2o.performance(DLt, test) )

# デフォルトモデルなどと正解率を比較
cbind(h2o.hit_ratio_table(DLd), h2o.hit_ratio_table(DLd, valid=TRUE), h2o.hit_ratio_table(DLt, valid=TRUE), h2o.hit_ratio_table(pRDLt))
tmp <- as.data.frame(h2o.scoreHistory(DLt))
par(mfrow=c(1,2))
plot(tmp$training_logloss, type="b", lty=1, col="black", ylab="LOGLOSS")
lines(tmp$validation_logloss, type="b", lty=2, col="red")
plot(tmp$training_rmse, type="b", lty=1, col="black", ylab="RMSE")
lines(tmp$validation_rmse, type="b", lty=2, col="red")


# -----------------------------------------------------------------------------
# シャットダウン
# -----------------------------------------------------------------------------
h2o.shutdown()


# -----------------------------------------------------------------------------
# logloss に関する補足
# logloss は、クラス予測で、間違ったクラスに対する予測確率が大きいと、大きな値になる
# -----------------------------------------------------------------------------
MultiLogLoss <- function(act, pred){
  eps <- 1e-15
  pred <- pmin(pmax(pred, eps), 1 - eps)
  sum(act * log(pred) + (1 - act) * log(1 - pred)) * -1/NROW(act)
}

# 1データ、2クラス予測
act <- c(1, 0)

pred1 <- c(0.8, 0.2)
pred2 <- c(0.6, 0.4)
pred3 <- c(0.2, 0.8)
MultiLogLoss(act, pred1)
MultiLogLoss(act, pred2)
MultiLogLoss(act, pred3)

# 2データ、2クラス予測
act1 <- c(1, 0)
act2 <- c(0, 1)
pred1 <- c(0.8, 0.2)
pred2 <- c(0.4, 0.6)
act <- rbind(act1, act2)
pred <- rbind(pred1, pred2)
MultiLogLoss(act, pred)

act1 <- c(1, 0)
act2 <- c(0, 1)
pred1 <- c(0.8, 0.2)
pred2 <- c(0.2, 0.8)
act <- rbind(act1, act2)
pred <- rbind(pred1, pred2)
MultiLogLoss(act, pred)

# 2データ、3クラス予測
act1 <- c(1, 0, 0)
act2 <- c(1, 0, 0)
pred1 <- c(0.60, 0.30, 0.10)
pred2 <- c(0.30, 0.60, 0.10)
act <- rbind(act1, act2)
pred <- rbind(pred1, pred2)
MultiLogLoss(act, pred)
