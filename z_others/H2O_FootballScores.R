## R　H2O
## データ:  Football Scores
##
## 目的変数:　ホームゲームでの勝敗、ドローあるいはアウェイでの勝敗
## 説明変数:　58変数
##
## データの特性
## - 時系列データ
## - ノイズが多い　-->　GBMは生成木の数を多くすると過剰適合になる
##
## 結果
## - たった1つの変数によるロジスティック回帰がもっとも良い
## - これに匹敵できるのはランダムフォレストがやっと
## - Deep Learning はパラメータチューニングが難しく、性能が最も悪い
## ----------------------------------------------------------------------------
setwd("C:/users/R_work")
library(h2o)
library(tidyverse)

# -----------------------------------------------------------------------------
# 初期化
# -----------------------------------------------------------------------------
h2o.init(nthreads = -1)

# -----------------------------------------------------------------------------
# オリジナルデータの読み込み、データ準備
# -----------------------------------------------------------------------------
path <- "C:\\DataforAnalysis2\\football\\england\\2013-2014\\"
data1 <- h2o.importFile(paste0(path, "Championship.csv"))
data2 <- h2o.importFile(paste0(path, "Conference.csv"))
data3 <- h2o.importFile(paste0(path, "E2.csv"))
data4 <- h2o.importFile(paste0(path, "League1.csv"))
data5 <- h2o.importFile(paste0(path, "League2.csv"))
data6 <- h2o.importFile(paste0(path, "Premier.csv"))

# 参考: pattern を指定するとそのファイルのみ読み込み
# data <- h2o.importFolder(path, pattern="[.]csv$", header=TRUE)

# league1 は除く
data <- h2o.rbind(data1, data2, data3, data5, data6)
head(data)
str(data)

betsH <- data[,c(((1:8)*3)+21, 49, 50)]
betsD <- data[,c(((1:8)*3)+22, 51, 52)]
betsA <- data[,c(((1:8)*3)+23, 53, 54)]
abets <- data[,c(56:59, 61:65)]
stats <- data[,c(5:10, 12:23)]
stats[,c("FTR", "HTR")] <- as.numeric(stats[,c("FTR", "HTR")])


# -----------------------------------------------------------------------------
# 基礎俯瞰
# 変数間の相関関係を確認
# 1.　モデリングに使用できそうな変数はどれか
# 2.　外れ値はないか
# 3.　相関は、直線か曲線か
# 4.　目視で相関はあるが、相関係数は低い組合せはあるか
# -----------------------------------------------------------------------------
h2o.describe(betsH)
h2o.describe(betsD)
h2o.describe(betsA)
h2o.describe(abets)
h2o.describe(stats)

# ピアソンの相関係数
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
 usr <- par("usr"); on.exit(par(usr))
 par(usr = c(0, 1, 0, 1))
 r <- cor(x, y, use = "complete.obs")
 txt <- format(c(r, 0.123456789), digits = digits)[1]
 txt <- paste0(prefix, txt)
 if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
 text(0.5, 0.5, txt, cex = cex.cor * (r-0.80) *5 )
}

# スピアマンの相関係数
panel.cor2 <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  #r <- round(cor(x, y, use="complete.obs", method = "pearson"), 2)
  r <- round(cor(x, y, use="complete.obs", method = "spearman"), 2)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)){
   if(r<0)cex.cor <- 0.8/strwidth(txt)
   else cex.cor <- 0.8/strwidth(paste0("+",txt))  #Allow for lack of minus sign
  }
  text(0.5, 0.5, txt, cex = cex.cor * ifelse(abs(r)<0.5,0.5,abs(r)) )
}

# ピアソンの相関係数
panel.cor3 <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y, use="complete.obs", method = "pearson"), 2)
  #r <- round(cor(x, y, use="complete.obs", method = "spearman"), 2)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)){
   if(r<0)cex.cor <- 0.8/strwidth(txt)
   else cex.cor <- 0.8/strwidth(paste0("+",txt))  #Allow for lack of minus sign
   }
  text(0.5, 0.5, txt, cex = cex.cor * (0.4 + abs(r)) )
}

# スピアマンの相関係数
panel.cor4 <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  #r <- round(cor(x, y, use="complete.obs", method = "pearson"), 2)
  r <- round(cor(x, y, use="complete.obs", method = "spearman"), 2)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)){
   if(r<0)cex.cor <- 0.8/strwidth(txt)
   else cex.cor <- 0.8/strwidth(paste0("+",txt))  #Allow for lack of minus sign
  }
  text(0.5, 0.5, txt, cex = cex.cor * ifelse(abs(r)<0.5,0.5,0.75) )
}

# 相関関係を確認
# どれも相関が高い、特にオッズが低い領域
d_h <- as.matrix(betsH);  d_h <- d_h[complete.cases(d_h),];  cor(d_h);
pairs(d_h, lower.panel = panel.smooth, upper.panel = panel.cor)

d_d <- as.matrix(betsD);  d_d <- d_d[complete.cases(d_d),];  cor(d_d);
pairs(d_d, lower.panel = panel.smooth, upper.panel = panel.cor)
# 比較的相関が低い組合せを拡大して確認
pairs(d_d[,c(3:6,10)], lower.panel = panel.smooth, upper.panel = panel.cor, xaxt = "n", yaxt = "n")

d_a <- as.matrix(betsA);  d_a <- d_a[complete.cases(d_a),];  cor(d_a);
pairs(d_a, lower.panel = panel.smooth, upper.panel = panel.cor)

# abets (Asian betting odds)　は曲線での相関があるので、ピアソンではなくスピアマンの相関係数を使う (panel_cor2)
d_abets <- as.matrix(abets);  d_abets <- d_abets[complete.cases(d_abets),];  cor(d_abets);
pairs(d_abets, lower.panel = panel.smooth, upper.panel = panel.cor)
round(cor(d_abets), 2)
round(cor(d_abets, method = "spearman"), 2)
pairs(d_abets, labels = colnames(d_abets), lower.panel = panel.smooth, upper.panel = panel.cor2, xaxt = "n", yaxt = "n")

# handicap とそれ以外は、相関がかなり強そうだが、-0.3 程度の相関しかない　-->　モデリングに工夫が必要
d <- as.numeric(data[,c("FTR", "BbAvH", "BbAHh")])
round(cor(as.matrix(d), use = "complete.obs"), 3)
round(cor(as.matrix(d), use = "complete.obs", method = "spearman"), 3)
pairs(data[,c("FTR", "BbAvH", "BbAHh")], lower.panel = panel.smooth, upper.panel = panel.cor4)


pairs(stats, lower.panel = panel.smooth, upper.panel = panel.cor3)
v <- cor(as.matrix(stats))
summary(v)
# handicap とそれ以外は、相関がかなり強そうだが、-0.3 程度の相関しかない　-->　モデリングに工夫が必要
d <- as.numeric(data[,c("FTR", "BbAvH", "BbAHh")])
round(cor(as.matrix(d), use = "complete.obs"), 3)
round(cor(as.matrix(d), use = "complete.obs", method = "spearman"), 3)
pairs(data[,c("FTR", "BbAvH", "BbAHh")], lower.panel = panel.smooth, upper.panel = panel.cor4)

# 一度シャットダウンする
h2o.shutdown()

# -----------------------------------------------------------------------------
# 訓練・検証・テストデータの読み込み、データ準備
# train.csv, valid.csv, test.csv　は missing data あり
# train2.csv, valid2.csv, test2.csv　は missing data なし
# -----------------------------------------------------------------------------
# メモリサイズを設定する
h2o.init(nthreads = -1, max_mem_size = "3G")

# データ読み込み:  missing data が残っているデータ
path <- "C:\\DataforAnalysis2\\football\\england\\2013-2014\\"
train <- h2o.importFile(paste0(path, "football.train.csv"))
valid <- h2o.importFile(paste0(path, "football.valid.csv"))
test <- h2o.importFile(paste0(path, "football.test.csv"))

# FTR == "H" を1とした因子を作成　など
train$HomeWin <- as.factor(train$FTR == "H")
valid$HomeWin <- as.factor(valid$FTR == "H")
test$HomeWin <- as.factor(test$FTR == "H")
train$ScoreDraw <- as.factor(train$FTHG > 0 & train$FTHG == train$FTAG)
valid$ScoreDraw <- as.factor(valid$FTHG > 0 & valid$FTHG == valid$FTAG)
test$ScoreDraw <- as.factor(test$FTHG > 0 & test$FTHG == test$FTAG)

statFields <- c(
   "FTHG", "FTAG", "FTR", "HTHG", "HTAG", "HTR",
   "HS", "AS", "HST", "AST", "HF", "AF",
   "HC", "AC", "HY", "AY", "HR", "AR",
   "HomeWin", "ScoreDraw"
)
ignoreFields <- c("Date", "HomeTeam", "AwayTeam", statFields)
xNoOdds <- c("Div", "HS1", "AS1", "HST1", "AST1",
   "HF1", "AF1", "HC1", "AC1", "HY1", "AY1", "HR1", "AR1",
   "res1H", "res1A", "res5H", "res5A", "res20H", "res20A"
)

 # 説明変数と目的変数の設定
x <- setdiff(colnames(train), ignoreFields)
#y <- "FTR"  #3-value multinomial
#y <- "ScoreDraw"  #Unbalanced binomial
y <- "HomeWin" #Balanced binomial

# 乱数の設定
seed = 10


# -----------------------------------------------------------------------------
# 複数モデルの指標比較の関数
# -----------------------------------------------------------------------------
compareModels <- function(models, test, labels = NULL){
  # Use model IDs as default labels, if not given
  if(is.null(labels)){ labels <- lapply(models, function(m) m@model_id) }

  res <- sapply(models, function (m){
    mcmsT <- m@model$training_metrics@metrics$max_criteria_and_metric_scores
    mcmsV <- m@model$validation_metrics@metrics$max_criteria_and_metric_scores
    maix <- which(mcmsT$metric=="max accuracy")  #4 (at the time of writing)
    th <- mean(mcmsT[maix, 'threshold'],  mcmsV[maix, 'threshold'] )

    pf <- h2o.performance(m, test)
    tms <- pf@metrics$thresholds_and_metric_scores
    ix <- apply(outer(th, tms$threshold, "<="), 1, sum)
    if(ix < 1) ix <- 1  #Use first entry if less than all of them

    matrix(c(
      h2o.auc(m, TRUE, TRUE), pf@metrics$AUC,
      mcmsT[maix, 'value'], mcmsV[maix, 'value'], tms[ix, 'accuracy'],
      h2o.logloss(m, TRUE, TRUE), pf@metrics$logloss,
      h2o.mse(m, TRUE, TRUE), pf@metrics$MSE
      ), ncol = 4)
  }, simplify = "array")

  dimnames(res) <- list(c("train","valid","test"), c("AUC","Accuracy","logloss", "MSE"), labels)
  res
}

# -----------------------------------------------------------------------------
# ランダムフォレスト
# -----------------------------------------------------------------------------
# デフォルトモデル:  検証データが別途用意されているので交差検証はせず、直接検証データを指定する
# Oddsあり、Oddsなしモデル
# 結果はあまりよくない -->　min_depth = max_depth = 20 となり、max_depth はチューニングが必要と思われる
RFd_Odds <- h2o.randomForest(x, y, train, validation_frame=valid, model_id="RF_defaults_Odds", seed=seed)
RFd_Odds; summary(RFd_Odds);
RFd_Odds@model$training_metrics
RFd_Odds@model$validation_metrics
par(mfrow=c(1,1)); barplot(h2o.varimp(RFd_Odds)$percentage, horiz=TRUE, names.arg=h2o.varimp(RFd_Odds)$variable, xlab="percentage")
( pRFd_Odds <- h2o.performance(RFd_Odds, test) )

RFd_NoOdds <- h2o.randomForest(xNoOdds, y, train, validation_frame=valid, model_id="RF_defaults_NoOdds", seed=seed)
RFd_NoOdds; summary(RFd_Noodds);
RFd_NoOdds@model$training_metrics
RFd_NoOdds@model$validation_metrics
( pRFd_NoOdds <- h2o.performance(RFd_NoOdds, test) )

# モデル比較
( res <- compareModels(c(RFd_Odds, RFd_NoOdds), test) )

# パラメータのランダムサーチ
# max_models はハイパーパラメタの組み合わせの 1/3
# デフォルトで精度がよくなかったため、指標はAUCを設定
g1 <- h2o.grid("randomForest",
  search_criteria=list(strategy="RandomDiscrete", max_models=54),
  hyper_params=list(max_depth=c(20, 40, 60), mtries=c(5, 7, 10), sample_rate=c(0.5, 0.75, 0.95), col_sample_rate_per_tree=c(0.9, 1.0), min_rows=c(1, 2, 5)),
  x=x, y=y, training_frame=train, validation_frame=valid, ntrees=500, stopping_metric="AUC", stopping_tolerance=0, stopping_rounds=4)

( g1_auc <- h2o.getGrid(g1@grid_id, sort_by="auc", decreasing=TRUE) )
range(g1_auc@summary_table$auc)

d <- as.data.frame(g1@summary_table)
topModels <- lapply(head(d$model_ids), h2o.getModel)
res <- compareModels(topModels, test)

# ベストモデルで予測
# 実際のパラメタは、min_rwos = 60 でかなり大きくなり、sample_rate = 0.35 と小さくなった　（データが大きいからか？）
RFt_Odds <- h2o.randomForest(x, y, train, validation_frame = valid, model_id="RF_tuned_Odds", seed=seed,
  sample_rate = 0.35, min_rows = 60, mtries = 5, ntrees=500, stopping_metric = "AUC", stopping_tolerance = 0, stopping_rounds = 4)
RFt_NoOdds <- h2o.randomForest(xNoOdds, y, train, validation_frame = valid, model_id="RF_tuned_NoOdds", seed=seed,
  sample_rate = 0.35, min_rows = 60, mtries = 5, ntrees=500, stopping_metric = "AUC", stopping_tolerance = 0, stopping_rounds = 4)
( pRFt_Odds <- h2o.performance(RFt_Odds, test) )
( pRFt_NoOdds <- h2o.performance(RFt_NoOdds, test) )
# ベストモデルでも、AUC はデフォルトから3～4%程度しか向上せず、60%台と低い
( res <- compareModels(c(RFd_Odds, RFt_Odds), test) )
( res <- compareModels(c(RFd_NoOdds, RFt_NoOdds), test) )


# -----------------------------------------------------------------------------
# Gradient Boosting Machines (GBM)
# -----------------------------------------------------------------------------
# デフォルトモデル:  検証データが別途用意されているので交差検証はせず、直接検証データを指定する
# Oddsあり、Oddsなしモデル
# AUCはランダムフォレストのデフォルトモデルよりも若干よい
# min_depth = max_depth = 5 で改善余地あり、また number of trees = 50 と少ない
GBMd_Odds <- h2o.gbm(x, y, train, validation_frame=valid, model_id="GBM_defaults_Odds", seed=seed)
GBMd_Odds; summary(GBMd_Odds);
GBMd_Odds@model$training_metrics
GBMd_Odds@model$validation_metrics
par(mfrow=c(1,1)); barplot(h2o.varimp(GBMd_Odds)$percentage, horiz=TRUE, names.arg=h2o.varimp(GBMd_Odds)$variable, xlab="percentage")
( pGBMd_Odds <- h2o.peGBMormance(GBMd_Odds, test) )

GBMd_NoOdds <- h2o.gbm(xNoOdds, y, train, validation_frame=valid, model_id="GBM_defaults_NoOdds", seed=seed)
GBMd_NoOdds; summary(GBMd_Noodds);
GBMd_NoOdds@model$training_metrics
GBMd_NoOdds@model$validation_metrics
( pGBMd_NoOdds <- h2o.peGBMormance(GBMd_NoOdds, test) )

# モデル比較
( res <- compareModels(c(GBMd_Odds, GBMd_NoOdds), test) )

# ベストモデル -->　生成する木を多くしたところ過剰適合になってしまった（データのノイズが大きい）
# score_tree_interval = 10 で長くして、生成される木の数を多くする
# max_depth = 12 に増やす
# learn_rate = 0.01（デフォルトは0.1）
# balance_classes = TRUE を導入
GBMt_Odds <- h2o.gbm(x, y, train, validation_frame = valid,  model_id="GBM_tuned_Odds", seed=seed,
  max_depth = 12, min_rows = 40, sample_rate = 0.9, col_sample_rate = 0.95, col_sample_rate_per_tree = 0.9, learn_rate = 0.01, balance_classes = TRUE, ntrees = 500,
  stopping_metric = "misclassification", stopping_tolerance = 0, stopping_rounds = 4, score_tree_interval = 10)

GBMt_NoOdds <- h2o.gbm(xNoOdds, y, train, validation_frame = valid,  model_id="GBM_tuned_NoOdds", seed=seed,
  max_depth = 12, min_rows = 40, sample_rate = 0.9, col_sample_rate = 0.95, col_sample_rate_per_tree = 0.9, learn_rate = 0.01, balance_classes = TRUE, ntrees = 500,
  stopping_metric = "misclassification", stopping_tolerance = 0, stopping_rounds = 4, score_tree_interval = 10)

( pGBMt_Odds <- h2o.performance(GBMt_Odds, test) )
( pGBMt_NoOdds <- h2o.performance(GBMt_NoOdds, test) )

# ベストモデルでも×？
( res <- compareModels(c(GBMd_Odds, GBMt_Odds), test) )
( res <- compareModels(c(GBMd_NoOdds, GBMt_NoOdds), test) )


# -----------------------------------------------------------------------------
# Generalized Linear Models (GLM)
# -----------------------------------------------------------------------------
# GLM
GLMd_base <- h2o.glm("BbAvH", y, train, model_id="GLM_defaults_base", validation_frame=valid, family="binomial")
GLMd_Odds <- h2o.glm(x, y, train, model_id="GLM_defaults_Odds", validation_frame=valid, family="binomial")
GLMd_NoOdds <- h2o.glm(xNoOdds, y, train, model_id="GLM_defaults_NoOdds", validation_frame=valid, family="binomial")
( pGLMd_base <- h2o.performance(GLMd_base, test) )
( pGLMd_Odds <- h2o.performance(GLMd_Odds, test) )
( pGLMd_NoOdds <- h2o.performance(GLMd_NoOdds, test) )

# モデル比較 -->　たった一つの変数の方が成績がよい
( res <- compareModels(c(GLMd_base, GLMd_Odds, GLMd_NoOdds), test) )

# 重要な変数を確認する
h2o.varimp(GLMd_Odds)
h2o.varimp(GLMd_NoOdds)

# チューニング  -->　結果は改善しない
# alpha=0: ridge regression   alpha=1: lasso regression   alpha=0.5: elastic net
g <- h2o.grid("glm", grid_id = "GLM_1",
  hyper_params=list(alpha=c(0, 0.5, 0.99)),
  x=x, y=y, training_frame=train, validation_frame=valid, family="binomial", lambda_search=TRUE,
  stopping_metric="AUC", stopping_tolerance=0, stopping_rounds=4, max_iterations=100)

( g_auc <- h2o.getGrid(g@grid_id, sort_by="auc", decreasing=TRUE) )
d <- as.data.frame(g@summary_table)
topModels <- lapply(head(d$model_ids), h2o.getModel)
( res <- compareModels(c(GLMd_base, topModels), test) )


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
# Deep Learning は missing data があると×なので、除いたデータを読み込む
path <- "C:\\DataforAnalysis2\\football\\england\\2013-2014\\"
train <- h2o.importFile(paste0(path, "football.train2.csv"))
valid <- h2o.importFile(paste0(path, "football.valid2.csv"))
test <- h2o.importFile(paste0(path, "football.test2.csv"))

train$HomeWin <- as.factor(train$FTR == "H")
valid$HomeWin <- as.factor(valid$FTR == "H")
test$HomeWin <- as.factor(test$FTR == "H")
train$ScoreDraw <- as.factor(train$FTHG > 0 & train$FTHG == train$FTAG)
valid$ScoreDraw <- as.factor(valid$FTHG > 0 & valid$FTHG == valid$FTAG)
test$ScoreDraw <- as.factor(test$FTHG > 0 & test$FTHG == test$FTAG)

# デフォルトモデル --> epoch数が少ない
DLd_Odds <- h2o.deeplearning(x, y, train, model_id="DL_defaults_Odds", validation_frame=valid, seed=seed)
DLd_Odds
h2o.scoreHistory(DLd_Odds)
( pDLd_Odds <- h2o.performance(DLd_Odds, test) )

DLd_NoOdds <- h2o.deeplearning(xNoOdds, y, train, model_id="DL_defaults_NoOdds", validation_frame=valid, seed=seed)
DLd_NoOdds
h2o.scoreHistory(DLd_NoOdds)
( pDLd_NoOdds <- h2o.performance(DLd_NoOdds, test) )

# モデル比較 -->　過剰適合はおこしていないようだ
( res <- compareModels(c(DLd_Odds, DLd_NoOdds), test) )

# epoch数を大きくするため、early stopping を導入 -->　特によくなっていない、学習曲線を描くと過剰適合がおこっている
m2_NoOdds <- h2o.deeplearning(xNoOdds, y, train, validation_frame=valid, model_id="DL_ES_NoOdds", seed=seed, replicate_training_data=TRUE,
  stopping_metric="AUC", stopping_tolerance=0.001, stopping_rounds=10, epochs=1000)
( res <- compareModels(c(DLd_NoOdds, m2_NoOdds), test) )
tmp <- as.data.frame(h2o.scoreHistory(m2_NoOdds))
par(mfrow=c(1,1))
plot(tmp$training_rmse, type="l", lty=1, col="black")
lines(tmp$validation_rmse, type="l", lty=2, col="red")

# ベストモデルの設定 -->　あまり良くならない
DLt_Odds <- h2o.deeplearning(x, y, train, validation_frame = valid, model_id = "DL_tuned_Odds", replicate_training_data = T, balance_classes = T, shuffle_training_data = T,
  hidden = c(200,200,200), activation = "RectifierWithDropout", hidden_dropout_ratios = c(0.5, 0.3, 0.3), input_dropout_ratio = 0.3, l1 = 0.0005, l2 = 0.0005,
  stopping_metric = "AUC", stopping_tolerance = 0.001, stopping_rounds = 4, epochs = 2000)
DLt_NoOdds <- h2o.deeplearning(xNoOdds, y, train, validation_frame = valid, model_id = "DL_tuned_NoOdds", replicate_training_data = T, balance_classes = T, shuffle_training_data = T,
  hidden = c(200,200,200), activation = "RectifierWithDropout", hidden_dropout_ratios = c(0.5, 0.3, 0.3), input_dropout_ratio = 0.3, l1 = 0.0005, l2 = 0.0005,
  stopping_metric = "AUC", stopping_tolerance = 0.001, stopping_rounds = 4, epochs = 2000)
( res <- compareModels(c(DLd_NoOdds, m2_NoOdds, DLt_NoOdds), test) )

tmp <- as.data.frame(h2o.scoreHistory(DLt_NoOdds))
par(mfrow=c(1,1))
plot(tmp$training_rmse, type="b", lty=1, col="black")
lines(tmp$validation_rmse, type="b", lty=2, col="red")

# -----------------------------------------------------------------------------
# シャットダウン
# -----------------------------------------------------------------------------
h2o.shutdown()
