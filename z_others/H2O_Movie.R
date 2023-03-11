## R　H2O
## データ:  Movie
## TFIDF計算済みのマトリクス：　映画　×　評論ワード
##
## データの特性
## - スパースマトリクス
##
## 手法
## - k-means クラスタリング
## - autoencoder で次元圧縮　-->　クラスタリング
## - 主成分分析
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
path <- "C:\\DataforAnalysis2\\"
data <- h2o.importFile(paste0(path, "movie.tfidf.csv"))


# -----------------------------------------------------------------------------
# k-means クラスタリングで、tfidf に基づき映画を分類
# -----------------------------------------------------------------------------
# モデル
# k-means++ を実施、その他 "Furthest", "Random" を指定できる
clst <- h2o.kmeans(data, x=2:564, k=5, standardize=FALSE, init="PlusPlus")
clst

# クラスター番号
( p <- h2o.predict(clst, data) )

# 映画をクラスターごとに分けて表示
tapply(as.vector(data[,1]), as.vector(p$predict), print)


# -----------------------------------------------------------------------------
# autoencoder で、映画評論のキーワードをさらに2次元に圧縮　-->　2次元でクラスタリング
# -----------------------------------------------------------------------------
m <- h2o.deeplearning(2:564, training_frame=data, hidden=c(2), autoencoder=TRUE, activation="Tanh")

# 二次元特徴量を抽出、最初の30映画のみ可視化
f <- h2o.deepfeatures(m, data, layer=1)
d <- as.matrix(f[1:30,])
labels <- as.vector(data[1:30,1])
plot(d, pch=17)
text(d, labels, pos=3)

# 二次元特徴量でクラスタリング
clst2 <- h2o.kmeans(f, x=1:2, k=5, standardize=FALSE, init="PlusPlus")
clst2

# クラスター番号
( p2 <- h2o.predict(clst2, f) )

# 映画をクラスターごとに分けて表示
tapply(as.vector(data[,1]), as.vector(p2$predict), print)


# 隠れ層を5層にし、第3層のノードを2～20に変化させてみる
# また、epochs=5, 400 を試す
node <- 2:20
sim1 <- lapply(node, function(n) h2o.deeplearning(2:564, training_frame=data, hidden=c(128,64,n,64,128), autoencoder=TRUE, activation="Tanh", epochs=5))
sim2 <- lapply(node, function(n) h2o.deeplearning(2:564, training_frame=data, hidden=c(128,64,n,64,128), autoencoder=TRUE, activation="Tanh", epochs=400))

# ノード数とMSEの変化を確認する
# ノード数が増えるほど、MSEも減少する、epochs が多いとMSEが低くなるが、ノード数が少ないと逆にepoch数が少ない方がMSEが小さい
sim1_mse <- c();  sim2_mse <- c();
sim1_mse <- sapply(1:19, function(n) { sim1_mse <- c(sim1_mse, sim1[[n]]@model$training_metrics@metrics$MSE) })
sim2_mse <- sapply(1:19, function(n) { sim2_mse <- c(sim2_mse, sim2[[n]]@model$training_metrics@metrics$MSE) })
mfrow(c=(1,1))
plot(sim1_mse, type="b", col="black", lty=1, ylim=c(0, 0.05), xlab="dimension reduction", ylab="MSE")
lines(sim2_mse, type="b", col="blue", lty=2, ylim=c(0, 0.05))


# 隠れ層=5 の第3層から二次元特徴量を抽出し、クラスタリング
f2 <- h2o.deepfeatures(sim2[[1]], data, layer=3)
d2 <- as.matrix(f2[1:30,])
labels <- as.vector(data[1:30,1])
plot(d2, pch=17)
text(d2, labels, pos=3)
clst3 <- h2o.kmeans(f2, x=1:2, k=5, standardize=FALSE, init="PlusPlus")
clst3

# クラスター番号
( p3 <- h2o.predict(clst3, f2) )

# 映画をクラスターごとに分けて表示
tapply(as.vector(data[,1]), as.vector(p3$predict), print)


# -----------------------------------------------------------------------------
# 主成分分析による次元削減
# -----------------------------------------------------------------------------
m_pcr <- h2o.prcomp(data, 2:564, k=2)
p_pcr <- h2o.predict(m_pcr, data)
d_pcr <- as.matrix(p_pcr[1:30,])
labels <- as.vector(data[1:30,1])
plot(d_pcr, pch=17)
text(d_pcr, labels, pos=3)


# -----------------------------------------------------------------------------
# シャットダウン
# -----------------------------------------------------------------------------
h2o.shutdown()
