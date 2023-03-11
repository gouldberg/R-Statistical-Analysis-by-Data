## R　H2O　基本
## ----------------------------------------------------------------------------
setwd("C:/users/R_work")
library(h2o)


# -----------------------------------------------------------------------------
# 基本：　ディープラーニングによるクラス所属確率予測
# irisデータの例
# -----------------------------------------------------------------------------
# 初期化: -1は all CPUs on the host
h2o.init(nthreads=-1)

# データの読み込み
url_name <- "https://raw.githubusercontent.com/DarrenCook/h2o/bk/datasets/"
data <- h2o.importFile(paste0(url_name, "iris_wheader.csv"))

# データの基礎統計
h2o.describe(data)

# ラベルと説明変数
y <- "class"
x <- setdiff(names(data), y)

# 訓練・テストデータ分割
splits <- h2o.splitFrame(data, 0.8)
train <- splits[[1]];  test <- splits[[2]];

# モデル
# m <- h2o.deeplearning(x, y, train, seed=99, reproducible=TRUE)
m <- h2o.deeplearning(x, y, train)
m

# 予測
# 結果をダウンロードするには、as.data.frame(p)
p <- h2o.predict(m, test)
p
mean(p$predict == test$class)

df <- as.data.frame( h2o.cbind(p$predict, test$class) )
df

# モデルパフォーマンスの詳細確認
# Hit Ratio は、所属確率　トップ1のみでの精度、トップ2を含めた場合の精度
h2o.performance(m, test)
