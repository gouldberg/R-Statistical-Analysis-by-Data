# ------------------------------------------------------------------------------
# ロジスティック回帰　rmsパッケージ
# Data Reduction
# Case Study:  データ prostate
# ------------------------------------------------------------------------------
# Redundancy Analysis：　他の変数の線形で予測できる変数の探索
# 変数のクラスタリング:  クラスタリングの括りを検討し、後にそのクラスタ内で主成分を抽出するなどに使う
# データ変換　および　欠損値補完
# 次元圧縮：　主成分分析、スパース主成分分析、クラスタ内スコアを使った主成分抽出
# モデル比較 by AIC
#  -- 主成分は第何成分まで使えばよいか？
#  -- クラスタ内スコアを使った主成分ではどうか？
#  -- スパース主成分分析で、nonzero 負荷のみを使った場合
#  -- ACE nonparametric additive regresssion で変換したデータではどうか？
# ------------------------------------------------------------------------------
setwd("C:\\Users\\光世\\Desktop\\#R_ロジスティック回帰\\")
library(dplyr)
library(rms)
library(Hmisc)


# ------------------------------------------------------------------------------
# データ読み込み
# ------------------------------------------------------------------------------
getHdata(prostate)

# 日付データが数値表現になっているので、変換
prostate$sdate %>% head()
prostate$sdate <- as.Date(prostate$sdate)
prostate$sdate %>% head()

summary(prostate[2:17])

d <- describe(prostate[2:17])
d


# ------------------------------------------------------------------------------
# Redundancy Analysis:  他の変数の線形で予測できる変数を探索
# ただし、予測できたとしてどの程度の決定係数が担保できるかを確認
# ------------------------------------------------------------------------------
# 自由度削減のため、ekg, rxn, pfn をそれぞれ自由度1に削減　（1-0にする、連続変数にする）
# また、線形仮定を強制するため
prostate <- transform(prostate,
            ekg.norm = 1*(ekg %in% c("normal","benign")),
            rxn = as.numeric(rx),
            pfn = as.numeric(pf))

# Force pfn, rxn to be linear because of difficulty of placing
# knots with so many ties in the data
# Note: all incomplete cases are deleted (inefficient)

# low threshold (0.3) for R^2
r2 <- 0.3
redun(~ stage + I(rxn) + age + wt + I(pfn) + hx +
      sbp + dbp + ekg.norm + hg + sz + sg + ap + bm,
      r2=r2, type='adjusted', data=prostate)

# stage:  他の変数で予測でき、R^2 = 0.658
# しかし、他の sbp, bm, sg の変数を削除すると、R^2 = 0.494 まで悪化


# ------------------------------------------------------------------------------
# 変数のクラスタリング
# ------------------------------------------------------------------------------
x <- with(prostate,
          cbind(stage, rx, age, wt, pf, hx, sbp, dbp,
                ekg.norm, hg, sz, sg, ap, bm))

# ap の分布は歪みが大きいので、連続変数についてはスピアマンの順位相関を使う
# If no missing data, could use cor(apply(x, 2, rank))
r <- rcorr(x, type="spearman")    # rcorr in Hmis
r <- r$r
( maxabsr <- max(abs(r[row(r) != col(r)])) )

# 最大の順位相関係数 = 0.785


# 順位相関を描画
# 最大の値は = 0.785
p <- nrow(r)
plot(c(-.35, p+.5),c(.5,p+.25), type='n', axes=FALSE, xlab='',ylab='')
v <- dimnames(r)[[1]]
text(rep(.5,p), 1:p, v, adj=1)
for(i in 1:(p-1)) {
  for(j in (i+1):p) {
    lines(c(i,i),c(j,j+r[i,j]/maxabsr/2), lwd=3, lend="butt")
    lines(c(i-.2,i+.2),c(j,j), lwd=1, col=gray(.7))
  }
  text(i, i, v[i], srt=-45, adj=0)
}


# 階層クラスタリング (Hoeffding D statistics　を類似度とする:  非単調な関連性を検知する）
# 確かに順位相関が高いもの同士がクラスタリングされている
vc <- varclus(~ stage + rxn + age + wt + pfn + hx +
              sbp + dbp + ekg.norm + hg + sz + sg + ap + bm,
              sim='hoeffding', data=prostate)
dev.new()
plot(vc)


# ------------------------------------------------------------------------------
# 変数変換　および　欠損値補完 (single imputation)
# transcan
#  - maximum generalized variance method を使って、回帰の左右両サイドの変数を変換する
#  - 各説明変数を他の変数で予測
#  - 連続変数には、restricted cubis splies、カテゴリー変数はダミー変数化
# ------------------------------------------------------------------------------
# 準備:　ekg の水準を統合する　など
levels(prostate$ekg)[levels(prostate$ekg) %in% c('old MI', 'recent MI')] <- 'MI'
prostate$pf.coded <- as.integer(prostate$pf)
# make a numeric version; combine last 2 levels of original
levels(prostate$pf) <- levels(prostate$pf)[c(1,2,3,3)]


# 変数の同時変換 (simultaneous transformation)　および　single imputation
ptrans <-
  transcan(~ sz + sg + ap + sbp + dbp +
           age + wt + hg + ekg + pf + bm + hx, imputed=TRUE,
           transformed=TRUE, trantab=TRUE, pl=FALSE,
           show.na=TRUE, data=prostate, frac=.1, pr=FALSE)
summary(ptrans, digits=4)

#
# ggplot に ptrans を入れると、指定しなくても各変数ごとの状況を表示
# red plus sign が補完された値
ggplot(ptrans, scale=TRUE) +
  theme(axis.text.x=element_text(size=6))


# 欠損値を補完
imputed <- impute(ptrans, data=prostate, list.out=TRUE)
imputed <- as.data.frame(imputed)


# ------------------------------------------------------------------------------
# 次元圧縮　主成分分析
# 本来は princomp ではなく 新しい prcomp を使う
# princomp は数値行列のみしか扱えない
# ------------------------------------------------------------------------------
# 関数:　分散の説明比率の累積値を算出
addscree <- function(x, npcs=min(10, length(x$sdev)), plotv=FALSE, col=1, offset=.8, adj=0, pr=FALSE) {
  vars <- x$sdev^2
  cumv <- cumsum(vars)/sum(vars)
  if(pr) print(cumv)
  text(1:npcs, vars[1:npcs] + offset*par('cxy')[2],
       as.character(round(cumv[1:npcs], 2)),
       srt=45, adj=adj, cex=.65, xpd=NA, col=col)
  if(plotv) lines(1:npcs, vars[1:npcs], type='b', col=col)
}


# 準備:　ekg は因子型なので、design matrix (dummy variable) にする等
Ekg <- model.matrix(~ ekg, data=imputed)[, -1]
pfn <- prostate$pfn


# 主成分分析:　欠損値補完された元のデータ　および　変換されたデータ
prin.raw <- princomp(~ sz + sg + ap + sbp + dbp + age + wt + hg + Ekg + pfn + bm + hx,　cor=TRUE, data=imputed)
prin.trans <- princomp(ptrans$transformed, cor=TRUE)

summary(prin.raw)
summary(prin.trans)

# バイプロット
# 各レコード:  1-2主成分での得点 = prin.raw$scores[,c(1,2)]
# 主成分ベクトル:
biplot(prin.raw, scale=0)
biplot(prin.trans, scale=0)


# スクリープロットを表示
# スクリープロットの縦軸は、固有値 = prin.raw$sdev^2
# ptrans の方が、同じ主成分数で説明力が高い
plot(prin.raw, type='lines', main='', ylim=c(0,3));  addscree(prin.raw);
addscree(prin.trans, npcs=10, plotv=TRUE, col='red', offset=-.8, adj=1)

# screeplot(prin.raw, tyoe="lines")
# screeplot(prin.trans, type="lines")


# ----　参考
# バイプロットの1-2主成分得点
prin.raw$scores[279,] / sqrt(prin.raw$sdev^2 * nrow(prin.raw$scores))
biplot(prin.raw)

# バイプロットの1-2主成分得点
prin.raw$scores[279,]
biplot(prin.raw, scale=0)

# バイプロットの主成分ベクトル　（ちょっと合わないが・・・） = √(固有値　×　n) * 主成分（固有ベクトル）
sqrt(prin.raw$sdev^2 * nrow(prin.raw$scores)) * prin.raw$loadings
biplot(prin.raw)

# バイプロットの主成分ベクトル　（ちょっと合わないが・・・） = 主成分（固有ベクトル）
prin.raw$loadings
biplot(prin.raw, scale=0)


# ------------------------------------------------------------------------------
# 主成分を使ったケース vs.　主成分を使わないケース　の比較
# どこまで主成分を使えば、AICが最小になるか？　-->　このデータでは主成分5つを使ったモデルが一番よい、次がスプライン適用のフルモデル
# 主成分を使わないフルモデル vs. 主成分を使ったモデル
# 主成分を使わないが非線形フルモデル vs. 主成分を使ったモデル
# ------------------------------------------------------------------------------
# Cox比例ハザードモデル用に変数生成
S <- with(prostate, Surv(dtime, status != "alive"))

pcs <- prin.raw$scores
aic <- numeric(16)
for(i in 1:16) {
  ps <- pcs[,1:i]
  aic[i] <- AIC(cph(S ~ ps))
}
plot(1:16, aic, xlab='Number of Components Used', ylab='AIC', type='l', ylim=c(3950,4000))

f <- cph(S ~ sz + sg + log(ap) + sbp + dbp + age + wt + hg + ekg + pf + bm + hx, data=imputed)
abline(h=AIC(f), col='blue')

## The following model in the 2nd edition no longer converges
# f <- cph(S ~ rcs(sz,5) + rcs(sg,5) + rcs(log(ap),5) +
#          rcs(sbp,5) + rcs(dbp,5) + rcs(age,3) + rcs(wt,5) +
#          rcs(hg,5) + ekg + pf + bm + hx,
#          tol=1e-14, data=imputed)
f <- cph(S ~ rcs(sz,4) + rcs(sg,4) + rcs(log(ap),5) + rcs(sbp,4) + rcs(dbp,4) + rcs(age,3) + rcs(wt,4) + rcs(hg,4) + ekg + pf + bm + hx, tol=1e-14, data=imputed)
abline(h=AIC(f), col='blue', lty=2)


# 主成分を使わないフルモデル（青線）よりも、5つの主成分を使った方が AICがよい
# 連続変数について非線形（スプラインモデル）を適用しても、主成分5つを使った方がよい


# ------------------------------------------------------------------------------
# 主成分を使ったケース vs.　主成分を使わないケース　の比較
# transformed データを使って
# 変数クラスターごとに第一主成分を抽出し、モデリング -->　さらにAICがよくなる !!!!
# ------------------------------------------------------------------------------
# 関数:  各レコードごとに第一主成分のスコアを取得する関数
pco <- function(v) {
  f <- princomp(ptrans$transformed[,v], cor=TRUE)
  vars <- f$sdev^2
  cat('Fraction of variance explained by PC1:', round(vars[1]/sum(vars),2), '\n')
  f$scores[,1]
}


# 変数を分けて主成分分析し、各レコードごとに第一主成分のスコアを取得する
tumor   <- pco(c('sz','sg','ap','bm'))
bp      <- pco(c('sbp','dbp'))
cardiac <- pco(c('hx','ekg'))

other   <- ptrans$transformed[,c('hg','age','pf','wt')]


# クラスターごとの第一主成分を使ってモデリング
f <- cph(S ~ tumor + bp + cardiac + other)
AIC(f)
print(f)


# さらにAICがよくなる !!!
abline(h=AIC(f), col='green', lty=3)


# ------------------------------------------------------------------------------
# 次元圧縮　スパース主成分分析　"Sparse Principal Components"
# 変数クラスタリング + クラスタ内スコアリング + Redundancy Analysis を含む
# ------------------------------------------------------------------------------
# スパース主成分分析
# グリッドサーチアルゴリズムで目的関数を探す、sPCAgrid はスパース主成分分析に対応
require(pcaPP)  # spca パッケージ　にもスパース主成分分析に対応している
s <- sPCAgrid(ptrans$transformed, k=10, method='sd', center=mean, scale=sd, scores=TRUE, maxiter=10)

plot(s, type='lines', main='', ylim=c(0,3))
addscree(s)
# スパースな負荷量　(nonzero のみ表示)
s$loadings

pcs <- s$scores
aic <- numeric(10)
for(i in 1:10) {
  ps <- pcs[,1:i]
  aic[i] <- AIC(cph(S ~ ps))
}
plot(1:10, aic, xlab='Number of Components Used', ylab='AIC', type='l',  ylim=c(3950,4000))

# 必要な変数の数は若干多いが、成分6-8あたりでは遜色ない


# ------------------------------------------------------------------------------
# ノンパラメトリクなスムーザーを使っての値変換
# ACE nonparametric additive regression:　R^2 を最適化するために、回帰式の左右両サイドを変換する
# ------------------------------------------------------------------------------
# transace　は、casewise deletion はするが、欠損値補完はしないので、最初に補完しておく
x <- with(imputed, cbind(sz, sg, ap, sbp, dbp, age, wt, hg, ekg, pf, bm, hx))

# 単調変換制約のあるものを指定
# binary で変換が不要なものを指定
monotonic <- c("sz","sg","ap","sbp","dbp","age","pf")
par(mfrow=c(4,3))
transace(x, monotonic, categorical="ekg", binary=c("bm","hx"))

# ekg, age および　正負逆転はあるが、transcan で変換したものと大きな差はない
dev.new()
ggplot(ptrans, scale=TRUE) + theme(axis.text.x=element_text(size=6))
