# ------------------------------------------------------------------------------
#
# ------------------------------------------------------------------------------
library(dplyr)
library(ROCR)


# if(dev.cur() <= 1) dev.new()
# opar <- par(ask = interactive() && (.Device %in% c("X11", "GTK", "gnome", "windows","quartz")))


# ------------------------------------------------------------------------------
# 様々なパフォーマンス評価指標と曲線
# ------------------------------------------------------------------------------
data(ROCR.hiv)
str(ROCR.hiv)

pp <- ROCR.hiv$hiv.svm$predictions
ll <- ROCR.hiv$hiv.svm$labels

# ROCRパッケージ特有のデータフォーマット変換
pred <- prediction(predictions=pp, labels=ll)


par(mfrow=c(1,1))

# ROC曲線
perf <- performance(pred, "tpr", "fpr")
plot(perf, avg= "threshold", colorize=T, lwd= 3, main= "With ROCR you can produce standard plots like ROC curves ...")
plot(perf, lty=3, col="grey78", add=T)

# Precision-Recall 曲線
perf <- performance(pred, "prec", "rec")
plot(perf, avg= "threshold", colorize=T, lwd= 3,main= "... Precision/Recall graphs ...")
plot(perf, lty=3, col="grey78", add=T)

# Sensitivity vs. Specificity
perf <- performance(pred, "sens", "spec")
plot(perf, avg= "threshold", colorize=T, lwd= 3, main="... Sensitivity/Specificity plots ...")
plot(perf, lty=3, col="grey78", add=T)

#
perf <- performance(pred, "lift", "rpp")
plot(perf, avg= "threshold", colorize=T, lwd= 3, main= "... and Lift charts.")
plot(perf, lty=3, col="grey78", add=T)


# ------------------------------------------------------------------------------
# クロスバリデーション結果の複数の予測を描画する
# 縦に平均、横に平均　など
# ------------------------------------------------------------------------------
data(ROCR.xval)
str(ROCR.xval)

pp <- ROCR.xval$predictions
ll <- ROCR.xval$labels

pred <- prediction(pp,ll)
perf <- performance(pred,'tpr','fpr')

par(mfrow=c(2,2))
plot(perf, colorize=T, lwd=2, main='ROC curves from 10-fold cross-validation')
plot(perf, avg='vertical', spread.estimate='stderror',lwd=3,main='Vertical averaging + 1 standard error',col='blue')
plot(perf, avg='horizontal', spread.estimate='boxplot',lwd=3,main='Horizontal averaging + boxplots',col='blue')
plot(perf, avg='threshold', spread.estimate='stddev',lwd=2, main='Threshold averaging + 1 standard deviation',colorize=T)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
data(ROCR.hiv)

pp.unnorm <- ROCR.hiv$hiv.svm$predictions
ll <- ROCR.hiv$hiv.svm$labels

# normalize predictions to 0..1
# approxfunc で　interpolation
v <- unlist(pp.unnorm)
pp <- lapply(pp.unnorm, function(run) {approxfun(x = c(min(v), max(v)), y = c(0,1))(run)})


par(mfrow=c(2,2))
# ROC曲線
pred <- prediction(pp, ll)
perf <- performance(pred, "tpr", "fpr")
plot(perf, avg= "threshold", colorize=T, lwd= 3, coloraxis.at=seq(0,1,by=0.2), main= "ROC curve")
plot(perf, col="gray78", add=T)
plot(perf, avg= "threshold", colorize=T, colorkey=F,lwd= 3, main= "ROC curve",add=T)

# Accuracy
perf <- performance(pred, "acc")
plot(perf, avg= "vertical", spread.estimate="boxplot", lwd=3,col='blue', show.spread.at= seq(0.1, 0.9, by=0.1), main= "Accuracy across the range of possible cutoffs")
plot(performance(pred, "cal", window.size= 10), avg="vertical", main= "How well are the probability predictions calibrated?")
plot(0,0,type="n", xlim= c(0,1), ylim=c(0,7), xlab="Cutoff", ylab="Density", main="How well do the predictions separate the classes?")
for (runi in 1:length(pred@predictions)) {
    lines(density(pred@predictions[[runi]][pred@labels[[runi]]=="-1"]), col= "red")
    lines(density(pred@predictions[[runi]][pred@labels[[runi]]=="1"]), col="green")
}

par(mfrow= c(2,2))

# ...you can freely combine performance measures (pcmiss,lift)
data(ROCR.xval)
pred <- prediction(ROCR.xval$predictions, ROCR.xval$labels)
perf <- performance(pred,"pcmiss","lift")

# plot(perf, colorize=T)
plot(perf, colorize=T, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(1.2,1.2), avg="threshold", lwd=3, main= "You can freely combine performance measures ...")


data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf, colorize=T, colorkey.pos="top", print.cutoffs.at=seq(0,1,by=0.1), text.cex=1, text.adj=c(1.2, 1.2), lwd=2)

# ... cutoff stacking
data(ROCR.xval)

pred <- prediction(ROCR.xval$predictions, ROCR.xval$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf, print.cutoffs.at=seq(0,1,by=0.2),
  text.cex=0.8,
  text.y=lapply(as.list(seq(0,0.5,by=0.05)), function(x) { rep(x,length(perf@x.values[[1]])) } ),
  col= as.list(terrain.colors(10)),
  text.col= as.list(terrain.colors(10)),
  points.col= as.list(terrain.colors(10)),
  main= "Cutoff stability")

# .... no functional dependencies needed, truly parametrized curve
data(ROCR.xval)
pred <- prediction(ROCR.xval$predictions, ROCR.xval$labels)
perf <- performance(pred,"acc","lift")
plot(perf, colorize=T, main="Truly parametrized curves")
plot(perf, colorize=T, print.cutoffs.at=seq(0,1,by=0.1), add=T, text.adj=c(1.2, 1.2), avg="threshold", lwd=3)


# ------------------------------------------------------------------------------
# (Expected cost) curve + ROC convex hull
# ------------------------------------------------------------------------------
data(ROCR.hiv)
ROCR.hiv

pred <- prediction(ROCR.hiv$hiv.nn$predictions, ROCR.hiv$hiv.nn$labels)

par(mfrow=c(1,1))
plot(0,0,xlim=c(0,1),ylim=c(0,1),xlab='Probability cost function',
  ylab="Normalized expected cost",
  main='HIV data: Expected cost curve (Drummond & Holte)')
lines(c(0,1),c(0,1))
lines(c(0,1),c(1,0))

perf1 <- performance(pred,'fpr','fnr')
for (i in 1:length(perf1@x.values)) {
  for (j in 1:length(perf1@x.values[[i]])) {
    lines(c(0,1),c(perf1@y.values[[i]][j], perf1@x.values[[i]][j]),col=rev(terrain.colors(10))[i],lty=3)
  }
}

perf <- performance(pred,'ecost')
plot(perf,lwd=1.5,xlim=c(0,1),ylim=c(0,1),add=T)


# ------------------------------------------------------------------------------
# ROC Convex Hull を描画
# ------------------------------------------------------------------------------
data(ROCR.simple)
ROCR.simple

ROCR.simple$labels[ROCR.simple$predictions >= 0.7 & ROCR.simple$predictions < 0.85] <- 0

#as.numeric(!labels[predictions >= 0.6 & predictions < 0.85])
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,'tpr','fpr')
perf1 <- performance(pred,'rch')

par(mfrow=c(1,1))
plot(perf, main="ROC curve with concavities (suboptimal) and ROC convex hull (Fawcett)")
plot(perf1,add=T,col='red',lwd=2)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# (plotting cutoff vs. measure)
data(ROCR.hiv)
pp <- ROCR.hiv$hiv.svm$predictions
ll <- ROCR.hiv$hiv.svm$labels
pred <- prediction(pp, ll)


measures <- c('tpr','fpr','acc','err','rec','sens','fnr','tnr','spec',
                   'ppv','prec','npv','fall','miss','pcfall','pcmiss',
                   'phi','mat','mi','chisq','odds','lift','f')

## Don't be surprised by the decreased cutoff regions produced by 'odds ratio'.
## Cf. ?performance for details.

par(mfrow=c(5,5))
for (measure in measures) {
  perf <- performance(pred, measure)
  plot(perf,avg="vertical",spread.estimate="boxplot")
}

measures <- c('tpr','err','prec','phi','mi','chisq','odds','lift','f')
par(mfrow=c(6,6))
for (i in 1:(length(measures)-1)) {
  for (j in (i+1):length(measures)) {
    perf <- performance(pred, measures[i], measures[j])
    plot(perf, avg="threshold", colorize=T)
  }
}



# ------------------------------------------------------------------------------
# 教師、予測値を設定し、
# ROC曲線, PR曲線, Lift Chart, Cost Curve を描画
# ------------------------------------------------------------------------------
predictions <- c(16, 15, 14, 13, 12, 11, 10, 9, 8, 8, 8, 8, 7, 6, 5, 5, 5, 4, 4, 3)
lbl1 <- c(T, F, T, F, T, F, F, F, T, F, T, F, F, F, F, F, F, F, F, F)
lbl2 <- c(T, F, F, T, F, T, T, F, F, F, F, F, F, T, F, F, F, F, F, F)
lbl3 <- c(T, T, T, F, F, F, F, F, F, F, F, F, F, F, F, F, T, T, F, F)

data <- data.frame(predictions=predictions, lbl1=lbl1, lbl2=lbl2, lbl3=lbl3)

pred1 <- prediction(predictions=data$predictions, labels=data$lbl1)
pred2 <- prediction(predictions=data$predictions, labels=data$lbl2)
pred3 <- prediction(predictions=data$predictions, labels=data$lbl3)

perf1 <- performance(pred1, "tpr", "fpr")
perf2 <- performance(pred2, "tpr", "fpr")
perf3 <- performance(pred3, "tpr", "fpr")

perf12 <- performance(pred1, "prec", "rec")
perf22 <- performance(pred2, "prec", "rec")
perf32 <- performance(pred3, "prec", "rec")

perf13 <- performance(pred1, "lift", "rpp")
perf23 <- performance(pred2, "lift", "rpp")
perf33 <- performance(pred3, "lift", "rpp")

perf14 <- performance(pred1,'fpr','fnr')
perf24 <- performance(pred2,'fpr','fnr')
perf34 <- performance(pred3,'fpr','fnr')

perf15 <- performance(pred1,'ecost')
perf25 <- performance(pred2,'ecost')
perf35 <- performance(pred3,'ecost')


par(mfrow=c(2,3))
plot(perf1, colorize=T, lwd= 2, main= "ROC curve: pred1"); plot(perf, lty=3, col="grey78", add=T);
plot(perf2, colorize=T, lwd= 2, main= "ROC curve: pred2"); plot(perf, lty=3, col="grey78", add=T);
plot(perf3, colorize=T, lwd= 2, main= "ROC curve: pred3"); plot(perf, lty=3, col="grey78", add=T);
plot(perf12, colorize=T, lwd= 2, main= "PR curve: pred1"); plot(perf, lty=3, col="grey78", add=T);
plot(perf22, colorize=T, lwd= 2, main= "PR curve: pred2"); plot(perf, lty=3, col="grey78", add=T);
plot(perf32, colorize=T, lwd= 2, main= "PR curve: pred3"); plot(perf, lty=3, col="grey78", add=T);

dev.new()
par(mfrow=c(2,3))
plot(perf13, colorize=T, lwd= 2, main= "Lift charts: pred1"); plot(perf, lty=3, col="grey78", add=T);
plot(perf23, colorize=T, lwd= 2, main= "Lift charts: pred2"); plot(perf, lty=3, col="grey78", add=T);
plot(perf33, colorize=T, lwd= 2, main= "Lift charts: pred3"); plot(perf, lty=3, col="grey78", add=T);

dev.new()
par(mfrow=c(2,3))

plot(0,0,xlim=c(0,1),ylim=c(0,1),xlab='Probability cost function',
  ylab="Normalized expected cost",
  main='Expected cost curve')
lines(c(0,1),c(0,1))
lines(c(0,1),c(1,0))
for (i in 1:length(perf14@x.values)) {
  for (j in 1:length(perf14@x.values[[i]])) {
    lines(c(0,1),c(perf14@y.values[[i]][j], perf14@x.values[[i]][j]),col=rev(terrain.colors(10))[i],lty=3)
  }
}
plot(perf15,lwd=1.5,xlim=c(0,1),ylim=c(0,1),add=T)

plot(0,0,xlim=c(0,1),ylim=c(0,1),xlab='Probability cost function',
  ylab="Normalized expected cost",
  main='Expected cost curve')
lines(c(0,1),c(0,1))
lines(c(0,1),c(1,0))
for (i in 1:length(perf24@x.values)) {
  for (j in 1:length(perf24@x.values[[i]])) {
    lines(c(0,1),c(perf24@y.values[[i]][j], perf24@x.values[[i]][j]),col=rev(terrain.colors(10))[i],lty=3)
  }
}
plot(perf25,lwd=1.5,xlim=c(0,1),ylim=c(0,1),add=T)

plot(0,0,xlim=c(0,1),ylim=c(0,1),xlab='Probability cost function',
  ylab="Normalized expected cost",
  main='Expected cost curve')
lines(c(0,1),c(0,1))
lines(c(0,1),c(1,0))
for (i in 1:length(perf34@x.values)) {
  for (j in 1:length(perf34@x.values[[i]])) {
    lines(c(0,1),c(perf34@y.values[[i]][j], perf34@x.values[[i]][j]),col=rev(terrain.colors(10))[i],lty=3)
  }
}
plot(perf35,lwd=1.5,xlim=c(0,1),ylim=c(0,1),add=T)
