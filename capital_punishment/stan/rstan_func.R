# ------------------------------------------------------------------
# 独立な2群の差を推測する
# 入力
#  - x1: ベクトル形式の第1群のデータ (平均値の大きな群)
#  - x2: ベクトル形式の第2群のデータ (平均値の小さな群)
#  - EQU=1:  論理値  1なら等分散  0なら異なる分散
#  - prior=F:  論理値  Tなら事前分布の範囲を指定 Fなら指定せず
#  - mL=-1000, mH=1000, sL=0, sH=100:  事前分布のパラメタ
#  - prob=c(0.025, 0.05, 0.5, 0.95, 0.975):  事後分布で報告する確率点
#  - see=1234, cha=5, war=1000, ite=21000:  MCMC関連のパラメタ
# 
# 出力
#  - fit:stanの出力, par:母数リスト, prob:確率ベクトル, 
#  - mu1:第1群平均, mu2:第2群平均, sigma1:第1群標準偏差, sigma2:第2群標準偏差, 
#  - xaste1:予測分布1, xaste2:予測分布2, log_lik:対数尤度
# ------------------------------------------------------------------

G2Ind <- function(x1, x2, EQU=1, prior=F, mL=-1000, mH=1000, sL=0, sH=100, prob=c(0.025, 0.05, 0.5, 0.95, 0.975), see=1234, cha=5, war=1000, ite=21000)
{
  require(rstan)
  if (prior) {
    dat <- list(n1=length(x1), n2=length(x2), x1=x1, x2=x2, EQU=EQU, prior=prior, mL=mL, mH=mH, sL=sL, sH=sH)
    scr <- ".//stan//G2IndPT.stan"
  } else {
    dat <- list(n1=length(x1), n2=length(x2), x1=x1, x2=x2, EQU=EQU)
    scr <- ".//stan//G2IndPF.stan"
  }
  par <- c("mu", "sigma1", "sigma2", "xaste", "log_lik")  # サンプリング変数
  fit <- stan(file=scr, data=dat, pars=par, seed=see, chains=cha, warmup=war, iter=ite)
  ext <- extract(fit, par)
  mu1 <- ext$mu[,1];  mu2<-ext$mu[,2];  sigma1<-ext$sigma1;  sigma2<-ext$sigma2;
  xaste1 <- ext$xaste[,1];  xaste2 <- ext$xaste[,2]; log_lik <- ext$log_lik

  out <- list(EQU=EQU, prior=prior, fit=fit, par=par, prob=prob, mu1=mu1, mu2=mu2, sigma1=sigma1, sigma2=sigma2, xaste1=xaste1, xaste2=xaste2, log_lik=log_lik)
  class(out) <- 'G2Ind'
  return(invisible(out))
}



# ------------------------------------------------------------------
# 印刷のメソッド
# 入力
#  - x: クラス 'G2Ind'のオブジェクト
#  - degits=3:  小数の丸め
#  - cr1=F:  平均値の差の基準点
#  - cr2=F:  閾上率の基準点
#  - cr3=F:  効果量の基準点
#  - pr1=F:  非重複度の基準確率
#  - pr2=F:  優越率の基準確率
#  - pr3=F:  閾上率の基準確率
# ------------------------------------------------------------------
print.G2Ind <- function(x, digits=3, cr1=F, cr2=F, cr3=F, pr1=F, pr2=F, pr3=F)
{
  EQU <- x$EQU
  prior <- x$prior

  if (EQU > 0.5) {
    print("******************等分散モデル******************")
  } else {
    print("******************異分散モデル******************")
  }
  if (prior > 0.5) {
    print("******************事前分布指定******************")
  } else {
    print("******************事前分布デフォルト******************")
  }

  mu1 <- x$mu1; mu2 <- x$mu2; sigma1 <- x$sigma1; sigma2 <- x$sigma2;
  xaste1 <- x$xaste1; xaste2 <- x$xaste2; log_lik <- x$log_lik;
  prob <- x$prob

  G <- matrix(0, length(mu1), 7)
  G0 <- sqrt(sigma1^2 + sigma2^2)
  G[,1] <- mu1 - mu2
  G[,2] <- G[,1]/sigma1
  G[,3] <- G[,1]/sigma2
  G[,4] <- pnorm(mu1, mu2, sigma2) #U3
  G[,5] <- 1-pnorm(mu2, mu1, sigma1) #U3
  G[,6] <- pnorm((G[,1]/G0), 0.0, 1.0) #π_d

  co <- 6
  lab <- c("平均値の差", "効果量1", "効果量2", "非重複度1群", "非重複度2群", "優越率")

  if(is.numeric(cr2)){
    co <- co + 1
    G[,co] <- pnorm(((G[,1] - cr2) / G0), 0.0, 1.0)
    lab < -c(lab, paste("閾上率(", cr2, ")", sep = ""))
  }

  Gc <- cbind(
    apply(G[,1:co], 2, mean),
    apply(G[,1:co], 2, sd),
    t(apply(G[,1:co], 2, quantile, probs=prob))
  )

  colnames(Gc) <- c("EAP", "post.sd", prob)
  rownames(Gc) <- lab
  U <- matrix(0, length(mu1), 11)
  U[,1] <- ifelse(G[,1] > 0.0, 1, 0)
  U[,2] <- ifelse(G[,1] <= 0.0, 1, 0)
  U[,3] <- ifelse(xaste1 - xaste2 > 0.0, 1, 0)
  co <- 3
  lab <- c("μ1-μ2が0より大きい確率", "μ1-μ2が0以下の確率", "優越率(直接比較)")

  if(is.numeric(cr2)){
    co <- co + 1
    U[,co] <- ifelse(xaste1 - xaste2 > cr2, 1, 0)
    lab <- c(lab, paste("閾上率(", cr2, ")(直接比較)", sep = ""))
  }
  
  if(is.numeric(cr1)){
    co<- co + 1
    U[,co] <- ifelse(G[,1] > cr1, 1, 0)
    lab <- c(lab, paste("μ1-μ2が(", cr1, ")より大きい確率", sep = ""))
  }

  if(is.numeric(cr3)){
    co <- co + 1
    U[,co] <- ifelse(G[,2] > cr3, 1, 0)
    lab <- c(lab, paste("効果量1が(", cr3, ")より大きい確率", sep = ""))
    co <- co + 1
    U[,co] <- ifelse(G[,3] > cr3, 1, 0)
    lab <- c(lab, paste("効果量2が(", cr3, ")より大きい確率", sep = ""))
  }
  
  if(is.numeric(pr1)){
    co<- co + 1
    U[,co] <- ifelse(G[,4] > pr1, 1, 0)
    lab <- c(lab, paste("第1群の非重複度が(", pr1, ")より大きい確率", sep = ""))
    co <- co + 1
    U[,co] <- ifelse(G[,5] > pr1, 1, 0)
    lab <- c(lab, paste("第2群の非重複度が(", pr1, ")より大きい確率", sep = ""))
  }
  
  if(is.numeric(pr2)){
    co <- co + 1
    U[,co] <- ifelse(G[,6] > pr2, 1, 0)
    lab <- c(lab, paste("優越率が(", pr2, ")より大きい確率", sep = ""))
  }
  
  if((is.numeric(cr2))&(is.numeric(pr3))){
    co <- co + 1
    U[,co] <- ifelse(G[,7] > pr3, 1, 0)
    lab <- c(lab, paste("閾上率(", cr2, ")が(", pr3, ")より大きい確率", sep = ""))
  }

  if(co > 0.9){
    Uc <- matrix(colMeans(as.matrix(U[,1:co])), co, 1)
    rownames(Uc) <- lab
  }else{ Uc <- 0.0 }

  waic <- (-2)*(log(mean(exp(log_lik)))) + 2*(var(log_lik))

  print(x$fit,pars=x$par, digits_summary=digits, probs=x$prob)
  print(round(Gc, digits))
  print(round(Uc, digits))
  print(paste("waic=", round(waic,digits), sep=""))
  
  out <- list(G=G, Gc=Gc, Uc=Uc, waic=waic)
  return(invisible(out))
}



# ------------------------------------------------------------------
# 独立な2群の差を推測する
# 入力
#  - x: n行2列の行列形式のデータ (平均値の大きな群は1列目に指定)
#  - EQU=1: 論理値 1なら等分散 0なら異なる分散
#  - prior=F: 論理値 Tなら事前分布の範囲を指定 Fなら指定せず
#  - mL=-1000, mH=1000, sL=0, sH=100: 事前分布のパラメタ
#  - prob=c(0.025, 0.05, 0.5, 0.95, 0.975): 事後分布で報告する確率点
#  - see=1234,cha=5,war=1000,ite=21000: MCMC関連のパラメタ
#
# 出力
#  - fit: stanの出力, par: 母数リスト, prob: 確率ベクトル, 
#  - mu1: 第1群平均, mu2: 第2群平均, sigma1: 第1群標準偏差, sigma2: 第2群標準偏差, 
#  - rho: 相関係数, xaste1: 予測分布1, xaste2: 予測分布2, log_lik: 対数尤度
# ------------------------------------------------------------------
G2pair <- function(x,EQU=1,prior=F,mL=-1000, mH=1000, sL=0, sH=100,
                   prob=c(0.025, 0.05, 0.5, 0.95, 0.975),
                   see=1234, cha=5, war=1000, ite=21000)
{
  library(rstan)
  if (prior) {
    dat <- list(n=nrow(x),x=x,EQU=EQU,mL=mL,mH=mH,sL=sL,sH=sH)
    scr <- ".//stan//G2pairPT.stan"
  } else {
    dat <- list(n=nrow(x),x=x,EQU=EQU)
    scr <- ".//stan//G2pairPF.stan"
  }
  par <- c("mu", "sigma1", "sigma2", "rho", "xaste", "log_lik") # サンプリング変数
  fit <- stan(file=scr, data=dat, pars=par, seed=see, chains=cha, warmup=war, iter=ite)
  ext <- extract(fit, par)
  mu1 <- ext$mu[,1]
  mu2 <- ext$mu[,2] 
  sigma1 <- ext$sigma1
  sigma2 <- ext$sigma2
  rho <- ext$rho
  xaste1 <- ext$xaste[,1]
  xaste2 < -ext$xaste[,2]
  log_lik <- ext$log_lik
  out<-list(EQU=EQU, prior=prior, fit=fit, par=par, prob=prob, mu1=mu1, mu2=mu2,
            sigma1=sigma1, sigma2=sigma2, rho=rho,
            xaste1=xaste1, xaste2=xaste2, log_lik=log_lik)

  class(out) <- 'G2pair'
  return(invisible(out))
}



# ------------------------------------------------------------------
# 印刷のメソッド
# 入力
#  - x:  クラス 'G2pair'のオブジェクト
#  - onlydiff=T 差得点に関する出力のみか否か
#  - degits=3:  小数の丸め
#  - cr1=F:  平均値の差の基準点
#  - cr2=F:  閾上率の基準点
#  - cr3=F:  効果量の基準点
#  - cr4=F:  差得点の標準偏差の基準点
#  - ra= 1.0:  相関上限
#  - rb=-1.0:  相関下限
#  - pr1=F:  非重複度の基準確率
#  - pr2=F:  優越率の基準確率
#  - pr3=F:  閾上率の基準確率
#  - pr4=F:  同順率の基準確率
# ------------------------------------------------------------------
print.G2pair <- function(x, onlydiff=T, degits=3, cr1=F, cr2=F, cr3=F, cr4=F, ra=1.0, rb=-1.0, pr1=F, pr2=F, pr3=F, pr4=F)
{
  EQU <- x$EQU
  prior <- x$prior

  if (EQU>0.5) {
    print("******************等分散モデル******************")
  } else {
    print("******************異分散モデル******************")
  }
  if (prior) {
    print("******************事前分布指定******************")
  } else {
    print("******************事前分布デフォルト******************")
  }

  mu1<-x$mu1; mu2<-x$mu2; sigma1 <- x$sigma1; sigma2 <- x$sigma2; 
  rho <- x$rho; xaste1 <- x$xaste1; xaste2 <- x$xaste2; log_lik <- x$log_lik;
  prob <- x$prob
  G <- matrix(0, length(mu1), 12)
  G0 <- sqrt(sigma1^2 + sigma2^2)
  G[,1] <- mu1 - mu2;                         #平均値差
  G[,2] <- G[,1]/sigma1;                      #効果量1
  G[,3] <- G[,1]/sigma2;                      #効果量2
  G[,4] <- pnorm(mu1,mu2,sigma2);             #非重複度1
  G[,5] <- 1-pnorm(mu2,mu1,sigma1);           #非重複度2
  G[,6] <- pnorm((G[,1]/G0), 0.0, 1.0);       #優越率
  #以下、rhoを利用した分析
  G[,7] <- sqrt(sigma1^2 + sigma2^2 -2*rho*sigma1*sigma2);  #差得点sd
  G[,8] <- G[,1]/G[,7];                       #差得点の効果量
  G[,9] <- pnorm(G[,8], 0.0, 1.0);            #差得点の優越率
  G[,10] <- 0.5+asin(rho)/pi                  #同順率
  co<-10
  lab <-c("平均値の差","効果量1","効果量2","非重複度1群","非重複度2群","優越率","差得点sd","差得点の効果量","差得点の優越率","同順率")

  if(is.numeric(cr2)){
    co <- co+1
    G[,co] <- pnorm((G[,1]-cr2)/G0, 0.0, 1.0)
    lab <-c(lab,paste("閾上率(", cr2, ")", sep = ""))
    co <- co+1
    G[,co] <- pnorm((G[,1]-cr2)/G[,7], 0.0, 1.0)
    lab <- c(lab, paste("差得点閾上率(", cr2, ")", sep = ""))
  }
  
  if (onlydiff) {if (is.numeric(cr2)) {Grow <- c(1,7:10,12)} else {Grow <- c(1,7:10)}}
  else {if (is.numeric(cr2)) {Grow <- c(1:12)} else {Grow <- c(1:10)}};
  
  Gc <- cbind(
    apply(G[,Grow], 2, mean),
    apply(G[,Grow], 2, sd),
    t(apply(G[,Grow], 2, quantile, probs=prob))
  )
  colnames(Gc) <- c("EAP", "post.sd", prob)
  rownames(Gc) <- lab[Grow]
  
  U <- matrix(0, length(mu1), 21)
  U[,1] <- ifelse(G[,1]>0.0,  1, 0)
  U[,2] <- ifelse(G[,1]<=0.0, 1, 0)
  U[,3] <- ifelse(xaste1-xaste2>0.0, 1, 0)
  U[,4] <- ifelse((rb<=rho)&(rho<=ra), 1, 0)
  
  co <-4
  lab <- c("μ1-μ2が0より大きい確率", "μ1-μ2が0以下の確率", "差得点の優越率(直接比較)", paste("相関が",rb,"から",ra,"の間にある確率",sep=""))

  if(is.numeric(cr4)){
    co<- co+1;U[,co] <- ifelse(G[,7]<cr4, 1, 0)
    lab <- c(lab, paste("差得点のsdが(", cr4, ")より小さい確率", sep = ""))
  }

  if(is.numeric(cr2)){
    co <- co + 1
    U[,co] <- ifelse(xaste1-xaste2>cr2, 1, 0)
    lab <- c(lab, paste("差得点の閾上率(", cr2, ")(直接比較)", sep = ""))
  }
  
  if(is.numeric(cr1)){
    co <- co + 1
    U[,co] <- ifelse(G[,1]>cr1, 1, 0)
    lab <- c(lab, paste("μ1-μ2が(", cr1, ")より大きい確率", sep = ""))
  }
  
  if(is.numeric(cr3)){
    co <- co + 1
    U[,co] <- ifelse(G[,8]>cr3, 1, 0)
    lab<-c(lab, paste("差得点の効果量が(", cr3, ")より大きい確率", sep = ""))
  }
  
  if(is.numeric(pr2)){
    co <- co + 1
    U[,co] <- ifelse(G[,9]>pr2, 1, 0)
    lab <- c(lab, paste("差得点の優越率が(", pr2, ")より大きい確率", sep = ""))
  }
  
  if((is.numeric(cr2))&(is.numeric(pr3))){
    co <- co + 1
    U[,co] <- ifelse(G[,12]>pr3, 1, 0)
    lab <- c(lab, paste("差得点の閾上率(", cr2, ")が(", pr3, ")より大きい確率", sep = ""))
  }
  
  if(is.numeric(pr4)){
    co <- co + 1
    U[,co] <- ifelse(G[,10]>pr4, 1, 0)
    lab <- c(lab, paste("同順率が(", pr4, ")より大きい確率", sep = ""))
  }
  
  if(!onlydiff & is.numeric(cr3)){
    co <- co + 1
    U[,co] <- ifelse(G[,2]>cr3, 1, 0)
    lab <- c(lab, paste("効果量1が(", cr3, ")より大きい確率", sep = ""))
    co <- co + 1
    U[,co] <- ifelse(G[,3]>cr3, 1, 0)
    lab <- c(lab, paste("効果量2が(", cr3, ")より大きい確率", sep = ""))
  }
  
  if(!onlydiff & is.numeric(pr1)){
    co <- co + 1
    U[,co] <- ifelse(G[,4]>pr1, 1, 0)
    lab <- c(lab, paste("第1群の非重複度が(", pr1, ")より大きい確率", sep = ""))
    co <- co + 1
    U[,co] <- ifelse(G[,5]>pr1, 1, 0)
    lab <- c(lab, paste("第2群の非重複度が(", pr1, ")より大きい確率", sep = ""))
  }
  
  if(!onlydiff & is.numeric(pr2)){
    co <- co + 1
    U[,co] <- ifelse(G[,6]>pr2, 1, 0)
    lab <- c(lab, paste("優越率が(", pr2, ")より大きい確率", sep = ""))
  }
  
  if(!onlydiff & (is.numeric(cr2))&(is.numeric(pr3))){
    co <- co + 1
    U[,co] <- ifelse(G[,11]>pr3, 1, 0)
    lab <- c(lab, paste("閾上率(", cr2, ")が(", pr3, ")より大きい確率", sep = ""))
  }
  
  if(co>0.9){
    Uc <- matrix(colMeans(as.matrix(U[,1:co])) ,co, 1)
    rownames(Uc) <- lab
  } else { Uc <- 0.0 }
  
  waic <- (-2)*(log(mean(exp(log_lik)))) + 2*(var(log_lik))
  
  print(x$fit,pars=x$par,digits_summary=degits,probs=x$prob)
  print(round(Gc,degits))
  print(round(Uc,degits))
  print(paste("waic=",round(waic,degits),sep=""))

  out <- list(G=G, Gc=Gc, Uc=Uc, waic=waic)
  return(invisible(out))
}

