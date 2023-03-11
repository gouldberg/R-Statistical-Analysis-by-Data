# setwd("//media//kswada//MyFiles//R//email")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\m_tenstocks")


packages <- c("dplyr", "tidyverse")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m-tenstocks
# ------------------------------------------------------------------------------

mtenstocks <- read.csv("m-tenstocks.txt", sep = " ", header = T)


str(mtenstocks)


dim(mtenstocks)


car::some(mtenstocks)



# ----------
# log returns

rtn <- log(mtenstocks[,2:11] + 1)



# ------------------------------------------------------------------------------
# compute correlation matrix
# ------------------------------------------------------------------------------

polcor <- cor(rtn)


polcor




# ------------------------------------------------------------------------------
# model-based clustering by MBcluster  -->  THIS IS MODEL-BASE, DOES NOT APPLY THIS MODEL
# FUNCTION "MBCluster" by Mr. Yongning Wang
# ------------------------------------------------------------------------------

library(pscl)

library(MCMCpack)

library(mvtnorm)

library(fBasics)

MBcluster <- function(data,p,K,mcmc,prior=NULL,differ=FALSE,start.values=NULL){
  # Perform Model-Based clustering analysis on the given data 
  # K            - number of clusters fitted for the data
  # p            - number of lags of AR(p) model fitted for each cluster
  # prior        - prior=list(phi0,Sigma0,n0,s02,e0)
  #                the priors of AR parameters are normal distribution 
  #                with mean phi0 and covariance Sigma0; priors of sigma2 are 
  #                inverse-gamma distribution with alpha=n0/2 and beta=n0*s02/2; 
  #                the priors of group indicator S are Dirichlet distribution with
  #                parameter e0
  # mcmc         - mcmc=list(burnin,rep)
  #                burnin is the burn-in period of mcmc; 
  #                rep is the number of runs of mcmc
  # start.values - start.values=list(phi,sigma2)
  #                where phi is a K*(p+1) matrix and sigma2 is a K*1 vector 
  data=as.matrix(data)
  N=length(data[1,])  # Number of time series
  colname=rep(0,N)
  if (length(colnames(data))==0){
    for (n in 1:N){
      colname[n]=paste('Series ',n)
    }
  }
  else {colname=colnames(data)}
  
  if (differ==TRUE){
    data=apply(data,2,diff)
  }
  T=length(data[,1])
  ## setup default prior
  if(is.null(prior)){
    cat("Use default priors","\n")
    phi0=rep(0,p+1)
    Sigma0_phi = 10^4*diag(p+1)
    n0=2
    sig02=0.5
    e0=rep(3,K)
    prior=list(phi0=phi0,Sigma0_phi=Sigma0_phi,n0=n0,sig02=sig02,e0=e0)
  }
  #
  # Initialization
  #
  s=rep(0,N) 
  S=matrix(0,(mcmc$burnin+mcmc$rep),N)
  phi=matrix(0,K*(mcmc$burnin+mcmc$rep),(p+1))
  sigma2=rep(0,K*(mcmc$burnin+mcmc$rep))
  pr=matrix(0,(mcmc$burnin+mcmc$rep),K)
  Sigma_inv=NULL
  Sigma1_inv_phi=NULL
  eta=rdirichlet(1,prior$e0)
  p0=log(eta) 
  n0=prior$n0
  sig02=prior$sig02
  phi0=prior$phi0
  Sigma0_inv=kron(diag(K),solve(prior$Sigma0_phi))
  n0sig02=n0*sig02
  if (is.null(start.values)){
    # use k-means to define initial values of ar-parameters
    init=kmeans(t(data),K)
    center=init$center
    for (k in 1:K){
      mfit=arima(center[k,],order=c(p,0,0),method='CSS')
      # 1st element of phi is unconditional mean, 
      # 2nd to (p+1)th elements are ar-parameters
      phi[k,1]=mfit$coef[p+1]
      phi[k,2:(p+1)]=mfit$coef[1:p]
    }
    sigma2[1:K]=rigamma(1,n0/2,n0sig02/2)
  }
  else {
    phi[1:K,]=start.values$phi
    sigma2[1:K]=start.values$sigma2
  }
  # Use Gibbs sampler to draw group indicator S, sigma2, and phi
  for (run in 2:(mcmc$burnin+mcmc$rep)){
    # Draw S
    lik=matrix(0,K,N)
    post=matrix(0,K,N)
    for (n in 1:N){
      y=data[T:(p+1),n]
      X=rep(1,T-p)
      for (q in 1:p){
        X=cbind(X,data[(T-q):(p+1-q),n])
      }
      #compute log-likelihood and posterior probability
      for (k in 1:K){        
        lik[k,n]=sum(dnorm(y,X%*%phi[(run-2)*K+k,],sqrt(sigma2[(run-2)*K+k]),log=TRUE))
        post[k,n]=p0[k]+lik[k,n]
      }
      post[,n]=post[,n]-max(post[,n])
      s[n]=sample(1:K,size=1,prob=exp(post[,n]),replace=TRUE)
    }
    ss=unique(s)
    # if there is no individual drawn to the cluster,
    # assign those with top [N/K] posterior probabilities to that cluster 
    a=rep(0,N)
    k=1
    while ((length(ss)<K)&&(k<=K)){
      if (sum(ss==k)==0){
        s1=sort.list(post[k,a==0],decreasing=TRUE)[1:floor(N/K)]
        s[s1]=k
        a[s1]=1
      }
      ss=unique(s)
      k=sample(1:K,1) 
    } 
    phi1=rep(0,(p+1)*K)
    Sigma_inv[[run]]=matrix(0,(p+1)*K,(p+1)*K)
    Sigma1_inv=matrix(0,(p+1)*K,(p+1)*K)
    for (k in 1:K){
      # Draw sigma2   
      data_pool=data[,s==k]
      n1=n0+(T-p)
      n1sig12=n0sig02
      pooldim=sum(s==k)
      data_pool=matrix(data_pool,T,pooldim)
      y=c(data_pool[T:(p+1),])
      X=rep(1,T-p)
      for (q in 1:p){
        X=cbind(X,data_pool[(T-q):(p+1-q),1])
      }
      if (pooldim>1){
        for (m in 2:pooldim){
          Xm=rep(1,T-p)
          for (q in 1:p){
            Xm=cbind(Xm,data_pool[(T-q):(p+1-q),m])
          }
          X=rbind(X,Xm)
          n1=n1+(T-p)
        }
      }
      n1sig12=n1sig12+t(y-X%*%phi[((run-2)*K+k),])%*%(y-X%*%phi[((run-2)*K+k),])
      sigma2[(run-1)*K+k]=rigamma(1,n1/2,n1sig12/2)
      # Draw phi
      # define phi1 and Sigma1_inv as the mean vector and covariance matrix 
      # of the posterior distribution of phi, Sigma1_inv_phi=Sigma1_inv%*%phi1      
      Sigma1_inv[((k-1)*(p+1)+1):(k*(p+1)),((k-1)*(p+1)+1):(k*(p+1))]=
        Sigma0_inv[((k-1)*(p+1)+1):(k*(p+1)),((k-1)*(p+1)+1):(k*(p+1))]+
        t(X)%*%X/sigma2[(run-1)*K+k]
      Sigma1_inv_phi[[k]]=
        Sigma0_inv[((k-1)*(p+1)+1):(k*(p+1)),((k-1)*(p+1)+1):(k*(p+1))]%*%phi0+
        t(X)%*%y/sigma2[(run-1)*K+k]
      phi1[((k-1)*(p+1)+1):(k*(p+1))]=
        solve(Sigma1_inv[((k-1)*(p+1)+1):(k*(p+1)),((k-1)*(p+1)+1):(k*(p+1))])%*%
        Sigma1_inv_phi[[k]]     
      phi[((run-1)*K+k),]=rmvnorm(1,phi1[((k-1)*(p+1)+1):(k*(p+1))],
                                  solve(Sigma1_inv[((k-1)*(p+1)+1):(k*(p+1)),((k-1)*(p+1)+1):(k*(p+1))]))
      Sigma_inv[[run]][((k-1)*(p+1)+1):(k*(p+1)),((k-1)*(p+1)+1):(k*(p+1))]=
        Sigma1_inv[((k-1)*(p+1)+1):(k*(p+1)),((k-1)*(p+1)+1):(k*(p+1))]
    }   
    S[run,]=s
    # print(run)
    # print(s)
    # print(phi[((run-1)*K+1):(run*K),])
    # print(sigma2[((run-1)*K+1):(run*K)])
  }
  phiGibbs=phi[-c(1:(K*mcmc$burnin)),]
  sigma2Gibbs=sigma2[-c(1:(K*mcmc$burnin))]
  SGibbs=S[-c(1:mcmc$burnin),]
  km=kmeans(cbind(phiGibbs,sqrt(sigma2Gibbs)),K,nstart=25)
  estim=km$centers
  cluster=matrix(0,mcmc$rep,N)
  pr=matrix(0,mcmc$rep,K)
  SigmaGibbs=NULL
  # label switching
  for (i in 1:mcmc$rep){
    switch=sort.list(km$cluster[((i-1)*K+1):(i*K)],decreasing=FALSE)
    phitemp=phiGibbs[((i-1)*K+1):(i*K),]
    phiGibbs[((i-1)*K+1):(i*K),]=phitemp[switch,]
    sigma2temp=sigma2Gibbs[((i-1)*K+1):(i*K)]
    sigma2Gibbs[((i-1)*K+1):(i*K)]=sigma2temp[switch]
    SigmaGibbs[[i]]=solve(Sigma_inv[[i+mcmc$burnin]])
    for (j in 1:N){
      cluster[i,j]=km$cluster[(i-1)*K+SGibbs[i,j]]
    }
  }
  Sigmahat=matrix(0,(p+1)*K,(p+1)*K)
  for (i in 1:mcmc$rep){
    Sigmatemp=SigmaGibbs[[i]]
    for (k in 1:K){
      pr[i,k]=sum(cluster[i,]==k)/N
      SigmaGibbs[[i]][((k-1)*(p+1)+1):(k*(p+1)),((k-1)*(p+1)+1):(k*(p+1))]=
        Sigmatemp[(switch[k]*(p+1)-p):(switch[k]*(p+1)),
                  (switch[k]*(p+1)-p):(switch[k]*(p+1))]
    }
    Sigmahat=Sigmahat+SigmaGibbs[[i]]
  }
  prhat=apply(pr,2,mean)
  Sigmahat=Sigmahat/mcmc$rep
  # 
  prob=matrix(0,K,N)
  for (k in 1:K){
    prob[k,]=apply(cluster==k,2,mean)
  }
  #
  # Calculate Marginal Likelihood with Generalized Harmonic Mean
  #
  logp=rep(0,mcmc$rep)
  for (i in 1:mcmc$rep){
    if (min(pr[i,])>0){
      L=matrix(0,N,K)
      for (j in 1:N){    
        y=data[T:(p+1),j]
        X=rep(1,T-p)
        for (q in 1:p){
          X=cbind(X,data[(T-q):(p+1-q),j])
        }
        for (k in 1:K){
          L[j,k]=log(pr[i,k])+sum(dnorm(y,X%*%phiGibbs[(i-1)*K+k,],sqrt(sigma2Gibbs[(i-1)*K+k]),log=TRUE))
        }
      }
      Lmax=max(L)
      Ld=L-Lmax
      logp[i]=logp[i]+N*Lmax+sum(log(apply(exp(Ld),1,sum)))
    }
  }
  logp=logp[logp>0]     
  A=max(logp)
  logphat=log(1/mean(1/exp(logp-A)))+A
  #
  # Output
  #
  rn=rep(0,K)
  for (i in 1:K){
    rn[i]=paste('Cluster',i)
  }
  rownames(estim)=rn
  cn=rep(0,p+2)
  cn[p+2]=paste('sigma')
  for (i in 1:(p+1)){
    cn[i]=paste('phi',i-1)
  }
  colnames(estim)=cn
  
  rn=rep(0,K)
  for (i in 1:K){
    rn[i]=paste('Cluster',i)
  }
  rownames(prob)=rn
  cn=rep(0,N)
  for (i in 1:N){
    cn[i]=colname[i]
  }
  colnames(prob)=cn
  cls=vector('list',K)
  for (k in 1:K){
    if (length(which(prob[k,]==apply(prob,2,max)))>0){
      cls[[k]]=colname[which(prob[k,]==apply(prob,2,max))]
    }
  }
  clsn=rep(0,N)
  for (i in 1:N){
    clsn[i]=which(prob[,i]==apply(prob,2,max)[i])[1]
  }
  #
  cat('\n Estimation for Cluster Parameters:\n')
  cat('Number of Clusters: K=',K,'\n')
  cat('Number of Lags in AR model: p=',p,'\n')
  print(estim,digits=5)
  cat('\n Classification Probabilities:\n')
  print(t(prob),digits=3)
  cat('\n Classification:\n')
  for (k in 1:K){
    if (length(cls[[k]])>0){
      cat('Cluster',k,':\n')
      print(cls[[k]])
    }
  }  
  cat('\n Marginal LogLikelihood:',logphat,'\n')
  #
  #
  #
  result=list(estim=estim,prob=prob,pr=pr,cluster=cls,clsindex=clsn,phiGibbs=phiGibbs,sigma2Gibbs=sigma2Gibbs,
              SigmaInv_Gibbs=Sigma_inv,S_draw=SGibbs,MarginalLik=logphat)
  return(result)
}



# ------------------------------------------------------------------------------
# model-based clustering by MBcluster
# ------------------------------------------------------------------------------

mcmc <- list(burning = 3000, rep = 2000)



# K:  number of clusters fitted for the data
# p:  number of lags of AR(p) model fitted for each cluster

mbcls <- MBcluster(rtn, p = 4, K = 4, mcmc = mcmc)



