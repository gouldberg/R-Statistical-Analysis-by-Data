# ------------------------------------------------------------------
# plot partial utility by each factor and level
# ------------------------------------------------------------------
plot.Conjoint <- function(utility, label,sd=FALSE)
{
  if(sd==FALSE){
    names(utility) <- NULL
    #切片の削除
    utility <- utility[-1]
    #プロット順になるようにベクトルを反対に
    utility <- rev(utility)
    cname <- rev(label)
    barplot(utility, horiz=TRUE, xlab="部分効用値", xlim=c(-0.6,0.6))
    text(0, 1.2*(1:length(cname)-0.5), cname, pos=ifelse(utility > 0, 2, 4))
  }else{
    names(utility) <- NULL
    #切片の削除
    utility <- utility[-1]
    #プロット順になるようにベクトルを反対に
    utility <- rev(utility)
    cname <- c("個人差","個人差",rev(label))
    barplot(c(sd, -sd, utility), horiz=TRUE, xlab="部分効用値")
    text(0, 1.2*(1:length(cname)-0.5), cname, pos=ifelse(utility > 0, 2, 4))
  }
}



# ------------------------------------------------------------------
# 回答者個人に対する分析
# 各個人ごとの各要因に対する相対的重要度: どの要因がどの程度重要視されているかを、水準数の違いに関係なく表す指標
# 例えば、すべてのプロファイルに対して「どちらともいえない」の回答だと、NAになる
# ------------------------------------------------------------------
sub.imp <- function(data, design, label){
  require(conjoint)
  
  uslall <- caPartUtilities(data, design, label)[,-1]
  np <- ncol(design)
  nl <- as.numeric(np)
  for(i in 1:np){
    nl[i] <- length(unique(design[,i]))
  }
  nl_cum <- c(0, cumsum(nl))
  nx <- nrow(data)
  R2 <- matrix(numeric(np*nx), nx, np)
  for(x in 1:nx){
    for(i in 1:np){
      a <- nl_cum[i]+1
      b <- nl_cum[i+1]
      R2[x,i] <- max(uslall[x,a:b]) - min(uslall[x,a:b])
    }
  }

  imp_new2 <- matrix(numeric(np*nx),nx,np)
  for(x in 1:nx){
    for(i in 1:np){
      imp_new2[x,i] <- R2[x,i]/sum(R2[x,])*100
    }
  }
  colnames(imp_new2) <- colnames(design)
  imp_new2 <- round(imp_new2,3)
  return(imp_new2)
}



# ------------------------------------------------------------------
# 効果量
# ------------------------------------------------------------------
effectsize <- function(x1, x2, toubunsan=TRUE, col_name){
  if(toubunsan==TRUE){
    n <- ncol(x1)
    M <- matrix(numeric(n),1,n)
    x <- rbind(x1,x2)
    sigma <- numeric(n)
    for(i in 1:n){
      sigma[i] <- sqrt((nrow(x)-1)/(nrow(x))*var(x[,i]))
      M[i] <- (mean(x1[,i])-mean(x2[,i]))/sigma[i]
    }
    M<-round(M,3)
    rownames(M) <- "delta"
    colnames(M) <- col_name
    return(M)
  } else{
    n <- ncol(x1)
    M <- matrix(numeric(n*2),2,n)
    sigma1 <- numeric(n)
    sigma2 <- numeric(n)
    for(i in 1:n){
      sigma1[i] <- sqrt((nrow(x1)-1)/(nrow(x1))*var(x1[,i]))
      sigma2[i] <- sqrt((nrow(x2)-1)/(nrow(x2))*var(x2[,i]))
      M[1,i] <- (mean(x1[,i])-mean(x2[,i]))/sigma1[i]
      M[2,i] <- (mean(x1[,i])-mean(x2[,i]))/sigma2[i]
    }
    M <- round(M,3)
    rownames(M) <- c("delta1","delta2")
    colnames(M) <- col_name
    return(M)
  }
}


# ------------------------------------------------------------------
# 非重複度
# ------------------------------------------------------------------
cohen_u3 <- function(x1, x2, toubunsan=TRUE, col_name){
  if(toubunsan==TRUE){
    n <- ncol(x1)
    M <- matrix(numeric(n), 1, n)
    x <- rbind(x1, x2)
    sigma <- numeric(n)
    
    for(i in 1:n){
      sigma[i] <- sqrt((nrow(x)-1)/(nrow(x))*var(x[,i]))
      M[1,i] <- pnorm(mean(x1[,i]), mean(x2[,i]), sigma[i])
    }
    
    M <- round(M,3)
    rownames(M) <- c("U3")
    colnames(M) <- col_name
    return(M)
  }
  else{
    n <- ncol(x1)
    M <- matrix(numeric(n*2), 2, n)
    sigma1 <- numeric(n)
    sigma2 <- numeric(n)
    for(i in 1:n){
      sigma1[i] <- sqrt((nrow(x1)-1)/(nrow(x1))*var(x1[,i]))
      sigma2[i] <- sqrt((nrow(x2)-1)/(nrow(x2))*var(x2[,i]))
      M[1,i] <- pnorm(mean(x1[,i]), mean(x2[,i]), sigma2[i])
      M[2,i] <- 1 - pnorm(mean(x2[,i]), mean(x1[,i]), sigma1[i])
    }

    M <- round(M, 3)
    rownames(M) <- c("U31", "U32")
    colnames(M) <- col_name
    return(M)
  }
}



# ------------------------------------------------------------------
# 優越率
# ------------------------------------------------------------------
pid <- function(x1, x2, toubunsan=TRUE, col_name){
  if(toubunsan==TRUE){
    n <- ncol(x1)
    x <- rbind(x1, x2)
    sigma <- numeric(n)
    delta <- numeric(n)
    Pd <- matrix(numeric(n), 1, n)
    for(i in 1:n){
      sigma[i] <- sqrt((nrow(x)-1)/(nrow(x))*var(x[,i])) 
      delta[i] <- (mean(x1[,i])-mean(x2[,i]))/sigma[i]
      Pd[i] <- pnorm( (delta[i]/sqrt(2)) , 0, 1) 
    }
    
    Pd<-round(Pd, 3)
    rownames(Pd) <- c("優越率(等分散)")
    colnames(Pd) <- col_name
    return(Pd)
  }
  else{
    n <- ncol(x1)
    Pd <- matrix(numeric(n), 1, n)
    sigma1 <- numeric(n)
    sigma2 <- numeric(n)
    z <- numeric(n)
    
    for(i in 1:n){
      sigma1[i] <- sqrt((nrow(x1)-1)/(nrow(x1))*var(x1[,i]))
      sigma2[i] <- sqrt((nrow(x2)-1)/(nrow(x2))*var(x2[,i]))
      z[i] <- (mean(x1[,i]) - mean(x2[,i]))/(sqrt(sigma1[i]^2 + sigma2[i]^2))
      Pd[i] <- pnorm(z[i], 0, 1)
    }
    
    Pd <- round(Pd,3)
    rownames(Pd) <- c("優越率(異分散)")
    colnames(Pd) <- col_name
    return(Pd)
  }
}


# ------------------------------------------------------------------
# 閾上率
# ------------------------------------------------------------------
pic <- function(x1, x2, c, toubunsan=TRUE, col_name){
  if(toubunsan==TRUE){
    n <- ncol(x1)
    x <- rbind(x1, x2)
    sigma <- numeric(n)
    z <- numeric(n)
    Pc <- matrix(numeric(n), 1, n)

    for(i in 1:n){
      sigma[i] <- sqrt((nrow(x)-1)/(nrow(x))*var(x[,i]))
      z[i] <- (mean(x1[,i]) - mean(x2[,i]) - c)/(sqrt(2)*sigma[i]) 
      Pc[i] <- pnorm( z[i] ,0, 1) 
    }

    Pc <- round(Pc, 3)
    rownames(Pc) <- c("閾上率(等分散)")
    colnames(Pc) <- col_name
    return(Pc)
  }
  else{
    n <- ncol(x1)
    Pc <- matrix(numeric(n), 1, n)
    sigma1 <- numeric(n)
    sigma2 <- numeric(n)
    z <- numeric(n)
    
    for(i in 1:n){
      sigma1[i] <- sqrt((nrow(x1)-1)/(nrow(x1))*var(x1[,i]))
      sigma2[i] <- sqrt((nrow(x2)-1)/(nrow(x2))*var(x2[,i]))
      z[i] <- (mean(x1[,i]) - mean(x2[,i]) - c)/(sqrt(sigma1[i]^2+sigma2[i]^2))
      Pc[i] <- pnorm(z[i], 0, 1)
    }
    
    Pc <- round(Pc, 3)
    rownames(Pc) <- c("閾上率(異分散)")
    colnames(Pc) <- col_name
    return(Pc)
  }
}



# ------------------------------------------------------------------
# 差得点の効果量
# ------------------------------------------------------------------
diff_effectsize <- function(x, toubunsan=TRUE, col_name){
  if(toubunsan==TRUE){
    n <- ncol(x)
    M <- matrix(numeric(n*n), n, n)
    sigma <- numeric(n)
    for(i in 1:n){
      for(j in 1:n){
        sigma[i] <- sqrt((nrow(x)-1)/(nrow(x))*var(x[,i]))
        M[i,j] <- (mean(x[,i])-mean(x[,j]))/(sigma[i]*sqrt(2*(1-cor(x[,i],x[,j]))))
      }
    }
  }
  else{
    n <- ncol(x)
    M <- matrix(numeric(n*n), n, n)
    sigma1 <- numeric(n)
    sigma2 <- numeric(n)
    for(i in 1:n){
      for(j in 1:n){
        sigma1[i] <- sqrt((nrow(x)-1)/(nrow(x))*var(x[,i]))
        sigma2[j] <- sqrt((nrow(x)-1)/(nrow(x))*var(x[,j]))
        M[i,j] <- (mean(x[,i])-mean(x[,j]))/(sqrt((sigma1[i])^2+(sigma2[j])^2-2*cor(x[,i],x[,j])*sigma1[i]*sigma2[j]))
      }
    }
  }
  
  M <- round(M,3)
  rownames(M) <- col_name
  colnames(M) <- col_name
  return(M)
}



# ------------------------------------------------------------------
# 差得点の優越率
# ------------------------------------------------------------------
diff_pid <- function(x, toubunsan=TRUE, col_name){
  if(toubunsan==TRUE){
    n <- ncol(x)
    Pd <- matrix(numeric(n*n), n, n)
    sigma <- numeric(n)
    z <- matrix(numeric(n*n), n, n)
    for(i in 1:n){
      for(j in 1:n){
        sigma[i] <- sqrt((nrow(x)-1)/(nrow(x))*var(x[,i]))
        z[i,j] <- (mean(x[,i]) - mean(x[,j]))/(sigma[i]*sqrt(2))
        Pd[i,j] <- pnorm(z[i,j],0,1)
      }
    }
  }
  else{
    n <- ncol(x)
    Pd <- matrix(numeric(n*n), n, n)
    sigma1 <- numeric(n)
    sigma2 <- numeric(n)
    z <- matrix(numeric(n*n), n, n)
    for(i in 1:n){
      for(j in 1:n){
        sigma1[i] <- sqrt((nrow(x)-1)/(nrow(x))*var(x[,i]))
        sigma2[j] <- sqrt((nrow(x)-1)/(nrow(x))*var(x[,j]))
        z[i,j] <- (mean(x[,i]) - mean(x[,j]))/(sqrt((sigma1[i])^2+(sigma2[j])^2-2*cor(x[,i],x[,j])*sigma1[i]*sigma2[j]))
        Pd[i,j] <- pnorm(z[i,j], 0, 1)
      }
    }
  }
  Pd <- round(Pd,3)
  rownames(Pd) <- col_name
  colnames(Pd) <- col_name
  return(Pd)
}



# ------------------------------------------------------------------
# 差得点の閾上率
# ------------------------------------------------------------------
diff_pic <- function(x, c, toubunsan=TRUE, col_name){
  if(toubunsan==TRUE){
    n <- ncol(x)
    Pc <- matrix(numeric(n*n), n, n)
    sigma <- numeric(n)
    z <- matrix(numeric(n*n), n, n)
    for(i in 1:n){
      for(j in 1:n){
        sigma[i] <- sqrt((nrow(x)-1)/(nrow(x))*var(x[,i]))
        z[i,j] <- (mean(x[,i]) - mean(x[,j]) - c)/(sigma[i]*sqrt(2*(1-cor(x[,i],x[,j]))))
        Pc[i,j] <- pnorm(z[i,j],0,1)
      }
    }
  }
  else{
    n <- ncol(x)
    Pc <- matrix(numeric(n*n), n, n)
    sigma1 <- numeric(n)
    sigma2 <- numeric(n)
    z<-matrix(numeric(n*n), n, n)
    for(i in 1:n){
      for(j in 1:n){
        sigma1[i] <- sqrt((nrow(x)-1)/(nrow(x))*var(x[,i]))
        sigma2[i] <- sqrt((nrow(x)-1)/(nrow(x))*var(x[,j]))
        z[i,j] <- (mean(x[,i]) - mean(x[,j]) - c)/(sqrt((sigma1[i])^2+(sigma2[j])^2-2*cor(x[,i],x[,j])*sigma1[i]*sigma2[j]))
        Pc[i,j] <- pnorm(z[i,j], 0, 1)
      }
    }
  }
  Pc <- round(Pc,3)
  rownames(Pc) <- col_name
  colnames(Pc) <- col_name
  return(Pc)
}



