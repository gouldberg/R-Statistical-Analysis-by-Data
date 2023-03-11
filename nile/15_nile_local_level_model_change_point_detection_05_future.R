rm(list=ls())

setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\nile")

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Nile
# ------------------------------------------------------------------------------

data(Nile)


dim(Nile)


str(Nile)



# ----------
y <- Nile


t_max <- length(y)



# ----------
# basic model
nStateVar <- 1
nHyperParam <- 2
n <- length(y)

funModel <- function(parm){ dlmModPoly(order = 1, dV = exp(parm[1]), dW = exp(parm[2])) }
oMLE.DLM <- dlmMLE(y, parm = rep(0, 2), build = funModel, hessian = T)


mod <- funModel(oMLE.DLM$par)

mod$W
mod$V



# ------------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------------

# logsumexp to avoid underflow

normalize <- function(l){
  max_ind <- which.max(l)
  
  # scaling
  return(
    l - l[max_ind] -
      log1p(sum(exp(l[-max_ind] - l[max_ind])))
  )
}



sys_resampling <- function(N, w){
  w <- exp(w)
  
  sfun <- stepfun(x = cumsum(w), y = 1:(N+1))
  
  sfun((1:N - runif(n = 1)) / N)
}



kernel_smoothing <- function(realization, w, a){
  w <- exp(w)
  
  mean_realization <- weighted.mean(realization, w)
  
  var_realization <- weighted.mean((realization - mean_realization)^2, w)
  
  mu <- a * realization + (1 - a) * mean_realization
  
  sigma2 <- (1 - a^2) * var_realization
  
  return(list(mu = mu, sigma = sqrt(sigma2)))
}



# quantile function for weighted particle clouds
weighted.quantile <- function(x, w, probs)
{
  ## Make sure 'w' is a probability vector
  if ((s <- sum(w)) != 1)
    w <- w / s
  ## Sort 'x' values
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  ## Evaluate cdf
  W <- cumsum(w)
  ## Invert cdf
  tmp <- outer(probs, W, "<=")
  n <- length(x)
  quantInd <- apply(tmp, 1, function(x) (1 : n)[x][1])
  ## Return
  ret <- x[quantInd]
  ret[is.na(ret)] <- x[n]
  return(ret)
}


# Kalman-Filtering for only 1 time point

Kalman_filtering <- function(y, state, param){
  res <- sapply(1:N, function(n){
    mod$m0 <-     state$m0[n]
    mod$C0 <-     state$C0[n]
    mod$W  <- exp(param$ W[n]) # W is logged value
    mod$V  <- exp(param$ V[n]) # V is logged value
    
    # Kalman-Filtering for only 1 time point
    KF_out <- dlmFilter(y = y, mod = mod)
    
    return(
      c(
        mm = KF_out$m[2],                             # 1: prior
        C = dlmSvd2var(KF_out$U.C, KF_out$D.C)[[2]],  # 1: prior
        
        f = KF_out$f,
        Q = mod$FF %*% dlmSvd2var(KF_out$U.R, KF_out$D.R)[[1]] %*% t(mod$FF) +
          mod$V
      )
    )
  })
  
  return(list(mm = res["mm", ], C = res["C", ], f = res["f", ], Q = res["Q", ]))
}



# ----------
smoothing_index <- function(t_current){
  # current index
  index <- 1:N
  
  # t_current+1 --> t_max:  repeating resampling virtually
  # if fix upper, fixed lag smoothing
  for (t in (t_current+1):t_max){
    index <- index[k[, t]]
  }
  
  return(index)
}



smoothing_index2 <- function(t_current, lag_val){
  index <- 1:N
  
  for (t in (t_current+1):ifelse(t_current + lag_val <= t_max,
                                 t_current + lag_val,   t_max)){
    index <- index[k[, t]]
  }
  
  return(index)
}



# ------------------------------------------------------------------------------
# State-space model Liu and West filtering + Rao-Blackwellization
# ------------------------------------------------------------------------------

# フィルタリング
# 平均を求める
LWF_lambda2_m <- sapply(1:t_max, function(t){exp(        # 線形領域に戻す
  weighted.mean(lambda2[t, ], w = exp(w[t, ]))
)})

# ts型に変換
LWF_lambda2_m <- ts(LWF_lambda2_m); tsp(LWF_lambda2_m) <- tsp(y)


# 固定ラグ平滑化（ラグ = 1, 2, 3）
LWF_lambda2_s_lag <- lapply(1:3, function(lag){
  # 未来の情報を考慮してフィルタリング粒子を再選択
  ki <- sapply(1:(t_max-1), function(t){ lambda2[t, smoothing_index2(t, lag_val = lag)] })
  ki <- t(cbind(ki, lambda2[t_max, ]))        # 最終時点での平滑化分布を追加
  
  # 平均を求める
  LWF_lambda2_s <- sapply(1:t_max, function(t){exp(        # 線形領域に戻す
    weighted.mean(ki[t, ], w = exp(w[t, ]))
  )})
  
  # ts型に変換
  LWF_lambda2_s <- ts(LWF_lambda2_s); tsp(LWF_lambda2_s) <- tsp(y)
  
  # 結果を返す
  return(LWF_lambda2_s)
})

# 分析結果の統合
LWF_lambda2 <-do.call("ts.union", c(list(LWF_lambda2_m), LWF_lambda2_s_lag))

# 結果のプロット
matplot(LWF_lambda2,
        xaxt = "n", xlab = "Time", ylab = "Wに対する時変の倍率", ylim = c(0, 14.5),
        lty = c("dashed", "dashed", "dashed", "dashed"),
        col = c("gray", "gray", "gray", "black"),
        type = "o", pch = as.character(0:3), cex = 0.7)
x_tick <- seq(from = 1, to = nrow(LWF_lambda2), by = 15)
axis(side = 1, labels = time(LWF_lambda2)[x_tick], at = x_tick)
mtext("1899", at = which(time(LWF_lambda2) == 1899), side = 1, adj = 1, cex = 0.6)
abline(h = 1, col = "lightgray", lty = "solid")
mtext("1", at = 1, side = 2, cex = 0.6)

# 凡例
legend(legend = c("フィルタリング",
                  "固定ラグ平滑化（ラグ=1）",
                  "固定ラグ平滑化（ラグ=2）",
                  "固定ラグ平滑化（ラグ=3）"),
       pch = as.character(0:3),
       lty = c("dashed", "dashed", "dashed", "dashed"),
       col = c("gray", "gray", "gray", "black"),
       x = "topleft", cex = 0.6)









