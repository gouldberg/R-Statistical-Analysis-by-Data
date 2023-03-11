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



# ------------------------------------------------------------------------------
# State-space model Liu and West filtering + Rao-Blackwellization
# ------------------------------------------------------------------------------

set.seed(4521)


# ----------
# Number of particle filters
# smaller value compared to model without Rao-Blackwellization
N <- 10000

# exponentially weighted moving average for parameters
a <- 0.975

# maximum value for parameter W and V
W_max <- 10 * var(diff(y))

V_max <- 10 * var(y)



# ----------
# recognize prior distribution as time 1
y <- c(NA_real_, y)



# ----------
# save index for resampling at all time point
k_save <- matrix(1:N, nrow = N, ncol = t_max + 1)



# ----------
# set prior distribution
# particle: variance of system noise (ratio for time-varying)

lambda2 <- matrix(NA_real_, nrow = t_max + 1, ncol = N)

lambda2[1,] <- log(rcauchy(N)^2)



# particle: parameter W (variance of sysmte noise, base for time invariant)

W <- matrix(NA_real_, nrow = t_max+1, ncol = N)

W[1, ] <- log(runif(N, min = 0, max = W_max))


# particle: parameter V
V <- matrix(NA_real_, nrow = t_max+1, ncol = N)

V[1, ] <- log(runif(N, min = 0, max = V_max))


# particle: State (mean and variance of filtering distribution)
mm <- matrix(NA_real_, nrow = t_max+1, ncol = N)

mm[1,] <- 0

C <- matrix(NA_real_, nrow = t_max+1, ncol = N)

C[1,] <- 1e+7



# ----------
# particle weight

w <- matrix(NA_real_, nrow = t_max+1, ncol = N)

w[1, ] <- log(1 / N)

dim(w)



# ----------
progress_bar <- txtProgressBar(min = 2, max = t_max + 1, style = 3)



# ----------
# forward smoothing + auxiliary particle filter + Rao-Blackwellization

for (t in (1:t_max)+1){

  setTxtProgressBar(pb = progress_bar, value = t)
    
  # parameters moving average
  W_ks <- kernel_smoothing(realization = W[t-1, ], w = w[t-1, ], a = a)
  V_ks <- kernel_smoothing(realization = V[t-1, ], w = w[t-1, ], a = a)
  
  
  # ----------
  # Kalman-Filtering for 1 time point
  KF_aux <- Kalman_filtering(y = y[t], state = list(m0 = mm[t-1,], C0 = C[t-1,]), param = list(W = W_ks$mu, V = V_ks$mu))
  
  
  # ----------
  # resampling
  probs <- w[t-1, ] + dnorm(y[t], mean = KF_aux$f, sd = sqrt(KF_aux$Q), log = TRUE)
  
  k <- sys_resampling(N = N, w = normalize(probs))

  
  # save all time points for particle smoothing (Kitagawa algorithm)
  k_save[, t] <- k
  
  
  # ----------
  ##### REFRESH #####
  lambda2[t, ] <- log(rcauchy(N)^2)
  

  # ----------
  W[t, ] <- rnorm(N, mean = W_ks$mu[k], sd = W_ks$sigma)
  V[t, ] <- rnorm(N, mean = V_ks$mu[k], sd = V_ks$sigma)
  
  
  # ----------
  # State Equation:  generate particle realization:  ##### lambda2 is added #####
  KF <- Kalman_filtering(y = y[t], state = list(m0 = mm[t-1,k], C0 = C[t-1,k]), 
                         param = list(W = lambda2[t,] + W[t,], V = V[t,]))
  
  mm[t,] <- KF$mm
  C[t,] <- KF$C
  
  
  # ----------
  # Observation Equation:  update particle weight
  w[t, ] <- dnorm(y[t], mean = KF$f, sd = sqrt(KF$Q), log = T) -
    dnorm(y[t], mean = KF_aux$f[k], sd = sqrt(KF_aux$Q[k]), log = T)

  
  # standardize weight
  w[t, ] <- normalize(w[t, ])
}



# ----------
# remove priors

y <- ts(y[-1])

k <-  k_save[, -1, drop = FALSE]

lambda2 <- lambda2[-1, , drop = FALSE]

W <- W[-1, , drop = FALSE]

V <- V[-1, , drop = FALSE]

mm <- mm[-1, , drop = FALSE]

C <- C[-1, , drop = FALSE]

w <- w[-1, , drop = FALSE]




# ------------------------------------------------------------------------------
# compute 50%, 25%, and 75%
# ------------------------------------------------------------------------------

LWF_W_m     <- sapply(1:t_max, function(t){exp(
  weighted.mean(W[t, ], w = exp(w[t, ]))
)})



LWF_W_quant <- lapply(c(0.25, 0.75), function(quant){
  sapply(1:t_max, function(t){exp(
    weighted.quantile(W[t, ], w = exp(w[t, ]), probs = quant)
  )})
})



LWF_V_m     <- sapply(1:t_max, function(t){exp(
  weighted.mean(V[t, ], w = exp(w[t, ]))
)})



LWF_V_quant <- lapply(c(0.25, 0.75), function(quant){
  sapply(1:t_max, function(t){exp(
    weighted.quantile(V[t, ], w = exp(w[t, ]), probs = quant)
  )})
})



# ------------------------------------------------------------------------------
# convert to time series object
# ------------------------------------------------------------------------------

tsp(y) <- tsp(Nile)


LWF_W_m <- ts(LWF_W_m); tsp(LWF_W_m) <- tsp(y)


LWF_V_m <- ts(LWF_V_m); tsp(LWF_V_m) <- tsp(y)


LWF_W_quant <- lapply(LWF_W_quant, function(x){
  tmp <- ts(x); tsp(tmp) <- tsp(y); return(tmp)
})


LWF_V_quant <- lapply(LWF_V_quant, function(x){
  tmp <- ts(x); tsp(tmp) <- tsp(y); return(tmp)
})




# ------------------------------------------------------------------------------
# plot
# ------------------------------------------------------------------------------

ts.plot(cbind(LWF_W_m, do.call("cbind", LWF_W_quant)),
        lty=c("solid", "dashed", "dashed"), ylab = "W", ylim = c(0, 2e+4), lwd = c(2,1,1))


legend(legend = c("平均値", "50%区間"),
       col = c("black", "black"),
       lty = c("solid", "dashed"),
       lwd = c(2,1),
       x = "topright", cex = 1.0)



# ----------
ts.plot(cbind(LWF_V_m, do.call("cbind", LWF_V_quant)),
        lty=c("solid", "dashed", "dashed"), ylab = "V", ylim = c(0, 1e+5), lwd = c(2,1,1))


legend(legend = c("平均値", "50%区間"),
       col = c("black", "black"),
       lty = c("solid", "dashed"),
       lwd = c(2,1),
       x = "topright", cex = 1.0)




# ------------------------------------------------------------------------------
# re-select filtering particles taking into account future data
# ------------------------------------------------------------------------------

ki <- sapply(1:(t_max-1), function(t){ lambda2[t, smoothing_index(t)] })


# add smoothed distribution at the last time point
ki <- t(cbind(ki, lambda2[t_max, ]))



# ------------------------------------------------------------------------------
# compute 50%, 25%, and 75%
# ------------------------------------------------------------------------------

LWF_lambda2_s     <- sapply(1:t_max, function(t){exp(
  weighted.mean(ki[t, ], w = exp(w[t, ]))
)})


LWF_lambda2_quant <- lapply(c(0.25, 0.75), function(quant){
  sapply(1:t_max, function(t){exp(      # 線形領域に戻す
    weighted.quantile(ki[t, ], w = exp(w[t, ]), probs = quant)
  )})
})



# ------------------------------------------------------------------------------
# convert to time series object
# ------------------------------------------------------------------------------

LWF_lambda2_s <- ts(LWF_lambda2_s); tsp(LWF_lambda2_s) <- tsp(y)



LWF_lambda2_quant <- lapply(LWF_lambda2_quant, function(x){
  tmp <- ts(x); tsp(tmp) <- tsp(y); return(tmp)
})



# ------------------------------------------------------------------------------
# plot
# ------------------------------------------------------------------------------

ts.plot(cbind(LWF_lambda2_s, do.call("cbind", LWF_lambda2_quant)),
        ylab = "Wに対する時変の倍率", ylim = c(0, 30),
        lty = c("solid", "dashed", "dashed"), lwd = c(2,2,2),
        col = c("black", "gray", "gray"))

abline(h = 1, col = "lightgray", lty = "solid")
mtext("1", at = 1, side = 2, cex = 0.6)

points(1899, LWF_lambda2_s[time(LWF_lambda2_s) == 1899], pch = 1)

points(1916, LWF_lambda2_s[time(LWF_lambda2_s) == 1916], pch = 1)

lines(x = c(1899, 1899), y = c(-2, LWF_lambda2_s[time(LWF_lambda2_s) == 1899]),
      lty = "dotted", col = "lightgray")

lines(x = c(1916, 1916), y = c(-2, LWF_lambda2_s[time(LWF_lambda2_s) == 1916]),
      lty = "dotted", col = "lightgray")

mtext("1899", at = 1899, side = 1, adj = 1  , cex = 0.6)

mtext("1916", at = 1916, side = 1, adj = 0.5, cex = 0.6)

legend(legend = c("平均", "50%区間"),
       col = c("black", "black"),
       lty = c("solid", "dashed"),
       x = "topright", cex = 1.0)




# ------------------------------------------------------------------------------
# Kalman smoothing
# ------------------------------------------------------------------------------

modtv_PF <- modtv

modtv_PF$X[ , 1]  <- LWF_lambda2_s * LWF_W_m[t_max]

modtv_PF$V[1, 1]  <- LWF_V_m[t_max]

as.vector(modtv_PF$X); modtv_PF$V



# ----------
# Kalman smoothing

dlmSmoothed_obj <- dlmSmooth(y = y, mod = modtv_PF)


stv_PF <- dropFirst(dlmSmoothed_obj$s)



# ------------------------------------------------------------------------------
# plot
# ------------------------------------------------------------------------------

ts.plot(cbind(y, stv_PF, stv),
        lty=c("solid", "solid", "dashed"), lwd = c(2,2,2),
        col=c("lightgray", "red", "blue"))


legend(legend = c("観測値", "平均（時変粒子フィルタ：馬蹄分布)", "平均（時変カルマンフィルタ)"),
       lty = c("solid", "solid", "dashed"),
       lwd = c(2,2,2),
       col = c("lightgray", "red", "blue"),
       x = "topright", cex = 0.7)

