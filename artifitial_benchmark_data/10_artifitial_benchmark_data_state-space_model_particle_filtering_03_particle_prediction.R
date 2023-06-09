rm(list=ls())

setwd("C:\\Users\\kswad\\OneDrive\\�f�X�N�g�b�v\\�Z�p�͋���_���v���\\51_��̓X�N���v�g\\z_for_demo_uncompleted\\artifitial_benchmark_data")

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Artifitial benchmark data
# ------------------------------------------------------------------------------

# generate benchmark data

set.seed(23)


library(dlm)



# ----------
# set model
W <- 1

V <- 2

m0 <- 10

C0 <- 9


# State Equation
f <- function(x, t) 1 / 2 * x + 25 * x / (1 + x^2) + 8 * cos(1.2 * t)


# Observation
h <- function(x) x^2 / 20


t_max <- 100


x_true <- rep(NA_real_, times = t_max + 1)

y <- rep(NA_real_, times = t_max + 1)


x_true[1] <- m0

for(it in (1:t_max) + 1){
  x_true[it] <- f(x_true[it - 1], it) + rnorm(n = 1, sd = sqrt(W))
  
  y[it] <- h(x_true[it]) + rnorm(n = 1, sd = sqrt(V))
}


x_true <- x_true[-1]

y <- y[-1]



# ----------

dat <- data.frame(x_true = x_true, y = y)

ts.plot(dat, ylab = "y", type = "l", lty = c(1,1), col = c("black", "gray"))


y <- as.ts(y)



# ----------
# ARIMA model
result <- arima(y, order = c(1,0,3), transform.pars = FALSE)
ahat <- result$residuals
result_hat <- y - ahat



# ----------
# DLM model
nStateVar <- 1
nHyperParam <- 2
n <- length(y)
funModel <- function(parm){ dlmModPoly(order = 1, dV = exp(parm[1]), dW = exp(parm[2])) }
oMLE.DLM <- dlmMLE(y, parm = rep(0, 2), build = funModel, hessian = T)

oFitted.DLM <- funModel(oMLE.DLM$par)
oFiltered.DLM <- dlmFilter(y, oFitted.DLM)
m <- oFiltered.DLM$m

var <- dlmSvd2var(oFiltered.DLM$U.R, oFiltered.DLM$D.R)
hwid <- qnorm(0.25, lower = FALSE) * sqrt(unlist(var))

# 50%
filt <- cbind(dropFirst(oFiltered.DLM$m), as.vector(dropFirst(oFiltered.DLM$m)) + hwid %o% c(-1, 1))


oSmoothed.DLM <- dlmSmooth(oFiltered.DLM)
s <- oSmoothed.DLM$s
var <- dlmSvd2var(oSmoothed.DLM$U.S, oSmoothed.DLM$D.S)
hwid <- qnorm(0.025, lower = FALSE) * sqrt(unlist(var))
# 95%
smooth <- cbind(oSmoothed.DLM$s, as.vector(oSmoothed.DLM$s) + hwid %o% c(-1, 1))



# ----------
oFcst.DLM <- dlmForecast(oFiltered.DLM, nAhead = 10, sampleNew = 3)
a <- oFcst.DLM$a



# ------------------------------------------------------------------------------
# State-space model Particle filtering:  Particle prediction
#   - Parameters are known
# ------------------------------------------------------------------------------

set.seed(4521)

x <- rbind(x, matrix(NA_real_, nrow = 10, ncol = N))

w <- rbind(w, matrix(NA_real_, nrow = 10, ncol = N))



# ----------
# time forward process
for (t in t_max+(1:10)){
  
  # State Equatoin
  x[t, ] <- rnorm(N, mean = f(x = x[t-1, ], t = t), sd = sqrt(W))
  
  # Update particle weight
  w[t, ] <- w[t-1, ] * h(x[t, ])
}




# ------------------------------------------------------------------------------
# compute 50%, 25% and 75%
# ------------------------------------------------------------------------------

scratch_a       <- sapply(t_max+(1:10), function(t){
  mean(x[t, ])
})


scratch_a_quant <- lapply(c(0.25, 0.75), function(quant){
  sapply(t_max+(1:10), function(t){
    quantile(x[t, ], probs = quant)
  })
})



scratch_a <- ts(scratch_a, start = t_max+1)


scratch_a_quant <- lapply(scratch_a_quant, function(dat){
  ts(dat, start = t_max+1)
})



# ------------------------------------------------------------------------------
# plot prediction and 50% interval
# ------------------------------------------------------------------------------

ts.plot(cbind(y, a, scratch_a),
        col = c("lightgray", "blue", "red"),
        lty = c("solid", "solid", "dashed"))


legend(legend = c("�ϑ��l", "���� �i�J���}���\��)",  "���� �i���q�\��)"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)


# ----------
ts.plot(cbind(y, do.call("cbind", scratch_a_quant)),
        col = c("lightgray", "blue", "blue", "red", "red"),
        lty = c("solid", "solid", "solid", "dashed", "dashed"))


legend(legend = c("�ϑ��l", "50%��� �i���q�\��)"),
       lty = c("solid", "solid", "dashed"),
       col = c("lightgray", "blue", "red"),
       x = "topright", text.width = 70, cex = 0.6)
