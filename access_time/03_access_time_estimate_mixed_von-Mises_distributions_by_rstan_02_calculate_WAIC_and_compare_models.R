# setwd("//media//kswada//MyFiles//R//access_time//")
setwd("//media//kswada//MyFiles//R//Bayesian_inference//access_time//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  access time
# ------------------------------------------------------------------------------

data <- read.csv("access_time.csv", header = T)


str(data)


library(circular)


checkin_data <- as.circular(data, units = "hours", control.circular = list(modulo="2pi"))

for (i in 1:length(checkin_data)){
  x[i] <- as.vector((checkin_data[i]/24) * (2*pi))
}

head(x)


data <- list(N = nrow(x), x = x[,1])



# ------------------------------------------------------------------------------
# Calculate WAIC for each estimation
# ------------------------------------------------------------------------------


logL <- extract(fit_2)$logLike

lppd <- mean(log(colMeans(exp(logL))))

p_waic <- mean(colMeans(logL^2) - colMeans(logL)^2)

( waic2 <- -2 * lppd + 2 * p_waic )



# ----------
logL <- extract(fit_3)$logLike

lppd <- mean(log(colMeans(exp(logL))))

p_waic <- mean(colMeans(logL^2) - colMeans(logL)^2)

( waic3 <- -2 * lppd + 2 * p_waic )



# ----------
logL <- extract(fit_4)$logLike

lppd <- mean(log(colMeans(exp(logL))))

p_waic <- mean(colMeans(logL^2) - colMeans(logL)^2)

( waic4 <- -2 * lppd + 2 * p_waic )



# ----------
waic2

waic3

waic4



# -->
# 4 mixed model is the best in terms of WAIC


