# setwd("//media//kswada//MyFiles//R//access_time//")
# setwd("//media//kswada//MyFiles//R//Bayesian_inference//access_time//")
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\access_time")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  access time
# ------------------------------------------------------------------------------

data <- read.csv("access_time.csv", header = T)


str(data)


library(circular)


checkin_data <- as.circular(data, units = "hours", control.circular = list(modulo="2pi"))


# convert to radian:  if 24:00:00 --> 2 * pi
for (i in 1:length(checkin_data)){
  x[i] <- as.vector((checkin_data[i]/24) * (2*pi))
}

head(x)


data <- list(N = nrow(x), x = x[,1])



# ------------------------------------------------------------------------------
# Set cores
# ------------------------------------------------------------------------------

# parallel::detectCores()

# options(mc.cores = parallel::detectCores())



# ------------------------------------------------------------------------------
# scan stan code
# ------------------------------------------------------------------------------

scr <- ".//stan//access_time_1.stan"

scan(scr, what = character(), sep = "\n", blank.lines.skip = F)



# ------------------------------------------------------------------------------
# set parameters
# ------------------------------------------------------------------------------

par <- c("logLike")

war <- 250        

ite <- 500       

see <- 1234      

dig <- 3         

cha <- 4



# ------------------------------------------------------------------------------
# Estimate:  2 mixed distribution
# ------------------------------------------------------------------------------

fit_2 <- stan(file=scr, model_name="access_time_1", data=data, pars=par, verbose=F, seed=see, chains=cha, warmup=war, iter=ite)



# ------------------------------------------------------------------------------
# Estimate:  3 mixed distribution
# ------------------------------------------------------------------------------

scr <- ".//stan//access_time_2.stan"

fit_3 <- stan(file=scr, model_name="access_time_2", data=data, pars=par, verbose=F, seed=see, chains=cha, warmup=war, iter=ite)



# ------------------------------------------------------------------------------
# Estimate:  4 mixed distribution
# ------------------------------------------------------------------------------

scr <- ".//stan//access_time_3.stan"

par <- c("mu1","mu2","mu3","mu4","sigma","theta","logLike","v","V")


fit_4 <- stan(file=scr, model_name="access_time_3", data=data, pars=par, verbose=F, seed=see, chains=cha, warmup=war, iter=ite)



# ------------------------------------------------------------------------------
# plot traceplot
# ------------------------------------------------------------------------------

traceplot(fit_2)


traceplot(fit_3)


traceplot(fit_4)



# ------------------------------------------------------------------------------
# posterior distribution
# ------------------------------------------------------------------------------

par <- c("logLike")
print(fit_2, pars=par, digits_summary=dig)


par <- c("logLike")
print(fit_3, pars=par, digits_summary=dig)


# par <- c("mu1","mu2","mu3","mu4","sigma","theta","logLike","v","V")
par <- c("mu1","mu2","mu3","mu4","sigma","theta","v","V")
print(fit_4, pars=par, digits_summary=dig)


# V:  circular variance
# v:  circular standard deviation



# -->
# access time mu1 - mu4
2.322 * 180 / pi / 360 * 24
3.127 * 180 / pi / 360 * 24
4.535 * 180 / pi / 360 * 24
5.851 * 180 / pi / 360 * 24


# circular variance
0.404 * 180 / pi / 360 * 24


# circular standard deviation
0.079 * 180 / pi / 360 * 24


