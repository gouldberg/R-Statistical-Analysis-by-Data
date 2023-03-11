setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\my1ef")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  MYE1F
# ------------------------------------------------------------------------------


dat <- read.table("MYE1F.txt", sep = "", header = T, colClasses = "numeric")


head(dat)


dat <- dat$x





# ------------------------------------------------------------------------------
# distribution of original series
# ------------------------------------------------------------------------------


MTSplot(dat)



# ----------
# distribution of original series

hist(dat, breaks = seq(-50, 50, by = 1))


psych::describe(dat)



# -->
# kurtosis is large  (heavy-tailed distribution) = 5.93




# ------------------------------------------------------------------------------
# time series transformation   1/3
# ------------------------------------------------------------------------------


# convert original series to s series

s1 <- log(dat^2 + 0.1)



# ----------
# decomposing time series

s1 <- ts(s1, start = 1, frequency = 12)

plot(decompose(s1))




# ----------
# removing trend

sr1 <- s1 - decompose(s1)$trend



# ----------
# distribution of observation noize

par(mfrow = c(2,1))

hist(dat, breaks = seq(-50, 50, by = 1), main = "original series")

hist(na.omit(sr1), breaks = seq(-8, 5, by = 0.1), main = "transformed series")

psych::describe(dat)

psych::describe(na.omit(sr1))



# -->
# kurtosis: 0.81  (much reduced)
# skewness = -0.85
# sd = 1.64



# ----------
acf2(dat)

acf2(sr1)




# ------------------------------------------------------------------------------
# time series transformation   2/3
# ------------------------------------------------------------------------------


seq1 <- seq(1, 2600, 2)
seq2 <- seq(2, 2600, 2)

s2 <- log((dat[seq1]^2 + dat[seq2]^2 + 0.1)/2)




# ----------
# decomposing time series

s2 <- ts(s2, start = 1, frequency = 12)

plot(decompose(s2))




# ----------
# removing trend

sr2 <- s2 - decompose(s2)$trend



# ----------
# distribution of observation noize

par(mfrow = c(2,1))

hist(dat, breaks = seq(-50, 50, by = 1), main = "original series")

hist(na.omit(sr2), breaks = seq(-7, 4, by = 0.1), main = "transformed series")

psych::describe(dat)

psych::describe(na.omit(sr2))



# -->
# kurtosis: 2.41  (approximately in half)
# skewness = -1.04  (larger)
# sd = 1.16



# ----------
acf2(dat)

acf2(sr2)



# ------------------------------------------------------------------------------
# time series transformation   3/3
# ------------------------------------------------------------------------------


n <- length(dat)

yy <- rep(0, n)

for(i in 2:n) yy[i] <- dat[i] - 0.5 * dat[i-1]


s3 <- log((yy[seq2]^2 + 0.1))




# ----------
# decomposing time series

s3 <- ts(s3, start = 1, frequency = 12)

plot(decompose(s3))




# ----------
# removing trend

sr3 <- s3 - decompose(s3)$trend



# ----------
# distribution of observation noize

par(mfrow = c(2,1))

hist(dat, breaks = seq(-50, 50, by = 1), main = "original series")

hist(na.omit(sr3), breaks = seq(-6, 5, by = 0.1), main = "transformed series")

psych::describe(dat)

psych::describe(na.omit(sr3))



# -->
# kurtosis: 0.39  (much reduced !!!)
# skewness = -0.69  (larger)
# sd = 1.6




# ------------------------------------------------------------------------------
# Compare time series transformation methods
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

hist(dat, breaks = seq(-50, 50, by = 1), main = "original series")

hist(na.omit(sr1), breaks = seq(-8, 5, by = 0.1), main = "transformed series 1")

hist(na.omit(sr2), breaks = seq(-8, 5, by = 0.1), main = "transformed series 2")

hist(na.omit(sr3), breaks = seq(-8, 5, by = 0.1), main = "transformed series 3")



# ----------
psych::describe(dat)

psych::describe(na.omit(sr1))

psych::describe(na.omit(sr2))

psych::describe(na.omit(sr3))




# ----------
acf2(dat, max.lag = 50)

acf2(sr1, max.lag = 50)

acf2(sr2, max.lag = 50)

acf2(sr3, max.lag = 50)




# ------------------------------------------------------------------------------
# double exponential distribution
# ------------------------------------------------------------------------------


u <- runif(n = 10000, min = 0, max = 1)


# double exponential distribution

v <- log(-log(u))



par(mfrow = c(1,1))

hist(v, breaks = seq(-11, 3, by = 0.1))


psych::describe(v)




# ----------
# simulate distribution of mean of double exponential distribution


n_sim <- 100000
n_samp <- 100


output <- rep(0, n_sim)


for(i in 1:n_sim){
  
  u <- runif(n = n_samp, min = 0, max = 1)
  
  output[i] <- mean(log(-log(u)))
}




par(mfrow = c(1,1))

hist(output, breaks = seq(-4, 1, by = 0.01))

psych::describe(output)



# ----------
# mean is close to -057722
mean(output)


# variance is pi^2/6
var(output) * n_samp

pi^2 / 6




