setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  econ5
#   - quarterly U.S. unemployment, GNP, consumption, and government and private investment from
#     1948 - III to 1988 - II.
#     The seasonal component has been removed from the data.
# ------------------------------------------------------------------------------


econ5 <- read.table("econ5.txt", header = T, sep = "\t")


str(econ5)


car::some(econ5)



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))


MTSplot(econ5)



# -->
# Note that all the series has some upward trend:  even for Unemp




# ----------
apply(econ5, MARGIN = 2, FUN = forecast::ndiffs)



# -->
# Note that consum has 2 order difference




# ----------
# 1st difference
mapply(FUN = diff, econ5)

MTSplot(mapply(FUN = diff, econ5))



# 2nd order difference
MTSplot(mapply(FUN = diff, MoreArgs = list(diff = 2), econ5))





# ------------------------------------------------------------------------------
# data exploration:  time series plot after data transformation
# ------------------------------------------------------------------------------

# growth rate

gr <- diff(log(ts(econ5, start = 1948, frequency = 4)))


MTSplot(gr)




# ------------------------------------------------------------------------------
# auto-correlation and partial-autocorrelation
# cross-correlation
# ------------------------------------------------------------------------------


acf2(gr[,"unemp"], max.lag = 50)


acf(gr, max.lag = 50)




# ------------------------------------------------------------------------------
# smoothed spectral
# ------------------------------------------------------------------------------

# scaling for comparison of spectra
gr <- ts(apply(gr, 2, scale), freq = 4)


L <- c(7,7)

gr.spec <- mvspec(gr, spans = L, taper = 0.25, lty = 1:5, lwd = 2, plot = FALSE)



graphics.off()
par(mfrow = c(1,1))


plot(gr.spec, log = "no", main = "Individual Spectra", lty = 1:5, lwd = 2)
abline(v = seq(0.25, 2.0, by = 0.25), lty = 2, col = "darkgray")

legend("topright", colnames(econ5), lty = 1:5, lwd = 2)




# ------------------------------------------------------------------------------
# Squared Coherency
# ------------------------------------------------------------------------------


# Squared coherency by multiple variables simultanesouly

plot.spec.coherency(gr.spec, main = "Squared Coherencies")




# ------------------------------------------------------------------------------
# Squared Coherency between unemp and other variables
# ------------------------------------------------------------------------------

# no scaled data

gr <- diff(log(ts(econ5, start = 1948, frequency = 4)))


L <- 15

m <- (L - 1) / 2



# ----------
# unemp vs. gnp, consum, govinv, priinv

ts1 <- gr[,"unemp"]

ts2 <- gr[,"gnp"]
# ts2 <- gr[,"consum"]
# ts2 <- gr[,"govinv"]
# ts2 <- gr[,"prinv"]


gr.spec_1 <- mvspec(cbind(ts1, ts2), kernel("daniell", m))

( Fq <- qf(0.999, 2, gr.spec_1$df) )

( cn <- Fq / (L - 1 + Fq) )


plot(gr.spec_1, plot.type = "coh")
abline(h = cn)





# ------------------------------------------------------------------------------
# prinv has additional predictive power to unemp ?
# Multiple Coherency:  gnp + prinv --> unemp
# ------------------------------------------------------------------------------


gr <- diff(log(ts(econ5, start = 1948, frequency = 4)))


head(gr)


# unemp is the last
gr <- gr[,c(2,5,1)]


L <- 15

m <- (L - 1) / 2


M <- 100


# Number of inputs
nq <- 2



# ----------
gr.spec <- mvspec(gr, kernel("daniell", m), plot = FALSE)

alpha <- 0.001

( Fq <- qf(1 - alpha, 2, gr.spec$df) )
( cn <- Fq / (L - 1 + Fq) )



# ----------
graphics.off()

par(mfrow = c(1,2))

( Fr <- gr.spec$freq )

plot(Fr, gr.spec$coh[,2], type = "l", ylab = "Sq Coherence", xlab = "Frequency", ylim = c(0,1), main = "Unemp with GNP")
abline(h = cn)



# Squared Coherency with 2 inputs
# stoch.reg:  frequency domain stochasitc regression  --> plot.which = "coh"
# 1: gnp   2: priinv

coh.12 <- stoch.reg(gr, cols.full = c(1,2), cols.red = NULL, alpha, L, M, plot.which = "coh")


# -->
# The additional contribution of prinv to the model seems somewhat marginal



# ----------
# plot(1 - coh.12$power.full / coh.12$power.red, type = "l", ylim = c(0, 1.0))




# ------------------------------------------------------------------------------
# Analysis of Power (ANAPOW)
# testing if consump is predictive of unemp when gnp is in the model ?
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(1,1))


# stoch.reg:  Frequency domain stochasitc regression
# 1:  gnp    2:  prinv
# cols.ful:  columns for full model
# cols.red:  column for reduced model (1:  only gnp)
# last column = unemp is treated as objective variable

out.12 <- stoch.reg(gr, cols.full = c(1,2), cols.red = 1, alpha, L, M, plot.which = "F.stat")


# -->
# solid line:  represents the corresponding quantile of the null F distribution  (null hypothesis)




# ----------
# F statistics for each frequency (240 points)
( eF <- out.12$eF )



# ----------
# p-values for each frequency (240 points)

( number.df <- 2 * nq )

( df <- 2 * L * length(gr) / gr.spec$n.used )

( denom.df <- 2 * (df / 2 - nq) )

( pvals <- pf(eF, number.df, denom.df, lower.tail = FALSE) )



# ----------
# Threshold values (FDR:  False Discovery Rate, for multiple testing adjustment)
# qlevel:  the proportion of false positive desired

pID <- astsa::FDR(pvals, qlevel = alpha)

abline(h = c(eF[pID]), v = pID / (2*n), lty = 2)

title(main = paste0("Partial F Statistic \n", "SOLID:  null   DASHED:  threshold adjusted for multipe testing"))



# -->
# dashed line:  threshold values corresponding to false discovery rate = 0.001 level




# ----------
# FOR REFERENCE:  FDR()

n <- length(pvals)

( sorted.pvals <- sort(pvals) )

( sort.index <- order(pvals) )

qlevel <- 0.001

( indices <- (1:n) * (sorted.pvals <= qlevel * (1:n)/n) )

max(indices)

u <- sort(sort.index[1:max(indices)])

u[which.max(pvals[u])]




# ------------------------------------------------------------------------------
# Eigen value Decomposition of spectral matrix
# ------------------------------------------------------------------------------


gr <- diff(log(ts(econ5, start = 1948, frequency = 4)))

# scaling for eigenvector decomposition
gr <- ts(apply(gr, 2, scale), freq = 4)


L <- c(7,7)

gr.spec <- mvspec(gr, spans = L, taper = 0.25, lty = 1:5, lwd = 2, plot = FALSE)


# spectral matrix: 5 * 5 * 80
dim(gr.spec$fxx)



# ----------
( n.freq <- length(gr.spec$freq) )


lam <- matrix(0, n.freq, 5)

# by frequency
for(k in 1:n.freq) lam[k,] <- eigen(gr.spec$fxx[,,k], symmetric = TRUE, only.values = TRUE)$values




# ----------
graphics.off()

par(mfrow = c(2, 1), mar = c(4,2,2,1), mgp = c(1.6, 0.6, 0))

plot(gr.spec$freq, lam[,1], type = "l", ylab = "", xlab = "Frequency", main = "First Eigenvalue")

abline(v = 0.25, lty = 2)

plot(gr.spec$freq, lam[,2], type = "l", ylab = "", xlab = "Frequency", main = "Second Eigenvalue")

abline(v = 0.125, lty = 2)



# -->
# These eigenvalues suggest the first factor is identified by the freequency of one cycle every 4 years,
# whereas the second factor is identified by the frequency of one cycle every 8 years.  --> govinv




# ----------
# eigen vectors at frequency 10/160 and 5/160 corresponding to 1st and 2nd eigen values

e.vec1 <- eigen(gr.spec$fxx[,,10], symmetric = TRUE)$vectors[,1]

e.vec2 <- eigen(gr.spec$fxx[,,5], symmetric = TRUE)$vectors[,2]

round(Mod(e.vec1), 3)

round(Mod(e.vec2), 3)



# -->
# The modulus of the corresponding eigenvectors at the frequencies of interest,

# These values confirm Unemployment, GNP, Consumption, and Private Investment load on the first factor,
# and Government Investment loads on the second factor.  (0.94)





# ------------------------------------------------------------------------------
# Regression with lagged variables
# ------------------------------------------------------------------------------

gr <- diff(log(ts(econ5, start = 1948, frequency = 4)))


# unemp is the last
gr <- gr[,c(2,5,1)]




# ----------
# Multiple impulse response functions

coh.12 <- stoch.reg(gr, cols.full = c(1,2), cols.red = NULL, alpha, L, M, plot.which = "coh")

S <- seq(from = -M / 2 + 1, to = M / 2 - 1, length = M - 1)


par(mfrow = c(2,1))


plot(S, coh.12$Betahat[,1], type = "h", lwd = 2, main = "Impulse Response Funcion for gnp")
abline(h = 0)

plot(S, coh.12$Betahat[,2], type = "h", lwd = 2, main = "Impulse Response Funcion for prinv")
abline(h = 0)


# astsa::LagReg(input = gr[,"prinv"], output = gr[,"unemp"], L = L, M = M, threshold = 0.01)




# ----------
econt <- ts.intersect(unemp = gr[,"unemp"], 
                      gnp0 = stats::lag(gr[,"gnp"], 0),
                      gnp1 = stats::lag(gr[,"gnp"], -1),
                      gnp2 = stats::lag(gr[,"gnp"], -2),
                      gnp3 = stats::lag(gr[,"gnp"], -3),
                      gnp4 = stats::lag(gr[,"gnp"], -4),
                      gnp5 = stats::lag(gr[,"gnp"], -5),
                      pr0 = stats::lag(gr[,"prinv"], 0),
                      pr1 = stats::lag(gr[,"prinv"], -1),
                      pr4 = stats::lag(gr[,"prinv"], -4),
                      pr7 = stats::lag(gr[,"prinv"], -7),
                      pr8 = stats::lag(gr[,"prinv"], -8))
                      
head(econt)



( u0 <- lm(unemp ~ gnp0 + gnp1 + gnp2 + gnp3 + gnp4 + gnp5 + pr0 + pr1 + pr4 + pr7 + pr8, data = econt, na.action = na.omit) )


( u1 <- lm(unemp ~ gnp0 + gnp1 + gnp2 + gnp3 + gnp4, data = econt, na.action = na.omit) )


( u2 <- lm(unemp ~ gnp0 + gnp1 + gnp2 + pr0, data = econt, na.action = na.omit) )


summary(u0)

summary(u1)

summary(u2)



# ----------
# model residuals

acf2(resid(u1), max.lag = 50)


