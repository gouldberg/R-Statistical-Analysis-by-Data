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
# Time Varying Coefficients AR Model by TSSS
# ------------------------------------------------------------------------------

library(TSSS)


# span: local stationary span  --> now we assume that coefficient is changing slowly
# outlier:  positions of outliers
# tau2.ini:  initial estimate of variance of the system noise tau^2
# delta:  search width


# trying trend.order = 1 and 2 + AR order 1 to 10

# we use dat_norm


result <- data.frame()

for(k in 1:2){
  for(m in 1:20){
    z <- tvar(dat_norm, trend.order = k, ar.order = m, span = 20, plot = FALSE)
    result0 <- c(k, m, round(z$aic, 3))
    result <- rbind(result, result0)
  }
}


colnames(result) <- c("k", "arorder", "aic")

result

result %>% filter(k == 1) %>% arrange(aic) %>% head(3)

result %>% filter(k == 2) %>% arrange(aic) %>% head(3)



# -->
# best model: trend.order = 1 and AR order = 10
# best model: trend.order = 2 and AR order = 6



# ----------

graphics.off()

z1 <- tvar(dat_norm, trend.order = 1, ar.order = 10, span = 20, plot = TRUE)


# for reference:  original data
z1_p <- tvar(dat, trend.order = 1, ar.order = 12, span = 20, plot = TRUE)



# ----------
# PARCOR for 1st coefficient
# note that we use span = 20 and the PARCORs are calculated for each span

idx <- sort(rep(seq(1, 130, by = 1), 20))

par(mfrow = c(2,1), ask = FALSE)
plot(z1$parcor[1,idx], type = "l")
abline(v = c(630, 1026), lty = 1, col = "darkgray")

plot(z1_p$parcor[1,idx], type = "l")
abline(v = c(630, 1026), lty = 1, col = "darkgray")




# ----------
z2 <- tvar(dat_norm, trend.order = 2, ar.order = 10, span = 20, plot = TRUE)




# ----------
# system noize variance tau2 --> 0.01, 1e-05

z1$tau2

z2$tau2



# ----------
# observation noize variance  sigma2 --> not much different
z1$sigma2

z2$sigma2




# ------------------------------------------------------------------------------
# Incorporating information of change points
# ------------------------------------------------------------------------------


result <- data.frame()

for(k in 1:2){
  for(m in 1:20){
    z <- tvar(dat_norm, trend.order = k, ar.order = m, span = 20, plot = FALSE,
              outlier = c(630, 1026))
    
    result0 <- c(k, m, round(z$aic, 3))
    result <- rbind(result, result0)
  }
}


colnames(result) <- c("k", "arorder", "aic")

result

result %>% filter(k == 1) %>% arrange(aic) %>% head(3)

result %>% filter(k == 2) %>% arrange(aic) %>% head(3)




# -->
# best model: trend.order = 1 and AR order = 6
# best model: trend.order = 2 and AR order = 4



# ----------
z1_f <- tvar(dat_norm, trend.order = 1, ar.order = 4, span = 20, outlier = c(630, 1026)) 
v1 <- tvvar(dat_norm, trend.order = 1, plot = FALSE)


z2_f <- tvar(dat_norm, trend.order = 2, ar.order = 6, span = 20, outlier = c(630, 1026)) 
v1 <- tvvar(dat_norm, trend.order = 2, plot = FALSE)




# ----------
# system noize variance tau2 --> 1e-03, 1e-10  (decreased for trend.order = 2 model)

z1_f$tau2

z2_f$tau2


z1_f$sigma2

z2_f$sigma2



# for reference:  trend.order = 2 and ar.order = 8
z2_bk <- tvar(dat_norm, trend.order = 2, ar.order = 8, span = 20, outlier = c(630, 1026), 
              tau2.ini = 6.6e-06, delta = 1.0e-06, plot = FALSE)

v2_bk <- tvvar(dat_norm, trend.order = 2,
               tau2.ini = 6.6e-06, delta = 1.0e-06, plot = FALSE)



# ------------------------------------------------------------------------------
# Evolutionary Power Spectra by Time Varying AR Model
# ------------------------------------------------------------------------------


spec1 <- tvspc(arcoef = z1_f$arcoef, span = 20, sigma2 = z1_f$sigma2)

spec2 <- tvspc(arcoef = z2_f$arcoef, span = 20, sigma2 = z2_f$sigma2)

spec3 <- tvspc(arcoef = z2_bk$arcoef, span = 20, sigma2 = z2_bk$sigma2)




# ----------
graphics.off()

par(mfrow = c(1,3))

# plot(spec1, tvv = v1$tvv, dx = 2, dy = 0.10)
plot(spec1, dx = 2, dy = 0.10)

plot(spec2, tvv = v2$tvv, dx = 2, dy = 0.10)

plot(spec3, tvv = v2_bk$tvv, dx = 2, dy = 0.10)

