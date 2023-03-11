rm(list=ls())
# setwd("/media/kswada/MyFiles/R/msci_day")
setwd("/media/kswada/MyFiles/R/Econometrics/msci_day")


packages <- c("dplyr", "AER", "stargazer", "broom", "knitr", "tseries", "vars", "MTS", "forecast")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  msci_day
# ------------------------------------------------------------------------------

# data <- read.table("/media/kswada/MyFiles/references/経済・ファイナンスデータの計量時系列分析/msci_day.txt", header=T, stringsAsFactors=F)

data <- read.table("msci_day.txt", header=T, stringsAsFactors=F)

str(data)


car::some(data)


data <- data[,c(2:8)]




# ------------------------------------------------------------------------------
# Generate the process:  p(t) = alpha + beta1 * rw(t) + beta2 * rw(t-1) + beta3 * p(t-1) + e(t)
# and regress each log(stock index) on the process
#
# VAR model:  includes lag of both of dependent variable (p(t)) and x variable (rw(t-1)) in order to avoid suprious regression
# ------------------------------------------------------------------------------

n <- nrow(data)

graphics.off()

par(mfrow=c(1,1))

mod <- lapply(1:ncol(data), function(x){
  e <- rnorm(n, mean = 0, sd = 1)
  # rn <- cumsum(e)
  
  rn0 <- 0
  rn <- c()
  for(i in 1:n){
    if(i == 1){ rn[i] = rn0 }
    else{ rn[i] <- rn[i-1] + e[i]}
  }
  

  rn20 <- 0
  rnt1 <- c()
  for(i in 1:n){
    if(i == 1){ rnt1[i] = rn20 }
    else{ rnt1[i] <- rn[i-1] }
  }
  
  y0 <- 0
  yt1 <- c()
  for(i in 1:n){
    if(i == 1){ yt1[i] = y0 }
    else{ yt1[i] <- log(data[i-1,x]) }
  }
  
  tmp <- data.frame(y = log(data[,x]), rw = rn, rwt1 = rnt1, yt1 = yt1)
  # tmp <- tmp[2:nrow(tmp),]
  # MTSplot(tmp[,c("yt1","y")])
  # plot(tmp[,c("yt1","y")], main = paste0("Generated Process vs. ", colnames(data)[x]))
  lm(y ~ rw + rwt1 + yt1, data = tmp) 
})



mod[[5]]



# ----------
plot(mod[[5]])


# JP
plot(model.matrix(mod[[1]])[2:1391,4], fitted(mod[[5]])[2:1391], col = "gray", pch = 20, cex = 0.8)




# ----------
library(broom)

output <- data.frame()

for(i in 1:length(mod)){
  var <- colnames(data)[i]
  beta1 <- round(mod[[i]]$coefficients[2], digits = 5)
  pval1 <- round(tidy(mod[[i]])$p.value[2], digits = 5)
  beta2 <- round(tidy(mod[[i]])$estimate[3], digits = 5)
  pval2 <- round(tidy(mod[[i]])$p.value[3], digits = 5)
  beta3 <- round(tidy(mod[[i]])$estimate[4], digits = 5)
  pval3 <- round(tidy(mod[[i]])$p.value[4], digits = 5)
  adj_r <- round(summary(mod[[i]])$adj.r.squared, digits = 3)
  dw <- round(car::durbinWatsonTest(resid(mod[[i]])), digits = 3)
  output0 <- data.frame(var = var, beta1 = beta1, pval1 = pval1, beta2 = beta2, pval2 = pval2, beta3 = beta3, pval3 = pval3, adj_r = adj_r, dw = dw)
  output <- rbind(output, output0)
}

output_pr <- output

output_pr




# -->
# the coefficients of beta1 (rw) and beta2 (rw2) is not significant but beta3 (lagged log(stock index)) is significant
# and still Adjustd R^2 is large
# VAR model (including lag of dependent variable and x variable) is successful to avoid spurious regression



# ----------
# Unit Root Test (KPSS):  not rejected  --> indicating that residuals are not unit root --> not Spurious Regression
kpsstest_pr <- list()

for(i in 1:length(mod)){
  kpsstest_pr[[i]] <- summary(urca::ur.kpss(resid(mod[[i]]), type ="mu", lags = "long"))
}

kpsstest_pr



# ------------------------------------------------------------------------------
# Compare the simulation
# ------------------------------------------------------------------------------

# pvalue of regression coeffs is not significant and Adjusted R is very low
output_wn



# pvalue of regression coeffs are almost zero. Adjusted R is high and low, but dw statistics is very low (close to zero), 
# indicating positive serial correlation
# adj_r > DW
# residuals are UNIT ROOT --> spurious regression
output_rn
kpsstest_rn



# pvalue of beta3 are almost zero. Adjusted R is high. (but dw statistics is close to 2, adj_r < DW ???)
# residuals are not UNIT ROOT
output_pr
kpsstest_pr

