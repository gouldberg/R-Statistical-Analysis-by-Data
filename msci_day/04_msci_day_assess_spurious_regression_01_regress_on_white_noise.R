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
# Generate Gaussian white noises:  p(t) = alpha + beta * wn(t) + e(t),  wn(t) ~ iid N(0,1)
# and regress each log(stock index) on the white noises
# ------------------------------------------------------------------------------

n <- nrow(data)

graphics.off()
par(mfrow=c(3,3))

mod <- lapply(1:ncol(data), function(x){
  wn <- rnorm(n, mean = 0, sd = 1)
  tmp <- data.frame(wn = wn, y = log(data[,x]))
  plot(tmp, main = paste0("Gausissan iid White Noise vs. ", colnames(data)[x]), pch = 20, col = "gray", cex = 0.8)
  abline(lm(y ~ wn, data = tmp))
  lm(y ~ wn, data = tmp) 
})



summary(mod[[1]])



# ----------

library(broom)

output <- data.frame()
for(i in 1:length(mod)){
  var <- colnames(data)[i]
  beta <- round(tidy(mod[[i]])$estimate[2], digits = 5)
  pval <- round(tidy(mod[[i]])$p.value[2], digits = 5)
  adj_r <- round(summary(mod[[i]])$adj.r.squared, digits = 3)
  dw <- round(car::durbinWatsonTest(resid(mod[[i]])), digits = 3)
  output0 <- data.frame(var = var, beta = beta, pval = pval, adj_r = adj_r, dw = dw)
  output <- rbind(output, output0)
}

output_wn <- output

output_wn



# -->
# We can not see the spurious regression by regressing log(stock index) on the white noise

