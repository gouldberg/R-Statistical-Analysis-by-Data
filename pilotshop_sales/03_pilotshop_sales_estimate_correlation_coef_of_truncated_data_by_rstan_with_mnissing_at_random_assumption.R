# setwd("//media//kswada//MyFiles//R//pilotshop_sales//")
setwd("//media//kswada//MyFiles//R//Bayesian_statistics//pilotshop_sales//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  pilotshop sales
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Scan
# ------------------------------------------------------------------------------
scr <- ".//stan//pilotshop_sales.stan"

scan(scr, what = character(), sep = "\n", blank.lines.skip = F)



# ------------------------------------------------------------------------------
# Set parameters
# ------------------------------------------------------------------------------

par <- c("rho_truncated", "rho_corrected")

war <- 1000               #バーンイン期間
ite <- 11000              #サンプル数
see <- 12345              #シード
dig <- 3                  #有効数字
cha <- 1                  #チェーンの数



# ------------------------------------------------------------------------------
# Estimation
# ------------------------------------------------------------------------------

fit <- stan(file = scr, data = data, iter = ite, seed = see, warmup = war, pars = par, chains = cha)



# ------------------------------------------------------------------------------
# Traceplot
# ------------------------------------------------------------------------------

graphics.off()

traceplot(fit, inc_warmup = F)



# ------------------------------------------------------------------------------
# Plot estimated correlation coefficient
# ------------------------------------------------------------------------------

plot(fit)



# ----------
print(fit, pars = par, digits_summary = dig)



# ----------
# もし東京会場でも同じ数だけ商品を販売した場合の売上の相関係数は？
#   - based on raw data (truncated data):  0.596
#   - corrected based on MAR (Missing at Random) assumption: 0.811


# -->
# Surprisingly, corrected base on MAR assumption is 0.811, which is very high.
# This is close to the complete data expample, showing in next.
