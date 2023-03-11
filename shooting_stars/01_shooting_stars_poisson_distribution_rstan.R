setwd("//media//kswada//MyFiles//R//shooting_stars//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  shooting stars
#   - 高原で流れ星を観測しており、5分ごとに10回記録したところ、
#     データの通りとなった
#        - 流れ星は5分間で平均何個見られるといえるか
#        - 5分間で見られる流れ星の数には、どのくらいのバラツキがあると考えられるか
#        - 次の5分間で流れ星が2個観測できる確率は？
# ------------------------------------------------------------------------------
N <- 10
x <- c(1, 0, 0, 3, 0, 0, 0, 0, 0, 1)

data <-list(N = N, x = x)



# ------------------------------------------------------------------------------
# Estimation by Hamilton Monte Carlo Method
# ------------------------------------------------------------------------------
scr <- ".//stan//shooting_stars.stan"
scan(scr, what = character(), sep = "\n", blank.lines.skip = F)

par <- c("lambda", "sqrt_lambda", "p")

war <- 5000   #バーンイン期間
ite <- 100000 #サンプル数
see <- 123    #シード
dig <- 3      #有効数字
cha <- 1      #チェーンの数



# ----------
fit <- stan(file = scr, data = data, iter = ite, seed = see, warmup = war, pars = par, chains = cha)



# ----------
graphics.off()
traceplot(fit, inc_warmup = F)

plot(fit)



# ----------
print(fit, pars = par, digits_summary = dig)



# ----------
# 流れ星は5分間で平均何個見られるといえるか: 0.601 [0.221, 1.170]
# 5分間で見られる流れ星の数には、どのくらいのバラツキがあると考えられるか:  0.759 [0.470, 1.081]
# 次の5分間で流れ星が2個観測できる確率は？:  9.8% [2.0%, 21.2%]



