setwd("//media//kswada//MyFiles//R//new_catalogue//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  new catalogue
#  - ある企業は、売上の向上を目的に製品カタログの見直しを始めた
#    従来のカタログAと新たに作成したカタログBの比較を行うため、20人の顧客にカタログBを使用してもらい、
#    そのときの平均購入金額を従来のものと比較することを検討
#    カタログAを使用していた期間の顧客の平均購入金額は 2500円であり、20人の顧客それぞれの購入金額はデータの通り
#       - カタログBにおける平均購入金額が従来のものと比べて高い確率は？
#       - カタログを刷新するのは、刷新時のコストを勘案し、カタログBにおける平均購入金額が3000円を超える確率が70%以上であるときのみ
#         カタログBにおける平均購入金額が3000円を超える確率は？
#       - カタログBにおける平均購入金額の2500円に対する効果量が 0.8 より大きくなる確率は？
# ------------------------------------------------------------------------------
N <- 20

x <- c(3060, 2840, 1780, 3280, 3550, 2450, 2200,
       3070, 2100, 4100, 3630, 3060, 3280, 1870,
       2980, 3120, 2150, 3830, 4300, 1880)

data <- list(N = N, x = x)



# ------------------------------------------------------------------------------
# Estimation by Hamilton Monte Carlo Method
# ------------------------------------------------------------------------------
scr <- ".//stan//new_catalogue.stan"
scan(scr, what = character(), sep = "\n", blank.lines.skip = F)


par <- c("mu", "mu_over", "mu_over2", "es_over")

war <- 1000               #バーンイン期間
ite <- 11000              #サンプル数
see <- 12345              #シード
dig <- 3                  #有効数字
cha <- 1                  #チェーンの数


# ----------
fit <- stan(file = scr, data = data, iter = ite, seed = see, warmup = war, pars = par, chains = cha)



# ----------
traceplot(fit,inc_warmup=F)

plot(fit)



# ----------
print(fit, pars = par, digits_summary = dig)



# ----------
# カタログBにおける平均購入金額が従来のものと比べて高い確率は？:  98.9%
# カタログBにおける平均購入金額が3000円を超える確率は？:  33.9%
# カタログBにおける平均購入金額の2500円に対する効果量が 0.8 より大きくなる確率は？: 14.4%


