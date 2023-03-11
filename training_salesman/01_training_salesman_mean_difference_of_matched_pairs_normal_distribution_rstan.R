setwd("//media//kswada//MyFiles//R//training_salesman//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  training salesman
#   - 営業部に2年目の社員が20人おり、毎年の傾向として4-9月にかけての営業成績（契約件数）はほとんど変わりがない
#     そこで今年から6月の終わりに営業成績を向上させるための研修を実施し、効果を測定することとした
#     20人の4-6月までの営業成績と、7-9月までの成績はデータの通り
#        - 研修前の平均契約件数よりも研修後の平均契約件数の方が多い確率は？
#        - 研修後の平均契約件数が研修前よりも2件以上多い確率は？
#        - 研修前と研修後の営業成績の相関が0.5以上の確率は？
# ------------------------------------------------------------------------------
N <- 20

x <- structure(
  .Data = c(6,11,10,13,17,10,10,7,9,1,14,7,7,11,12,12,14,12,7,13,
            7,11,14,13,16,12,8,15,12,3,17,11,9,11,14,12,11,15,11,17),
  .Dim=c(20,2))


data <-list(N = N, x = x)



# ------------------------------------------------------------------------------
# Estimation by Hamilton Monte Carlo Method
# ------------------------------------------------------------------------------
scr <- ".//stan//training_salesman.stan"
scan(scr, what = character(), sep = "\n", blank.lines.skip = F)


par <- c("mu","Sigma","rho","delta","delta_over","delta_over2","rho_over","rho_over05")

war <- 1000               #バーンイン期間
ite <- 11000              #サンプル数
see <- 12345              #シード
dig <- 3                  #有効数字
cha <- 1                  #チェーンの数



# ----------
fit <- stan(file = scr, data = data, iter = ite, seed = see, warmup = war, pars = par, chains = cha)



# ----------
traceplot(fit, inc_warmup = F)

plot(fit)



# ----------
print(fit, pars = par, digits_summary = dig)



# ----------
# 研修前の平均契約件数よりも研修後の平均契約件数の方が多い確率は？:  99.5%
# 研修後の平均契約件数が研修前よりも2件以上多い確率は？:  37.5%  --> We can not say that the training effect is significant.
# 研修前と研修後の営業成績の相関が0.5以上の確率は？:  92.6%

