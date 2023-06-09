setwd("//media//kswada//MyFiles//R//directmail_promotion//")

packages <- c("dplyr", "rstan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  directmail_promotion
#   - 化粧品を扱う会社で、ダイレクトメール(DM)がもたらす顧客の購買行動の促進効果について議論になっている
#     ダイレクトメールの購買促進効果が2倍以下であれば、ダイレクトメールを辞めて、宣伝費を節約する予定
#     20代の女性顧客を400名抽出し、無作為に選んだ200名に20代女性用化粧品に関するダイレクトメールを送付し、
#     もう200名には送付せず、1ヶ月後までの購買行動を調査
#     1ヶ月後に購買結果について集計した結果はデータの通り
#        - ダイレクトメールを送付した群と送付しなかった群で購入者の比率の差の期待値は？
#        - この比率の差がプラスとなる確率は？
#        - 群ごとの反応率の比（リスク比）は？
#        - 反応（購入）が観察されない割合に対する、反応率の割合（オッズ比）の期待値は？
# ------------------------------------------------------------------------------
N <- c(200,200)
n <- structure(.Data=c(128,72,97,103),.Dim=c(2,2))

data <- list(N1 = N1, N2 = N2, x1 = x1, x2 = x2)



# ------------------------------------------------------------------------------
# Estimation by Hamilton Monte Carlo Method
# ------------------------------------------------------------------------------
scr <- ".//stan//directmail_promotion.stan"
scan(scr, what = character(), sep = "\n", blank.lines.skip = F)


par<-c("p", "d", "delta_over", "RR", "OR")

war <- 1000               #バーンイン期間
ite <- 11000              #サンプル数
see <- 12345              #シード
dig <- 3                  #有効数字
cha <- 1                  #チェーンの数



# ----------
fit <- stan(file = scr, data = data, iter = ite, seed = see, warmup = war, pars = par, chains = cha)



# ----------
graphics.off()
traceplot(fit, inc_warmup = F)

plot(fit)



# ----------
print(fit, pars = par, digits_summary = dig)



# ----------
# ダイレクトメールを送付した群と送付しなかった群で購入者の比率の差の期待値は？:  0.154
# この比率の差がプラスとなる確率は？: 100%
# 群ごとの反応率の比（リスク比）は？: 1.321 [1.164, 1.498]
# 反応（購入）が観察されない割合に対する、反応率の割合（オッズ比）の期待値は？: 1.902 [1.420, 2.511]


