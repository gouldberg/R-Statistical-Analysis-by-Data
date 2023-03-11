## Empirical Bayesian
## ----------------------------------------------------------------------------
setwd("C:\\Users\\光世\\Desktop\\#R_ベイズ推定\\baseball")

theme_set(theme_bw())


# -----------------------------------------------------------------------------
# Beta Distribution および Bayesian Updating
# 二項分布の事前分布として使われることが多い
# -----------------------------------------------------------------------------
library(knitr)
opts_chunk$set(cache = TRUE, warning = FALSE, tidy = FALSE, fig.height=5, fig.width=3, dev="cairi_pdf")
library(ggplot2)
theme_set(theme_bw())
options(tibble.print_min = 6, scipen = 7)
library(dplyr)


# ベータ分布
sim <- data_frame(a = c(1, 3, 50, 20), b = c(2, 3, 10, 20)) %>% group_by(a, b) %>%
  do(data_frame(x = seq(0, 1, 0.001), y = dbeta(x, .$a, .$b))) %>%
  mutate(Parameters = paste0("\u03B1 = ", a, ", \u03B2 = ", b)) %>%
  ungroup() %>% mutate(Parameters = factor(Parameters, levels=unique(Parameters)))

ggplot(sim, aes(x, y, color=Parameters)) + geom_line() + xlab("Batting average") + ylab("Density of beta")


# 全打者のシーズン全体の打率の事前分布：　平均 0.75,  range from 0.21 to 0.35  --> alpha = 81,  beta = 219
alpha <- 81
beta <- 219
hit1 <- 1;  miss1 <- 0;
hit2 <- 100;  miss2 <- 200;

sim <- data_frame(a = c(alpha, alpha+hit1, alpha+hit2), b = c(beta, beta+miss1, beta+miss2)) %>% group_by(a, b) %>%
  do(data_frame(x = seq(0, 0.5, 0.001), y = dbeta(x, .$a, .$b))) %>%
  mutate(Parameters = paste0("\u03B1 = ", a, ", \u03B2 = ", b)) %>%
  ungroup() %>% mutate(Parameters = factor(Parameters, levels=unique(Parameters)))

# 全打者のシーズン全体の打率の事前分布　および　平均打率
sim %>% filter(a == alpha) %>%
  ggplot(aes(x, y, color=Parameters)) + geom_line() + xlab("Batting average") + ylab("Density of beta")
( alpha ) / ( alpha　+ beta )

# 1打数1ヒットの打者の事後分布　および　事後平均
sim %>% filter(a == alpha + hit1) %>%
  ggplot(aes(x, y, color=Parameters)) + geom_line() + xlab("Batting average") + ylab("Density of beta")
( alpha + 1 + hit1 ) / ( alpha + 1 + hit1 + beta + miss1 )

# 300打数100ヒットの打者の事後分布　および　事後平均
sim %>% filter(a == alpha + hit2) %>%
  ggplot(aes(x, y, color=Parameters)) + geom_line() + xlab("Batting average") + ylab("Density of beta")
( alpha + 1 + hit2 ) / ( alpha + 1 + hit2 + beta + miss2 )


# ---　真の平均打率を知りたい
alpha <- 81
beta <- 219
AB <- 300
num_trials <- 10e6

# 300打席で、平均打率をもつ打者のサンプル
simulations <- data_frame(
  true_average = rbeta(num_trials, alpha, beta),
  hits = rbinom(num_trials, AB, true_average))

simulations

# 300打席で100ヒットを打った打者の打率の分布 --> 事前分布では打率=0.20があったが、この分布ではなくなった
# つまり、実力が0.20の人が、平均的な打率である確率はほとんどない
# Bayesian Updating が赤線の密度分布であるが、シミュレーション結果と合致している
hit <- 100
miss <- 200
dens <- function(x) dbeta(x, alpha + hit, beta + miss)
ggplot(simulations %>% filter(hits == hit), aes(true_average)) + geom_histogram(aes(y = ..density..)) +
  stat_function(color="red", fun=dens) + labs(x = "Batting average of players who got 100 H / 300 AB")

# 60, 80, 100 ヒットで確認してみる
simulations %>% filter(hits %in% c(60, 80, 100)) %>% ggplot(aes(true_average, color = factor(hits))) + geom_density() + labs(x = "True average of players with H hits / 300 at_bats", color="H")


# -----------------------------------------------------------------------------
# 事前分布の推定　および　事前分布を使っての事後平均の推定
# 打数の少ない人、多い人、それぞれの打率が推定される　（つまり1打数ノーヒット、1打数1ヒット　のようなケースも扱える）
# -----------------------------------------------------------------------------
library(knitr)
opts_chunk$set(cache = TRUE, warning = FALSE, message = FALSE, echo = TRUE, tidy = FALSE, fig.height=5, fig.width=3)
library(ggplot2)
theme_set(theme_bw())
options(tibble.print_min = 6, scipen = 7)
library(dplyr);  library(tidyr);  library(Lahman);

# --　データ準備
pitchers <- Pitching %>% group_by(playerID) %>% summarize(gamesPitched = sum(G)) %>% filter(gamesPitched > 3)
career <- Batting %>% filter(AB > 0) %>% anti_join(pitchers, by="playerID") %>% group_by(playerID) %>% summarize(H = sum(H), AB = sum(AB)) %>% mutate(average = H / AB)
career <- Master %>% tbl_df %>% dplyr::select(playerID, nameFirst, nameLast) %>% unite(name, nameFirst, nameLast, sep=" ") %>% inner_join(career, by="playerID")

career %>% arrange(desc(average)) %>% head(5) %>% knitr::kable(booktabs = TRUE)


# --　事前分布の推定
# 分布データから、最も対数尤度が高くなるベータ分布のパラメータを探索する
library(stats4)

# 500打数以上の打者の打率分布
career %>% filter(AB >= 500) %>% ggplot(aes(average)) + geom_histogram(binwidth = 0.005)
career_filtered <- career %>% filter(AB > 500)

# log-likelihood function
ll <- function(alpha, beta){
  x <- career_filtered$H
  total <- career_filtered$AB
  -sum(VGAM::dbetabinom.ab(x, total, alpha, beta, log=TRUE))
}

# maximum likelihood estimation --> 事前分布のパラメータ推定
m <- mle(ll, start = list(alpha = 1, beta = 10), method = "L-BFGS-B", lower=c(0.0001, 0.1))
ab <- coef(m)
alpha0 <- ab[1]
beta0 <- ab[2]


# 推定されたパラメータが実データに適合するかを確認
career_filtered %>% filter(AB > 500) %>% ggplot() + geom_histogram(aes(average, y = ..density..), binwidth = 0.005) +
  stat_function(color="red", fun=function(x) dbeta(x, alpha0, beta0), size=1) +
  labs(x = "Batting average")

# --　事前分布を使って、Bayesian Update　による各打者の打率推定
career_eb <- career %>% mutate(eb_estimate = ( H + alpha0 ) / ( AB + alpha0 + beta0 ))
career_eb

#
options(digits = 3)
career_eb %>% arrange(desc(eb_estimate)) %>% head(5) %>% kable(booktabs = TRUE)
options(digits = 1)


# shrinkage が確認できる !!!:  Extraordinary outliers require extraordinary evidence
ggplot(career_eb, aes(average, eb_estimate, color=AB)) +
  geom_hline(yintercept = alpha0 / (alpha0 + beta0), color="red", lty=2) +
  geom_point() + geom_abline(color="red") +
  scale_colour_gradient(trans = "log", breaks = 10^(1:5)) +
  xlab("Batting average") + ylab("Empirical Bayes batting average")


# -----------------------------------------------------------------------------
# Credible Intervals
# -----------------------------------------------------------------------------
library(knitr)
opts_chunk$set(cache = TRUE, warning = FALSE, tidy = FALSE, fig.height=5, fig.width=6.67, out.height="3in", out.width="4in")
options(digits = 3)
library(ggplot2)
theme_set(theme_bw())
library(dplyr);  library(tidyr);  library(Lahman);

# --　データ準備
career <- Batting %>% filter(AB > 0) %>% anti_join(pitchers, by="playerID") %>% group_by(playerID) %>% summarize(H = sum(H), AB = sum(AB)) %>% mutate(average = H / AB)
career <- Master %>% tbl_df %>% dplyr::select(playerID, nameFirst, nameLast) %>% unite(name, nameFirst, nameLast, sep=" ") %>% inner_join(career, by="playerID")

alpha0 <- 101.4
beta0 <- 287.3

career_eb <- career %>% mutate(eb_estimate = ( H + alpha0 ) / ( AB + alpha0 + beta0 ))
career_eb


# 各打者の打率分布
career_eb <- career_eb %>% mutate(alpha1 = alpha0 + H, beta1 = beta0 + AB - H)

yankee_1998 <- c("brosisc01", "jeterde01", "knoblch01", "martiti02", "posadjo01", "strawda01", "willibe02")
yankee_1998_career <- career_eb %>% filter(playerID %in% yankee_1998)
yankee_beta <- yankee_1998_career %>% crossing(x = seq(0.18, 0.33, 0.0002)) %>% ungroup() %>% mutate(density = dbeta(x, alpha1, beta1))

ggplot(yankee_beta, aes(x, density, color=name)) + geom_line() +
  stat_function(fun=function(x) dbeta(x, alpha0, beta0), lty=2, color="black") +
  labs(x = "Batting average", color = "Player")


# ジーターの95% credible interval
jeter <- yankee_beta %>% filter(name == "Derek Jeter")
jeter_pred <- jeter %>% mutate(cumulative = pbeta(x, alpha1, beta1)) %>% filter(cumulative > 0.025, cumulative < 0.975)
jeter_low <- qbeta(0.025, jeter$alpha1[1], jeter$beta1[1])
jeter_high <- qbeta(0.975, jeter$alpha1[1], jeter$beta1[1])

jeter %>% ggplot(aes(x, density)) + geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = density), data = jeter_pred, alpha = 0.25, fill = "red") +
  stat_function(fun = function(x) dbeta(x, alpha0, beta0), lty=2, color="black") +
  geom_errorbarh(aes(xmin = jeter_low, xmax = jeter_high, y = 0), height = 3.5, color="red") + xlim(0.18, 0.34)


#　各打者の 95% credible interval
yankee_1998_career <- yankee_1998_career %>% mutate(low = qbeta(0.025, alpha1, beta1), high = qbeta(0.975, alpha1, beta1))
yankee_1998_career %>% dplyr::select(-alpha1, -beta1, -eb_estimate) %>% knitr::kable()

yankee_1998_career %>% mutate(name = reorder(name, eb_estimate)) %>%
  ggplot(aes(eb_estimate, name)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low, xmax = high)) +
  geom_vline(xintercept = alpha0 / (alpha0 + beta0), color = "red", lty=2) +
  xlab("Estimated batting average (w/ 95% interval)") +
  ylab("Player")


#　credible interval vs. condidence interval
# 打数が多い場合はほぼ一致するが、打数が少ない場合credible の方が狭い
career_eb <- career_eb %>% mutate(low = qbeta(0.025, alpha1, beta1), high = qbeta(0.975, alpha1, beta1))

library(broom)
set.seed(2015)
some <- career_eb %>% sample_n(20) %>% mutate(name = paste0(name, " (", H, "/", AB, ")"))

frequentist <- some %>% group_by(playerID, name, AB) %>% do(tidy(binom.test(.$H, .$AB))) %>% ungroup() %>%
  dplyr::select(playerID, name, estimate, low=conf.low, high=conf.high) %>%
  mutate(method = "Confidence")

bayesian <- some %>% dplyr::select(playerID, name, AB, estimate = eb_estimate, low=low, high=high) %>%
  mutate(method = "Credible")

combined <- bind_rows(frequentist, bayesian)

combined %>%
  mutate(name = reorder(name, -AB, na.rm=TRUE)) %>%
  ggplot(aes(estimate, name, color=method, group=method)) +
  geom_point() +
  geom_errorbarh(aes(xmin = low, xmax = high)) +
  geom_vline(xintercept = alpha0 / (alpha0 + beta0), color = "red", lty=2) +
  xlab("Estimated batting average") +
  ylab("Player") + labs(color="")


# -----------------------------------------------------------------------------
# Bayesian AB testing
# If I picked a random draw from Piazza's distribution and a random draw from Aaron's, what7s the probability Piazza is higher ?
# -----------------------------------------------------------------------------
library(knitr)
opts_chunk$set(cache = TRUE, warning = FALSE, tidy = FALSE, fig.height=5, fig.width=6.67, out.height="3in", out.width="4in")
options(digits = 3)

pitchers <- Pitching %>% group_by(playerID) %>% summarize(gamesPitched = sum(G)) %>% filter(gamesPitched > 3)
career <- Batting %>% filter(AB > 0) %>% anti_join(pitchers, by="playerID") %>% group_by(playerID) %>% summarize(H = sum(H), AB = sum(AB)) %>% mutate(average = H / AB)
career <- Master %>% tbl_df %>% dplyr::select(playerID, nameFirst, nameLast) %>% unite(name, nameFirst, nameLast, sep=" ") %>% inner_join(career, by="playerID")

alpha0 <- 101.4
beta0 <- 287.3

career_eb <- career %>% mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0)) %>% mutate(alpha1 = alpha0 + H, beta1 = beta0 + AB - H) %>% arrange(desc(eb_estimate))
( two_players <- bind_rows(aaron, piazza) )

# Piazza vs Aaron
two_players %>% crossing(x = seq(0.28, 0.33, 0.00025)) %>% mutate(density = dbeta(x, alpha1, beta1)) %>%
  ggplot(aes(x=x, y=density, color=name)) + geom_line() + labs(x = "Batting average", color="") + theme_bw()

# Piazza vs Aaron vs. Matsui
career_eb %>% filter(name %in% c("Hank Aaron", "Mike Piazza", "Hideki Matsui")) %>%
  crossing(x = seq(0.26, 0.33, 0.00025)) %>% mutate(density = dbeta(x, alpha1, beta1)) %>%
  ggplot(aes(x=x, y=density, color=name)) + geom_line() + labs(x = "Batting average", color="") + theme_bw()


# -- 1. Simulation of posterior draws
# 59.4% probability Piazza is better than Aaron !
aaron_simulation <- rbeta(1e6, aaron$alpha1, aaron$beta1)
piazza_simulation <- rbeta(1e6, piazza$alpha1, piazza$beta1)
( sim <- mean(piazza_simulation > aaron_simulation) )
scales::percent(sim)


# --  2. Numerical integration:  joint distribution
library(tidyr)

# show joint distribution
x <- seq(0.29, 0.318, 0.0002)
crossing(piazza_x = x, aaron_x = x) %>%
  mutate(piazza_density = dbeta(piazza_x, piazza$alpha1, piazza$beta1),
        aaron_density = dbeta(aaron_x, aaron$alpha1, aaron$beta1),
        joint = piazza_density * aaron_density) %>%
  ggplot(aes(piazza_x, aaron_x, fill=joint)) + geom_tile() + geom_abline() +
  scale_fill_gradient2(low="white", high="red") +
  labs(x = "Piazza batting average", y = "Aaron batting average", fill = "Joint density") +
  theme(legend.position = "none")


d <- 0.00002
limits <- seq(0.29, 0.33, d)
tmp <- outer(limits, limits, function(x,y){ (x > y) * dbeta(x, piazza$alpha1, piazza$beta1) * dbeta(y, aaron$alpha1, aaron$beta1) * d^2 })
sum(tmp)


#　-- 3. Closed-form solution
# alpha_b が大きいと計算が遅い
# alpha_b の値に対して exact である
h <- function(alpha_a, beta_a, alpha_b, beta_b){
  j <- seq.int(0, round(alpha_b) - 1)
  log_vals <- (lbeta(alpha_a + j, beta_a + beta_b) - log(beta_b + j) - lbeta(1 + j, beta_b) - lbeta(alpha_a, beta_a))
  1 - sum(exp(log_vals))
}

h(piazza$alpha1, piazza$beta1, aaron$alpha1, aaron$beta1)


#　-- 4. Colosed-form approximation
# alpha　と　beta の値が大きいと、ベータ分布が正規分布に近いことから近似可能
# alpha, beta のいずれかが小さい値だと近似精度が悪い
# systematically biased

# 二人のベータ分布は、正規分布に極めて近い
two_players %>% mutate(mu = alpha1 / (alpha1 + beta1), var = alpha1 * beta1 / ((alpha1 + beta1)^2 * (alpha1 + beta1 + 1))) %>%
  crossing(x = seq(0.28, 0.33, 0.00025)) %>%
  mutate(density = dbeta(x, alpha1, beta1), normal = dnorm(x, mu, sqrt(var))) %>%
  ggplot(aes(x, density, group=name)) + geom_line(aes(color=name)) + geom_line(lty=2)

# 正規分布だとみなして計算
h_approx <- function(alpha_a, beta_a, alpha_b, beta_b){
  u1 <- alpha_a / (alpha_a + beta_a)
  u2 <- alpha_b / (alpha_b + beta_b)
  var1 <- (alpha_a * beta_a) / ((alpha_a + beta_a)^2 * (alpha_a + beta_a + 1))
  var2 <- (alpha_b * beta_b) / ((alpha_b + beta_b)^2 * (alpha_b + beta_b + 1))
  pnorm(0, u2 - u1, sqrt(var1 + var2))
}

h_approx(piazza$alpha1, piazza$beta1, aaron$alpha1, aaron$beta1)


#　-- Confidence and credible intervals
# contingency table
two_players %>% transmute(Player = name, Hits = H, Misses = AB - H) %>% knitr::kable(booktabs = TRUE)


#　ピアソンのカイ二乗検定（2つの比率の差の検定）では、差があるとは言えない
#  confidence intervals も確認できる
prop.test(two_players$H, two_players$AB)


#　normal approximation により、比率の credible interval
credible_interval_approx <- function(a, b, c, d){
  u1 <- a / (a + b)
  u2 <- c / (c + d)
  var1 <- a * b / ((a + b)^2*(a + b + 1))
  var2 <- c * d / ((c + d)^2*(c + d + 1))
  mu_diff <- u2 - u1
  sd_diff <- sqrt(var1 + var2)
  data_frame(posterior = pnorm(0, mu_diff, sd_diff),
    estimate = mu_diff,
    conf.low = qnorm(0.025, mu_diff, sd_diff),
    conf.high = qnorm(0.975, mu_diff, sd_diff))
}

credible_interval_approx(piazza$alpha1, piazza$beta1, aaron$alpha1, aaron$beta1)


# 20プレーヤーでやってみる
set.seed(2016)

intervals <- career_eb %>% filter(AB > 10) %>% sample_n(20) %>% group_by(name, H, AB) %>%
  do(credible_interval_approx(piazza$alpha1, piazza$beta1, .$alpha1, .$beta1)) %>%
  ungroup() %>% mutate(name = reorder(paste0(name, " (", H, " / ", AB, ")"), -estimate))

intervals

f <- function(H, AB) broom::tidy(prop.test(c(H, piazza$H), c(AB, piazza$AB)))
prop_tests <- purrr::map2_df(intervals$H, intervals$AB, f) %>% mutate(estimate = estimate1 - estimate2, name = intervals$name)
prop_tests

all_intervals <- bind_rows(
    mutate(intervals, type="Credible"),
    mutate(prop_tests, type="Confidence"))

# ほとんどのケースで、credible の方が狭い
all_intervals %>% mutate(name = reorder(name, -AB, na.rm=TRUE)) %>%
  ggplot(aes(x = estimate, y = name, color = type)) + geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) + xlab("Piazza average - Player average") + ylab("Player")


# -----------------------------------------------------------------------------
# Beta binomial regression
# When players are better they are given more chances to batting
# There's a relationship between the AB and true batting average
#
# GAMLSS:  generalized additive model for location scale and shape
# -----------------------------------------------------------------------------
library(knitr)
opts_chunk$set(cache = TRUE, warning = FALSE, tidy = FALSE, fig.height=5, fig.width=6.67, out.height="3in", out.width="4in")
options(digits = 3)

library(ggplot2)
theme_set(theme_bw())
library(dplyr);  library(tidyr);  library(Lahman);


# --　データ準備
pitchers <- Pitching %>% group_by(playerID) %>% summarize(gamesPitched = sum(G)) %>% filter(gamesPitched > 3)
career <- Batting %>% filter(AB > 0) %>% anti_join(pitchers, by="playerID") %>% group_by(playerID) %>% summarize(H = sum(H), AB = sum(AB)) %>% mutate(average = H / AB)
career <- Master %>% tbl_df %>% dplyr::select(playerID, nameFirst, nameLast) %>% unite(name, nameFirst, nameLast, sep=" ") %>% inner_join(career, by="playerID")

alpha0 <- 101.4
beta0 <- 287.3
prior_mu <- alpha0 / (alpha0 + beta0)

career_eb <- career %>% mutate(eb_estimate = (H + alpha0) / (AB + alpha0 + beta0)) %>% mutate(alpha1 = alpha0 + H, beta1 = beta0 + AB - H) %>% arrange(desc(eb_estimate))


# --　打数と打率との関係
career %>% filter(AB >= 10) %>% ggplot(aes(AB, average)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) + scale_x_log10() +
  labs(x = "Number of at-bats(AB)", y = "Raw batting average (H/AB)")

# EB shrinkage でもその傾向がみられる
# EB shrinkage でも、打数が少ない人は、overestimate になっていることが確認できる
tmp <- career_eb %>% filter(AB >= 10) %>% gather(type, value, average, eb_estimate) %>%
  mutate(type = plyr::revalue(type, c(average = "Raw", eb_estimate = "with EB shrinkage")))
tmp
ggplot(tmp, aes(AB, value)) + geom_point() + scale_x_log10() +
  geom_hline(color="red", lty=2, size=1.5, yintercept=prior_mu) +
  facet_wrap(~type) + ylab("average") + geom_smooth(method = "lm")
median_lt_20 <- career_eb %>% filter(AB >= 10, AB <= 20) %>% summarize(average = median(H/AB))
median_lt_20

# --
# Step1:  Fit the model across all players
# prior beta distribution for a player depends on the value of AB !!
library(gamlss)
library(broom)
# BB: Beta Binomial
fit <- gamlss(cbind(H, AB - H) ~ log(AB), data = career_eb, family = BB(mu.link = "identity"))
td <- tidy(fit)
td

( sigma <- exp(td %>% filter(parameter == "sigma") %>% dplyr::select(estimate)) )

mu_0 <- td$estimate[1]
mu_AB <- td$estimate[2]
sigma <- exp(td$estimate[3])

# 打数による事前分布の違い
crossing(x = seq(0.08, 0.35, 0.001), AB = c(1,10,100,1000,10000)) %>%
  mutate(density = dbeta(x, (mu_0 + mu_AB * log(AB)) / sigma,  ( 1 - (mu_0 + mu_AB * log(AB))) / sigma)) %>%
  mutate(AB = factor(AB)) %>%
  ggplot(aes(x, density, color=AB, group=AB)) + geom_line() + xlab("Batting average") + ylab("Prior density")


# Step2:  Estimate each player's average using this prior
mu <- fitted(fit, parameter = "mu")
sigma <- fitted(fit, parameter = "sigma")

head(mu)
head(sigma)

career_eb_WAB <- career_eb %>% dplyr::select(name, H, AB, original_eb = eb_estimate) %>%
  mutate(mu = mu, alpha0 = mu / sigma, beta0 = (1 - mu) / sigma, alpha1 = alpha0 + H, beta1 = beta0 + AB - H, new_eb = alpha1 / (alpha1 + beta1))

# 打数が少ない人のバイアスは減っている
ggplot(career_eb_WAB, aes(original_eb, new_eb, color=AB)) +
  geom_point() + geom_abline(color="red") +
  xlab("Original EB Estimate") + ylab("EB Estimate w/ AB term") +
  scale_colour_gradient(trans = "log", breaks=10^(0:4))


# 今度は trend に合わせて shrinkage !!!
library(tidyr)
lev <- c(raw = "Raw H /AB", original_eb = "EB Estimate", new_eb = "EB w/ Regression")
career_eb_WAB %>% filter(AB >= 10) %>% mutate(raw = H/AB) %>% gather(type, value, raw, original_eb, new_eb) %>%
  mutate(mu = ifelse(type == "original_eb", prior_mu, ifelse(type == "new_eb", mu, NA))) %>%
  mutate(type = factor(plyr::revalue(type, lev), lev)) %>%
  ggplot(aes(AB, value)) + geom_point() + geom_line(aes(y = mu), color="red") + scale_x_log10() + facet_wrap(~type) +
  xlab("At-Bats (AB)") + ylab("Estimate")


# -----------------------------------------------------------------------------
# mixture model
# -----------------------------------------------------------------------------
library(knitr)
opts_chunk$set(cache = TRUE, warning = FALSE, tidy = FALSE, fig.height=5, fig.width=6.67, out.height="3in", out.width="4in")
options(digits = 3)

library(ggplot2)
theme_set(theme_bw())
library(dplyr);  library(tidyr);  library(Lahman);


# --　データ準備
pitchers <- Pitching %>% group_by(playerID) %>% summarize(gamesPitched = sum(G)) %>% filter(gamesPitched > 3)
career <- Batting %>% filter(AB > 0, lgID == "NL", yearID >= 1980) %>% group_by(playerID) %>% summarize(H = sum(H), AB = sum(AB), year = mean(yearID)) %>% mutate(average = H/AB, isPitcher = playerID %in% pitchers$playerID)
career <- Master %>% tbl_df %>% dplyr::select(playerID, nameFirst, nameLast) %>% unite(name, nameFirst, nameLast, sep=" ") %>% inner_join(career, by="playerID")


library(VGAM)
fit_bb_mle <- function(x, n){
  ll <- function(alpha, beta){
    -sum(dbetabinom.ab(x, n, alpha, beta, log=TRUE))
  }
  m <- stats4::mle(ll, start = list(alpha=30, beta=100), method="L-BFGS-B", lower=c(0.0001, 0.1))
  ab <- stats4::coef(m)
  data_frame(alpha = ab[1], beta = ab[2])
}

batting_w_pitchers <- Batting %>% filter(AB >= 50, lgID == "NL", yearID > 1985) %>% group_by(playerID) %>% summarize(H = sum(H), AB = sum(AB), year = mean(yearID)) %>%
  mutate(average = H/AB,
    isPitcher = ifelse(playerID %in% pitchers$playerID, "Pitcher", "Non-Pitcher"),
    isPitcher = relevel(factor(isPitcher), "Pitcher"))

fit <- fit_bb_mle(batting_w_pitchers$H, batting_w_pitchers$AB)

batting_w_pitchers %>%
  ggplot(aes(average, fill = isPitcher)) +
  geom_histogram(bins = 30) +
  stat_function(fun = function(x) 30 * dbeta(x, fit$alpha, fit$beta), lty=2) +
  xlim(0, 0.4) + labs(fill = "", x = "Batting average (H / AB)")


# -- Expectation - maximaization
# at first, assign clusters randomly
set.seed(2016)
starting_data <- career %>% filter(AB >= 20) %>% dplyr::select(-year, -isPitcher) %>% mutate(cluster = factor(sample(c("A", "B"), n(), replace = TRUE)))

starting_data %>% ggplot(aes(average, color=cluster)) + geom_density()

# --maximization
fit_bb_mle <- function(x, n){
  ll <- function(alpha, beta){
    -sum(dbetabinom.ab(x, n, alpha, beta, log=TRUE))
  }
  m <- stats4::mle(ll, start = list(alpha=3, beta=100), method="L-BFGS-B", lower=c(0.0001, 0.001))
  ab <- stats4::coef(m)
  data_frame(alpha = ab[1], beta = ab[2])
}

fit_bb_mle(starting_data$H, starting_data$AB)

fits <- starting_data %>% group_by(cluster) %>% do(fit_bb_mle(.$H, .$AB)) %>% ungroup()
fits

# --expectation
fits %>% crossing(x = seq(0, 0.4, 0.0001)) %>% mutate(density = dbeta(x, alpha, beta)) %>%
  ggplot() + geom_histogram(aes(x = average, y = ..density.., fill = cluster), data = starting_data, alpha = 0.2) +
  geom_line(aes(x, density, color=cluster))

crosses <- starting_data %>% dplyr::select(-cluster) %>% crossing(fits) %>% mutate(likelihood = VGAM::dbetabinom.ab(H, AB, alpha, beta))

assignments <- starting_data %>% dplyr::select(-cluster) %>% crossing(fits) %>% mutate(likelihood = VGAM::dbetabinom.ab(H, AB, alpha, beta)) %>%
  group_by(playerID) %>% top_n(1, likelihood) %>% ungroup()

ggplot(assignments, aes(average, fill = cluster)) + geom_histogram()

# -- iteration over expectation and maximization
assignments %>% group_by(cluster) %>% do(fit_bb_mle(.$H, .$AB)) %>% ungroup() %>%
  crossing(x = seq(0, 0.4, 0.0001)) %>%
  mutate(density = 0.01 * nrow(assignments) * dbeta(x, alpha, beta)) %>%
  ggplot() + geom_histogram(aes(average, fill = cluster), data=assignments, alpha=0.25, binwidth=0.01) +
  geom_line(aes(x, density, color=cluster))


set.seed(1337)
iterate_em <- function(state, ...){
  fits <- state$assignments %>% group_by(cluster) %>% do(fit_bb_mle(.$H, .$AB)) %>% ungroup()
  assignments <- state$assignments %>% dplyr::select(playerID:average) %>% crossing(fits) %>% mutate(likelihood = VGAM::dbetabinom.ab(H, AB, alpha, beta)) %>% group_by(playerID) %>% top_n(1, likelihood) %>% ungroup()
  list(assignments = assignments, fits = fits)
}

library(purrr)
init <- list(assignments = starting_data)
iterations <- accumulate(1:5, iterate_em, .init=init)

assignment_iterations <- iterations %>%
  map_df("assignments", .id="iteration")

assignment_iterations %>%
  ggplot(aes(average, fill=cluster)) + geom_histogram() + facet_wrap(~iteration)


# assign players to clusters
( final_parameters <- last(iterations)$fits )

batter_100 <- career %>% filter(AB == 100) %>% arrange(average) %>% dplyr::select(-playerID)
batter_100 %>% knitr::kable(booktabs = TRUE)

final_parameters %>% crossing(x = 0:45) %>%
  mutate(density = VGAM::dbetabinom.ab(x, 100, alpha, beta)) %>%
  ggplot(aes(x, density)) +
  geom_line(aes(color = cluster)) +
  geom_vline(aes(xintercept = H), data = batter_100, lty=2) +
  geom_text(aes(x=H, y=-0.022, label=name), data = batter_100, hjust = 1, vjust = 1, angle = 270) +
  labs(x="H (out of 100 at-bats)", y="Likelihood of this H out of 100 hits")


final_parameters %>% crossing(H = 1:40) %>% transmute(H, cluster, likelihood = VGAM::dbetabinom.ab(H, 100, alpha, beta)) %>%
  spread(cluster, likelihood) %>%
  mutate(probability_A = A / ( A + B )) %>%
  ggplot(aes(H, probability_A)) +
  geom_line() +
  geom_vline(aes(xintercept = H), data = batter_100, lty=2) +
  geom_text(aes(x=H, y=0, label=name), data = batter_100, hjust = 1, vjust = 1, angle = 270) +
  labs(x="H (out of 100 at-bats)", y="(Likelihood if pitcher) / (Likelihood if pitcher + Likelihood if not)")


career_likelihoods <- career %>% filter(AB > 20) %>% crossing(final_parameters) %>% mutate(likelihood = VGAM::dbetabinom.ab(H, AB, alpha, beta)) %>% group_by(playerID) %>%
  mutate(posterior = likelihood / sum(likelihood))
career_assignments <- career_likelihoods %>% top_n(1, posterior) %>% ungroup()

career_assignments %>% filter(posterior > 0.8) %>% count(isPitcher, cluster) %>% spread(cluster, n) %>% ungroup() %>%
  transmute("True category" = ifelse(isPitcher, "Pitcher", "Non-Pitcher"), A, B) %>% knitr::kable(booktabs = TRUE)


# -- Empirical Bayes shrinkage with a mixture model
batting_data <- career_likelihoods %>% ungroup() %>% filter(AB == 100) %>%
  mutate(name = paste0(name, " (", H, "/", AB, ")"), name = reorder(name, H), alpha1 = H + alpha, beta1 = AB - H + beta)

batting_data %>% crossing(x = seq(0, 0.4, 0.001)) %>%
  mutate(posterior_density = posterior * dbeta(x, alpha1, beta1)) %>%
  group_by(name, x) %>%
  summarize(posterior_density = sum(posterior_density)) %>%
  ggplot(aes(x, posterior_density, color=name)) +
  geom_line(show.legend = FALSE) +
  geom_vline(aes(xintercept = average), data = batting_data, lty=2) +
  facet_wrap(~ name) +
  labs(x="Batting average (actual average shown as dashed line)", y="Posterior density after updating")


eb_shrinkage <- career_likelihoods %>% mutate(shrunken_average = ( H + alpha ) / ( AB + alpha + beta )) %>%
  group_by(playerID) %>% summarize(shrunken_average = sum(posterior * shrunken_average))

library(forcats)
cluster_means <- final_parameters$alpha / (final_parameters$alpha + final_parameters$beta)
levs <- c("Raw batting average", "EB estimate", "EB estimate; mixture model")
lines <- data_frame(type=factor(c("EB estimate", rep("EB estimate; mixture model", 2)), levs),  value=c(fit$alpha / (fit$alpha + fit$beta), cluster_means))

eb_shrinkage %>%
  inner_join(career_assignments) %>% filter(AB > 50) %>%
  mutate(eb_estimate = (fit$alpha + H) / (fit$alpha + fit$beta + AB)) %>%
  gather(type, estimate, average, eb_estimate, shrunken_average) %>%
  mutate(type = forcats::fct_recode(type, "Raw batting average" = "average", "EB estimate" = "eb_estimate", "EB estimate; mixture model" = "shrunken_average"),
          type = factor(type, levels = levs)) %>%
  ggplot(aes(AB, estimate)) + geom_point(aes(color=cluster)) + geom_hline(aes(yintercept=value), lty=2, data=lines) +
  scale_x_log10() + facet_wrap(~type) + geom_abline(color="red") + labs(y="Estimated batting average", color="Assignment")




# -----------------------------------------------------------------------------
# Simulation
# -----------------------------------------------------------------------------
devtools::install_github("dgrtwo/ebbr")

library(knitr)
opts_chunk$set(message = FALSE, warning = FALSE)

library(scales)
library(ggplot2)
theme_set(theme_bw())

library(Lahman);  library(dplyr);  library(tidyr);  library(purrr)

# Grab career batting average of non-pitchers
# (allow players that have pitched <= 3 games, like Ty Cobb)
pitchers <- Pitching %>% group_by(playerID) %>% summarize(gamesPitched = sum(G)) %>% filter(gamesPitched > 3)
career <- Batting %>% filter(AB > 0) %>% anti_join(pitchers, by = "playerID") %>% group_by(playerID) %>% summarize(H = sum(H), AB = sum(AB))

library(ebbr)
prior <- career %>% ebb_fit_prior(H, AB)
prior

alpha0 <- tidy(prior)$alpha
beta0 <- tidy(prior)$beta

# for example, generating 10 values
rbeta(10, alpha0, beta0)
ggplot(career, aes(AB)) + geom_histogram() + scale_x_log10()

set.seed(2017)

career_sim <- career %>% mutate(p = rbeta(n(), alpha0, beta0), H = rbinom(n(), AB, p))
career_sim

career_sim_eb <- career_sim %>% add_ebb_estimate(H, AB)

career_sim_gathered <- career_sim_eb %>% rename(Shrunken = .fitted, Raw = .raw) %>% gather(type, estimate, Shrunken, Raw)
