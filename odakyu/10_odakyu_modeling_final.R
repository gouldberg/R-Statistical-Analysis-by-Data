
setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\odakyu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  odakyu
# ------------------------------------------------------------------------------

dat <- read.csv("odakyu_dat.txt", header = TRUE, sep = "\t")


str(dat)


car::some(dat)



# ----------
dat$floor <- as.factor(dat$floor)

dat$ac <- as.factor(dat$ac)

dat$closet <- as.factor(dat$closet)

dat$flooring <- as.factor(dat$flooring)

dat$balcony <- as.factor(dat$balcony)



# ----------
# total minutes
dat$min_tot <- dat$bus_min + dat$walk_min + dat$train_min


# price including train fares for 20 days per month
dat$price_tot <- dat$price + dat$train_fare * 20


# price per area
dat$price_ave <- dat$price / dat$area
dat$price_tot_ave <- dat$price_tot / dat$area


# bus
dat <- dat %>% mutate(bus_flg = ifelse(bus_min > 0, 1, 0))
dat$bus_flg <- as.factor(dat$bus_flg)



# Tokyo and 23 wards
( station <- unique(dat$station) )

tokyo_flg <- c(rep(1,16), rep(0,7), rep(1,3))

ward23_flg <- c(rep(1,14), rep(0,12))


dat$tokyo_flg <- tokyo_flg[match(dat$station, station)]

dat$ward23_flg <- ward23_flg[match(dat$station, station)]

dat$tokyo_ward23_flg <- paste0(dat$tokyo_flg, dat$ward23_flg)

table(dat$tokyo_ward23_flg)




# ------------------------------------------------------------------------------
# no interaction model
# ------------------------------------------------------------------------------


# ----------
# all variable
mod0 <- lm(price ~ area + years + km + min_tot + shikirei + num_exchange + num_stations + train_fare +
             bus_flg + ac + closet + flooring + balcony + floor, data = dat)


summary(mod0)




# ----------
# inlcuding only non high GVIF terms
mod1 <- lm(price ~ area + years + shikirei + num_exchange + num_stations +
             bus_min + walk_min + train_min + ac + closet + flooring + balcony + floor, data = dat)


summary(mod1)


drop1(mod1, test = "Chi")



# -->
# excluding flooring and balcony

mod1 <- lm(price ~ area + years + shikirei + num_exchange + num_stations +
             bus_min + walk_min + train_min + ac + closet + floor, data = dat)


summary(mod1)




# ------------------------------------------------------------------------------
# stepwise selection with all 2-way interactions
# ------------------------------------------------------------------------------

library(MASS)

intercept_only_model <- lm(price ~ 1, data = dat)


total_model <- lm(price ~ (area + years + shikirei + num_exchange + num_stations +
             bus_min + walk_min + train_min + ac + closet + flooring + balcony + floor)^2, data = dat)


mod_step1 <- stepAIC(intercept_only_model, direction = 'both', scope = formula(total_model))


summary(mod_step1)


drop1(mod_step1, test = "Chi")



# ----------
# only select highly significant terms and remove non-significant terms iteratively
mod_inter <- lm(price ~ area + years + shikirei + num_exchange + num_stations +
                 bus_min + walk_min + train_min + ac + closet + balcony + floor +
                 area : (floor + num_exchange + shikirei + num_stations + closet) +
                 num_exchange : num_stations,
               data = dat)
               

summary(mod_inter)




# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------


AIC(mod1, mod_inter)



# -->
# mod_inter is better



# ------------------------------------------------------------------------------
# model dignostics
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(mod_inter)




car::residualPlots(mod_inter)




# ----------
lattice::bwplot(dat$station ~ resid(mod_inter))

lattice::bwplot(dat$tokyo_flg ~ resid(mod_inter))

lattice::bwplot(dat$tokyo_ward23_flg ~ resid(mod_inter))




# ------------------------------------------------------------------------------
# incorporate poly(.,2) terms
# ------------------------------------------------------------------------------

# but here, floor:area is not estimated
mod_inter_pol <- lm(price ~ poly(area,2) + years + shikirei + num_exchange + num_stations +
                  bus_min + walk_min + train_min + ac + closet + balcony + floor +
                  area : (floor + num_exchange + shikirei + num_stations + closet) +
                  num_exchange : num_stations,
                data = dat)


summary(mod_inter_pol)




# ------------------------------------------------------------------------------
# Separate lm fits per station by lmlist for mod_inter
# ------------------------------------------------------------------------------


# excluding num_stations, num_exchange, and train_min
# since if limited to one station, those term has only one level.

mod_inter_ns <- lm(price ~ area + years + shikirei +
                bus_min + walk_min + ac + closet + balcony + floor +
                area : (floor + shikirei + closet),
              data = dat)


summary(mod_inter_ns)




# ----------
# separate lm fits per station

library(nlme)

mod_inter_ns_lmlist <- lmList(price ~ area + years + shikirei +
                     bus_min + walk_min + ac + closet + balcony + floor +
                     area : (floor + shikirei + closet) | station,
                   data = dat)


print(mod_inter_ns_lmlist)



# ----------
# sorting by descending order of Intercept

par(mfrow = c(1,1))
car::densityPlot(coef(mod_inter_ns_lmlist)[,"(Intercept)"])

rownames(coef(mod_inter_ns_lmlist))[order((coef(mod_inter_ns_lmlist)[,"(Intercept)"]), decreasing = T)]




# ----------
# assess relationship between coefficients by station


# scatterplot of coefficient:  area  vs. Intercept

lattice::xyplot(coef(mod_inter_ns_lmlist)[,"area"] ~ coef(mod_inter_ns_lmlist)[,"(Intercept)"], cex = 1.5, pch = 20)


# -->
# suggesting that the larger the intercept, the smaller the coeff of area
# very clear negative correlation ...



# ----------
# scatterplot matrix of coefficient

graphics.off()

pairs(mod_inter_ns_lmlist)




# ------------------------------------------------------------------------------
# Try incorporating variance heterogeneity
# ------------------------------------------------------------------------------


# formula of mod_inter
form_mod_inter <- price ~ area + years + shikirei + num_exchange + num_stations +
                  bus_min + walk_min + train_min + ac + closet + balcony + floor +
                  area : (floor + num_exchange + shikirei + num_stations + closet) +
                  num_exchange : num_stations



# ----------
# default model (= mod_inter) by gls (REML estimation)

library(nlme)

gls0 <- gls(form_mod_inter, data = dat)


printCoefmat(coef(summary(gls0)))




# ----------
# define variance structure

# varFixed(~ area) allows for larger residual spread if area increases
# no extra parameters involved !!
vf1 <- varFixed(~ area)

vf2 <- varIdent(form = ~ 1 | station)

vf3 <- varPower(form = ~ area)

vf32 <- varPower(form = ~ area | station)

vf4 <- varExp(form = ~ area)

vf42 <- varExp(form = ~ area | station)

vf5 <- varComb(varIdent(form = ~ 1 | station), varExp(form = ~ area))




# ----------
# fit model
gls1 <- update(gls0, weights = vf1, data = dat)

gls2 <- update(gls0, weights = vf2, data = dat)

gls3 <- update(gls0, weights = vf3, data = dat)

gls32 <- update(gls0, weights = vf32, data = dat)

gls4 <- update(gls0, weights = vf4, data = dat)

gls42 <- update(gls0, weights = vf42, data = dat)

gls5 <- update(gls0, weights = vf5, data = dat)



# ----------
printCoefmat(coef(summary(gls1)))

printCoefmat(coef(summary(gls2)))

printCoefmat(coef(summary(gls3)))

printCoefmat(coef(summary(gls32)))

printCoefmat(coef(summary(gls4)))

printCoefmat(coef(summary(gls42)))

printCoefmat(coef(summary(gls5)))




# ----------
gls1$modelStruct$varStruct

gls2$modelStruct$varStruct

gls3$modelStruct$varStruct

gls32$modelStruct$varStruct

gls4$modelStruct$varStruct

gls42$modelStruct$varStruct

gls5$modelStruct$varStruct




# ----------
AIC(gls0, gls1, gls2, gls3, gls32, gls4, gls42, gls5)



# -->
# gls5 is best



# ----------
plot(gls0, which = c(1), col = dat$station, ylim = c(-5, 7))

plot(gls5, which = c(1), col = dat$station, ylim = c(-5, 7))

coplot(resid(gls5) ~ area | station, data = dat)



intervals(gls5, which = "var-cov")



# ----------
# Compare normalized residuals

graphics.off()

lattice::bwplot(resid(gls0, type = "n") ~ station, data = dat, ylim = c(-4, 7))

lattice::bwplot(resid(gls5, type = "n") ~ station, data = dat, ylim = c(-4, 7))



# -->
# outliers are less for gls5



# ----------
# Scale-location plots

plot(gls0, sqrt(abs(resid(., type = "n"))) ~ fitted(.), type = c("p", "smooth"))

plot(gls5, sqrt(abs(resid(., type = "n"))) ~ fitted(.), type = c("p", "smooth"))




# ------------------------------------------------------------------------------
# Try mixed effects model
# ------------------------------------------------------------------------------


library(nlme)

lmecont <- lmeControl(maxIter = 1000, msMaxIter = 1000, tolerance = 1e-6, niterEM = 100, msMaxEval = 500)



# station specific intercept

lme0 <- lme(form_mod_inter, random = ~ 1 | station, data = dat, control = lmecont)

printCoefmat(summary(lme0)$tTable, has.Pvalue = TRUE, P.value = TRUE)


summary(lme0)


# -->
# standard deviation of random interceps = 2449.206, unexpectedly small
# residual standard deviation, sigma, = 7357.951

# intra-class correlatioin is only 0.1
2449.206^2 / (2440.206^2 + 7357.951^2)



# ----------
# test for random intercept

anova.res <- anova(gls0, lme0)


# 0.5 * X(0)^2 + 0.5 * X(1)^2
anova.res[["p-value"]][2] / 2


# -->
# reject the null hypothesis that the variance of the distribution of random intercepts is equal to zero



library(RLRsim)

exactRLRT(lme0)




# ----------
# add random intercept and random area slopes
lme0_2 <- lme(form_mod_inter, random = ~ 1 + area | station, data = dat, control = lmecont)

summary(lme0_2)

# note that correlation of intercept and area is -0.971


printCoefmat(summary(lme0_2)$tTable, has.Pvalue = TRUE, P.value = TRUE)


an.res <- anova(lme0, lme0_2)
RLRT <- an.res[["L.Ratio"]][2]

0.5 * pchisq(RLRT, 1, lower.tail = FALSE) + 0.5 * pchisq(RLRT, 2, lower.tail = FALSE)




# ----------
AIC(lme0, lme0_2)



# -->
# much better for lme0_2




# ------------------------------------------------------------------------------
# Try mixed effects model + heterogeneous variance
# ------------------------------------------------------------------------------


lme1 <- update(lme0_2, weights = vf1, data = dat, control = lmecont)

lme2 <- update(lme0_2, weights = vf2, data = dat, control = lmecont)

lme3 <- update(lme0_2, weights = vf3, data = dat, control = lmecont)

lme32 <- update(lme0_2, weights = vf32, data = dat, control = lmecont)

lme4 <- update(lme0_2, weights = vf4, data = dat, control = lmecont)

lme42 <- update(lme0_2, weights = vf42, data = dat, control = lmecont)

lme5 <- update(lme0_2, weights = vf5, data = dat, control = lmecont)



# ----------
printCoefmat(summary(lme1)$tTable, has.Pvalue = TRUE, P.value = TRUE)

printCoefmat(summary(lme2)$tTable, has.Pvalue = TRUE, P.value = TRUE)

printCoefmat(summary(lme3)$tTable, has.Pvalue = TRUE, P.value = TRUE)

printCoefmat(summary(lme32)$tTable, has.Pvalue = TRUE, P.value = TRUE)

printCoefmat(summary(lme4)$tTable, has.Pvalue = TRUE, P.value = TRUE)

printCoefmat(summary(lme42)$tTable, has.Pvalue = TRUE, P.value = TRUE)

printCoefmat(summary(lme5)$tTable, has.Pvalue = TRUE, P.value = TRUE)




# ----------
AIC(gls0, gls1, gls2, gls3, gls32, gls4, gls42, gls5,
    lme0, lme0_2, lme1, lme2, lme3, lme32, lme4, lme42, lme5)



# -->
# lme5 is best




# ----------
lme5$modelStruct$varStruct




# ----------
# but try parsimounious model by removing non significant terms


form_mod_inter_modify <- price ~ area + shikirei +
  bus_min + walk_min + train_min + ac + balcony +
  area : shikirei



lme5_2 <- lme(form_mod_inter_modify, weights = vf5, random = ~ 1 + area | station, data = dat, control = lmecont)


printCoefmat(summary(lme5_2)$tTable, has.Pvalue = TRUE, P.value = TRUE)


lme5_2$modelStruct$varStruct



AIC(gls0, gls1, gls2, gls3, gls32, gls4, gls42, gls5,
    lme0, lme0_2, lme1, lme2, lme3, lme32, lme4, lme42, lme5, lme5_2)




# ------------------------------------------------------------------------------
# compare models
# ------------------------------------------------------------------------------


car::compareCoefs(gls0, gls5, lme5)




# ------------------------------------------------------------------------------
# Scale-location residuals plots
# ------------------------------------------------------------------------------


plot(gls0, sqrt(abs(resid(., type = "n"))) ~ fitted(.), type = c("p", "smooth"), ylim = c(-1, 3))

plot(gls5, sqrt(abs(resid(., type = "n"))) ~ fitted(.), type = c("p", "smooth"), ylim = c(-1, 3))


# ----------
plot(lme5, sqrt(abs(resid(., type = "n", level = 0))) ~ fitted(.), type = c("p", "smooth"), ylim = c(-1, 3))

plot(lme5, sqrt(abs(resid(., type = "n", level = 1))) ~ fitted(.), type = c("p", "smooth"), ylim = c(-1, 3))


# ----------
plot(lme5_2, sqrt(abs(resid(., type = "n", level = 0))) ~ fitted(.), type = c("p", "smooth"), ylim = c(-1, 3))

plot(lme5_2, sqrt(abs(resid(., type = "n", level = 1))) ~ fitted(.), type = c("p", "smooth"), ylim = c(-1, 3))




# ------------------------------------------------------------------------------
# Plots (and boxplots) of normalized residuals by station
# ------------------------------------------------------------------------------

library(lattice)

bwplot(resid(gls0, type = "n") ~ station, data = dat, ylim = c(-5,5))

bwplot(resid(gls5, type = "n") ~ station, data = dat, ylim = c(-5,5))


# ----------
# level = 0
bwplot(resid(lme5, type = "n", level = 0) ~ station, data = dat, ylim = c(-5,5))

# level = 1:  highest group (after including random intercept)
bwplot(resid(lme5, type = "n", level = 1) ~ station, data = dat, ylim = c(-5,5))


# ----------
bwplot(resid(lme5_2, type = "n", level = 0) ~ station, data = dat, ylim = c(-5,5))

bwplot(resid(lme5_2, type = "n", level = 1) ~ station, data = dat, ylim = c(-5,5))




# ------------------------------------------------------------------------------
# Normal Q-Q plots of Pearson residuals and predicted random effects
# ------------------------------------------------------------------------------

qqnorm(lme5, ~ resid(., type = "p", level = 1) | station)



# ------------------------------------------------------------------------------
# Observed and predicted values of price
# ------------------------------------------------------------------------------

# augPred() allows obtaining predicted values for the object specified as the first argument
# primary: one-sided formula indicating the covariate at which values the predicted values shoud be computed.
# by default, the arguments become equal to, respectively, the minimum and maximum of the values of the covariate.

# level = 0:1:  population level(Marginal(0)) and (subject level) subj.-spec.(1)

aug.Pred <- augPred(lme5,                  
          primary = ~area,
          level = 0:1)


head(aug.Pred)

table(aug.Pred$.type)




# ----------
graphics.off()


lattice::xyplot(price ~ area | .groups, data = aug.Pred %>% filter(.groups == "machida", .type == "original"))

lattice::xyplot(price ~ area | .type, data = aug.Pred %>% filter(.groups == "machida"))

lattice::xyplot(price ~ area, data = aug.Pred %>% filter(.groups == "chitose_funabashi"))


plot(aug.Pred, layout = c(4, 4, 2), col = c("black", "blue"), lty = c(1,3), lwd = c(3, 1),
     key = list(lines = list(col = c("black", "blue"), lty = c(1,3), lwd = c(3, 1)),
                text = list(c("Marginal", "station-specific")),
                columns = 2))




# ------------------------------------------------------------------------------
# Try additive model
# ------------------------------------------------------------------------------


library(mgcv)

# smoothing area and shikirei
mod1_gam <- gam(price ~ s(area) + years + s(shikirei) + num_exchange + num_stations +
             bus_min + walk_min + train_min + ac + closet + flooring + balcony + floor, data = dat)


summary(mod1_gam)




# ----------
# excluding closet and flooring
# but add smoothing by flooring  --> very close to lme5_2

mod1_gam <- gam(price ~ s(area, by = flooring) + years + s(shikirei) + num_exchange + num_stations +
                  bus_min + walk_min + train_min + ac + balcony + floor, data = dat)


summary(mod1_gam)


par(mfrow = c(2,2))
plot(mod1_gam)




# ----------
# heterogenous variannce
gls4_gam <- gamm(price ~ s(area, by = flooring) + years + s(shikirei) + num_exchange + num_stations +
                  bus_min + walk_min + train_min + ac + balcony + floor, 
                 weight = varExp(form = ~ area + shikirei), data = dat, control = lmecont)


# + random intercept model: without shikirei
lme1_gam <- gamm(price ~ s(area, by = flooring) + years + num_exchange + num_stations +
                   bus_min + walk_min + train_min + ac + balcony + floor, 
                 weight = varExp(form = ~ area), 
                 random = list(station = ~ 1), data = dat, control = lmecont)


# + random intercept + slope  model: adding area
# add shikirei to variance structure
lme2_gam <- gamm(price ~ s(area, by = flooring) + years + num_exchange + num_stations +
                   bus_min + walk_min + train_min + ac + balcony + floor, 
                 weight = varExp(form = ~ area + shikirei), 
                 random = list(station = ~ 1 + area), data = dat, control = lmecont)



summary(lme2_gam$gam)

anova(lme2_gam$gam)

summary(lme2_gam$lme)


lme2_gam$lme$modelStruct$varStruct



par(mfrow = c(2,2))

plot(lme1_gam$gam)

plot(lme2_gam$gam)





# ----------

bwplot(resid(lme5_2, type = "n", level = 1) ~ station, data = dat, ylim = c(-5,5))

bwplot(resid(gls4_gam$gam, type = "scaled.pearson") ~ station, data = dat, ylim = c(-5, 5))
bwplot(resid(gls4_gam$lme, type = "n") ~ station, data = dat, ylim = c(-5, 5))

bwplot(resid(lme1_gam$lme, type = "n") ~ station, data = dat, ylim = c(-5, 5))

bwplot(resid(lme2_gam$lme, type = "n") ~ station, data = dat, ylim = c(-5, 5))




# ----------
AIC(gls0, gls1, gls2, gls3, gls32, gls4, gls42, gls5,
    lme0, lme0_2, lme1, lme2, lme3, lme32, lme4, lme42, lme5, lme5_2,
    mod1_gam, lme1_gam$lme, lme2_gam$lme)



# -->
# lme2_gam is best




# ------------------------------------------------------------------------------
# Compare predicted values
# ------------------------------------------------------------------------------


pred_mod_inter <- predict(mod_inter)

pred_gls5 <- predict(gls5)

pred_lme5 <- predict(lme5)

pred_lme52 <- predict(lme5_2)

pred_lme2gam <- predict(lme2_gam$gam)

pred_lme2gam2 <- predict(lme2_gam$lme)


tmp <- data.frame(id = 1:nrow(dat), station = dat$station, 
                  price = dat$price,
                  pred_mod_inter = pred_mod_inter,
                  pred_gls5 = pred_gls5,
                  pred_lme5 = pred_lme5,
                  pred_lme52 = pred_lme52,
                  pred_lme2gam = pred_lme2gam,
                  pred_lme2gam2 = pred_lme2gam2)


tmp <- tmp %>% mutate(dif1 = round(pred_mod_inter - price, 0),
                      dif2 = round(pred_gls5 - price, 0),
                      dif3 = round(pred_lme5 - price, 0),
                      dif4 = round(pred_lme52 - price, 0),
                      dif5 = round(pred_lme2gam - price, 0),
                      dif6 = round(pred_lme2gam2 - price, 0))


# outliers
idx <- c(54, 166, 180, 253, 378, 405, 406, 640, 744, 761)


tmp %>% filter(id %in% idx)




# ----------
# mean absolute error

myfunc <- function(df) sum(abs(df))/nrow(dat)

apply(tmp[,c("dif1", "dif2", "dif3", "dif4", "dif5", "dif6")], 2, myfunc)



# -->
# model lme2_gam is the best




# ----------
car::densityPlot(tmp$dif3)

car::densityPlot(tmp$dif6)




# ----------
# lme5_2 is unstable (interval of area include 0)
intervals(lme5_2, which = "fixed")


# lme2_gam is stable
intervals(lme2_gam$lme, which = "fixed")




