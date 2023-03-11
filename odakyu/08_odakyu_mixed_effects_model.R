
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




# ----------
mod0 <- lm(price ~ area + years + km + min_tot + shikirei + num_exchange + num_stations + train_fare +
             bus_flg + ac + closet + flooring + balcony + floor, data = dat)


mod1 <- lm(price ~ area + years + km + min_tot + shikirei + num_exchange + num_stations +
             bus_flg + ac + closet + balcony + floor + 
             closet : area + balcony : area, data = dat)

mod_step1

mod4 <- lm(price ~ area + years + min_tot + shikirei + num_exchange + num_stations +
             bus_flg + ac + closet + balcony + floor + 
             closet : area + balcony : area + ac : area + floor : area +
             num_exchange : area + num_exchange : num_stations + shikirei : train_fare + 
             num_exchange : min_tot + num_exchange + balcony : years + shikirei : area + shikirei : num_exchange, data = dat)



mod5 <- lm(price ~ area + years + min_tot + shikirei + num_exchange + num_stations +
             bus_flg + ac + closet + balcony + floor + 
             closet : area + ac : area + floor : area +
             num_exchange : area + num_exchange : num_stations + shikirei : train_fare + 
             num_exchange : min_tot + num_exchange + shikirei : area + shikirei : num_exchange, data = dat)



# ------------------------------------------------------------------------------
# Random intercepts and homogeneous residual variance
# ------------------------------------------------------------------------------

# station-specific intercepts

mod5.form <- formula(price ~ area + years + min_tot + shikirei + num_exchange + num_stations +
                       bus_flg + ac + closet + balcony + floor + 
                       closet : area + ac : area + floor : area +
                       num_exchange : area + num_exchange : num_stations + shikirei : train_fare + 
                       num_exchange : min_tot + num_exchange + shikirei : area + shikirei : num_exchange)



library(nlme)

lmecont <- lmeControl(maxIter = 100, msMaxIter = 100, tolerance = 1e-6, niterEM = 25, msMaxEval = 200)

lme6 <- lme(mod5.form, random = ~ 1 | station, data = dat, control = lmecont)




# ----------
# fixed effects table
# Note that t-stats and p-values for the fixed-effects coefficients are for the marginal-approach tests.

printCoefmat(summary(lme6)$tTable, has.Pvalue = TRUE, P.value = TRUE)



# ----------
summary(lme6)



# -->
# standard deviation of random interceps = 2398.37
# residual standard deviation, sigma, is 7219.897



# lme6.2 <- lme(mod5.form, random = ~ 1 | station/balcony, data = dat)

# AIC(lme6, lme6.2)




# ------------------------------------------------------------------------------
# Test for random intercept
# ------------------------------------------------------------------------------


anova(gls5, lme6)



# -->
# reject the null hypothesis that the variance of the distribution of random intercepts is equal to zero




# ------------------------------------------------------------------------------
# Test for random intercept:  alternative
# ------------------------------------------------------------------------------

library(RLRsim)    


# m:  alternative model
# we test a random effect in model lme6.2,
# which contains only a single random effect, we use the abbreviated form of the function call,
# with m as the only argument.

exactRLRT(m = lme6)




# ------------------------------------------------------------------------------
# Random intercepts and varIdent residual variance function
# ------------------------------------------------------------------------------

# add "area" variance structure
lme6.2 <- update(lme6, weights = varExp(form = ~ area), data = dat, control = lmecont)

printCoefmat(summary(lme6.2)$tTable, has.Pvalue = TRUE, P.value = TRUE)


formula <- price ~ area + years + min_tot + shikirei + num_stations + bus_flg + ac + balcony + floor +
  ac : area + shikirei : train_fare + shikirei : area + shikirei : num_exchange


lme6.2 <- lme(formula, random = ~1 | station, weights = varExp(form = ~ area), data = dat, control = lmecont)

printCoefmat(summary(lme6.2)$tTable, has.Pvalue = TRUE, P.value = TRUE)


AIC(gls5, gls10, lme6, lme6.2)



# ----------
# add "min_tot" variance structure
lme6.22 <- update(lme6, weights = varExp(form = ~ min_tot), data = dat, control = lmecont)

printCoefmat(summary(lme6.22)$tTable, has.Pvalue = TRUE, P.value = TRUE)

AIC(gls5, gls10, lme6, lme6.2, lme6.22)




# ----------
# add "varComb" but now only significant terms
lme6.23 <- update(lme6,
               weights = varComb(varIdent(form = ~1 | station), varExp(form = ~ area)),
               data = dat,
               control = lmecont)

printCoefmat(summary(lme6.23)$tTable, has.Pvalue = TRUE, P.value = TRUE)


formula <- price ~ area + years + min_tot + shikirei + num_stations + bus_flg + ac + balcony + floor +
  ac : area + shikirei : train_fare + shikirei : area + shikirei : num_exchange


lme6.23 <- lme(formula, random = ~1 | station, 
               weights = varComb(varIdent(form = ~1 | station), varExp(form = ~ area)),
               data = dat,
               control = lmecont)

printCoefmat(summary(lme6.23)$tTable, has.Pvalue = TRUE, P.value = TRUE)



AIC(gls5, gls10, lme6, lme6.2, lme6.22, lme6.23)



# -->
# best is lme6.23



# ----------
summary(lme6.23)


# -->
# standard deviation of random intercept = 2705.476
# residual standard deviation, sigma, is 1716.904 (smaller than lme6)



# variance function structure
lme6.23$modelStruct$varStruct





# ------------------------------------------------------------------------------
# Random intercepts and slopes
# ------------------------------------------------------------------------------


lme6.3 <- update(lme6.23, random = ~ 1 + area | station, data = dat, control = lmecont)


printCoefmat(summary(lme6.3)$tTable, has.Pvalue = TRUE, P.value = TRUE)


# only significant terms
formula <- price ~ area + years + min_tot + shikirei + num_stations + bus_flg + ac + balcony +
  ac : area + shikirei : train_fare + shikirei : area + shikirei : num_exchange + num_stations : num_exchange


lme6.3 <- lme(formula, random = ~area | station, 
               weights = varComb(varIdent(form = ~1 | station), varExp(form = ~ area)),
               data = dat,
               control = lmecont)


AIC(gls5, gls10, lme6, lme6.2, lme6.22, lme6.23, lme6.3)



# -->
# still best is lme6.23





# ------------------------------------------------------------------------------
# compare models
# ------------------------------------------------------------------------------


car::compareCoefs(gls5, gls10, lme6.23)




# ------------------------------------------------------------------------------
# Scale-location residuals plots
# ------------------------------------------------------------------------------


plot(rstandard(mod5) ~ fitted(mod5), ylim = c(-5, 5))
abline(h = 0)

plot(gls5, sqrt(abs(resid(., type = "pearson"))) ~ fitted(.), type = c("p", "smooth"))

plot(gls10, sqrt(abs(resid(., type = "pearson"))) ~ fitted(.), type = c("p", "smooth"))

plot(lme6.23, sqrt(abs(resid(., type = "pearson", level = 0))) ~ fitted(.), type = c("p", "smooth"))

plot(lme6.23, sqrt(abs(resid(., type = "pearson", level = 1))) ~ fitted(.), type = c("p", "smooth"))




# ------------------------------------------------------------------------------
# Plots (and boxplots) of Pearson residuals per station
# ------------------------------------------------------------------------------


# id = 0.05:  label the residuals larger, in absolute value,
# than the 97.5th percential of the standard normal distribution

plot(lme6.23, resid(., type = "pearson", level = 0) ~ area | station)

plot(lme6.32, resid(., type = "pearson", level = 1) ~ area | station)


bwplot(resid(gls5, type = "p") ~ station, 
       #       panel = panle.bwxplot2,
       data = dat, ylim = c(-5,5))


bwplot(resid(gls10, type = "p") ~ station, 
       #       panel = panle.bwxplot2,
       data = dat, ylim = c(-5,5))



# ----------
# level = 1:  highest group (after including random intercept)

bwplot(resid(lme6.23, type = "p", level = 1) ~ station, 
       #       panel = panle.bwxplot2,
       data = dat, ylim = c(-5,5))


# level = 0

bwplot(resid(lme6.23, type = "p", level = 0) ~ station, 
       #       panel = panle.bwxplot2,
       data = dat, ylim = c(-5,5))




# ------------------------------------------------------------------------------
# Normal Q-Q plots of Pearson residuals and predicted random effects
# ------------------------------------------------------------------------------


qqnorm(lme6.23, ~ resid(., type = "p", level = 1) | station)

qqnorm(lme6.23, ~ ranef(.) | station)




# ------------------------------------------------------------------------------
# Outlying conditional Pearson residuals for model
# ------------------------------------------------------------------------------

id <- 0.05


outliers.idx <- 
  within(dat,
         {
           resid.p <- resid(lme6.2, type = "pearson")
           idx <- abs(resid.p) > -qnorm(id/2)
         })


outliers  <- subset(outliers.idx, idx)


row.names(outliers)


outliers


table(outliers$station)




# ------------------------------------------------------------------------------
# Observed and predicted values of price
# ------------------------------------------------------------------------------



# augPred() allows obtaining predicted values for the object specified as the first argument
# primary: one-sided formula indicating the covariate at which values the predicted values shoud be computed.
# by default, the arguments become equal to, respectively, the minimum and maximum of the values of the covariate.

# level = 0:1:  population level(Marginal(0)) and (subject level) subj.-spec.(1)

aug.Pred <-
  augPred(lme6.23,                             
          primary = ~area,
#          primary = ~min_tot,
#          primary = ~shikirei,
          level = 0:1)


aug.Pred <-
  augPred(lme6.32,                             
          primary = ~area,
          #          primary = ~min_tot,
          #          primary = ~shikirei,
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

