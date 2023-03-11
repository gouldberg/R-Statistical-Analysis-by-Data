
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


formula <- price ~ area + years + min_tot + shikirei + num_stations + bus_flg + ac + balcony + floor +
  ac : area + shikirei : train_fare + shikirei : area + shikirei : num_exchange


lme6.23 <- lme(formula, random = ~1 | station, 
               weights = varComb(varIdent(form = ~1 | station), varExp(form = ~ area)),
               data = dat,
               control = lmecont)



# ------------------------------------------------------------------------------
# Calculation of Cook's distances for model
#   - Cook's distance measure the scaled change induced by the exclusion of a particular observations
#     in the estimated parameter vector
#   - The larger the Cook's distance, the larger the influence of the i-th observation on the estimate of beta
# ------------------------------------------------------------------------------


# calculation of Cook's distances


betaUall <- sapply(lmeUall, fixef)


vb.inv <- solve(vcovb)

CookDfun <- function(betaU){
  dbetaU <- betaU - beta0
  cookD.value <- t(dbetaU) %*% vb.inv %*% dbetaU
}


CookD.num <- apply(betaUall, 2, CookDfun)


(n.fixeff <- length(beta0))



rankX <- n.fixeff


CookD <- CookD.num / rankX



# ----------
# plot of Cook's distances using traditional graphics.

outD <- CookD > 0.03


subject.c[outD]


plot(CookD ~ subject.c, ylab = "Cook's D", type = "h")

text(as.numeric(subject.c[outD]), CookD[outD], subject.c[outD])

points(subject.c[outD], CookD[outD])



