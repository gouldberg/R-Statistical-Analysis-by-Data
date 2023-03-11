
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
# Extracting selected results for mdodel M16.5
# ------------------------------------------------------------------------------


lme6.ml <- update(lme6, method = "ML")

lme6.23.ml <- update(lme6.23, method = "ML")



# ----------
# log-ML value
# Note that the number of degrees of freedom reported by lokLik() is equal to 8.
# It corresponds to the total number of the parameters in the model,
# i.e., four fixed-effects coefficients (beta),
# four variance-covariance parameters describing the diagonal matri D,
# one parameter (delta) related to the power variance function describing the diagonal matrix R
# and the scale parameter sigam

logLik(lme6.ml)




# ----------
# fixed effects estimates and their variance-covariance matrix
beta0 <- fixef(lme6.ml)

names(beta0)

names(beta0) <- abbreviate(names(beta0), minlength = 10)

beta0

vcovb <- vcov(lme6.ml)

vcovb



# ------------------------------------------------------------------------------
# contributions of individual subjecs to the log-likelihood for the model:  illustration purpose
# ------------------------------------------------------------------------------


library(nlmeU)



# ----------
# calculate the contribution of one subject in the data to the overall log-likelihood
# logLik, for first record

logLik1(lme6.ml, dt1 = dat[1,])




# ----------
dat$subject <- 1:nrow(dat)

lLik.i <- by(dat, dat$subject,
             FUN = function(dfi) logLik1(lme6.ml, dfi))



lLik.i <- as.vector(lLik.i)


# logLik(i) for the first 5 subjects
lLik.i[1:5]


sum(lLik.i)




# ----------
# plot of individual contributions to the log-likelihood


nx <- by(dat, dat$subject, nrow)

lLik.n <- lLik.i / as.vector(nx)


summary(lLik.n)


outL <- lLik.n < -15

lLik.n[outL]


subject.c <- 1:nrow(dat)

subject.x <- as.numeric(subject.c)

plot(lLik.n ~ subject.x, type = "h")

points(subject.x[outL], lLik.n[outL], type = "p", pch = 16)

text(subject.x[outL], lLik.n[outL], subject.c[outL])




# ------------------------------------------------------------------------------
# model fitted to a sequence of "leave-one-subject-out" (LOO) datasets
# ------------------------------------------------------------------------------

# creating the object lmeUall containing models

# here we use lme6.23.ml

lmeU <- function(cx){
  dfU <- subset(dat, subject != cx)
  
  update(lme6.ml, data = dfU)
  # update(lme6.23.ml, data = dfU)
}



# list with Leave-One-Subject-Out fits
dat$subject <- 1:nrow(dat)

subject.c <- 1:nrow(dat)

lmeUall <- lapply(subject.c, lmeU)


names(lmeUall) <- subject.c




# ----------
# exploring the contents of the lmeUall objects

names(lmeUall)[1:6]

dataU6 <- lmeUall[["6"]]$data


dim(dataU6)


unique(dataU6$subject)[1:6]




# ------------------------------------------------------------------------------
# Likelihood displacements for model
#   - twice the difference between the log-likelihood computed at a maximum and displaced values of estimated parameters
# ------------------------------------------------------------------------------

# caltulating of the likelihood displacements

lLik <- function(cx){
  
  lmeU <- lmeUall[[cx]]
  
  lLikU <- logLik(lmeU, REML = FALSE)
  
  df.s <- subset(dat, subject == cx)
  
  lLik.s <- logLik1(lmeU, df.s)
  
  return(lLikU + lLik.s)
}



# for all subject
lLikUall <- sapply(subject.c, lLik)


dif.2Lik <- 2 * (logLik(lme6) - lLikUall)


summary(dif.2Lik)




# ----------
# plot of the likelihood displacements with an indicatoin of outlying values

names(dif.2Lik) <- subject.c

outL <- dif.2Lik > 265


dif.2Lik[outL]


subject.f <- factor(subject.c, levels = subject.c)


myPanel <- function(x, y, ...){
  x1 <- as.numeric(x)
  
  panel.xyplot(x1, y, ...)
  
  ltext(x1[outL], y[outL], subject.c[outL])
}



dtp <- lattice::dotplot(dif.2Lik ~ subject.f, panel = myPanel, type = "h")

lxlims <- length(dtp$x.limits)

update(dtp, xlim = rep("", lxlims), grid = "h")




# -->
# the some subjects with the likelihood-displacement values larger than 265 are clearly identified.


