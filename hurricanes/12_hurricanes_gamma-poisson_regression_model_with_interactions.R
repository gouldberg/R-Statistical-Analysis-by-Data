# https://github.com/jffist/statistical-rethinking-solutions/blob/master/ch11_hw.R

setwd("//media//kswada//MyFiles//R//hurricanes")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hurricanes
# ------------------------------------------------------------------------------
data("Hurricanes", package = "rethinking")

d <- Hurricanes

dim(d)

str(d)


# ----------
car::some(d)

summary(d$deaths)


normalise <- function(x) (x-mean(x))/sd(x)

d$damage_norm_c <- normalise(d$damage_norm)
d$femininity_c <- normalise(d$femininity)
d$min_pressure_c <- normalise(d$min_pressure)
d$log_damage_norm_c <- normalise(log(d$damage_norm))


# ------------------------------------------------------------------------------
# gamma-poisson regression model with damage_norm, min_pressure
# ------------------------------------------------------------------------------
# damage_norm:  Normlized estimate of damage in dollars
# min_pressure:  Minimum pressure, a measure of storm strength, low is stronger

mod4.c <- map(
  alist(
    deaths ~ dgampois(mu, theta),
    log(mu) ~ a + b_fem * femininity_c + b_dam * log_damage_norm_c + b_mp * min_pressure_c,
    a ~ dnorm(0, 10),
    b_fem ~ dnorm(0, 2),
    b_dam ~ dnorm(0, 2),
    b_mp ~ dnorm(0, 2),
    theta ~ dexp(1)
  ),
  data=d
)


# ----------
# in this model, b_fem overlaps zero 
precis(mod4.c, digits = 3)

postcheck(mod4.c, window = 100)

pairs(mod4.c)



# ----------
postcheck(mod3, window = 100)
postcheck(mod4.c, window = 100)

compare(mod1, mod2, mod3, mod4.c)



# ------------------------------------------------------------------------------
# gamma-poisson regression model with interactions, including feminitiy_c
# ------------------------------------------------------------------------------
mod5.c <- map(
  alist(
    deaths ~ dgampois(mu, theta),
    log(mu) ~ a + b_fem * femininity_c + b_dam * log_damage_norm_c + b_mp * min_pressure_c + b_fem_dam * femininity_c * log_damage_norm_c,
    a ~ dnorm(0, 10),
    b_fem ~ dnorm(0, 2),
    b_dam ~ dnorm(0, 2),
    b_mp ~ dnorm(0, 2),
    b_fem_dam ~ dnorm(0, 2),
    theta ~ dexp(1)
  ),
  data=d
)


# in this model, b_fem anb b_mp overlaps zero, but b_fem_dam is positive strangely
precis(mod5.c, digits = 3)

postcheck(mod5.c, window = 100)

pairs(mod5.c)



# ----------
mod6.c <- map(
  alist(
    deaths ~ dgampois(mu, theta),
    log(mu) ~ a + b_fem * femininity_c + b_dam * log_damage_norm_c + b_mp * min_pressure_c + b_fem_mp * femininity_c * min_pressure_c,
    c(b_fem,b_dam,b_mp,b_fem_mp) ~ dnorm(0, 2),
    a ~ dnorm(0, 10),
    theta ~ dexp(1)
  ),
  data=d
)


# in this model, b_fem overlaps zero, but b_fem_mp is negative
precis(mod6.c, digits = 3)



# ----------
# all interactions
mod7.c <- map(
  alist(
    deaths ~ dgampois(mu, theta),
    log(mu) ~ a + b_fem * femininity + b_dam * log_damage_norm_c + b_mp * min_pressure_c + b_dam_mp * log_damage_norm_c * min_pressure_c + b_mp_fem * min_pressure_c * femininity_c,
    a ~ dnorm(0, 10),
    b_fem ~ dnorm(0, 2),
    b_dam ~ dnorm(0, 2),
    b_mp ~ dnorm(0, 2),
    b_dam_mp ~ dnorm(0, 2),
    b_mp_fem ~ dnorm(0, 2),
    theta ~ dexp(1)
  ),
  data=d
)

precis(mod7.c, digits = 3)

postcheck(mod7.c, window = 100)


# ------------------------------------------------------------------------------
# gamma-poisson regression model with interactions, without femininity_c
# ------------------------------------------------------------------------------
mod8.c <- map(
  alist(
    deaths ~ dgampois(mu, theta),
    log(mu) ~ a + b_dam * log_damage_norm_c + b_mp * min_pressure_c + b_dam_mp * log_damage_norm_c * min_pressure_c,
    a ~ dnorm(0, 10),
    b_dam ~ dnorm(0, 2),
    b_mp ~ dnorm(0, 2),
    b_dam_mp ~ dnorm(0, 2),
    theta ~ dexp(1)
  ),
  data=d
)


precis(mod8.c, digits = 3)

postcheck(mod8.c, window = 100)



# mod7.c is best in terms of WAIC
compare(mod1, mod2, mod3, mod4.c, mod5.c, mod6.c, mod7.c, mod8.c)



# ------------------------------------------------------------------------------
# visualisation:  counterfactual plot
# ------------------------------------------------------------------------------
predict_lambda_counts <- function(model, data){
  lambda.sample <- link(model, data = data)
  lambda.avg <- apply(lambda.sample, 2, mean )
  lambda.pi <- apply(lambda.sample, 2, PI )
  
  count.sample <- sim(model, data = data)
  count.avg <- apply(count.sample, 2, mean )
  count.pi <- apply(count.sample, 2, PI )
  
  list(
    l_avg=lambda.avg,
    l_pi=lambda.pi,
    cnt_avg=count.avg,
    cnt_pi=count.pi
  )
}

plot_lambda_cnt <- function(x, pred, color_name) {
  lines(x, pred$l_avg, col=color_name)
  shade(pred$l_pi, x)
  lines(x, pred$cnt_avg, col=color_name, lty=2)
  shade(pred$cnt_pi, x) #shade of counts predictions
}



# ----------
# plot for avg min_pressure_c
model <- mod4.c

log_damage_seq <- seq(-3.1, 2, 0.1)
d.predict.male <- data.frame(
  femininity_c=-1.3,
  log_damage_norm_c=log_damage_seq,
  min_pressure_c=0
)
d.predict.female <- data.frame(
  femininity_c=1,
  log_damage_norm_c=log_damage_seq,
  min_pressure_c=0
)

p.male <- predict_lambda_counts(model, d.predict.male)
p.female <- predict_lambda_counts(model, d.predict.female)


idx.male <- d$female!=1
idx.female <- d$female==1
plot(d$log_damage_norm_c[idx.male], d$deaths[idx.male], xlim=range(log_damage_seq), pch=16, col='blue', ylim=range(d$deaths))
points(d$damage_norm_c[idx.female], d$deaths[idx.female], pch=16, col='red')
plot_lambda_cnt(d.predict.male$log_damage_norm_c, p.male, 'blue')
plot_lambda_cnt(d.predict.female$log_damage_norm_c, p.female, 'red')



# ----------
model <- mod8.c

log_damage_seq <- seq(-3.1, 2, 0.1)
qvar_name <- "min_pressure_c"
par(mfrow=c(1,3))
left_q <- 0
for(right_q in c(0.33, 0.66, 1)){
  # calculate qunatiles range
  qvar_vec <- d[[qvar_name]]
  qq <- quantile(qvar_vec, probs=c(left_q, right_q))
  lval = qq[1]
  rval = qq[2]
  # filter data subset
  if(right_q!=1){
    d.raw <- d[(qvar_vec >= lval) & (qvar_vec < rval),]
  } else {
    d.raw <- d[(qvar_vec >= lval) & (qvar_vec <= rval),]
  }
  # calc avg of the variable
  qvar_avg = mean(d.raw[[qvar_name]])
  # create data for prediction
  d.predict.male <- data.frame(
    femininity_c=-1.3,
    log_damage_norm_c=log_damage_seq,
    min_pressure_c=qvar_avg
  )
  d.predict.female <- data.frame(
    femininity_c=0.66,
    log_damage_norm_c=log_damage_seq,
    min_pressure_c=qvar_avg
  )
  ## predict
  p.male <- predict_lambda_counts(model, d.predict.male)
  p.female <- predict_lambda_counts(model, d.predict.female)
  ## plot
  idx.male <- d.raw$female!=1
  idx.female <- d.raw$female==1
  plot(d.raw$log_damage_norm_c[idx.male], d.raw$deaths[idx.male], xlim=range(log_damage_seq), pch=16, col='blue', ylim=range(d$deaths))
  points(d.raw$log_damage_norm_c[idx.female], d.raw$deaths[idx.female], pch=16, col='red')
  plot_lambda_cnt(d.predict.male$log_damage_norm_c, p.male, 'blue')
  plot_lambda_cnt(d.predict.female$log_damage_norm_c, p.female, 'red')
  mtext(sprintf("%s=%5.4f (%3.2f, %3.2f)", qvar_name, qvar_avg, left_q, right_q))
  ## end
  left_q <- right_q
}

