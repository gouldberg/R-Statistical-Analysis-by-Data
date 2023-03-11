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



# ------------------------------------------------------------------------------
# poisson regression model with intercept only
# ------------------------------------------------------------------------------
mod1 <- map(
  alist(
    deaths ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(0, 5)
  ), data = d
)


precis(mod1, digits = 3)



# ------------------------------------------------------------------------------
# poisson regression model with variable femininity
# ------------------------------------------------------------------------------
mod2 <- map(
  alist(
    deaths ~ dpois(lambda),
    log(lambda) <- a + b_fem * femininity,
    a ~ dnorm(0, 5),
    b_fem ~ dnorm(0, 1)
  ), data = d
)


precis(mod2, digits = 3)

exp(0.07)


# coefficient is strongly positive
coef <- extract.samples(mod2)
dens(coef$b_fem)



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------

( cmp <- compare(mod1, mod2) )


par(mfrow = c(1,2))
plot(cmp)
plot(coeftab(mod1, mod2))


# -->
# According to WAIC comparison model with femininity is better and take all score, but the dispersion of the difference and SE of WAIC itself is huge. 
# The difference is less than standard error of the difference. 



# ------------------------------------------------------------------------------
# posterior validation check:  posterior distribution of deaths by each hurricane
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))
postcheck(mod2, window = 100)
abline(h=40, col='red')
abline(h=10, col='blue')
abline(h=mean(d$deaths), lty=2)


# Visual exploration shows that model significantly underestimates deaths for hurricanes with a number of deaths greater than 40
# and overestimates counts for cases with a number of death less than 10.
# Model is good at predicting number of deaths for hurricanes with target variable close to the average across the sample.



# ------------------------------------------------------------------------------
# counter factual plot
#   - Because model contains only a single variable, we can draw counter factual plot to illustrate dependency
# ------------------------------------------------------------------------------
d.predict <- data.frame(femininity=seq(1,11,0.1))
lambda.sample <- link(mod2, d.predict)
lambda.avg <- apply(lambda.sample, 2, mean )
lambda.pi <- apply(lambda.sample, 2, PI )


# predict actual counts
count.sample <- sim(mod2, data = d.predict)
count.avg <- apply(count.sample, 2, mean )
count.pi <- apply(count.sample, 2, PI )


# plot
plot(d$femininity, d$deaths, xlim=c(0,12), col='blue', pch=16)
lines(d.predict$femininity, lambda.avg)
shade(lambda.pi, d.predict$femininity) #gives very narrow shade

lines(d.predict$femininity, count.avg, col='red')
shade(count.pi, d.predict$femininity) #shade of counts predictions

# Summary: Intuitively there is some hidden variable that better explains deaths.  
# Visually relation induced by the model looks suspicious for me because it looks like being caused by several outliers.

