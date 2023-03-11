setwd("//media//kswada//MyFiles//R//fish")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fish
# ------------------------------------------------------------------------------
data("Fish", package = "rethinking")

d <- Fish

dim(d)

str(d)



# ------------------------------------------------------------------------------
# base model:  zero-inflated poisson regression
# ------------------------------------------------------------------------------
mod.base <- map(
  alist(
    fish_caught ~ dzipois(p, lambda),
    logit(p) <-  ap,
    log(lambda) <- log(hours) + al,
    ap ~ dnorm(0,10),
    al ~ dnorm(0,10)
  ), data=d
)


precis(mod.base, digits = 3)

postcheck(mod.base, window=260)

logistic(-0.75) # probability of not fishing is 0.32

exp(-0.14) # avg number of caught fish per hour when fishing is 0.87 vs. mean(d$fish_caught/d$hours)=1.1

pairs(mod.base)



# ------------------------------------------------------------------------------
# zero-inflated poisson regression + main effect
# ------------------------------------------------------------------------------
mod.f1 <- map(
  alist(
    fish_caught ~ dzipois(p, lambda),
    logit(p) <-  ap + bp_c * camper + bp_p * persons + bp_nchd * child,
    log(lambda) <- log(hours) + al + bl_lb * livebait + bl_c * camper + bl_p * persons + bl_nchd * child,
    ap ~ dnorm(0,10),
    al ~ dnorm(0,10),
    c(bp_c, bp_p, bp_nchd) ~ dnorm(0,2),
    c(bl_lb, bl_c, bl_p, bl_nchd) ~ dnorm(0,2)
  ), data=d
)


# bp_c and bl_c overlops zero
precis(mod.f1)

postcheck(mod.f1, window=260)

pairs(mod.f1)



# ------------------------------------------------------------------------------
# zero-inflated poisson regression + main effect but no child
# ------------------------------------------------------------------------------

mod.no.child <- map(
  alist(
    fish_caught ~ dzipois(p, lambda),
    logit(p) <-  ap + bp_c * camper + bp_p * persons,
    logit(p) <-  ap + bp_p * persons,
    log(lambda) <- log(hours) + al + bl_lb * livebait + bl_c * camper + bl_p * persons,
    ap ~ dnorm(0,10),
    al ~ dnorm(0,10),
    c(bp_c, bp_p) ~ dnorm(0,2),
    c(bl_lb, bl_c, bl_p, bl_p) ~ dnorm(0,2)
  ), data=d
)




precis(mod.f1, digits = 3)
precis(mod.no.child, digits = 3)



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------
( cmp <- compare(mod.base, mod.f1, mod.no.child) )

plot(cmp)



# ------------------------------------------------------------------------------
# counterfactual prediction
# ------------------------------------------------------------------------------
model <- mod.f1

d.predict <- data.frame(
  hours=1,
  livebait=c(0,1,0,1),
  camper=c(0,0,1,1),
  persons=c(1,1,1,1),
  child=c(0,0,0,0)
)

lambda.sample <- link(model, data = d.predict)
lambda.avg <- apply(lambda.sample$lambda, 2, mean )
lambda.pi <- apply(lambda.sample$lambda, 2, PI )

p.avg <- apply(lambda.sample$p, 2, mean )
p.pi <- apply(lambda.sample$p, 2, PI )

count.sample <- sim(model, data = d.predict)
count.avg <- apply(count.sample, 2, mean )
count.pi <- apply(count.sample, 2, PI )

d.predict$lambda <- lambda.avg
d.predict$p <- p.avg
d.predict$cnt <- count.avg

d.predict

# Group with camper has more chances to start fishing, but lower number of caught fishes per hour, that's counterfactual. 
# I suspect that there is some correlation inside the model. 
# As expected, a group that uses livebait has larger expected number of caught fishes per hour.

