setwd("//media//kswada//MyFiles//R//reedfrogs")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  reedfrogs
#   - Experiments exploring Reed frog tadpole mortality.
#   - outcome is "surv"
# ------------------------------------------------------------------------------
data("reedfrogs", package = "rethinking")

d <- reedfrogs

dim(d)

str(d)


# ----------
d$tank <- 1:nrow(d)

table(d$pred)
d$pred.cat <- ifelse(d$pred == "no", 0, 1)

table(d$size)
d$size.cat <- ifelse(d$size == "small", 0, 1)



# ------------------------------------------------------------------------------
# varing intercept only
# ------------------------------------------------------------------------------
mod4_0 <- map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 1)
  ),
  data = d, iter = 5000, warmup = 1000, chains = 4
)


# plot(mod4_0)

precis(mod4_0, digits = 3, depth = 2)



# ------------------------------------------------------------------------------
# varing intercept + treatment variable:  predation effect only
# ------------------------------------------------------------------------------
mod4_pred <- map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank] + bp * pred.cat,
    a_tank[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 1),
    bp ~ dnorm(0, 1)
    ),
    data = d, iter = 5000, warmup = 1000, chains = 4
)


plot(mod4_pred)

precis(mod4_pred, digits = 3, depth = 2)

logistic(coef(mod4_pred)["bp"])


# -->
# predation effect is negative, almost same magnitude of mean intercept of tank

pairs(mod4_pred, pars=c("a", "sigma", "bp"))



# ------------------------------------------------------------------------------
# varing intercept + treatment variable:  size effect only
# ------------------------------------------------------------------------------
mod4_size <- map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank] + bs * size.cat,
    a_tank[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 1),
    bs ~ dnorm(0, 1)
  ),
  data = d, iter = 5000, warmup = 1000, chains = 4
)


plot(mod4_size)

precis(mod4_size, digits = 3, depth = 2)

logistic(coef(mod4_size)["bs"])


# -->
# main effect of size has large unsercenty

pairs(mod4_size, pars=c("a", "sigma", "bs"))


# --> 
# intercept "a" and "bs" is strongly correlated



# ------------------------------------------------------------------------------
# varing intercept + treatment variable:  predation and size effects
# ------------------------------------------------------------------------------
mod4_ps <- map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank] + bp * pred.cat + bs * size.cat,
    a_tank[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 1),
    bp ~ dnorm(0, 1),
    bs ~ dnorm(0, 1)
  ),
  data = d, iter = 5000, warmup = 1000, chains = 4
)


plot(mod4_ps)

precis(mod4_ps, digits = 3, depth = 2)

pairs(mod4_ps, pars=c("a", "sigma", "bs", "bp"))



# ------------------------------------------------------------------------------
# varing intercept + treatment variable:  predation and size effects + interaction
# ------------------------------------------------------------------------------
mod4_psi <- map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank] + bp * pred.cat + bs * size.cat + bps * pred.cat * size.cat,
    a_tank[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 1),
    bp ~ dnorm(0, 1),
    bs ~ dnorm(0, 1),
    bps ~ dnorm(0, 1)
  ),
  data = d, iter = 5000, warmup = 1000, chains = 4
)


precis(mod4_psi, digits = 3, depth = 2)



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------

compare(mod4_0, mod4_pred, mod4_size, mod4_ps, mod4_psi)


# --> 
# interaction model is best in terms of WAIC value but the difference is small
# all models containing “predator” are equally good


# ------------------------------------------------------------------------------
# shrinkage effect
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))
pr_ip   <- precis(mod4_pred, depth = 2)
pr_ips  <- precis(mod4_ps, depth = 2)
pr_ipsi <- precis(mod4_psi, depth = 2)

pred_idx <- d$pred.cat == 1


par(mfrow=c(1,2))

plot(y=d[pred_idx,]$propsurv, x=d[pred_idx,]$tank, ylim=c(0,1), pch=19, col="black")
lines(y=d[pred_idx,]$propsurv, x=d[pred_idx,]$tank, ylim=c(0,1), col="black")
abline(h=median(d$propsurv), col="black", lty=2)
abline(h=median(d[pred_idx,]$propsurv), col="orange", lty=2)
points(y=inv_logit(pr_ip@output$Mean[pred_idx]), x=d[pred_idx,]$tank, col="red")
lines(y=inv_logit(pr_ip@output$Mean[pred_idx]), x=d[pred_idx,]$tank, col="red")
points(y=inv_logit(pr_ips@output$Mean[pred_idx]), x=d[pred_idx,]$tank, col="green")
lines(y=inv_logit(pr_ips@output$Mean[pred_idx]), x=d[pred_idx,]$tank, col="green")
points(y=inv_logit(pr_ipsi@output$Mean[pred_idx]), x=d[pred_idx,]$tank, col="blue")
lines(y=inv_logit(pr_ipsi@output$Mean[pred_idx]), x=d[pred_idx,]$tank, col="blue")
text(20, 0, "no predation")

plot(y=d[! pred_idx,]$propsurv, x=d[! pred_idx,]$tank, ylim=c(0,1), pch=19, col="black")
lines(y=d[! pred_idx,]$propsurv, x=d[! pred_idx,]$tank, ylim=c(0,1), col="black")
abline(h=median(d$propsurv), col="black", lty=2)
abline(h=median(d[! pred_idx,]$propsurv), col="orange", lty=2)
points(y=inv_logit(pr_ip@output$Mean[1:48][! pred_idx]), x=d[! pred_idx,]$tank, col="red")
lines(y=inv_logit(pr_ip@output$Mean[1:48][! pred_idx]), x=d[! pred_idx,]$tank, col="red")
points(y=inv_logit(pr_ips@output$Mean[1:48][! pred_idx]), x=d[! pred_idx,]$tank, col="green")
lines(y=inv_logit(pr_ips@output$Mean[1:48][! pred_idx]), x=d[! pred_idx,]$tank, col="green")
points(y=inv_logit(pr_ipsi@output$Mean[1:48][! pred_idx]), x=d[! pred_idx,]$tank, col="blue")
lines(y=inv_logit(pr_ipsi@output$Mean[1:48][! pred_idx]), x=d[! pred_idx,]$tank, col="blue")
text(20, 0, "predation")

# -->
# blue line (both main effect (predation + size) and interactions) shrinks toward to median proportions across tanks.
# This shrinkage effect is large at big tank


# ------------------------------------------------------------------------------
# model inspection:  focus on inferred variation across tanks
# ------------------------------------------------------------------------------

# the distribution of each tank intercept is smaller when predation effect is included in the model compared to the model without predation variable
par(mfrow=c(2,2))
plot(coeftab(mod4_0))
plot(coeftab(mod4_pred))
plot(coeftab(mod4_ps))
plot(coeftab(mod4_psi))


# This is because the predation effect is negatively correlated tank intercept
# the correlation coef is -0.74
pairs(mod4_pred, pars=c("a", "sigma", "bp"))

post <- extract.samples(mod4_pred)
cor(post$a, post$bp)



# ----------
par(mfrow=c(1,1))

library(lattice)
histogram(~ surv | density + pred, data = d, breaks = seq(0, 35, 1), type = "density",
          xlab = "surv",
          panel = function(x, ...) {
            panel.histogram(x, ...)
            panel.mathdensity(dmath = dnorm, col = "black",
                              args = list(mean=mean(x), sd=sd(x)))
          } )



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------
post_ip   <- extract.samples(mod4_pred)
post_ips  <- extract.samples(mod4_ps)
post_ipsi <- extract.samples(mod4_psi)
post_is   <- extract.samples(mod4_size)
post_i    <- extract.samples(mod4_0)


par(mfrow=c(2,2))

dens(post_ip$bp, main="b_predator", xlim=c(-5,1))
dens(post_ips$bp, add=T, col="red")
dens(post_ipsi$bp, add=T, col="blue")

dens(post_is$bs, main="b_size",xlim=c(-3,3), ylim=c(0,1.2))
abline(v=0)
dens(post_ips$bs, add=T, col="red")
dens(post_ipsi$bs, add=T, col="blue")

dens(post_ipsi$bps, main="b_interaction", col="blue")
plot.new()
legend("topleft", legend=c("single variable","both variables","interaction"), fill=c("black","red","blue"))


# Predator has a strong negative effect on survival, while the effect of size is modest.
# All of the models containing predator fit equally well. 
# For the interaction model, the effect of predator seems divided over the single and interaction coefficients.
