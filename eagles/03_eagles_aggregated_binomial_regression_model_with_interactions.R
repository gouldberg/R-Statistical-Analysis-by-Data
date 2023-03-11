setwd("//media//kswada//MyFiles//R//eagles")

packages <- c("dplyr", "rethinking", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eagles
# ------------------------------------------------------------------------------
data("eagles", package = "MASS")

d <- eagles

dim(d)

str(d)

d



# ----------
d$p_is_large <- ifelse(d$P=='L', 1, 0)
d$p_is_adult <- ifelse(d$A=='A', 1, 0)
d$v_is_large <- ifelse(d$V=='L', 1, 0)

d$prob <- d$y/d$n



# ------------------------------------------------------------------------------
# binomial regression by aggregated data incorporating interactions
# ------------------------------------------------------------------------------
# pirating eagle size(P) * adult(A)
mod_bpa <- map2stan(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- a + bp * p_is_large + bv * v_is_large + ba * p_is_adult + bpa * p_is_large * p_is_adult,
    a ~ dnorm(0, 10),
    c(bp, bv, ba, bpa) ~ dnorm(0, 5)
  ), data=d
)


precis(mod_bpa, digits=3)

pairs(mod_bpa)

compare(mod.base_MCMC, mod_bpa)



# ----------
# victim eagle size(V) * pirating eagle size(P)
mod_bva <- map2stan(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- a + bp * p_is_large + bv * v_is_large + ba * p_is_adult + bva * (1-v_is_large) * p_is_adult,
    a ~ dnorm(0, 10),
    c(bp, bv, ba, bva) ~ dnorm(0, 5)
  ), data=d
)


precis(mod_bva)

pairs(mod_bva)

compare(mod.base_MCMC, mod_bpa, mod_bva)



# ----------
# victim eagle size(V) * pirating eagle size(P)
# pirating eagle size(P) * adult(A)
mod_bvpa <- map2stan(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- a + bp * p_is_large + bv * v_is_large + ba * p_is_adult + bva * (1-v_is_large) * p_is_adult + bpa * p_is_large * p_is_adult,
    a ~ dnorm(0, 10),
    c(bp, bv, ba, bpa, bva) ~ dnorm(0, 5)
  ), data=d
)


precis(mod_bvpa)

pairs(mod_bvpa)



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------
( cmp <- compare(mod.base_MCMC, mod_bpa, mod_bva, mod_bvpa) )

plot(cmp)


# bpa model - all predictors with large-x-age interaction looks like most appropriate(smallest WAIC and reasonable set of parameters)


# ------------------------------------------------------------------------------
# posterior prediction
# ------------------------------------------------------------------------------
par(mfrow=c(1,1))
postcheck(mod_bpa, prob=0.89)



# ----------
# manually calculate
post_dens_probs <- function(model, d){
  prob.sample <- link(model)
  prob.mean <- apply(prob.sample, 2, mean)
  prob.pi <- apply(prob.sample, 2, PI, 0.89)
  
  dr <- d
  dr$p_pred <- prob.mean
  dr$p_pred_low <- prob.pi[1,]
  dr$p_pred_high <- prob.pi[2,]
  
  par(mfrow=c(4,2))
  for(i in 1:8){
    dens(prob.sample[,i]) #, xlim=c(0,1)
    abline(v=d$prob[i], col='red')
    abline(v=prob.mean[i], col='blue')
    abline(v=prob.pi[,i], col='blue', lty=2)
    mtext(paste('Probability for case#',i,dr$lbl[i]))
  }
  dr
}



# ----------
# plot counts
post_dens_counts <- function(model, d){
  cnt.sample <- sim(model)
  cnt.mean <- apply(cnt.sample, 2, mean)
  cnt.pi <- apply(cnt.sample, 2, PI, 0.89)
  
  dr <- d
  dr$y_pred <- cnt.mean
  dr$y_pred_low <- cnt.pi[1,]
  dr$y_pred_high <- cnt.pi[2,]
  dr
  
  par(mfrow=c(4,2))
  for(i in 1:8){
    dens(cnt.sample[,i]) 
    abline(v=d$y[i], col='red')
    abline(v=cnt.mean[i], col='blue')
    abline(v=cnt.pi[,i], col='blue', lty=2)
    mtext(paste('Count for case#',i,dr$lbl[i]))
  }
  dr
}


dr <- post_dens_probs(mod_bpa, d)

dr <- post_dens_counts(mod_bpa, dr)



# let's check parameters values
pairs(mod_bpa)
precis(mod_bpa)

# a - coefficient when all predictors are zero (SIS case)
logistic(c(-0.78, -2.45, 0.77))
# bp - change in odds if pirate is large
exp(6.6)
# bv - change in odds if victim is large
exp(-5.31)
# I think there is a issue with interpreting this coefficitents, as bp and bv are correlated, also there is an interaction
plot(coeftab(m10.3stan, m10.3bpa))
# from this comparison we can see, that adding interaction term increases bp and ba coefficients as we have negative bpa coefficent. Also as ba and bpa are negatively correlated they influence each other.


pd <- position_dodge(0.1) # move points .05 to the left and right
# check influence of the pirate is large varibale on the probability (join by to other variables)
dr$lablel_av <- paste0('v.size=',dr$V,' ','p.age=',dr$A)
ggplot(dr, aes(x=lablel_av, y=p_pred, color=P, group=P)) + 
  geom_point(size=3, position=pd) +
  geom_line( position=pd) + 
  geom_errorbar(aes(ymin=p_pred_low, ymax=p_pred_high), width=.1, position=pd)


# check influence of the pirate is adult varibale on the probability (join by to other variables)
dr$lablel_pv <- paste0('p.size=',dr$P,' ','v.size=',dr$V)
ggplot(dr, aes(x=lablel_pv, y=p_pred, color=A, group=A)) + 
  geom_point(size=3, position=pd) + 
  geom_line( position=pd) +
  geom_errorbar(aes(ymin=p_pred_low, ymax=p_pred_high), width=.1, position=pd)


# check influence of the victim is large varibale on the probability (join by to other variables)
dr$lablel_pa <- paste0('p.size=',dr$P,' ','p.age=',dr$A)
ggplot(dr, aes(x=lablel_pa, y=p_pred, color=V, group=V)) + 
  geom_point(size=3, position=pd) + 
  geom_line( position=pd) + 
  geom_errorbar(aes(ymin=p_pred_low, ymax=p_pred_high), width=.1, position=pd)


#####
dr.base <- post_dens_probs(m10.3stan, d)
dr.base$model = 'base'
dr.bpa <- post_dens_probs(m10.3bpa, d)
dr.bpa$model = 'bpa'

dr2 <- bind_rows(dr.base, dr.bpa)

dr <- dr2
dr$lablel_av <- paste0('v.size=',dr$V,' ','p.age=',dr$A)
ggplot(dr, aes(x=lablel_av, y=p_pred, color=paste(P,model), group=paste(P,model))) + 
  geom_point(size=3, position=pd) +
  geom_line(position=pd) + 
  geom_errorbar(aes(ymin=p_pred_low, ymax=p_pred_high), width=.1, position=pd) +
  geom_point(aes(x=lablel_av, y=prob), size=3, color='black')

# This image shows that bpa models better reflect fact, that move from pirate.age=A to pirate.age=I for small victims has bigger impact on probability due to pirate size-age interaction
# Still, the simplest interpreation is that being large and adult pirate gives additional bonus in the fights for food :)
