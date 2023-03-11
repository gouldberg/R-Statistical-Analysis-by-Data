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
# binomial regression by aggregated data, by quadratic approximation
# ------------------------------------------------------------------------------
mod.base_qa <- map(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- a + bp*p_is_large + bv*v_is_large + ba*p_is_adult,
    a ~ dnorm(0, 10),
    c(bp, bv, ba) ~ dnorm(0, 5)
  ),
  data = d
)


precis(mod.base_qa, digits=3)



# ------------------------------------------------------------------------------
# binomial regression by aggregated data, by MCMC
# ------------------------------------------------------------------------------

mod.base_MCMC <- map2stan(mod.base_qa)

precis(mod.base_MCMC)



# ------------------------------------------------------------------------------
# compare algorithm:  quadratic approximation and MCMC
# ------------------------------------------------------------------------------
# bv:  small difference
par(mfrow=c(1,1))
plot(coeftab(mod.base_qa, mod.base_MCMC))
     


# ----------
pairs(mod.base_qa)

# bp and bv is skewed
pairs(mod.base_MCMC)


# While models' WAIC is comparable there is a difference in parameters estimation.
# Parameters `bp` and `bv` are negatively correlated and have long tails - both coefficients are not symmetric related to the mean.
# One of the possible causes of this phenomena is that for pairs (P=L,V=S, A={A,I}) probability of pirate to win is always equal to 1.
# It forces coefficient towards large values to shift logistic score closer to 1 (as in 10H1)



# ------------------------------------------------------------------------------
# plot the posterior predictions
#   - the predicted probability of success and its 89% interval for each row
#   - the predicted success count and its 89% interval
# ------------------------------------------------------------------------------

# it plots predicted and actual probablity with their HPDI intervals
par(mfrow=c(1,1))
postcheck(mod.base_MCMC, prob=0.89)



# ----------
# let's do it manually
d$lbl <- with(d, paste0(P,A,V))

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



# ----------
# case 8 has the worst predictions
dr <- post_dens_probs(mod.base_MCMC, d)



# ----------
dr <- post_dens_counts(mod.base_MCMC, dr)



# ----------
par(mfrow=c(1,1))
plot(dr$y, col='blue', pch=16,  xlab="case", xaxt="n" )

points(dr$y_pred)
for(i in 1:8) lines( c(i, i), c(dr$y_pred_low[i], dr$y_pred_high[i]) )
axis(1, at=1:8, labels=dr$lbl)


# From the visual exploration we can recognise a pattern,
# that interaction of adult pirate with small victim always gives an advantage to the pirate (for counts plot but not for probabilities)
# Let's check this hypothesis with the model


