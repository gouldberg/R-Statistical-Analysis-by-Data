setwd("//media//kswada//MyFiles//R//trolley")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Trolley
# ------------------------------------------------------------------------------
data("Trolley", package = "rethinking")

d <- Trolley

dim(d)

str(d)

d$person_id <- as.integer(d$id)

d$story_id <- as.integer(d$story)



# ------------------------------------------------------------------------------
# Probability of response per gender for questions that involves contact
# ------------------------------------------------------------------------------
d %>% filter(contact==1) %>% ggplot( aes(x=as.factor(response), group=as.factor(male), fill=as.factor(male))) + 
  geom_bar(aes(y=..prop..), position = "dodge") + 
  ggtitle("Probabiliy of response per gender for questions that involves contact")

# We are interested in checking how gender influences decision.
# We expect that women have more responses that will qualify story as immoral when contact is included.



# ------------------------------------------------------------------------------
# One hypothesis from developmental psychology, usually attributed to Carol Gilligan, proposes that women and men have different average tendencies in moral reasoning.
# The notion is that women are more concerned with care (avoiding harm), while men are more concerned with justice and rights.
#
# --> Evaluate this hypothesis, supposing that contact provides a proxy for physical harm.
# Are women more or less bothered by contact than are men ?
# ------------------------------------------------------------------------------
# best model
mod.base <- map( 
  alist(
    response ~ dordlogit(phi, c(a1,a2,a3,a4,a5,a6) ),
    phi <- bA * action + bI * intention + bC * contact + bAI * action * intention + bCI * contact * intention,
    c(bA,bI,bC,bAI,bCI) ~ dnorm(0,10),
    c(a1,a2,a3,a4,a5,a6) ~ dnorm(0,10)
  ),
  data=d,
  start=list(a1=-1.9,a2=-1.2,a3=-0.7,a4=0.2,a5=0.9,a6=1.8) )


precis(mod.base, digits = 3)



# ----------
# model that contains gender and gender vs contact interaction
mod.f <- map( 
  alist(
    response ~ dordlogit(phi , c(a1,a2,a3,a4,a5,a6) ) ,
    phi <- bA * action + bI * intention + bC * contact + bAI * action * intention + bCI * contact * intention + bF * (1-male) + bFC * (1-male) * contact,
    c(bA,bI,bC,bAI,bCI,bFC,bF) ~ dnorm(0,10),
    c(a1,a2,a3,a4,a5,a6) ~ dnorm(0,10)
  ),
  data=d,
  start=list(a1=-1.9,a2=-1.2,a3=-0.7,a4=0.2,a5=0.9,a6=1.8) 
)


# it seems that
#  - gender main effect is strongly negative
#  - but bFC is positive
precis(mod.f, digits = 3)



# ----------
# model that contains only gender vs contact interaction
mod.fc <- map( 
  alist(
    response ~ dordlogit(phi , c(a1,a2,a3,a4,a5,a6) ) ,
    phi <- bA * action + bI * intention + bC * contact + bAI * action * intention + bCI * contact * intention + bFC * (1-male) * contact,
    c(bA,bI,bC,bAI,bCI,bFC) ~ dnorm(0,10),
    c(a1,a2,a3,a4,a5,a6) ~ dnorm(0,10)
  ) ,
  data=d ,
  start=list(a1=-1.9,a2=-1.2,a3=-0.7,a4=0.2,a5=0.9,a6=1.8) 
)


# it seems that
#  - bFC is negative
precis(mod.fc, digits = 3)



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------
( cmp <- compare(mod.base, mod.f, mod.fc) )
plot(cmp)


plot(coeftab(mod.base, mod.f, mod.fc))

# According to WAIC comparison model with gender and gender-contact is significantly better and takes all weights.
# Sign of coefficient near gender-contact interaction term changes across models, 
# but it's correlated with the coefficient for gender variable, so let's examine changes in posterior distribution of predictions instead of coefficients



# ------------------------------------------------------------------------------
# visualisation of distrubution shift due to gender variable
# ------------------------------------------------------------------------------
post <- extract.samples( mod.f )
str(post)

plot(1, 1, type="n", xlab="gender==female", ylab="probability", xlim=c(0,1) , ylim=c(0,1) , xaxp=c(0,1,1) , yaxp=c(0,1,2) )
abline(h=c(0,1), lty=2, col='blue')



kI <- 0 # value of intention
kA <- 0 # value for action 
kC <- 1 # value for contact
kF <- c(0,1)# values of gender==female flag to calculate over
for ( s in 1:100 ) {
  p <- post[s,]
  ak <- as.numeric(p[1:6])
  phi <- p$bA*kA + p$bI*kI + p$bC*kC + p$bAI*kA*kI + p$bCI*kC*kI + p$bF*kF + p$bFC*kF*kC
  pk <- pordlogit( 1:6 , a=ak , phi=phi )
  for ( i in 1:6 )
    lines( kF , pk[,i] , col=col.alpha(rangi2,0.1) )
}
mtext( concat( "action=",kA,", contact=",kC,", intention=",kI ) )



# calculate phi for all posterior samples for male
kF <- 0
post$phi_m <- with(post, bA*kA + bI*kI + bC*kC + bAI*kA*kI + bCI*kC*kI + bF*kF + bFC*kF*kC)
# calculate phi for all posterior samples for female
kF <- 1
post$phi_f <- with(post, bA*kA + bI*kI + bC*kC + bAI*kA*kI + bCI*kC*kI + bF*kF + bFC*kF*kC)

n <- nrow(post)
a.mx = data.matrix(select(post, a1:a6))
# calculate mean cumulative and per response probabilities from all posterior sample


# male
p_m <- sapply(1:n, function(idx) pordlogit(1:6 , a=a.mx[idx,] , phi=post$phi_m[idx] )  )
p_m <- apply(p_m, 1, mean)
p_m <- c(p_m,1)
cp_m <- p_m
p_m <- p_m-c(0,p_m)[1:7]
# female
p_f <- sapply(1:n, function(idx) pordlogit(1:6 , a=a.mx[idx,] , phi=post$phi_f[idx] )  )
p_f <- apply(p_f, 1, mean)
p_f <- c(p_f,1)
cp_f <- p_f
p_f <- p_f-c(0,p_f)[1:7]
# add values to the plot
text(1, cp_f-0.04, sprintf("%3.2f",p_f))
text(0, cp_m-0.04, sprintf("%3.2f",p_m))

# From the plot we see that females tend to have a larger proportion of responses equal to 1, 2 or 3 and smaller proportion of responses equal to 5 or 6. 
# So it looks that data support the hypothesis.

