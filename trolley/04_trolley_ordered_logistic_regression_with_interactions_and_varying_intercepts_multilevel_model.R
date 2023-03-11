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



# ------------------------------------------------------------------------------
# Base model with interaction
# ------------------------------------------------------------------------------
# !!! IT TAKES TIME !!! ~ 9 mins to run it
mod.base <- map2stan( 
  alist(
    response ~ dordlogit(phi, cutpoints),
    phi <- bA * action + bI * intention + bC * contact + bAI * action * intention + bCI * contact * intention,
    c(bA, bI, bC, bAI, bCI) ~ dnorm(0, 10),
    cutpoints ~ dnorm(0, 10)
  ),
  data = d, iter = 5000, warmup = 1000, chains = 4, cores = 10,
  start = list(cutpoints=c(-1.9, -1.2, -0.7, 0.2, 0.9, 1.8)),
  types = list(cutpoints = "ordered") 
)


precis(mod.base, digits = 3, depth = 2)

set.seed(123)

(d.pred <- sample_n(d, 10))

sm <- sim(mod.base, data = d.pred)

simplehist(sm[,5])



# ------------------------------------------------------------------------------
# interactions + fixed effect model
# ------------------------------------------------------------------------------
# !!! IT TKAES TIME  ~25mins to run it

mod.fe <- map2stan(
  alist(
    response ~ dordlogit(phi, cutpoints),
    phi <- a_person[person_id] + bA * action + bI * intention + bC * contact + bAI * action * intention + bCI * contact * intention,
    c(bA, bI, bC, bAI, bCI) ~ dnorm(0, 10),
    cutpoints ~ dnorm(0, 10),
    a_person[person_id] ~ dnorm(0, 10)
  ),
  data = d, iter = 5000, warmup = 1000, chains = 4, cores = 10,
  start = list(cutpoints = c(-1.9, -1.2, -0.7, 0.2, 0.9, 1.8)),
  types = list(cutpoints = "ordered")
)


# enormous Rhat(>1.05), n_eff<100 we couldn't trust this model
precis(mod.fe, depth = 2)

post <- extract.samples(mod.fe)

str(post)

hist(post$a_person[,1])

compare(mod.base, mod.fe) #still it's much better according to WAIC



# ------------------------------------------------------------------------------
# interactions + varying intercept (multilevel) model
# ------------------------------------------------------------------------------
# !!! IT TAKES TIME  ~15min to run

mod.ve <- map2stan(
  alist(
    response ~ dordlogit(phi, cutpoints),
    phi <- a_person[person_id] + bA * action + bI * intention + bC * contact + bAI * action * intention + bCI * contact * intention,
    c(bA, bI, bC, bAI, bCI) ~ dnorm(0, 10),
    cutpoints ~ dnorm(0, 10),
    a_person[person_id] ~ dnorm(0, sigma_person),
    sigma_person ~ dcauchy(0, 2)
  ),
  data = d, iter = 5000, warmup = 1000, chains = 4, cores = 10,
  start = list(cutpoints = c(-1.9, -1.2, -0.7, 0.2, 0.9, 1.8)),
  types = list(cutpoints = "ordered")
)


precis(mod.ve, digits = 3, depth=2)



# ------------------------------------------------------------------------------
# model comparison
# ------------------------------------------------------------------------------
# model with varying intercepts (multilevel model) is significantly good
compare(mod.base, mod.fe, mod.ve)



# ------------------------------------------------------------------------------
# Plot a_person coefficient distribution
# ------------------------------------------------------------------------------
# I don't know how to compare uncertainty in predictions among seven ordered categories for 331 individuals.
# So let's plot a_person coefficients. I assume if these coefficients are far from zero it would mean that
# they have a significant influence on the result.

post <- extract.samples(mod.ve)
str(post)

a_pi <- apply(post$a_person, 2, PI)
a_avg <- apply(post$a_person, 2, mean)
d.res <- data.frame(person_id=1:length(unique(d$person_id)), a=a_avg, a_low=a_pi[1,], a_high=a_pi[2,])
head(d.res)
d.res$plabel <- reorder(levels(d$id), d.res$a)


d.res %>% ggplot(aes(plabel, a)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=a_low, ymax=a_high)) + 
  geom_hline(yintercept = 0, linetype='dashed') + 
  theme(axis.text.x = element_text(angle = 90))


# ----------
# This plots illustrate that clustering per person captures a lot of variances.

# It's interesting to check individuals with a_person approaching extreme values -4 or 8 (p==0 or 1 on logit scale)
head(arrange(d.res, a))
d %>% filter(id=='96;550') #a_person ~= -5, almost all responses are equal to 1
head(arrange(d.res, desc(a)))
d %>% filter(id=='97;644') #a_person ~= 6,  all responses are equal to 7
d %>% filter(id=='98;214') #a_person ~= 6,  all responses are equal to 7


