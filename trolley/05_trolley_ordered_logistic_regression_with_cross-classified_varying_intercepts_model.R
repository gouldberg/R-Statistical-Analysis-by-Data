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
# Cross-classified varying intercepts model with both id and story
# ------------------------------------------------------------------------------
# !!! IT TAKES TIME !!! ~ XX mins to run it

mod.id.story.ve <- map2stan(
  alist(
    response ~ dordlogit(phi, cutpoints),
    phi <- a_story[story_id] + a_person[person_id] + bA * action + bI * intention + bC * contact + bAI * action * intention + bCI * contact * intention,
    c(bA, bI, bC, bAI, bCI) ~ dnorm(0, 10),
    cutpoints ~ dnorm(0, 10),
    a_person[person_id] ~ dnorm(0, sigma_person),
    a_story[story_id] ~ dnorm(0, sigma_story),
    sigma_person ~ dcauchy(0, 2),
    sigma_story ~ dcauchy(0, 2)
  ) ,
  data = d, iter = 5000, warmup = 1000, chains = 4, cores = 10,
  start = list(cutpoints = c(-1.9, -1.2, -0.7, 0.2, 0.9, 1.8)),
  types = list(cutpoints = "ordered")
)


precis(mod.id.story.ve, digits = 3, depth = 2)


# ----------
# model with cluster per story looks better
( cmp <- compare(mod.ve, mod.id.story.ve) ) 
plot(cmp)



# ------------------------------------------------------------------------------
# plot the distribution of a_person
# ------------------------------------------------------------------------------
post <- extract.samples(mod.id.story.ve)
str(post)

a_pi <- apply(post$a_person, 2, PI)
a_avg <- apply(post$a_person, 2, mean)
d.res2 <- data.frame(d.res) %>% arrange(person_id) %>% mutate(a=a_avg, a_low=a_pi[1,], a_high=a_pi[2,], model='person_id+story')

head(d.res2)
d.res$model='person_id'
d.m.res <- bind_rows(d.res, d.res2)


pd <- position_dodge(0.2)

# plot first 50 a_person
d.m.res %>% arrange(person_id) %>% head(100) %>% ggplot(aes(plabel, a, color=model, fill=model)) + 
  geom_point(size=2, position = pd, pch=21, colour='black') + 
  geom_errorbar(aes(ymin=a_low, ymax=a_high), position = pd) + 
  geom_hline(yintercept = 0, linetype='dashed') + 
  theme(axis.text.x = element_text(angle = 90,  vjust=0.5))


# Quite interesting that values for a_person intercepts stayed almost the same.
# Only for extreme cases, they shifted back from the mean, as, I assume, variance was partially explained by the story parameters.
# let's check story intercepts
s_pi <- apply(post$a_story, 2, PI)
s_avg <- apply(post$a_story, 2, mean)
d.s.res <- data.frame(story_id=1:length(unique(d$story_id)), a=s_avg, a_low=s_pi[1,], a_high=s_pi[2,])
head(d.s.res)
d.s.res$slabel <- reorder(levels(d$story), d.s.res$a)
d.s.res
d.s.res %>% ggplot(aes(slabel, a)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=a_low, ymax=a_high)) + 
  geom_hline(yintercept = 0, linetype='dashed') + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Avg value and 89 percentile interval of varying intercept per story")

# There are stories that tends to shift decision towards 1 (pon) or towards 7 (aqu) significantly.


# Plot probability per response level for single case and unique individual
model_name <- 'mod.id.story.ve'
model <-  mod.id.story.ve

pid <- '96;474' #'96;474' #'96;512'
case_name <- 'cfaqu' #cfaqu, #ikpon
(d.pred <- d %>% filter(id==pid, case==case_name)) 
post <- extract.samples(model)


plot(1,1,type='n',xlim=c(1,7), ylim=c(0,1.0), xlab="response", ylab="probability")
n_samples <- 1000
mx <- matrix(0, nrow=n_samples, ncol=7)

for(i in 1:n_samples) {
  ak <- post$cutpoints[i,]
  phi <- 0
  if('a_story' %in% names(post)) phi <- phi + post$a_story[i, d.pred$story_id]
  if('a_person' %in% names(post)) phi <- phi + post$a_person[i, d.pred$person_id]
  phi <- phi +
    post$bA[i]*d.pred$action + 
    post$bI[i]*d.pred$intention + 
    post$bC[i]*d.pred$contact + 
    post$bAI[i]*d.pred$action*d.pred$intention + 
    post$bCI[i]*d.pred$contact*d.pred$intention
  pk <- pordlogit( 1:6 , a=ak , phi=phi )
  levels_prob <- c(pk,1) - c(0,pk)
  if(i%%5==0) lines(levels_prob, col=col.alpha(rangi2,0.1))
  mx[i,] <- levels_prob
}

lvl_avg <- apply(mx, 2, mean)
lvl_pi <- apply(mx, 2, PI)
points(1:7, lvl_avg)
for(i in 1:7) lines(c(i,i), lvl_pi[,i])
abline(v=d.pred$response+0.01, col='red', lty=2)
mtext(sprintf("Probability per level for id=%s case=%s | model:%s", pid, case_name, model_name))
