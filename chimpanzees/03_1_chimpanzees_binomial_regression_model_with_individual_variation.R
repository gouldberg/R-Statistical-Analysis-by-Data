setwd("//media//kswada//MyFiles//R//chimpanzees")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  prosocial chimpanzees
# ------------------------------------------------------------------------------
data("chimpanzees", package = "rethinking")

d <- chimpanzees

dim(d)

str(d)



# ------------------------------------------------------------------------------
# binomial regression:  modeling individual variation
# ------------------------------------------------------------------------------
# Four of the individuals tend to pull the right-hand lever, across all treatments.
# Three individuals tend to pull the left across all treatments.
# One individual, actor number 2, always pulled the left-hand lever, regardless of treatment.

# clean NAs from the data
d2 <- d
d2$recipient <- NULL


unique(d$actor)


# Think of handedness here as a maskng variable. Estimate handedness as a distinct intercept for each individual
mod4 <- map2stan(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + (bp + bpC * condition) * prosoc_left,
    a[actor] ~ dnorm(0, 10),
    bp ~ dnorm(0, 10),
    bpC ~ dnorm(0, 10)
  ),
  data = d2, chains = 2, iter = 2500, warmup = 500
)



# Note that these values are on the scale of log-oods
precis(mod4, digits=3, depth = 2)


pairs(mod4)


# -->
# Note that this posterior is definitely not entirely Gaussian, look at the interval for a[2]



# ----------
# Check the marginal density for a[2] --> this is not entirely Gaussian
# Very many large values are plausible.
# Since actor number 2 always pulled the left-hand lever, the data cannot discriminate among these alrge values, as all of them guarantee pulling the left-hand lever.
table(d2[d2$actor == 2, "pulled_left"])

post <- extract.samples(mod4)

str(post)

dens(post$a[,2])



# ------------------------------------------------------------------------------
# plot posterior predictions to appreciate the way these individual intercepts influence fit
# ------------------------------------------------------------------------------
graphics.off()
par(mfrow=c(3,3))


# blue line: empirical proportion of left pulls in each treatment for a single individual
# black line and shaded region: the average predicted probability and its 89% interval.

for(chimp in 1:7){
  
  d.pred <- list(
    pulled_left = rep(0, 4), # empty outcome
    prosoc_left = c(0, 1, 0, 1), # right/left/right/left
    condition = c(0, 0, 1, 1),  # control/control/partner/partner
    actor = rep(chimp, 4)
  )
  
  
  link.mod4 <- link(mod4, data = d.pred)
  pred.p <- apply(link.mod4, 2, mean)
  pred.p.PI <- apply(link.mod4, 2, PI)
  
  
  # plot raw data, one trend for each of 7 individual chimpanzees
  plot(0, 0, type = "n", xlab = "prosoc_left/condition", ylab = "proportion pulled left", ylim = c(0, 1), xlim = c(1,4), yaxp = c(0, 1, 2))
  axis(1, at = 1:4, labels = c("0/0", "1/0", "0/1", "1/1"))
  mtext(paste("actor", chimp))
  
  p <- by(d$pulled_left, list(d$prosoc_left, d$condition, d$actor), mean)
  lines(1:4, as.vector(p[,,chimp]), col = rangi2, lwd = 2)
  lines(1:4, pred.p)
  shade(pred.p.PI, 1:4)
}


